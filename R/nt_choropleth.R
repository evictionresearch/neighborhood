#' Add a choropleth (filled polygon) layer
#'
#' @description
#' The workhorse map layer. Given a `mapgl` map and an `sf` polygon layer, it
#' colors features by `column`, then — unless you turn them off — adds a matching
#' legend, a hover tooltip, and a click popup. It picks sensible defaults from
#' the data so a one-liner looks right, while leaving every piece overridable.
#'
#' Coloring adapts to the column:
#'
#' * **Categorical** columns (factor/character) get one color per category. If
#'   the column is the neighborhood typology `nt_conc`, the canonical Eviction
#'   Research Network palette is used automatically (the same colors as
#'   [nt_pal()]); otherwise an evenly-spaced qualitative palette is generated.
#' * **Numeric** columns are binned with the ERN sequential ramp
#'   (yellow -> orange -> red -> purple). Use `type = "continuous"` for a smooth
#'   interpolation instead, and `breaks`/`colors`/`labels` to take full control.
#'
#' Small layers are embedded directly in the map; large ones can be served as
#' vector tiles — see `tiles`.
#'
#' @param map A map from [nt_maplibre()] (or [mapgl::maplibre()]).
#' @param data Polygon spatial data: an `sf` object, or a path to a shapefile /
#'   GeoJSON / GeoPackage, an `sfc`, or an `sp`/`terra` object (normalized to
#'   `sf` and re-projected to WGS84 as needed).
#' @param column Name of the column to map.
#' @param type One of `"auto"` (categorical for factor/character, binned for
#'   numeric), `"categorical"`, `"continuous"` (smooth interpolation), or
#'   `"step"` (binned).
#' @param breaks Numeric interior break thresholds for `"step"`/`"continuous"`.
#'   Length should be one less than the number of colors; if you pass the same
#'   number of breaks as colors (a leading floor, e.g. `c(0, 15, 25, 35)`), the
#'   floor is dropped. Defaults to data quantiles.
#' @param colors Fill colors. Categorical: one per category. Numeric: the ramp
#'   (defaults to the 4-class ERN ramp).
#' @param labels Legend labels for binned numeric maps (defaults to
#'   `c("Lower","Moderate","High","Extreme")` for a 4-class ramp, otherwise
#'   range labels like `"15–25"`).
#' @param fill_color Optional MapLibre paint expression (built with
#'   `mapgl::interpolate()`/`step_expr()`/`match_expr()` or a raw list) to use
#'   verbatim as the layer fill, taking full control of coloring. This is the
#'   escape hatch for reproducing bespoke maps — e.g. a no-renter (or
#'   missing-data) override wrapping an `interpolate`. When supplied, pass `colors`
#'   and `labels` if you also want a legend (one can't be inferred from a raw
#'   expression); the column is still used for the popup, tooltip, and `id`.
#' @param id Layer id. Defaults to a name derived from `column`.
#' @param legend `TRUE` (default), `FALSE`, or `"interactive"` for a clickable
#'   legend that filters the map.
#' @param legend_title Legend title (defaults to `column`).
#' @param popup Click popup. `TRUE` (default) builds a sensible popup from the
#'   data; `FALSE`/`NULL` for none; a column name to use an existing HTML column
#'   (e.g. one from [nt_popup()]); or a character vector of length `nrow(data)`.
#' @param tooltip Hover tooltip. `TRUE` (default) shows the mapped value;
#'   `FALSE`/`NULL` for none; or a column name / character vector.
#' @param opacity Fill opacity (default `0.7`).
#' @param outline_color Polygon outline color (default white).
#' @param na_color Color for missing values (default ERN grey `#C0C0C0`).
#' @param visible Logical; whether the layer starts visible (default `TRUE`).
#'   Set `FALSE` for all-but-one layer when building a multi-layer map with a
#'   layer switcher ([mapgl::add_layers_control()]), so only the first layer
#'   shows initially. Each layer's legend is tied to its layer and toggles with
#'   it.
#' @param tiles `"auto"` (default; inline if `nrow(data) <= max_inline`, else
#'   PMTiles), `"inline"`, or `"pmtiles"`. The PMTiles path needs the
#'   `tippecanoe` command-line tool; see [nt_pmtiles()].
#' @param max_inline Feature-count threshold for `"auto"` (default `25000`).
#' @param ... Passed to [mapgl::add_fill_layer()].
#' @return The map, with the choropleth layer (and legend/popup/tooltip) added.
#' @family ern_maps
#' @seealso [nt_maplibre()], [nt_popup()], [nt_pmtiles()];
#'   [mapgl::add_fill_layer()] and [mapgl::step_expr()] underneath.
#' @examplesIf interactive() && requireNamespace("mapgl", quietly = TRUE)
#' md <- ntdf(state = "MD", county = "Baltimore City", geometry = TRUE)
#'
#' # Categorical typologies, ERN colors, auto legend + popup
#' nt_maplibre(md) |> nt_add_choropleth(md, "nt_conc")
#'
#' # A numeric column, binned with custom thresholds and an interactive legend
#' nt_maplibre(md) |>
#'   nt_add_choropleth(md, "pBlack", breaks = c(0.25, 0.5, 0.75),
#'                     legend = "interactive", legend_title = "% Black")
#'
#' # Multiple switchable layers: typology + each demographic share. All but the
#' # first start hidden; a layer control switches between them (legends follow).
#' demos <- c(White = "pWhite", Black = "pBlack",
#'            Asian = "pAsian", Latine = "pLatine")
#' m <- nt_maplibre(md) |>
#'   nt_add_choropleth(md, "nt_conc", id = "Typology")
#' for (i in seq_along(demos)) {
#'   m <- nt_add_choropleth(m, md, demos[i], id = names(demos)[i],
#'                          legend_title = paste("Share", names(demos)[i]),
#'                          visible = FALSE)
#' }
#' m |> mapgl::add_layers_control(layers = c("Typology", names(demos)))
#' @export
nt_add_choropleth <- function(map, data, column,
                              type = c("auto", "categorical", "continuous", "step"),
                              breaks = NULL, colors = NULL, labels = NULL,
                              fill_color = NULL,
                              id = NULL, legend = TRUE, legend_title = NULL,
                              popup = TRUE, tooltip = TRUE,
                              opacity = 0.7, outline_color = "#ffffff",
                              na_color = NULL, visible = TRUE,
                              tiles = c("auto", "inline", "pmtiles"),
                              max_inline = 25000, ...) {
  type  <- match.arg(type)
  tiles <- match.arg(tiles)
  data <- .nt_ensure_wgs84(.nt_as_sf(data))
  # Strip any name attribute: a named column (e.g. from `demos[i]` in a loop)
  # would otherwise serialize into the MapLibre ["get", column] expression as an
  # object and the layer would be silently rejected by the browser.
  column <- unname(as.character(column))[1]
  if (is.null(id)) id <- paste0("nt_", gsub("[^A-Za-z0-9]", "_", column))
  vis <- if (isTRUE(visible)) "visible" else "none"

  # Fill: a caller-supplied `fill_color` expression takes full control (used to
  # reproduce bespoke maps — e.g. a no-renter `case` wrapping an `interpolate`).
  # Otherwise build the ERN-styled expression from the column.
  if (!is.null(fill_color)) {
    layer_fill <- fill_color
    spec <- NULL
  } else {
    spec <- .nt_fill_spec(data, column, type = type, breaks = breaks,
                          colors = colors, labels = labels, na_color = na_color)
    layer_fill <- spec$fill_color
  }

  # --- Resolve popup / tooltip into column names on a working copy ----------
  pt <- .nt_resolve_popup_tooltip(data, column, popup, tooltip)
  data <- pt$data

  # --- Decide inline vs vector tiles ----------------------------------------
  use_pmtiles <- switch(tiles,
    inline  = FALSE,
    pmtiles = TRUE,
    auto    = nrow(data) > max_inline
  )
  if (use_pmtiles && Sys.which("tippecanoe") == "") {
    if (tiles == "auto") {
      warning(sprintf(
        paste0("%d features exceed max_inline (%d) but tippecanoe is not installed; ",
               "drawing inline (may be slow). Install tippecanoe or pass tiles = \"inline\"."),
        nrow(data), max_inline), call. = FALSE)
      use_pmtiles <- FALSE
    } else {
      stop("tiles = \"pmtiles\" requires the tippecanoe command-line tool. ",
           "Install it (e.g. `brew install tippecanoe`) or use tiles = \"inline\".",
           call. = FALSE)
    }
  }

  if (use_pmtiles) {
    tile_path <- nt_pmtiles(data, layer = "nt", quiet = TRUE)
    src_id <- paste0(id, "_src")
    map <- mapgl::add_pmtiles_source(map, id = src_id, url = tile_path,
                                     promote_id = if ("GEOID" %in% names(data)) "GEOID" else NULL)
    map <- mapgl::add_fill_layer(
      map, id = id, source = src_id, source_layer = "nt",
      fill_color = layer_fill, fill_opacity = opacity,
      fill_outline_color = outline_color, visibility = vis,
      popup = pt$popup, tooltip = pt$tooltip, ...
    )
  } else {
    map <- mapgl::add_fill_layer(
      map, id = id, source = data,
      fill_color = layer_fill, fill_opacity = opacity,
      fill_outline_color = outline_color, visibility = vis,
      popup = pt$popup, tooltip = pt$tooltip, ...
    )
  }

  # --- Legend ----------------------------------------------------------------
  if (!is.null(legend) && !isFALSE(legend)) {
    if (is.null(spec)) {
      # custom fill_color: build a legend only if the caller supplied the
      # colors + labels to describe it (we can't infer one from a raw expression)
      if (!is.null(colors) && !is.null(labels)) {
        spec_leg <- list(legend_type = "categorical",
                         legend_values = labels, legend_colors = colors)
        map <- .nt_add_legend(map, spec_leg, column, legend, legend_title, id)
      }
    } else {
      map <- .nt_add_legend(map, spec, column, legend, legend_title, id)
    }
  }
  map
}

# Build the default popup HTML for a choropleth: a per-tract title plus the
# mapped column (and county/place name when handy).
.nt_default_popup <- function(data, column) {
  # A numeric GEOID would otherwise be comma-formatted (27,053,026,200) by the
  # generic field formatter; keep id-like titles as plain digit strings.
  if ("GEOID" %in% names(data) && is.numeric(data$GEOID)) {
    data$GEOID <- format(data$GEOID, scientific = FALSE, trim = TRUE)
  }
  title <- if ("GEOID" %in% names(data)) "GEOID" else NULL
  flds <- stats::setNames(column, column)
  if ("county" %in% names(data) && column != "county") {
    flds <- c(stats::setNames("county", "County"), flds)
  } else if ("NAME" %in% names(data) && column != "NAME") {
    flds <- c(stats::setNames("NAME", "Area"), flds)
  }
  nt_popup(data, title = title, fields = flds)
}

# Turn the popup/tooltip arguments (TRUE/FALSE/colname/vector) into actual
# column names on a (possibly augmented) copy of the data.
.nt_resolve_popup_tooltip <- function(data, column, popup, tooltip) {
  popup_col <- NULL
  if (isTRUE(popup)) {
    data[[".nt_popup"]] <- .nt_default_popup(data, column)
    popup_col <- ".nt_popup"
  } else if (is.character(popup) && length(popup) == 1L && popup %in% names(data)) {
    popup_col <- popup
  } else if (is.character(popup) && length(popup) == nrow(data)) {
    data[[".nt_popup"]] <- popup
    popup_col <- ".nt_popup"
  }

  tooltip_col <- NULL
  if (isTRUE(tooltip)) {
    tooltip_col <- column
  } else if (is.character(tooltip) && length(tooltip) == 1L && tooltip %in% names(data)) {
    tooltip_col <- tooltip
  } else if (is.character(tooltip) && length(tooltip) == nrow(data)) {
    data[[".nt_tooltip"]] <- tooltip
    tooltip_col <- ".nt_tooltip"
  }

  list(data = data, popup = popup_col, tooltip = tooltip_col)
}

# Add the legend that matches a fill spec.
.nt_add_legend <- function(map, spec, column, legend, legend_title, id = NULL) {
  lt <- legend_title %nt||% column
  interactive <- identical(legend, "interactive")

  # If the map already carries a legend (i.e. this is a 2nd+ choropleth layer),
  # APPEND rather than replace, and tie each legend to its layer via layer_id so
  # it shows/hides with the layer when a layers control toggles it.
  has_legend <- !is.null(map$x$legend_html) &&
    any(nzchar(unlist(map$x$legend_html)))

  if (identical(spec$legend_type, "continuous")) {
    return(mapgl::add_continuous_legend(
      map, legend_title = lt,
      values = .nt_fmt(spec$legend_values),
      colors = spec$legend_colors,
      add = has_legend, layer_id = id
    ))
  }

  # categorical (incl. binned numeric)
  args <- list(
    map, legend_title = lt,
    values = spec$legend_values, colors = spec$legend_colors,
    interactive = interactive, add = has_legend, layer_id = id
  )
  if (interactive) {
    args$filter_column <- column
    if (!is.null(spec$breaks)) {
      args$breaks <- spec$breaks            # binned numeric filtering
    } else {
      args$filter_values <- spec$legend_values  # categorical filtering
    }
  }
  do.call(mapgl::add_categorical_legend, args)
}
