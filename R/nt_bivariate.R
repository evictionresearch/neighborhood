#' Bivariate (two-way) color grid
#'
#' @description
#' Builds the `n * n` grid of colors a bivariate choropleth needs, where one
#' variable varies across the columns and the other up the rows. Returned as a
#' named character vector whose names are `"<x class>-<y class>"`, which is the
#' key [nt_add_bivariate()] matches on.
#'
#' At `n = 3` the named palettes return their published values verbatim (the
#' Stevens grids), because those middle tones were hand-chosen and a mechanical
#' blend does not reproduce them. At any other `n` the grid is interpolated
#' bilinearly between the four corner colors, so the corners stay exact and the
#' interior spaces evenly.
#'
#' @param n Number of classes per variable (default `3`). Two gives a coarse
#'   four-cell grid; four is about the most a reader can still decode.
#' @param palette One of `"bluered"` (default, the Stevens blue/red grid),
#'   `"purplegreen"`, or `"ern"` (built from the Eviction Research Network
#'   house colors: the pale tint at the base, house red across, house blue-navy
#'   up, and their dark mix in the corner).
#' @return A named character vector of `n * n` hex colors, names `"x-y"`. Carries
#'   attributes `n`, `palette`, and `corners`.
#' @family ern_maps
#' @seealso [nt_add_bivariate()], [nt_pal()]
#' @examples
#' nt_bivariate_pal()
#' nt_bivariate_pal(n = 2, palette = "ern")
#' @export
nt_bivariate_pal <- function(n = 3, palette = c("bluered", "purplegreen", "ern")) {
  palette <- match.arg(palette)
  n <- as.integer(n)
  if (is.na(n) || n < 2L || n > 6L) {
    stop("`n` must be a whole number between 2 and 6.", call. = FALSE)
  }

  corners <- .nt_biv_corners[[palette]]

  out <- if (n == 3L && !is.null(.nt_biv_published[[palette]])) {
    .nt_biv_published[[palette]]
  } else {
    .nt_biv_interpolate(corners, n)
  }

  attr(out, "n") <- n
  attr(out, "palette") <- palette
  attr(out, "corners") <- corners
  out
}

# Four corners of each grid: base is low/low, x is high on the x variable only,
# y is high on the y variable only, xy is the both-high corner.
.nt_biv_corners <- list(
  bluered     = c(base = "#E8E8E8", x = "#C85A5A", y = "#64ACBE", xy = "#574249"),
  purplegreen = c(base = "#E8E8E8", x = "#C86ABE", y = "#7FBF7B", xy = "#3F4A3C"),
  ern         = c(base = "#E8EEF4", x = "#C33127", y = "#223754", xy = "#3D2B33")
)

# Published 3 x 3 grids, kept verbatim rather than regenerated.
.nt_biv_published <- list(
  bluered = c(
    "1-1" = "#E8E8E8", "2-1" = "#E4ACAC", "3-1" = "#C85A5A",
    "1-2" = "#B0D5DF", "2-2" = "#AD9EA5", "3-2" = "#985356",
    "1-3" = "#64ACBE", "2-3" = "#627F8C", "3-3" = "#574249"
  ),
  purplegreen = c(
    "1-1" = "#E8E8E8", "2-1" = "#E4ACD3", "3-1" = "#C86ABE",
    "1-2" = "#ACE4B8", "2-2" = "#AD9EA5", "3-2" = "#8A6A8C",
    "1-3" = "#7FBF7B", "2-3" = "#6A8C6E", "3-3" = "#3F4A3C"
  )
)

# Bilinear blend between the four corners. u runs along x, v along y, so the
# corner colors come back exactly and the interior spaces evenly between them.
.nt_biv_interpolate <- function(corners, n) {
  rgb_of <- function(hex) grDevices::col2rgb(hex)[, 1]
  cb <- rgb_of(corners[["base"]]); cx <- rgb_of(corners[["x"]])
  cy <- rgb_of(corners[["y"]]);    cxy <- rgb_of(corners[["xy"]])

  grid <- expand.grid(xi = seq_len(n), yi = seq_len(n))
  cols <- vapply(seq_len(nrow(grid)), function(k) {
    u <- (grid$xi[k] - 1L) / (n - 1L)
    v <- (grid$yi[k] - 1L) / (n - 1L)
    mix <- cb * (1 - u) * (1 - v) + cx * u * (1 - v) +
           cy * (1 - u) * v      + cxy * u * v
    grDevices::rgb(mix[1], mix[2], mix[3], maxColorValue = 255)
  }, character(1))

  stats::setNames(cols, paste0(grid$xi, "-", grid$yi))
}


#' Add a bivariate (two-way) choropleth layer
#'
#' @description
#' Maps two variables at once by crossing them into an `n * n` color grid: one
#' varies across, the other up, and the corner cell is where both run high. This
#' is the layer to reach for when the question is about overlap rather than
#' either measure on its own, e.g. where high veteran density coincides with
#' high housing precarity.
#'
#' Each variable is cut into `n` classes (quantiles by default, so each class
#' holds roughly the same number of features), the pair is matched to a color
#' from [nt_bivariate_pal()], and a two-dimensional grid legend is drawn instead
#' of the usual linear one.
#'
#' The layer is built through [nt_add_choropleth()], so popups, tooltips,
#' PMTiles handling, and the `visible = FALSE` convention for multi-layer maps
#' all behave exactly as they do elsewhere. The legend is bound to the layer id,
#' so it shows and hides with the layer under [nt_layers_control()] and
#' [nt_sync_maps()].
#'
#' @param map A map from [nt_maplibre()] (or [mapgl::maplibre()]).
#' @param data Polygon spatial data, as for [nt_add_choropleth()].
#' @param x Name of the column mapped across the grid (the columns).
#' @param y Name of the column mapped up the grid (the rows).
#' @param n Classes per variable (default `3`). Passed to [nt_bivariate_pal()].
#' @param palette Passed to [nt_bivariate_pal()].
#' @param x_breaks,y_breaks Optional interior break thresholds, length `n - 1`.
#'   Default is quantiles, which keeps the classes equal-count. Supply these to
#'   cut on substantive thresholds instead.
#' @param x_label,y_label Axis labels for the legend (default the column names).
#' @param id Layer id. Defaults to `"<x>_x_<y>"`.
#' @param legend `TRUE` (default) to draw the grid legend, `FALSE` for none.
#' @param legend_position One of `"top-left"` (default), `"top-right"`,
#'   `"bottom-left"`, `"bottom-right"`.
#' @param popup,tooltip,opacity,outline_color,na_color,visible,tiles,max_inline
#'   Passed straight to [nt_add_choropleth()].
#' @param ... Passed to [nt_add_choropleth()].
#' @return The map, with the bivariate layer and its grid legend added.
#' @family ern_maps
#' @seealso [nt_bivariate_pal()], [nt_add_choropleth()], [nt_layers_control()]
#' @examplesIf interactive() && requireNamespace("mapgl", quietly = TRUE)
#' md <- ntdf(state = "MD", county = "Baltimore City", geometry = TRUE)
#' nt_maplibre(md) |>
#'   nt_add_bivariate(md, x = "pBlack", y = "pRent",
#'                    x_label = "Share Black", y_label = "Share renting")
#' @export
nt_add_bivariate <- function(map, data, x, y,
                             n = 3,
                             palette = c("bluered", "purplegreen", "ern"),
                             x_breaks = NULL, y_breaks = NULL,
                             x_label = NULL, y_label = NULL,
                             id = NULL, legend = TRUE,
                             legend_position = c("top-left", "top-right",
                                                 "bottom-left", "bottom-right"),
                             popup = TRUE, tooltip = TRUE,
                             opacity = 0.7, outline_color = "#ffffff",
                             na_color = NULL, visible = TRUE,
                             tiles = c("auto", "inline", "pmtiles"),
                             max_inline = 25000, ...) {
  palette <- match.arg(palette)
  legend_position <- match.arg(legend_position)
  tiles <- match.arg(tiles)

  x <- unname(as.character(x)); y <- unname(as.character(y))
  data <- .nt_ensure_wgs84(.nt_as_sf(data))

  for (col in c(x, y)) {
    if (!col %in% names(data)) {
      stop(sprintf("Column `%s` is not in `data`.", col), call. = FALSE)
    }
    if (!is.numeric(data[[col]])) {
      stop(sprintf("`%s` must be numeric; bivariate maps cross two measures.", col),
           call. = FALSE)
    }
  }

  pal <- nt_bivariate_pal(n = n, palette = palette)
  n <- attr(pal, "n")

  xb <- .nt_biv_breaks(data[[x]], n, x_breaks, "x_breaks")
  yb <- .nt_biv_breaks(data[[y]], n, y_breaks, "y_breaks")

  xi <- .nt_biv_class(data[[x]], xb)
  yi <- .nt_biv_class(data[[y]], yb)

  # A tract missing either measure cannot be placed on the grid; leave it NA so
  # nt_add_choropleth paints it with na_color rather than guessing a cell.
  cls <- ifelse(is.na(xi) | is.na(yi), NA_character_, paste0(xi, "-", yi))

  # Name the class column after the layer so two bivariate layers on one map
  # cannot collide on a shared column name.
  id <- id %nt||% paste0(x, "_x_", y)
  col_nm <- paste0(".biv_", gsub("[^A-Za-z0-9]+", "_", id))
  data[[col_nm]] <- factor(cls, levels = names(pal))

  map <- nt_add_choropleth(
    map, data, column = col_nm,
    type = "categorical",
    colors = unname(pal[levels(data[[col_nm]])]),
    id = id, legend = FALSE,
    popup = popup, tooltip = tooltip,
    opacity = opacity, outline_color = outline_color,
    na_color = na_color, visible = visible,
    tiles = tiles, max_inline = max_inline, ...
  )

  if (isTRUE(legend)) {
    map <- .nt_biv_legend(
      map, pal = pal, n = n, id = id, visible = visible,
      x_label = x_label %nt||% x, y_label = y_label %nt||% y,
      position = legend_position
    )
  }
  map
}

# Interior break thresholds for one variable: quantiles unless supplied.
.nt_biv_breaks <- function(values, n, breaks, arg) {
  if (is.null(breaks)) return(.nt_quantile_breaks(values, n))
  breaks <- sort(unique(as.numeric(breaks)))
  if (length(breaks) != n - 1L) {
    stop(sprintf("`%s` must have %d interior break(s) for n = %d.",
                 arg, n - 1L, n), call. = FALSE)
  }
  breaks
}

# Class index 1..n. A value sitting exactly on a break belongs to the class
# ABOVE it, so the top class is never left empty by a degenerate quantile.
# findInterval's defaults give that; rightmost.closed = TRUE would not, since it
# closes the top interval on the right and drops values equal to the highest
# break back into the class below.
.nt_biv_class <- function(values, breaks) {
  if (length(breaks) == 0L) return(ifelse(is.na(values), NA_integer_, 1L))
  idx <- findInterval(values, breaks) + 1L
  idx[is.na(values)] <- NA_integer_
  idx
}

# The two-dimensional grid legend. Built as its own DOM node rather than through
# mapgl's legend helpers, which draw a one-dimensional list. It carries the
# mapboxgl-legend class and a data-layer-id so nt_layers_control() and
# nt_sync_maps() toggle it with the layer exactly like any other legend.
.nt_biv_legend <- function(map, pal, n, id, visible, x_label, y_label, position) {
  esc <- function(s) gsub('"', '\\\\"', gsub("\\\\", "\\\\\\\\", s))

  cells <- vapply(seq_len(n), function(row) {
    yi <- n - row + 1L
    paste0(vapply(seq_len(n), function(xi) {
      sprintf("<div style=\\\"background:%s;width:17px;height:17px\\\"></div>",
              unname(pal[[paste0(xi, "-", yi)]]))
    }, character(1)), collapse = "")
  }, character(1))
  grid_html <- paste0(cells, collapse = "")

  js <- paste0(
    "function(el, x){\n",
    "  var map = el.map || (window.HTMLWidgets && HTMLWidgets.getInstance(el) ? HTMLWidgets.getInstance(el).getMap() : null);\n",
    "  if(!map) return;\n",
    "  var places = {'top-left':'top:10px;left:10px;','top-right':'top:10px;right:10px;',",
    "'bottom-left':'bottom:28px;left:10px;','bottom-right':'bottom:28px;right:10px;'};\n",
    "  var box = document.createElement('div');\n",
    "  box.className = 'mapboxgl-legend nt-bivariate-legend';\n",
    "  box.setAttribute('data-layer-id', \"", esc(id), "\");\n",
    "  box.style.cssText = 'position:absolute;z-index:3;background:#fff;padding:9px 11px;",
    "border-radius:4px;box-shadow:0 1px 4px rgba(25,34,44,.25);",
    "font-family:Inter,-apple-system,system-ui,sans-serif;font-size:11px;color:#19222C;' + ",
    "(places[\"", position, "\"] || places['top-left']);\n",
    "  box.innerHTML = ''\n",
    "    + '<div style=\"display:flex;align-items:stretch;gap:5px\">'\n",
    "    +   '<div style=\"writing-mode:vertical-rl;transform:rotate(180deg);",
    "text-align:center;white-space:nowrap;font-size:10.5px;color:#586573\">", esc(y_label), " &#9650;</div>'\n",
    "    +   '<div style=\"display:grid;grid-template-columns:repeat(", n, ",17px);gap:1px\">", grid_html, "</div>'\n",
    "    + '</div>'\n",
    "    + '<div style=\"margin-left:16px;margin-top:3px;text-align:center;white-space:nowrap;",
    "font-size:10.5px;color:#586573\">", esc(x_label), " &#9654;</div>';\n",
    "  if(", if (isTRUE(visible)) "false" else "true", ") box.style.display = 'none';\n",
    "  el.appendChild(box);\n",
    "}"
  )
  htmlwidgets::onRender(map, js)
}
