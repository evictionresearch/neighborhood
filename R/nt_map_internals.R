# Internal constants and helpers for the MapLibre mapping functions.
#
# These are NOT exported. They centralize the Eviction Research Network (ERN)
# house style -- the neighborhood-typology color key, the sequential risk ramp,
# and the small bits of geometry/CRS/expression plumbing -- so that nt_pal(),
# nt_add_choropleth(), and the legend builders can never drift apart.
#
# Built with assistance from Claude Opus 4.8 (Anthropic). The mapping layer is
# a thin, deterministic wrapper around Kyle Walker's mapgl package.

# ---------------------------------------------------------------------------
# Color key for the 19 neighborhood-typology levels (nt_conc).
# The order matches the factor levels created in ntdf() exactly, so
# unname(.nt_colors) can be handed to leaflet::colorFactor() (which maps the
# i-th color to the i-th factor level) and the named vector can be handed to
# mapgl::match_expr() for MapLibre fills. One source of truth, two consumers.
# ---------------------------------------------------------------------------
.nt_colors <- c(
  "Mostly Asian"      = "#33a02c",
  "Mostly Black"      = "#1f78b4",
  "Mostly Latine"     = "#e31a1c",
  "Mostly Other"      = "#9b66b0",
  "Mostly White"      = "#C95123",
  "Asian-Black"       = "#1fc2ba",
  "Asian-Latine"      = "#d6ae5c",
  "Asian-Other"       = "#91c7b9",
  "Asian-White"       = "#b2df8a",
  "Black-Latine"      = "#de4e4b",
  "Black-Other"       = "#71a1f5",
  "Black-White"       = "#a6cee3",
  "Latine-Other"      = "#f0739b",
  "Latine-White"      = "#fb9a99",
  "Other-White"       = "#c28a86",
  "3 Group Mixed"     = "#fdbf6f",
  "4 Group Mixed"     = "#cab2d6",
  "Diverse"           = "#1d5fd1",
  "Unpopulated Tract" = "#FFFFFF"
)

# Grey for missing/NA categories (matches the historical nt_pal na.color).
.nt_na_color <- "#C0C0C0"

# ERN sequential ramp + tier labels, used for numeric (continuous / binned)
# choropleths. This is the same 4-class yellow -> orange -> red -> purple ramp
# used across the HPRM, Minnesota, and Washington state-profile maps.
.ern_ramp   <- c("#fed976", "#fd8d3c", "#F9322B", "#54278f")
.ern_labels <- c("Lower", "Moderate", "High", "Extreme")

# ERN editorial brand tokens (from the state-profile CSS :root). Shared by the
# chart functions so the "newspaper graphic" look stays consistent.
.ern_brand <- list(
  navy      = "#19222C",
  navy_soft = "#223754",
  steel     = "#8BA3BE",
  accent    = "#F9322B",
  accent_deep = "#D6231C",
  muted     = "#6c7a89",
  paper     = "#ffffff"
)
.ern_font <- "Inter, -apple-system, system-ui, 'Helvetica Neue', Arial, sans-serif"

# Default qualitative series palette for charts: navy first (the editorial trend
# line), then accent and supporting tones for multi-series charts.
.nt_chart_palette <- c("#19222C", "#F9322B", "#223754", "#8BA3BE",
                       "#C95123", "#33a02c", "#9b66b0", "#1f78b4")

# Null-coalescing helper (avoids importing rlang's %||% just for this).
`%nt||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

# ---------------------------------------------------------------------------
# Validation + CRS plumbing
# ---------------------------------------------------------------------------

# Coerce a user's spatial input into an sf object. Accepts, in order:
#   * an sf object (returned as-is);
#   * a length-1 path to any file sf can read (.shp, .geojson, .gpkg, .kml, ...)
#     or a directory/datasource string -> read with sf::st_read();
#   * a bare sfc geometry column -> wrapped into an sf;
#   * an sp Spatial* object or a terra SpatVector -> converted with st_as_sf();
#   * anything else st_as_sf() understands (e.g. a data frame with a geometry
#     list-column).
# sf is the canonical spatial representation; everything is normalized to it.
# A non-spatial data frame (the common mistake, since us_nt_tracts2024 and the
# default ntdf() are non-spatial) gets an actionable message.
.nt_as_sf <- function(data, arg = "data") {
  if (inherits(data, "sf")) return(data)

  # A path (or any OGR/GDAL data-source string) to a shapefile, GeoJSON, etc.
  if (is.character(data) && length(data) == 1L) {
    looks_like_path <- file.exists(data) || grepl("\\.(shp|geojson|json|gpkg|kml|gml|gdb|gpx)$",
                                                   data, ignore.case = TRUE)
    if (!looks_like_path) {
      stop(sprintf("`%s` is a string but not a readable spatial file path.", arg),
           call. = FALSE)
    }
    out <- sf::st_read(data, quiet = TRUE)
    if (!inherits(out, "sf")) {
      stop(sprintf("Reading `%s` did not yield spatial features.", arg), call. = FALSE)
    }
    return(out)
  }

  # A bare geometry column.
  if (inherits(data, "sfc")) return(sf::st_sf(geometry = data))

  # sp Spatial* or terra SpatVector, or any class with an st_as_sf method.
  if (inherits(data, c("Spatial", "SpatVector"))) {
    return(sf::st_as_sf(data))
  }
  coerced <- tryCatch(sf::st_as_sf(data), error = function(e) NULL)
  if (inherits(coerced, "sf")) return(coerced)

  stop(
    sprintf(
      paste0(
        "`%s` is not spatial and could not be converted to sf.\n",
        "  Pass an sf object, an sfc, an sp/terra object, or a path to a\n",
        "  shapefile/GeoJSON/GeoPackage. To map a non-spatial table such as\n",
        "  us_nt_tracts2024, attach geometry first:\n",
        "    - ntdf(state, county, geometry = TRUE), or\n",
        "    - tigris::tracts(state, county, cb = TRUE, year = 2024) |>\n",
        "        dplyr::left_join(your_table, by = \"GEOID\")"
      ),
      arg
    ),
    call. = FALSE
  )
}

# Ensure data is in WGS84 (EPSG:4326), which MapLibre requires.
# If the CRS is missing but coordinates look like lon/lat, assume 4326 and say
# so; if it is set but different, transform; otherwise pass through untouched.
.nt_ensure_wgs84 <- function(data) {
  crs <- sf::st_crs(data)
  if (is.na(crs)) {
    bb <- sf::st_bbox(data)
    if (bb[["xmin"]] >= -180 && bb[["xmax"]] <= 180 &&
        bb[["ymin"]] >= -90  && bb[["ymax"]] <= 90) {
      sf::st_crs(data) <- 4326
      message("nt: input CRS was missing; assuming WGS84 (EPSG:4326).")
    } else {
      stop(
        "`data` has no CRS and its coordinates are not lon/lat. ",
        "Set one with sf::st_crs(data) <- <epsg> before mapping.",
        call. = FALSE
      )
    }
    return(data)
  }
  if (crs != sf::st_crs(4326)) {
    data <- sf::st_transform(data, 4326)
  }
  data
}

# Interior point per feature, suitable for placing a label. point_on_surface
# (not centroid) guarantees the point falls inside irregular polygons.
.nt_label_points <- function(data) {
  data <- .nt_ensure_wgs84(data)
  suppressWarnings(sf::st_point_on_surface(data))
}

# ---------------------------------------------------------------------------
# Categorical color resolution
# ---------------------------------------------------------------------------

# Resolve colors for a categorical column. If every observed value is a known
# neighborhood-typology level, use the canonical .nt_colors; otherwise fall
# back to an evenly-spaced qualitative palette (deterministic, no extra deps).
# Returns a list(values = <levels in display order>, colors = <hex>).
.nt_categorical_colors <- function(x, colors = NULL) {
  if (is.factor(x)) {
    vals <- levels(x)[levels(x) %in% unique(as.character(x))]
  } else {
    vals <- sort(unique(as.character(x[!is.na(x)])))
  }
  if (!is.null(colors)) {
    if (length(colors) < length(vals)) {
      stop("`colors` has fewer entries than the column has categories.",
           call. = FALSE)
    }
    return(list(values = vals, colors = unname(colors)[seq_along(vals)]))
  }
  if (all(vals %in% names(.nt_colors))) {
    return(list(values = vals, colors = unname(.nt_colors[vals])))
  }
  list(values = vals, colors = grDevices::hcl.colors(length(vals), "Dark 3"))
}

# ---------------------------------------------------------------------------
# Numeric break computation + label formatting
# ---------------------------------------------------------------------------

# Quantile breakpoints (interior thresholds) for `n_classes` bins.
# Returns length n_classes - 1, de-duplicated.
.nt_quantile_breaks <- function(values, n_classes) {
  values <- values[is.finite(values)]
  if (length(values) == 0L) return(numeric(0))
  probs <- seq_len(n_classes - 1L) / n_classes
  brks <- unname(stats::quantile(values, probs = probs, na.rm = TRUE, names = FALSE))
  brks <- round(brks, 2)
  unique(brks)
}

# Human range labels for a step/binned scale, e.g. c("< 15","15-25","25-35","35+").
# Uses a typographic en-dash between bounds, built with intToUtf8() so the source
# stays pure ASCII (a portable-package requirement).
.nt_range_labels <- function(breaks) {
  en_dash <- intToUtf8(8211L)
  k <- length(breaks) + 1L
  labs <- character(k)
  labs[1] <- paste0("< ", breaks[1])
  if (k > 2L) {
    for (i in 2:(k - 1L)) labs[i] <- paste0(breaks[i - 1L], en_dash, breaks[i])
  }
  labs[k] <- paste0(breaks[k - 1L], "+")
  labs
}

# ---------------------------------------------------------------------------
# Fill-color expression builder
# ---------------------------------------------------------------------------
#
# Given the data, a column, and a type, return everything nt_add_choropleth
# needs: the mapgl paint expression plus the matching legend specification.
# Dispatches on `type`:
#   "categorical" -> match_expr() + categorical legend
#   "step"        -> step_expr()  + categorical (binned) legend
#   "continuous"  -> interpolate() + continuous legend
#   "auto"        -> categorical for factor/character, step for numeric
#
# `breaks` for numeric types are INTERIOR thresholds. As a convenience, if the
# caller passes one more break than there are classes (the "[0,15,25,35]"
# config style with an explicit floor), the leading floor is dropped.
.nt_fill_spec <- function(data, column,
                          type = c("auto", "categorical", "continuous", "step"),
                          breaks = NULL, colors = NULL, labels = NULL,
                          na_color = NULL) {
  type <- match.arg(type)
  if (!column %in% names(data)) {
    stop(sprintf("Column \"%s\" not found in the data.", column), call. = FALSE)
  }
  x <- data[[column]]
  if (type == "auto") {
    type <- if (is.numeric(x)) "step" else "categorical"
  }

  if (type == "categorical") {
    cc <- .nt_categorical_colors(x, colors)
    na_col <- na_color %nt||% .nt_na_color
    expr <- mapgl::match_expr(
      column = column, values = cc$values, stops = cc$colors, default = na_col
    )
    return(list(
      fill_color   = expr,
      legend_type  = "categorical",
      legend_values = cc$values,
      legend_colors = cc$colors,
      type         = type
    ))
  }

  # numeric: continuous or step
  colors <- colors %nt||% .ern_ramp
  n_classes <- length(colors)
  if (is.null(breaks)) {
    breaks <- .nt_quantile_breaks(as.numeric(x), n_classes)
  } else {
    # drop an explicit leading floor if the caller passed n_classes breaks
    if (length(breaks) == n_classes) breaks <- breaks[-1L]
  }
  if (length(breaks) != (n_classes - 1L)) {
    stop(sprintf(
      "Expected %d interior break(s) for %d colors, got %d. Pass `breaks` of length %d (or %d with an explicit floor).",
      n_classes - 1L, n_classes, length(breaks), n_classes - 1L, n_classes
    ), call. = FALSE)
  }

  if (type == "continuous") {
    # interpolate uses paired value/color stops; use the bin edges as anchors.
    vals <- c(min(as.numeric(x), na.rm = TRUE), breaks)
    expr <- mapgl::interpolate(
      column = column, type = "linear", values = vals, stops = colors,
      na_color = na_color %nt||% .nt_na_color
    )
    return(list(
      fill_color    = expr,
      legend_type   = "continuous",
      legend_values = vals,
      legend_colors = colors,
      type          = type
    ))
  }

  # step (binned)
  expr <- mapgl::step_expr(
    column = column, base = colors[1], values = breaks, stops = colors[-1],
    na_color = na_color %nt||% .nt_na_color
  )
  leg_labels <- labels %nt||% if (n_classes == length(.ern_labels)) {
    .ern_labels
  } else {
    .nt_range_labels(breaks)
  }
  list(
    fill_color    = expr,
    legend_type   = "categorical",
    legend_values = leg_labels,
    legend_colors = colors,
    breaks        = breaks,
    type          = type
  )
}

# ---------------------------------------------------------------------------
# Standard ERN control set
# ---------------------------------------------------------------------------
.nt_add_controls <- function(map, search = TRUE) {
  map <- mapgl::add_navigation_control(map)
  map <- mapgl::add_fullscreen_control(map)
  map <- mapgl::add_geolocate_control(map)
  if (isTRUE(search)) {
    # provider = "osm" -> OpenStreetMap/Nominatim, no API key required.
    map <- mapgl::add_geocoder_control(map, provider = "osm",
                                       placeholder = "Search location")
  }
  map
}
