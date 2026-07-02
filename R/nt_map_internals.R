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

# ERN editorial brand tokens, synced to the canonical brand guide
# (cidrlab/library/brand/ern/BRAND.md). The red rule, verified against WCAG 2.1:
#   * accent  #F9322B -- GRAPHICS red (fills, lines, markers, map washes).
#     3.8:1 on white / 3.3:1 on tint: passes 1.4.11 non-text contrast (>= 3:1)
#     and large text, but FAILS AA body text (needs 4.5:1).
#   * accent_deep #CC2118 -- TEXT red (direct labels, annotations, links).
#     5.5:1 on white / 4.7:1 on tint: passes AA. Never put red text on navy.
#   * accent_deeper #B01D16 -- hover / active / deep emphasis (6.9:1).
# steel #8BA3BE is dark-background-only as text (2.6:1 on white); steel_chart
# #7B96B5 is the same hue darkened just enough to clear 3:1 as a chart fill.
.ern_brand <- list(
  navy          = "#19222C",
  navy_soft     = "#223754",
  steel         = "#8BA3BE",
  steel_chart   = "#7B96B5",
  accent        = "#F9322B",
  accent_deep   = "#CC2118",
  accent_deeper = "#B01D16",
  muted         = "#586573",
  tint          = "#e8eef4",
  paper         = "#ffffff"
)
.ern_font <- "Inter, -apple-system, system-ui, 'Helvetica Neue', Arial, sans-serif"

# Default qualitative series palette for charts, brand triad first: blue navy
# (the editorial trend line), brand red, then supporting tones. Ordered so the
# first four -- the common chart sizes -- are the most colorblind-separable
# subset (gold arrives fifth: it collides with red under deuteranopia, and by
# then the house style's direct labels carry identification). Every fill
# clears WCAG 1.4.11 (>= 3:1 on white); see ern_swatch() for receipts.
.nt_chart_palette <- c("#223754", "#F9322B", "#7B96B5", "#2A8A86",
                       "#C8860D", "#5A3A88", "#2C6B3A", "#586573")

# Pre-1.1.0 qualitative palette, kept verbatim so published figures can be
# reproduced exactly (ern_palette("legacy")).
.nt_chart_palette_legacy <- c("#19222C", "#F9322B", "#223754", "#8BA3BE",
                              "#C95123", "#33a02c", "#9b66b0", "#1f78b4")

# Red-free qualitative palette for categories where the accent would moralize
# (red reads as "bad" on tenure, program, or demographic compositions).
.ern_qual_neutral <- c("#223754", "#C8860D", "#2A8A86", "#5A3A88",
                       "#7B96B5", "#2C6B3A", "#7A4A1E", "#586573")

# Sequential ramps (light -> dark, luminance-monotone; interpolable to any n).
# blues:  neutral magnitude, no valence -- the default "more vs less" ramp,
#         brand tint -> steel -> dark navy.
# reds:   brand-red intensity for red-themed quantities (eviction filings,
#         displacement counts); light end is the semantic red tint #FCE8E7.
# greens: positive quantities where more = good (affordability, stability).
.ern_seq_blues  <- c("#E8EEF4", "#C3D2E2", "#8BA3BE", "#5B7BA0",
                     "#3A5578", "#223754", "#19222C")
.ern_seq_reds   <- c("#FCE8E7", "#FABBB4", "#F97D71", "#F9322B",
                     "#CC2118", "#8F1710")
.ern_seq_greens <- c("#E8F3EA", "#C2E0C6", "#8CC494", "#4F9A5A",
                     "#2C6B3A", "#1A4626")

# Diverging ramps (7 stops, warm-white center, luminance peaks at the middle).
# div_brand: navy <-> brand red, the valenced scale -- use when the axis truly
#            runs good <-> bad, with red on the bad pole.
# div_gold:  semantic blue <-> gold, the neutral scale -- two directions,
#            neither a warning (gain/loss, above/below average). Blue-orange
#            is the most colorblind-safe diverging pair.
.ern_div_brand <- c("#223754", "#5E7CA0", "#C2CFDD", "#F4F1EE",
                    "#F9ABA4", "#F9322B", "#B01D16")
.ern_div_gold  <- c("#1B5A8A", "#6A95BB", "#BCD0E0", "#F2F0EC",
                    "#E6BE7A", "#C8860D", "#8B5A00")

# Neutral, accent-free ramp for COMPOSITION charts (stacked bars, waffles) where
# every series is a recurring segment — the qualitative palette would otherwise
# flood one whole band with the accent red.
.nt_seq_palette <- c("#19222C", "#223754", "#8BA3BE", "#6c7a89", "#aab4be")

# Interpolate that neutral ramp to exactly n distinct tones, so a 6-7 category
# race/ethnicity composition stays on-brand and legible instead of recycling
# colors (US race compositions routinely run past the 5 base tones).
.nt_seq_ramp <- function(n) {
  if (n <= length(.nt_seq_palette)) return(.nt_seq_palette[seq_len(n)])
  grDevices::colorRampPalette(.nt_seq_palette)(n)
}

# Canonical race/ethnicity colors (the Minnesota/Washington plate convention):
# the focus group (Black) carries the accent; White is the lightest/recessive.
.nt_race_palette <- c(Black = "#F9322B", Latine = "#223754", Asian = "#33a02c",
                      Other = "#6c7a89", White = "#aab4be")

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
  # Round to a precision derived from the data spread so low-variance columns —
  # proportions and rates in [0, 1], exactly the eviction data this targets —
  # keep distinct breaks instead of collapsing to a constant at 2 decimals.
  spread <- diff(range(values))
  if (is.finite(spread) && spread > 0) {
    brks <- round(brks, max(2L, 2L - floor(log10(spread))))
  }
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
  # Fail loud and clear on a column with nothing to map (a common post-join
  # miss) rather than emitting a malformed expression or a cryptic break error.
  if (is.numeric(x) && !any(is.finite(x))) {
    stop(sprintf("Column \"%s\" has no non-missing values to map; check the join/source.",
                 column), call. = FALSE)
  }

  if (type == "categorical") {
    cc <- .nt_categorical_colors(x, colors)
    if (length(cc$values) == 0L) {
      stop(sprintf("Column \"%s\" has no non-NA categories to color.", column),
           call. = FALSE)
    }
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
    # Heavy ties or a tiny spread can collapse quantile breaks below the count
    # the colors need; fall back to equal-interval breaks over the data range
    # before giving up (only a genuinely all-equal column has no spread).
    if (length(breaks) != (n_classes - 1L)) {
      rng <- range(as.numeric(x), na.rm = TRUE)
      if (diff(rng) > 0) {
        breaks <- seq(rng[1], rng[2], length.out = n_classes + 1L)[-c(1L, n_classes + 1L)]
      }
    }
    if (length(breaks) != (n_classes - 1L)) {
      stop(sprintf(
        "Column \"%s\" is constant (or nearly so): cannot derive %d break(s) for %d colors. Pass `breaks`, or fewer `colors`.",
        column, n_classes - 1L, n_classes), call. = FALSE)
    }
  } else {
    # drop an explicit leading floor if the caller passed n_classes breaks
    if (length(breaks) == n_classes) breaks <- breaks[-1L]
    if (length(breaks) > 1L && is.unsorted(breaks, strictly = TRUE)) {
      stop("`breaks` must be strictly increasing.", call. = FALSE)
    }
    if (length(breaks) != (n_classes - 1L)) {
      stop(sprintf(
        "Expected %d interior break(s) for %d colors, got %d. Pass `breaks` of length %d (or %d with an explicit floor).",
        n_classes - 1L, n_classes, length(breaks), n_classes - 1L, n_classes
      ), call. = FALSE)
    }
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
