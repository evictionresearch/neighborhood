#' Start an ERN-styled MapLibre map
#'
#' @description
#' Initializes an interactive [MapLibre GL JS](https://maplibre.org/) map with
#' the Eviction Research Network house style: the ERN terrain basemap
#' ([nt_terrain_style()] — hillshade relief under a minimalist navy vector
#' map), a flat (web-mercator) projection, the standard control set
#' (zoom/compass, fullscreen, geolocate, and an address search box), and — if
#' you hand it spatial data — an automatic fit to that data's bounds.
#'
#' This is the foundation the other `nt_*` map functions build on. It returns an
#' ordinary `mapgl` map object, so you can keep building with [nt_add_choropleth()]
#' and [nt_add_labels()] **or** drop straight into any `mapgl` function (e.g.
#' [mapgl::add_line_layer()], [mapgl::add_layers_control()]) at any point in the
#' pipe. The neighborhood wrappers never lock you in.
#'
#' Maps are `htmlwidgets`: they display in RStudio, Quarto/R Markdown, and Shiny,
#' and can be written to a self-contained HTML file with
#' [htmlwidgets::saveWidget()] — the same delivery format as the HPRM, Minnesota,
#' and Washington state-profile maps.
#'
#' @param data Optional spatial data to frame the initial view: an `sf` object,
#'   or a path to a shapefile / GeoJSON / GeoPackage, an `sfc`, or an `sp`/`terra`
#'   object (all normalized to `sf` and re-projected to WGS84 as needed). If
#'   supplied and `fit = TRUE`, the map opens framed to its bounds. (Data is not
#'   drawn here — add it with [nt_add_choropleth()].)
#' @param style Basemap. `"terrain-light"` (default) or `"terrain-dark"` for
#'   the house terrain style ([nt_terrain_style()]); one of the OpenFreeMap
#'   style names `"positron"`, `"bright"`, `"liberty"`, `"dark"`, or
#'   `"fiord"`; **or** any value `mapgl` accepts (a style URL, a style list
#'   such as `nt_terrain_style(hillshade = FALSE)`, or the result of
#'   [mapgl::carto_style()], [mapgl::maptiler_style()], etc.).
#' @param controls Logical; add the standard control set (default `TRUE`).
#' @param search Logical; include an OpenStreetMap/Nominatim address search box
#'   (default `TRUE`, no API key required). Ignored when `controls = FALSE`.
#' @param fit Logical; if `data` is supplied, fit the initial view to it
#'   (default `TRUE`). Ignored if `center` is given.
#' @param center Optional `c(lng, lat)` to center the map instead of fitting.
#' @param zoom Optional initial zoom (used with `center`).
#' @param projection Map projection; defaults to `"mercator"` (the flat web map
#'   look of the state-profile maps). Pass `"globe"` for the 3-D globe.
#' @param ... Passed to [mapgl::maplibre()] (e.g. `height`, `bearing`, `pitch`).
#' @return A `mapgl`/`maplibregl` `htmlwidget`.
#' @family ern_maps
#' @seealso [nt_add_choropleth()], [nt_add_labels()], [nt_map()];
#'   [mapgl::maplibre()] and [mapgl::openfreemap_style()] for the underlying
#'   engine by Kyle Walker.
#' @examplesIf interactive() && requireNamespace("mapgl", quietly = TRUE)
#' md <- ntdf(state = "MD", county = "Baltimore City", geometry = TRUE)
#' nt_maplibre(md) |>
#'   nt_add_choropleth(md, "nt_conc")
#' @export
nt_maplibre <- function(data = NULL, style = "terrain-light", controls = TRUE,
                        search = TRUE, fit = TRUE, center = NULL, zoom = NULL,
                        projection = "mercator", ...) {
  style_obj <- .nt_resolve_style(style)

  ml_args <- list(style = style_obj, projection = projection, ...)
  if (!is.null(center)) ml_args$center <- center
  if (!is.null(zoom))   ml_args$zoom <- zoom
  map <- do.call(mapgl::maplibre, ml_args)

  if (!is.null(data)) {
    data <- .nt_ensure_wgs84(.nt_as_sf(data))
    if (isTRUE(fit) && is.null(center)) {
      map <- mapgl::fit_bounds(map, data, animate = FALSE)
    }
  }

  if (isTRUE(controls)) {
    map <- .nt_add_controls(map, search = search)
  }
  nt_legend_collapse(map)
}

# Resolve a basemap argument to something mapgl::maplibre() accepts. The two
# house terrain names build the style list via nt_terrain_style(); known
# OpenFreeMap style names route through openfreemap_style(); anything else
# (URL, carto_style() object, style list, ...) passes through untouched.
.nt_resolve_style <- function(style) {
  if (is.character(style) && length(style) == 1L) {
    ofm <- c("positron", "bright", "liberty", "dark", "fiord")
    if (style %in% ofm) return(mapgl::openfreemap_style(style))
    if (style %in% c("terrain-light", "terrain-dark"))
      return(nt_terrain_style(mode = sub("^terrain-", "", style)))
  }
  style
}
