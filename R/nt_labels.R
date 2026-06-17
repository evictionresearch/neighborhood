#' Add text labels to a map
#'
#' @description
#' Places a text label at an interior point of each feature — county names,
#' place names, or any other column. Labels are drawn with a white halo for
#' legibility over a choropleth, matching the state-profile maps.
#'
#' Points are computed with [sf::st_point_on_surface()] (not a centroid), so the
#' label always lands inside irregular polygons rather than in a bay or notch.
#'
#' @param map A map from [nt_maplibre()] (or [mapgl::maplibre()]).
#' @param data Spatial data (`sf`, a shapefile/GeoJSON/GeoPackage path, `sfc`, or
#'   `sp`/`terra`; normalized to `sf`). Polygons are reduced to interior label
#'   points; point/line inputs are used as-is.
#' @param text_column Name of the column whose values are shown as labels.
#' @param id Layer id. Defaults to a name derived from `text_column`.
#' @param size Text size in pixels (default `11`).
#' @param color Text color (default dark grey).
#' @param halo_color Halo color (default white).
#' @param halo_width Halo width in pixels (default `1.2`).
#' @param min_zoom Optional minimum zoom at which labels appear (handy to avoid
#'   clutter when zoomed out).
#' @param ... Passed to [mapgl::add_symbol_layer()].
#' @return The map, with the label layer added.
#' @family ern_maps
#' @seealso [nt_add_choropleth()]; [mapgl::add_symbol_layer()] underneath.
#' @examplesIf interactive() && requireNamespace("mapgl", quietly = TRUE)
#' md <- ntdf(state = "MD", county = "Baltimore City", geometry = TRUE)
#' nt_maplibre(md) |>
#'   nt_add_choropleth(md, "nt_conc") |>
#'   nt_add_labels(md, "NAME", min_zoom = 10)
#' @export
nt_add_labels <- function(map, data, text_column, id = NULL,
                          size = 11, color = "#333333",
                          halo_color = "#ffffff", halo_width = 1.2,
                          min_zoom = NULL, ...) {
  data <- .nt_as_sf(data)
  if (!text_column %in% names(data)) {
    stop(sprintf("Column \"%s\" not found in the data.", text_column), call. = FALSE)
  }
  geom_type <- as.character(sf::st_geometry_type(data, by_geometry = FALSE))
  pts <- if (grepl("POLYGON", geom_type)) .nt_label_points(data) else .nt_ensure_wgs84(data)
  if (is.null(id)) id <- paste0("nt_labels_", gsub("[^A-Za-z0-9]", "_", text_column))

  mapgl::add_symbol_layer(
    map, id = id, source = pts,
    text_field = mapgl::get_column(text_column),
    text_size = size, text_color = color,
    text_halo_color = halo_color, text_halo_width = halo_width,
    min_zoom = min_zoom, ...
  )
}
