#' Map neighborhood data in one line
#'
#' @description
#' The quickest path to a finished map. `nt_map()` initializes an ERN-styled
#' MapLibre map and draws `data` as a choropleth — basemap, fit-to-data, legend,
#' popup, tooltip, and controls included. It is shorthand for
#' [nt_maplibre()] piped into [nt_add_choropleth()].
#'
#' When `color` is omitted and the data has an `nt_conc` column (from [ntdf()]),
#' it maps the neighborhood typologies with the canonical palette.
#'
#' Need more control? Stop using `nt_map()` and build the map up yourself with
#' [nt_maplibre()] + [nt_add_choropleth()] + [nt_add_labels()] (+ any `mapgl`
#' function). Same pieces, fully exposed.
#'
#' @param data Polygon spatial data. An `sf` object (e.g. from
#'   `ntdf(geometry = TRUE)`), **or** a path to a shapefile / GeoJSON /
#'   GeoPackage, an `sfc` geometry, or an `sp`/`terra` object — anything is
#'   normalized to `sf`.
#' @param color Name of the column to map. Defaults to `"nt_conc"` if present.
#' @param ... Passed to [nt_add_choropleth()] (e.g. `type`, `breaks`, `legend`,
#'   `popup`).
#' @return A `mapgl`/`maplibregl` `htmlwidget`.
#' @family ern_maps
#' @seealso [nt_maplibre()], [nt_add_choropleth()], [nt_add_labels()].
#' @examplesIf interactive() && requireNamespace("mapgl", quietly = TRUE)
#' md <- ntdf(state = "MD", county = "Baltimore City", geometry = TRUE)
#' nt_map(md)                       # typologies, one line
#' nt_map(md, color = "pBlack")     # a numeric column instead
#' @export
nt_map <- function(data, color = NULL, ...) {
  data <- .nt_as_sf(data)   # accept sf, a shapefile/GeoJSON path, sfc, sp/terra
  if (is.null(color)) {
    if ("nt_conc" %in% names(data)) {
      color <- "nt_conc"
    } else {
      stop("Specify `color` (a column to map); no `nt_conc` column was found.",
           call. = FALSE)
    }
  }
  nt_maplibre(data) |> nt_add_choropleth(data, column = color, ...)
}
