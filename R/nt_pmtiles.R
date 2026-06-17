#' Build a PMTiles archive from spatial data
#'
#' @description
#' Converts an `sf` object into a [PMTiles](https://docs.protomaps.com/pmtiles/)
#' vector-tile archive using the [`tippecanoe`](https://github.com/felt/tippecanoe)
#' command-line tool. PMTiles is how the state-profile maps serve large layers
#' (the full-US tract file is ~74,000 polygons) fast and from a single static
#' file — far beyond what can be embedded directly in an HTML page.
#'
#' Hand the returned path to [mapgl::add_pmtiles_source()], or let
#' [nt_add_choropleth()] do it for you when a layer is large
#' (`tiles = "auto"` / `"pmtiles"`).
#'
#' @details
#' `tippecanoe` is an **optional external dependency** — it is not an R package.
#' Install it once (for example `brew install tippecanoe` on macOS, or build from
#' source) and it is picked up from your `PATH`. Without it, this function stops
#' with installation guidance.
#'
#' The default tippecanoe flags (`-zg --drop-densest-as-needed
#' --extend-zooms-if-still-dropping --force`) auto-select a zoom range and keep
#' tiles a reasonable size; override them with `tippecanoe_args` for full control.
#' The layer name written into the archive is `layer`, which must match the
#' `source_layer` used when adding the layer to a map — [nt_add_choropleth()]
#' keeps these in lockstep by defaulting both to `"nt"`.
#'
#' @param data Spatial data: an `sf` object, or a path to a shapefile / GeoJSON /
#'   GeoPackage, an `sfc`, or an `sp`/`terra` object (normalized to `sf` and
#'   re-projected to WGS84 as needed). All non-geometry columns are carried into
#'   the tiles as feature properties (so popup/tooltip columns survive tiling).
#' @param path Output `.pmtiles` path. Defaults to a temporary file.
#' @param layer Tile layer name (default `"nt"`). Must equal the `source_layer`
#'   used to draw it.
#' @param tippecanoe_args Optional character vector of tippecanoe flags that
#'   *replaces* the defaults (the output `-o`, layer `-l`, and input are always
#'   set for you).
#' @param quiet Logical; suppress tippecanoe's console output (default `FALSE`).
#' @return The path to the written `.pmtiles` file (invisibly), suitable for
#'   [mapgl::add_pmtiles_source()].
#' @family ern_maps
#' @seealso [nt_add_choropleth()] (which calls this for large layers) and
#'   [mapgl::add_pmtiles_source()].
#' @examplesIf interactive() && nzchar(Sys.which("tippecanoe"))
#' # Tile the full-US typologies (join the bundled table to tract geometry first)
#' \dontrun{
#' us <- tigris::tracts(cb = TRUE, year = 2024) |>
#'   dplyr::left_join(us_nt_tracts2024, by = "GEOID")
#' tiles <- nt_pmtiles(us, path = "us_typologies.pmtiles")
#'
#' nt_maplibre() |>
#'   mapgl::add_pmtiles_source("us", url = tiles, promote_id = "GEOID") |>
#'   mapgl::add_fill_layer(id = "nt", source = "us", source_layer = "nt",
#'                         fill_color = "#1f78b4")
#' }
#' @export
nt_pmtiles <- function(data, path = NULL, layer = "nt",
                       tippecanoe_args = NULL, quiet = FALSE) {
  data <- .nt_as_sf(data)
  tipp <- Sys.which("tippecanoe")
  if (!nzchar(tipp)) {
    stop(
      "tippecanoe was not found on your PATH.\n",
      "  PMTiles building needs the tippecanoe command-line tool:\n",
      "    macOS:  brew install tippecanoe\n",
      "    source: https://github.com/felt/tippecanoe\n",
      "  For small areas you can skip tiling and draw inline instead.",
      call. = FALSE
    )
  }

  data <- .nt_ensure_wgs84(data)
  if (is.null(path)) path <- tempfile(fileext = ".pmtiles")

  geojson <- tempfile(fileext = ".geojson")
  on.exit(unlink(geojson), add = TRUE)
  sf::st_write(data, geojson, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)

  flags <- tippecanoe_args %nt||%
    c("-zg", "--drop-densest-as-needed", "--extend-zooms-if-still-dropping", "--force")
  args <- c("-o", path, "-l", layer, flags, geojson)

  status <- system2("tippecanoe", args,
                    stdout = if (quiet) FALSE else "",
                    stderr = if (quiet) FALSE else "")
  if (!identical(status, 0L) || !file.exists(path)) {
    stop("tippecanoe failed to build PMTiles (exit status ", status, ").",
         call. = FALSE)
  }
  if (!quiet) {
    message(sprintf("nt_pmtiles: wrote %s (%s)", path,
                    format(structure(file.size(path), class = "object_size"),
                           units = "auto")))
  }
  invisible(path)
}
