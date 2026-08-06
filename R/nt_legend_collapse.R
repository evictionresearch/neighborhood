#' Collapsible map legends
#'
#' @description
#' Attaches a small script and stylesheet that make every mapgl legend on the
#' page collapsible: a chevron on the legend title (click the title or the
#' chevron) folds the legend down to its title line so the map underneath
#' stays visible. Legends on small panes (under roughly 480 x 340 px — synced
#' multi-pane grids, stacked mobile layouts) start collapsed; a legend the
#' reader toggles by hand keeps their choice. Headless print snapshotting can
#' opt out of the auto-collapse by setting `window.__NT_LEGEND_NO_AUTO = true`
#' before the page loads.
#'
#' [nt_maplibre()] attaches this automatically, so every `nt_*` map already
#' has it; call this yourself only on maps built straight from
#' [mapgl::maplibre()].
#'
#' @param map A `mapgl`/`maplibregl` htmlwidget.
#' @return The widget, with the legend-collapse dependency attached (once;
#'   repeated calls are a no-op).
#' @family ern_maps
#' @export
nt_legend_collapse <- function(map) {
  already <- vapply(map$dependencies,
                    function(d) identical(d$name, "nt-legend-collapse"),
                    logical(1))
  if (any(already)) return(map)
  dep <- htmltools::htmlDependency(
    name = "nt-legend-collapse", version = "1.0.2",
    src = c(file = system.file("assets", "legend-collapse",
                               package = "neighborhood")),
    script = "nt-legend-collapse.js",
    stylesheet = "nt-legend-collapse.css")
  map$dependencies <- c(map$dependencies, list(dep))
  map
}
