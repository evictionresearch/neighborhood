# ==========================================================================
# nt_sync_maps() — side-by-side MapLibre panels with synchronized movement
# ==========================================================================

#' Lay out MapLibre maps side by side with synchronized zoom and pan
#'
#' @description
#' Places two or more \pkg{mapgl} MapLibre widgets in a flex row and wires
#' their movement together: pan or zoom any panel and the others follow.
#' The small-multiples pattern for comparing surfaces over the same
#' geography (e.g. rent change / income change / renter-share change).
#'
#' mapgl's own [mapgl::compare()] handles exactly two maps (swipe or sync);
#' this helper takes any number. Each widget's MapLibre `Map` object is
#' reached through the DOM element mapgl binds it to (`el.map`), so no
#' plugin is needed.
#'
#' @param ... mapgl/MapLibre htmlwidgets (two or more), or a single list of
#'   them.
#' @param titles Optional character vector, one short title per panel,
#'   shown above each map.
#' @param height CSS height of the row of maps (default `"520px"`).
#' @param gap CSS gap between panels (default `"10px"`).
#' @return An [htmltools::tagList()]. Save it as a page with
#'   [htmltools::save_html()] (dependencies land in a `lib/` folder next to
#'   the file), or print it in an interactive session to preview.
#' @examplesIf interactive()
#' # m1, m2, m3: mapgl widgets over the same area
#' # htmltools::save_html(
#' #   nt_sync_maps(m1, m2, m3, titles = c("Rent", "Income", "Renters")),
#' #   "market_maps/index.html"
#' # )
#' @export
nt_sync_maps <- function(..., titles = NULL, height = "520px", gap = "10px") {
  maps <- list(...)
  if (length(maps) == 1L && is.list(maps[[1]]) &&
      !inherits(maps[[1]], "htmlwidget")) {
    maps <- maps[[1]]
  }
  stopifnot(
    "nt_sync_maps() needs at least two maps" = length(maps) >= 2L,
    "every argument must be an htmlwidget"   =
      all(vapply(maps, inherits, logical(1), "htmlwidget")),
    "titles must match the number of maps"   =
      is.null(titles) || length(titles) == length(maps)
  )

  ids <- sprintf("nt-sync-map-%d", seq_along(maps))
  for (i in seq_along(maps)) {
    maps[[i]]$elementId <- ids[i]
    maps[[i]]$width  <- "100%"
    maps[[i]]$height <- "100%"
  }

  panels <- lapply(seq_along(maps), function(i) {
    htmltools::tags$div(
      style = "flex: 1 1 0; min-width: 0; display: flex; flex-direction: column;",
      if (!is.null(titles)) {
        htmltools::tags$div(
          titles[i],
          style = paste0("font: 600 13px/1.3 ", .ern_font, "; color: ",
                         .ern_brand$navy, "; padding: 0 2px 6px;")
        )
      },
      htmltools::tags$div(style = "flex: 1 1 auto; position: relative;",
                          maps[[i]])
    )
  })

  # Sync: on any panel's move, jump the others to the same camera. The
  # `moving` latch stops the jumpTo() calls from re-triggering themselves.
  id_array <- paste0("['", paste(ids, collapse = "','"), "']")
  sync_js <- htmltools::HTML(paste0("
(function () {
  var ids = ", id_array, ";
  function ready() {
    var ms = ids.map(function (id) {
      var el = document.getElementById(id);
      return el && el.map;
    });
    if (ms.some(function (m) { return !m; })) { setTimeout(ready, 100); return; }
    var moving = false;
    ms.forEach(function (m) {
      m.on('move', function () {
        if (moving) return;
        moving = true;
        ms.forEach(function (o) {
          if (o !== m) {
            o.jumpTo({center: m.getCenter(), zoom: m.getZoom(),
                      bearing: m.getBearing(), pitch: m.getPitch()});
          }
        });
        moving = false;
      });
    });
  }
  if (document.readyState === 'complete') ready();
  else window.addEventListener('load', ready);
})();"))

  htmltools::tagList(
    htmltools::tags$div(
      style = sprintf(
        "display: flex; gap: %s; width: 100%%; height: %s; margin: 0;",
        gap, height),
      panels
    ),
    htmltools::tags$script(sync_js)
  )
}
