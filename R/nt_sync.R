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
#' @param switcher Optional layer switcher shared by every panel: a
#'   segmented control above the maps that flips between layer *groups*
#'   across all panels at once (e.g. one layer per AMI tier from
#'   [afford_map()]`(tier = c(...))`). A list:
#'   \describe{
#'     \item{`layers`}{Named character vector, `c(<layer_id> = "Button
#'       label", ...)`. Clicking a button sets that layer id visible (and the
#'       others hidden) on every panel where the id exists — panels missing
#'       an id are left alone, so a panel whose surface doesn't vary between
#'       groups (a single-layer map) simply never changes.}
#'     \item{`active`}{Layer id that starts selected (default the first).}
#'     \item{`label`}{Optional short label to the left of the buttons.}
#'     \item{`notes`}{Optional named character vector (by layer id) of HTML
#'       shown to the right of the buttons, updated on switch.}
#'   }
#'   Legends bound to a layer (`nt_add_choropleth(id = , ...)` ties them via
#'   `data-layer-id`) show and hide with it.
#' @return An [htmltools::tagList()]. Save it as a page with
#'   [htmltools::save_html()] (dependencies land in a `lib/` folder next to
#'   the file), or print it in an interactive session to preview.
#' @examplesIf interactive()
#' # m1, m2, m3: mapgl widgets over the same area
#' # htmltools::save_html(
#' #   nt_sync_maps(m1, m2, m3, titles = c("Rent", "Income", "Renters")),
#' #   "market_maps/index.html"
#' # )
#' # Tiered panels + a switcher (see afford_map(tier = c("ELI","VLI","LI"))):
#' # nt_sync_maps(m1, m2, m3, switcher = list(
#' #   label  = "Household income",
#' #   layers = c(afford_ELI = "ELI", afford_VLI = "VLI", afford_LI = "LI"),
#' #   active = "afford_VLI"))
#' @export
nt_sync_maps <- function(..., titles = NULL, height = "520px", gap = "10px",
                         switcher = NULL) {
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
  if (!is.null(switcher)) {
    stopifnot(
      "`switcher` must be a list with a named `layers` vector" =
        is.list(switcher) && is.character(switcher$layers) &&
        !is.null(names(switcher$layers)) && all(nzchar(names(switcher$layers)))
    )
    if (is.null(switcher$active)) switcher$active <- names(switcher$layers)[1]
    stopifnot("`switcher$active` must be one of names(switcher$layers)" =
                switcher$active %in% names(switcher$layers))
  }

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

  # The shared switcher bar: a segmented control that flips layer groups
  # across every panel (plus each layer's data-layer-id-bound legend).
  switch_bar <- NULL
  switch_js  <- ""
  if (!is.null(switcher)) {
    lids <- names(switcher$layers)
    btn_style <- function(on) paste0(
      "appearance:none; border:0; margin:0; padding:6px 14px; cursor:pointer;",
      " font:600 12.5px/1.2 ", .ern_font, ";",
      if (on) paste0(" background:", .ern_brand$navy, "; color:#ffffff;")
      else    paste0(" background:transparent; color:", .ern_brand$navy, ";"))
    buttons <- lapply(seq_along(lids), function(i) {
      htmltools::tags$button(
        `data-nt-layer` = lids[i], type = "button",
        style = paste0(btn_style(lids[i] == switcher$active),
                       if (i > 1) " border-left:1px solid #d5dde6;"),
        switcher$layers[i])
    })
    switch_bar <- htmltools::tags$div(
      class = "nt-sync-switcher",
      style = paste0("display:flex; align-items:center; flex-wrap:wrap; ",
                     "gap:12px; padding:0 2px 10px; font:13px/1.4 ",
                     .ern_font, "; color:", .ern_brand$navy, ";"),
      if (!is.null(switcher$label))
        htmltools::tags$span(style = "font-weight:700;", switcher$label),
      htmltools::tags$div(
        style = paste0("display:flex; border:1px solid #d5dde6; ",
                       "border-radius:999px; overflow:hidden;"),
        buttons),
      htmltools::tags$span(class = "nt-sync-note",
                           style = "color:#5a6673; min-width:0;")
    )
    notes_json <- if (is.null(switcher$notes)) "{}" else paste0(
      "{", paste(sprintf('"%s": "%s"', names(switcher$notes),
                         gsub('"', '\\\\"', unname(switcher$notes))),
                 collapse = ", "), "}")
    switch_js <- paste0("
    var switchIds = ['", paste(names(switcher$layers), collapse = "','"), "'];
    var notes = ", notes_json, ";
    var navy = '", .ern_brand$navy, "';
    function setActive(id) {
      ms.forEach(function (m) {
        switchIds.forEach(function (l) {
          if (m.getLayer(l))
            m.setLayoutProperty(l, 'visibility', l === id ? 'visible' : 'none');
        });
      });
      ids.forEach(function (elId) {
        var el = document.getElementById(elId);
        if (!el) return;
        switchIds.forEach(function (l) {
          el.querySelectorAll('.mapboxgl-legend[data-layer-id=\"' + l + '\"]')
            .forEach(function (lg) { lg.style.display = l === id ? '' : 'none'; });
        });
      });
      document.querySelectorAll('[data-nt-layer]').forEach(function (b) {
        var on = b.getAttribute('data-nt-layer') === id;
        b.style.background = on ? navy : 'transparent';
        b.style.color = on ? '#ffffff' : navy;
      });
      var note = document.querySelector('.nt-sync-note');
      if (note) note.innerHTML = notes[id] || '';
    }
    document.querySelectorAll('[data-nt-layer]').forEach(function (b) {
      b.addEventListener('click', function () {
        setActive(b.getAttribute('data-nt-layer'));
      });
    });
    // legends append after el.map exists -- retry until they are in the DOM
    var initTries = 0;
    (function initActive() {
      var haveLegends = document.querySelector('.mapboxgl-legend[data-layer-id]');
      setActive('", switcher$active, "');
      if (!haveLegends && initTries++ < 50) setTimeout(initActive, 200);
    })();")
  }

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
    });", switch_js, "
  }
  if (document.readyState === 'complete') ready();
  else window.addEventListener('load', ready);
})();"))

  htmltools::tagList(
    switch_bar,
    # below ~880px the panels stack vertically and split the height evenly
    htmltools::tags$style(htmltools::HTML(
      "@media (max-width: 880px) { .nt-sync-row { flex-direction: column; } }")),
    htmltools::tags$div(
      class = "nt-sync-row",
      style = sprintf(
        "display: flex; gap: %s; width: 100%%; height: %s; margin: 0;",
        gap, height),
      panels
    ),
    htmltools::tags$script(sync_js)
  )
}
