# Synchronized side-by-side MapLibre maps: pan or zoom one and the rest of its
# group follow, so an area of interest can be read across several layers at
# once (e.g. the v1-vs-v2 affordability comparison in dev/report_style_maps.Rmd,
# where this pattern was proven before being promoted here).

.nt_sync_env <- new.env(parent = emptyenv())
.nt_sync_group <- function() {
  n <- get0("n", envir = .nt_sync_env, ifnotfound = 0L) + 1L
  assign("n", n, envir = .nt_sync_env)
  paste0("ntsync-", n)
}

# The browser-side hook. Each map registers into a per-group registry
# (window._ntSync[group]); on 'move' it copies its camera to the others with
# jumpTo. MapLibre event dispatch is synchronous, so the shared `busy` flag
# makes the copy re-entrancy-proof (no feedback loop). getMap() is the
# accessor mapgl's widget binding returns; the retry loop covers async init.
.nt_sync_js <- function(group) sprintf("
function(el, x) {
  var w = this, tries = 0;
  (function attach() {
    var m = w.getMap && w.getMap();
    if (!m) { if (tries++ < 60) setTimeout(attach, 250); return; }
    window._ntSync = window._ntSync || {};
    var g = window._ntSync['%s'] = window._ntSync['%s'] || {maps: [], busy: false};
    g.maps.push(m);
    m.on('move', function() {
      if (g.busy) return;
      g.busy = true;
      var c = m.getCenter(), z = m.getZoom(), b = m.getBearing(), p = m.getPitch();
      g.maps.forEach(function(o) {
        if (o !== m) o.jumpTo({center: c, zoom: z, bearing: b, pitch: p});
      });
      g.busy = false;
    });
  })();
}", group, group)

#' Synchronize MapLibre maps side by side
#'
#' @description
#' Lays two or more MapLibre maps out in a row and keeps their views locked
#' together: pan, zoom, or rotate any one and the others follow instantly, so
#' the same area can be compared across layers at once (before/after, two
#' engines, afford vs. stability, ...).
#'
#' Build each map first — [nt_map()], [nt_maplibre()] +
#' [nt_add_choropleth()], or any `mapgl::maplibre()` pipeline — then hand them
#' to `nt_map_sync()`. Works in R Markdown/Quarto chunks, the RStudio viewer,
#' and saved HTML alike.
#'
#' @details
#' Synchronization is client-side: each map registers into a small per-group
#' registry when the page loads, and on every `move` event copies its camera
#' (center, zoom, bearing, pitch) to the rest of the group via `jumpTo`. A
#' shared guard flag prevents feedback loops (MapLibre event dispatch is
#' synchronous). Maps only sync *within* a group: rows built by separate
#' `nt_map_sync()` calls stay independent unless you pass the same `group`.
#'
#' To sync maps that are **not** side by side (e.g. two maps in different
#' sections of a document), give both calls the same `group` — the layout is
#' per-call, the synchronization is per-group.
#'
#' @param ... Two or more MapLibre maps (`mapgl` `maplibregl` htmlwidgets),
#'   or a single `list()` of them.
#' @param group Sync-group id. Default `NULL` auto-generates a fresh group per
#'   call (maps sync within the call only). Pass the same string across calls
#'   to sync maps in different rows/sections.
#' @param height Height (CSS pixels) applied to every map (default `480`);
#'   `NULL` keeps each widget's own height.
#' @param gap Horizontal gap between the maps, in pixels (default `8`).
#' @param titles,subtitles Optional per-map heading and small grey subheading
#'   shown above each panel; length 1 (recycled) or one per map.
#' @return A browsable `htmltools` tag: the row of synced maps. Print it,
#'   include it in an R Markdown chunk, or embed it with other tags.
#' @family ern_maps
#' @seealso [nt_map()], [nt_maplibre()]; `mapgl::compare()` for a two-map
#'   swipe/slider comparison.
#' @examplesIf interactive() && requireNamespace("mapgl", quietly = TRUE)
#' md <- ntdf(state = "MD", county = "Baltimore City", geometry = TRUE)
#' nt_map_sync(
#'   nt_map(md),                                  # typologies
#'   nt_map(md, color = "pBlack"),                # % Black
#'   titles = c("Neighborhood typologies", "Percent Black")
#' )
#' @export
nt_map_sync <- function(..., group = NULL, height = 480, gap = 8,
                        titles = NULL, subtitles = NULL) {
  maps <- list(...)
  if (length(maps) == 1L && is.list(maps[[1L]]) && !inherits(maps[[1L]], "htmlwidget"))
    maps <- maps[[1L]]
  if (length(maps) < 2L)
    stop("Pass at least two maps to sync.", call. = FALSE)
  ok <- vapply(maps, inherits, logical(1), "maplibregl")
  if (!all(ok))
    stop("Every input must be a MapLibre map (from nt_map(), nt_maplibre(), ",
         "or mapgl::maplibre()).", call. = FALSE)
  lab <- function(x, nm) {
    if (is.null(x)) return(NULL)
    if (length(x) == 1L) x <- rep(x, length(maps))
    if (length(x) != length(maps))
      stop("`", nm, "` needs one entry per map (or a single one to recycle).",
           call. = FALSE)
    x
  }
  titles <- lab(titles, "titles")
  subtitles <- lab(subtitles, "subtitles")
  if (is.null(group)) group <- .nt_sync_group()
  stopifnot(is.character(group), length(group) == 1L, nzchar(group))

  js <- .nt_sync_js(group)
  maps <- lapply(maps, function(m) {
    if (!is.null(height)) m$height <- height
    htmlwidgets::onRender(m, js)
  })
  panel <- function(i) htmltools::div(
    style = "flex:1 1 0; min-width:0;",
    if (!is.null(titles)) htmltools::div(
      style = "font-weight:700; font-size:0.95rem; margin:0 0 1px 0;", titles[[i]]),
    if (!is.null(subtitles)) htmltools::div(
      style = "color:#5a5a5a; font-size:0.78rem; line-height:1.25; margin:0 0 4px 0;",
      subtitles[[i]]),
    maps[[i]])
  htmltools::browsable(htmltools::div(
    style = sprintf("display:flex; gap:%dpx; align-items:flex-end;", gap),
    lapply(seq_along(maps), panel)))
}
