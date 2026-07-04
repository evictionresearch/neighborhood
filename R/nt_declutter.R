# Hide the basemap's decorative land tints (parks, landcover, hillshade, ...)
# that wash out semi-transparent choropleth fills -- the "light white stuff"
# visible over tract colors on the Positron basemap.

#' Hide decorative basemap layers under a choropleth
#'
#' @description
#' Basemap styles tint parks, landcover, hillshade, and similar land features;
#' under a semi-transparent choropleth those tints read as pale washes over
#' the fill colors. `nt_declutter_basemap()` hides every basemap layer whose
#' id matches `pattern` (case-insensitive) once the style loads, leaving
#' roads, water, boundaries, and labels intact. Data layers added by this
#' package (`nt_*`, hatches) are never touched.
#' @param map A MapLibre map (`nt_map()`, [nt_maplibre()], or
#'   `mapgl::maplibre()`).
#' @param pattern Regular expression of basemap layer ids to hide. The default
#'   covers the usual suspects on Positron-style basemaps: park, landcover,
#'   landuse, hillshade, wood, grass, scrub, cemetery, golf, pitch, sand,
#'   glacier.
#' @return The map, with a render hook that hides the matching layers.
#' @family ern_maps
#' @examplesIf interactive() && requireNamespace("mapgl", quietly = TRUE)
#' md <- ntdf(state = "MD", county = "Baltimore City", geometry = TRUE)
#' nt_map(md) |> nt_declutter_basemap()
#' @export
nt_declutter_basemap <- function(map,
    pattern = "park|landcover|landuse|hillshade|wood|grass|scrub|cemetery|golf|pitch|sand|glacier") {
  if (!inherits(map, "maplibregl"))
    stop("`map` must be a MapLibre map (nt_map()/nt_maplibre()/mapgl::maplibre()).",
         call. = FALSE)
  stopifnot(is.character(pattern), length(pattern) == 1L, nzchar(pattern))
  js <- sprintf("
function(el, x) {
  var w = this, tries = 0;
  (function go() {
    var m = w.getMap && w.getMap();
    if (!m) { if (tries++ < 60) setTimeout(go, 250); return; }
    var re = new RegExp('%s', 'i');
    function hide() {
      var st = m.getStyle && m.getStyle();
      if (!st || !st.layers) return;
      st.layers.forEach(function(l) {
        if (re.test(l.id) && l.id.indexOf('nt_') !== 0)
          try { m.setLayoutProperty(l.id, 'visibility', 'none'); } catch (e) {}
      });
    }
    if (m.isStyleLoaded && m.isStyleLoaded()) hide();
    m.on('load', hide);
  })();
}", gsub("'", "", pattern))
  htmlwidgets::onRender(map, js)
}
