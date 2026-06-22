#' Add a layer switcher for multi-layer maps
#'
#' @description
#' Adds a compact radio control that switches between choropleth layers added
#' with [nt_add_choropleth()], showing **one layer at a time** and revealing only
#' that layer's legend. This is the clean way to put several measures on a single
#' map — for example neighborhood typology plus the share of each demographic
#' group — and let the reader flip between them.
#'
#' Add the layers first: give each its own `id`, and set `visible = FALSE` on all
#' but the first. Then call `nt_layers_control()` with those ids.
#'
#' @details
#' MapLibre lets a map hold many layers at once; this control just toggles their
#' visibility (and the matching legends) so they don't pile on top of one
#' another. It is a small, self-contained piece of JavaScript attached with
#' [htmlwidgets::onRender()] — deterministic, and it reads/sets layer state
#' through the standard MapLibre API. For a raw multi-select (checkbox) toggle
#' instead, use [mapgl::add_layers_control()].
#'
#' @param map A map with two or more layers added via [nt_add_choropleth()]
#'   (each with a distinct `id`).
#' @param layers Character vector of layer `id`s to switch between, in order. The
#'   first is shown initially; the rest should have been added with
#'   `visible = FALSE`.
#' @param labels Optional display labels for the control (defaults to `layers`).
#' @param title Optional title shown above the options.
#' @param position One of `"bottom-right"` (default), `"bottom-left"`,
#'   `"top-left"`, or `"top-right"`. The default keeps the switcher clear of the
#'   legend (top-left) and the navigation/search controls (top-right).
#' @return The map, with the layer switcher added.
#' @family ern_maps
#' @seealso [nt_add_choropleth()] (use `visible = FALSE` for the hidden layers);
#'   [mapgl::add_layers_control()] for a raw checkbox toggle.
#' @examplesIf interactive() && requireNamespace("mapgl", quietly = TRUE)
#' md <- ntdf(state = "MD", county = "Baltimore City", geometry = TRUE)
#'
#' # Typology + the share of each demographic group, switchable on one map
#' demos <- c(White = "pWhite", Black = "pBlack", Asian = "pAsian", Latine = "pLatine")
#' m <- nt_maplibre(md) |>
#'   nt_add_choropleth(md, "nt_conc", id = "Typology", legend_title = "Neighborhood type")
#' for (i in seq_along(demos)) {
#'   m <- nt_add_choropleth(m, md, demos[[i]], id = names(demos)[i],
#'                          legend_title = paste("Share", names(demos)[i]),
#'                          visible = FALSE)
#' }
#' nt_layers_control(m, layers = c("Typology", names(demos)), title = "Show")
#' @export
nt_layers_control <- function(map, layers, labels = NULL, title = NULL,
                              position = c("bottom-right", "bottom-left",
                                           "top-left", "top-right")) {
  position <- match.arg(position)
  layers <- unname(as.character(layers))
  if (length(layers) < 1L) {
    stop("`layers` must name at least one layer id.", call. = FALSE)
  }
  if (is.null(labels)) labels <- layers
  labels <- unname(as.character(labels))
  if (length(labels) != length(layers)) {
    stop("`labels` must be the same length as `layers`.", call. = FALSE)
  }

  esc <- function(s) gsub('"', '\\\\"', gsub("\\\\", "\\\\\\\\", s))
  arr <- function(x) paste0("[", paste0('"', esc(x), '"', collapse = ","), "]")
  title_js <- if (is.null(title)) "null" else paste0('"', esc(title), '"')

  js <- paste0(
    "function(el, x){\n",
    "  var map = el.map || (window.HTMLWidgets && HTMLWidgets.getInstance(el) ? HTMLWidgets.getInstance(el).getMap() : null);\n",
    "  if(!map) return;\n",
    "  var layers = ", arr(layers), ", labels = ", arr(labels),
    ", title = ", title_js, ", pos = \"", position, "\";\n",
    "  var places = {'top-left':'top:10px;left:10px;','top-right':'top:10px;right:10px;',",
    "'bottom-left':'bottom:28px;left:10px;','bottom-right':'bottom:28px;right:10px;'};\n",
    "  var box = document.createElement('div');\n",
    "  box.className = 'nt-layers-control';\n",
    "  box.style.cssText = 'position:absolute;z-index:3;background:#fff;padding:8px 11px;",
    "border-radius:4px;box-shadow:0 1px 4px rgba(25,34,44,.25);",
    "font-family:Inter,-apple-system,system-ui,sans-serif;font-size:13px;color:#19222C;",
    "line-height:1.7;' + (places[pos] || places['bottom-left']);\n",
    "  var nm = 'nt-lyr-' + layers.join('|').replace(/[^A-Za-z0-9]+/g,'-');\n",
    "  var html = title ? ('<div style=\"font-weight:600;margin-bottom:4px\">'+title+'</div>') : '';\n",
    "  for (var i=0;i<layers.length;i++){ html += '<label style=\"display:block;cursor:pointer;",
    "white-space:nowrap\"><input type=\"radio\" name=\"'+nm+'\" value=\"'+layers[i]+'\"' + ",
    "(i===0?' checked':'') + ' style=\"margin-right:6px;vertical-align:middle\">' + labels[i] + '</label>'; }\n",
    "  box.innerHTML = html;\n",
    "  function select(layer){ layers.forEach(function(L){ if(map.getLayer(L)) ",
    "map.setLayoutProperty(L,'visibility', L===layer?'visible':'none'); ",
    "document.querySelectorAll('.mapboxgl-legend[data-layer-id=\"'+L+'\"]').forEach(",
    "function(le){ le.style.display = (L===layer?'':'none'); }); }); }\n",
    "  box.addEventListener('change', function(e){ if(e.target && e.target.name===nm) select(e.target.value); });\n",
    "  function ready(){ el.appendChild(box); select(layers[0]); }\n",
    "  if (map.getLayer(layers[0])) ready(); else map.on('load', ready);\n",
    "}"
  )

  htmlwidgets::onRender(map, js)
}
