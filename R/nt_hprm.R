# =============================================================================
# The HPRM jurisdiction map, as a product: palette presets for the scored HPRM
# tier vocabularies (EDR / EER / divergence / composite) plus nt_hprm_map(),
# the report-facing multi-layer MapLibre map embedded on the jurisdiction
# report pages (one embed per jurisdiction, framed to its boundary).
# =============================================================================

# Tier vocabularies exactly as they appear in the scored HPRM data
# (hprm_v5_full_2022): dis_risk2 / ev_risk2 / divergence. dis_risk2 also
# carries three non-risk context flags, which render in context grey.
.hprm_edr_levels    <- c("Lower", "At Risk/Early", "Elevated", "High", "Extreme")
.hprm_eer_levels    <- c("Lower", "At Risk", "Elevated", "High", "Extreme")
.hprm_context_flags <- c("30%+ Student Population", "Military Infrastructure",
                         "40%+ Retired Population")
.hprm_no_renters    <- "No renters"   # appears in BOTH dis_risk2 and ev_risk2
.hprm_div_levels    <- c("Convergent", "EDR-Dominant", "EER-Dominant", "Neither")

# .hprm_ramp5 (the 5-class risk ramp) is defined next to .ern_ramp in
# nt_map_internals.R: it derives from .ern_ramp at load time, and R sources
# package files alphabetically, so the definition must live downstream of it.
.hprm_context_grey <- "#d3d3d3"

# Divergence keeps o1_map.R's semantic hue assignment (purple = both risks,
# red = displacement-led, blue = eviction-led) mapped onto brand tokens;
# "Neither" = both scores NA = no measured risk, so it wears the same brand
# tint as HPRM 0 and the tier "Lower" floors.
.hprm_div_colors <- c("Convergent"   = "#54278f",
                      "EDR-Dominant" = "#F9322B",
                      "EER-Dominant" = "#223754",
                      "Neither"      = "#e8eef4")

# Composite bins follow the methods guide's interpretation table
# (HPRM_METHODS_AND_GUIDE.md sec. 3): 0 = no component registered (brand tint,
# off the risk ramp), then 1-2 / 3-4 / 5-6 / 7-8. Keeping 0 off the ramp is
# what lets a score-1 tract read differently from a score-0 tract — on the CA
# surface the composite tops out at 4, so the bottom of the scale carries all
# the local contrast.
.hprm_breaks <- c(1, 3, 5, 7)
# .hprm_composite_colors is defined next to .hprm_ramp5 in nt_map_internals.R:
# like the ramp it derives from .ern_ramp at load time, and this file sources
# first in alphabetical collation.

#' HPRM palette presets
#'
#' @description
#' The canonical colors for the scored HPRM tier vocabularies, so interactive
#' maps ([nt_hprm_map()]) and static exports color identically. Tiers use the
#' ERN risk ramp (`ern_palette("ramp")`) over the brand-tint floor — the light
#' blue that means "no measured risk" on the report scale plates; the
#' `dis_risk2` context flags (student / military / retired tracts) render in
#' context grey; divergence keeps the purple/red/blue semantics of the HPRM
#' site maps, with "Neither" on the same tint floor.
#'
#' @param model One of `"edr"` (`dis_risk2` tiers + context flags + no-renter),
#'   `"eer"` (`ev_risk2` tiers + no-renter), `"divergence"`, or `"hprm"` (the 0-8 composite,
#'   binned per the methods-guide interpretation table: 0 alone on the brand
#'   tint, then 1-2 / 3-4 / 5-6 / 7-8 on the risk ramp; names are the bin
#'   labels — the matching interior breaks are `c(1, 3, 5, 7)`).
#' @return A named character vector of hex colors; names are the data values
#'   (or bin labels, for `"hprm"`) in display order.
#' @family ern_maps
#' @seealso [nt_hprm_map()]
#' @examples
#' nt_hprm_pal("edr")
#' nt_hprm_pal("hprm")
#' @export
nt_hprm_pal <- function(model = c("edr", "eer", "divergence", "hprm")) {
  model <- match.arg(model)
  en <- intToUtf8(8211L)  # en-dash, kept out of the source for portability
  switch(model,
    edr = c(stats::setNames(.hprm_ramp5, .hprm_edr_levels),
            stats::setNames(rep(.hprm_context_grey, length(.hprm_context_flags)),
                            .hprm_context_flags),
            stats::setNames(.hprm_context_grey, .hprm_no_renters)),
    eer = c(stats::setNames(.hprm_ramp5, .hprm_eer_levels),
            stats::setNames(.hprm_context_grey, .hprm_no_renters)),
    divergence = .hprm_div_colors,
    hprm = stats::setNames(.hprm_composite_colors,
                           c("0", paste0("1", en, "2"), paste0("3", en, "4"),
                             paste0("5", en, "6"), paste0("7", en, "8")))
  )
}

#' The HPRM jurisdiction map (the report-page embed)
#'
#' @description
#' Renders scored HPRM tract data as the jurisdiction-report interactive map:
#' switchable choropleth layers for **displacement risk (EDR)**, **eviction
#' risk (EER)**, the **HPRM composite (0-8)**, and **convergence** (tracts
#' carrying both signals), each with its own legend, a rich per-tract popup,
#' and the jurisdiction boundary drawn on top with the view framed to it.
#' The surface is drawn county-wide so boundary effects stay visible — the
#' jurisdiction is framed, not clipped.
#'
#' Colors come from [nt_hprm_pal()], so the interactive map and any static
#' export read identically. Layer order in `layers` is switcher order; the
#' first layer is the one shown initially.
#'
#' @param hprm Scored HPRM tract data: an `sf` object, or a plain data frame
#'   to be joined onto `tigris` tract geometry via `state`/`county`/`year`.
#'   Needs a `geoid`/`GEOID` column plus, per requested layer, `dis_risk2`,
#'   `ev_risk2`, `hprm_score`, `divergence`. Optional popup context columns
#'   (used when present): `p_li`, `renters_e`, `nt_conc2`, `data_quality`
#'   (a display-ready note, e.g. for tracts below the published map's
#'   inclusion criteria).
#' @param state,county Passed to [tigris::tracts()] when `hprm` has no
#'   geometry (cartographic-boundary tracts, `cb = TRUE`).
#' @param place Optional jurisdiction name(s) ([tigris::places()]): drawn as a
#'   boundary line and used to frame the initial view. Incorporated places
#'   (LSAD 25) are preferred when a name matches more than one polygon.
#' @param layers Which layers to add, in switcher order: any of `"edr"`,
#'   `"eer"`, `"hprm"`, `"divergence"` (default all four, EDR first — the
#'   model's live signal in California).
#' @param year Geometry vintage for tracts/places (default 2020, matching the
#'   hprm_v5 tract vintage).
#' @param erase_water Clip open water from tracts and boundary for display
#'   ([nt_erase_water()]); the scores themselves are untouched. Default `TRUE`.
#' @param declutter Hide decorative basemap tints under the fills
#'   ([nt_declutter_basemap()]). Default `TRUE`.
#' @param popup_note Optional HTML/text appended to every popup — the seam for
#'   region-specific caveats (e.g. California's EER suppression note). For a
#'   per-tract note (e.g. the share of a straddling tract inside the framed
#'   jurisdiction), put an `in_city_note` character column on `hprm`; it is
#'   prepended to `popup_note` feature by feature.
#' @param opacity Fill opacity (default `0.8`).
#' @param height Optional map height in CSS pixels.
#' @param ... Passed to [nt_add_choropleth()] (e.g. `legend`, `tooltip`;
#'   `before_id` defaults to `"water"` so fills sit under basemap water and
#'   labels).
#' @return A `maplibregl` htmlwidget — save with [htmlwidgets::saveWidget()]
#'   and iframe it from the report page.
#' @family ern_maps
#' @seealso [nt_hprm_pal()], [nt_add_choropleth()], [nt_layers_control()]
#' @examples \dontrun{
#' smc <- arrow::read_parquet("~/data/evictionresearch/hprm/hprm_v5_full_2022.parquet") |>
#'   dplyr::filter(substr(geoid, 1, 5) == "06081", map)
#' m <- nt_hprm_map(smc, state = "CA", county = "San Mateo",
#'                  place = "Redwood City",
#'                  popup_note = "California is excluded from the eviction-filing model; EER here is an extrapolation.")
#' htmlwidgets::saveWidget(m, "hprm_redwood_city.html", selfcontained = TRUE)
#' }
#' @export
nt_hprm_map <- function(hprm, state = NULL, county = NULL, place = NULL,
                        layers = c("edr", "eer", "hprm", "divergence"),
                        year = 2020, erase_water = TRUE, declutter = TRUE,
                        popup_note = NULL, opacity = 0.8, height = NULL, ...) {
  layers <- match.arg(layers, several.ok = TRUE)
  layer_cols <- c(edr = "dis_risk2", eer = "ev_risk2",
                  hprm = "hprm_score", divergence = "divergence")

  x <- hprm
  if (!"GEOID" %in% names(x)) {
    if ("geoid" %in% names(x)) x$GEOID <- as.character(x$geoid)
    else stop("`hprm` needs a geoid/GEOID column.", call. = FALSE)
  }
  missing_cols <- setdiff(unname(layer_cols[layers]), names(x))
  if (length(missing_cols)) {
    stop("`hprm` is missing column(s) for the requested layers: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # --- Geometry: join tigris tracts unless the data is already spatial ------
  if (!inherits(x, "sf")) {
    if (is.null(state)) {
      stop("`hprm` has no geometry; pass `state` (and usually `county`) ",
           "so tracts can be pulled from tigris.", call. = FALSE)
    }
    tr <- tigris::tracts(state = state, county = county, year = year,
                         cb = TRUE, progress_bar = FALSE)
    x <- dplyr::inner_join(tr["GEOID"], sf::st_drop_geometry(x), by = "GEOID")
    stopifnot("no HPRM rows matched the tract geometry" = nrow(x) > 0)
  }
  x <- .nt_ensure_wgs84(x)
  if (isTRUE(erase_water) && !is.null(state)) {
    x <- nt_erase_water(x, state, county)
  }

  # --- Jurisdiction boundary + view frame -----------------------------------
  pl <- NULL
  if (!is.null(place)) {
    if (is.null(state)) stop("`place` needs `state`.", call. = FALSE)
    pl <- tigris::places(state = state, year = year, cb = TRUE,
                         progress_bar = FALSE)
    pl <- pl[pl$NAME %in% place, ]
    if (any(pl$LSAD == "25")) pl <- pl[pl$LSAD == "25", ]  # incorporated city
    if (nrow(pl) == 0) {
      warning("place \"", paste(place, collapse = ", "),
              "\" not found; framing to the data instead.", call. = FALSE)
      pl <- NULL
    } else {
      pl <- .nt_ensure_wgs84(pl)
      if (isTRUE(erase_water)) pl <- nt_erase_water(pl, state, county)
    }
  }

  # --- Rich popup, shared by every layer -------------------------------------
  x$hprm_tract <- paste("Tract", x$GEOID)
  fld_all <- c("Displacement risk (EDR)" = "dis_risk2",
               "Eviction risk (EER)"     = "ev_risk2",
               "HPRM composite (0-8)"    = "hprm_score",
               "Convergence"             = "divergence",
               "Low-income households"   = "p_li",
               "Renter households"       = "renters_e",
               "Neighborhood type"       = "nt_conc2",
               "Data quality"            = "data_quality")
  flds <- fld_all[unname(fld_all) %in% names(x)]
  # Per-feature note (an `in_city_note` column travels through the geometry
  # join, so it stays row-aligned) + the shared region caveat, one styled div.
  row_note  <- if ("in_city_note" %in% names(x)) {
    dplyr::coalesce(as.character(x$in_city_note), "")
  } else ""
  combined <- paste0(row_note, if (is.null(popup_note)) "" else popup_note)
  note_html <- if (any(nzchar(combined))) {
    ifelse(nzchar(combined),
           paste0("<div style='margin-top:6px;font-size:10px;color:#586573;max-width:250px'>",
                  combined, "</div>"),
           "")
  } else NULL
  x$hprm_popup <- nt_popup(x, title = "hprm_tract", prefix_title = FALSE,
                           fields = flds, html = note_html)
  x$in_city_note <- NULL   # folded into the popup; drop from feature properties

  # --- Base map + switchable layers ------------------------------------------
  dots <- list(...)
  if (!"before_id" %in% names(dots)) dots$before_id <- "water"
  add_layer <- function(m, data, column, id, title, colors = NULL,
                        breaks = NULL, labels = NULL, visible) {
    do.call(nt_add_choropleth,
            c(list(m, data, column, id = id, legend_title = title,
                   colors = colors, breaks = breaks, labels = labels,
                   popup = "hprm_popup", opacity = opacity, visible = visible),
              dots))
  }
  # keep each level's color even when a level is absent from the region
  tier_colors <- function(vals, pal) {
    present <- names(pal)[names(pal) %in% unique(as.character(vals))]
    list(values = factor(as.character(vals), levels = present),
         colors = unname(pal[present]))
  }

  m <- nt_maplibre(if (!is.null(pl)) pl else x)
  ids <- c(edr = "EDR", eer = "EER", hprm = "HPRM", divergence = "Divergence")
  labs <- c(edr = "Displacement risk (EDR)", eer = "Eviction risk (EER)",
            hprm = "HPRM composite (0-8)", divergence = "Divergence (EDR + EER)")
  first <- TRUE
  for (ly in layers) {
    col <- layer_cols[[ly]]
    if (ly == "hprm") {
      m <- add_layer(m, x, col, ids[[ly]], labs[[ly]],
                     colors = unname(nt_hprm_pal("hprm")),
                     breaks = .hprm_breaks, labels = names(nt_hprm_pal("hprm")),
                     visible = first)
    } else {
      tc <- tier_colors(x[[col]], nt_hprm_pal(ly))
      x[[col]] <- tc$values
      m <- add_layer(m, x, col, ids[[ly]], labs[[ly]], colors = tc$colors,
                     visible = first)
    }
    first <- FALSE
  }
  if (length(layers) > 1) {
    m <- nt_layers_control(m, layers = unname(ids[layers]),
                           labels = unname(labs[layers]), title = "Map layer")
  }

  # --- Boundary on top, then polish ------------------------------------------
  if (!is.null(pl)) {
    m <- mapgl::add_line_layer(m, id = "place_boundary", source = pl,
                               line_color = "#19222C", line_width = 2.2)
  }
  if (isTRUE(declutter)) m <- nt_declutter_basemap(m)
  if (!is.null(height)) m$height <- height
  m
}
