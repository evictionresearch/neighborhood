# =============================================================================
# The affordability pane, as a product: the verdict map ("map 3") plus its
# locked plain-language reading. afford_map() renders one tier of an
# afford_verdict() result as a categorical MapLibre verdict choropleth, with
# an optional displacement hatch; afford_caption() emits the standard reading
# and the "what ifs" that must travel with the figure. Pane 1 of the
# three-pane product (affordability -> availability -> stability).
# =============================================================================

# internal: 45-degree hatch lines clipped to polygons (promoted from
# dev/report_style_maps.Rmd, where the pattern was proven)
.nt_hatch_lines <- function(g, spacing = 1200) {
  g <- sf::st_union(sf::st_transform(g, 3857))
  bb <- sf::st_bbox(g)
  hgt <- unname(bb["ymax"] - bb["ymin"])
  x0 <- seq(unname(bb["xmin"]) - hgt, unname(bb["xmax"]), by = spacing)
  ln <- sf::st_sfc(lapply(x0, function(x)
    sf::st_linestring(rbind(c(x, unname(bb["ymin"])),
                            c(x + hgt, unname(bb["ymax"]))))), crs = 3857)
  ln <- suppressWarnings(sf::st_intersection(ln, g))
  sf::st_as_sf(data.frame(hatch = "displacement"),
               geometry = sf::st_transform(ln, 4326))
}

# plain-language legend labels for the three verdict classes
.afi_verdict_labels <- c(
  "affordable"         = "Affordable (\u226430% of income)",
  "roughly affordable" = "Roughly affordable (30\u201350%)",
  "not affordable"     = "Not affordable (>50%)")

# ... and for the three stability classes (afford_stability()'s stable_cat)
.afi_stability_labels <- c(
  "stable"     = "Stable (\u22650.8)",
  "elevated"   = "Elevated risk (0.5\u20130.8)",
  "precarious" = "Precarious (<0.5)")

#' The affordability verdict map (pane 1 of three)
#'
#' @description
#' Renders one AMI tier of an [afford_verdict()] result as the headline
#' affordability map: a three-class categorical MapLibre choropleth --
#' **Affordable (at or under 30 percent of income) / Roughly affordable
#' (30-50 percent) / Not affordable (over 50 percent)** -- in the house
#' verdict palette, optionally
#' with a red displacement hatch on top. The plain reading: *this is how much
#' a household at the tier's income limit will pay if they can move in there.*
#'
#' Pair it with [afford_caption()], which states that reading and the "what
#' ifs" the map assumes; those caveats must travel with the figure.
#' @details
#' The locked descriptor for the red class (VLI): *"even at the rents current
#' tenants pay, a VLI family would give up more than half their income in
#' these neighborhoods -- and entry prices run higher still."* The map prices
#' tracts at what **current tenants** pay (ACS gross rent, utilities
#' included), so it *understates* the cost of entry -- asking rents on the
#' units that actually open run higher. It is **price-only** (pane 1): it says
#' nothing about whether a unit is open (availability, pane 2) or whether a
#' household can stay (stability, pane 3).
#'
#' Colors stay aligned to the three classes even when a region is missing one
#' (an absent class does not shift the remaining colors).
#' @param x An [afford_index()] result with geometry (`geometry = TRUE`) and a
#'   `verdict` column ([afford_verdict()]; derived automatically when both
#'   `supply` and `supply_stretch` are present).
#' @param tier Which AMI tier to map (default `"VLI"`, the pilot group).
#' @param verdict_col Which verdict column to render: `"verdict"` (default,
#'   pane 1 -- incumbent prices), `"entry_verdict"` (pane 2 -- entry prices,
#'   from [afford_entry()]), or `"stable_cat"` (pane 3 -- stability, from
#'   [afford_stability()]; stable / elevated / precarious take the same
#'   green / yellow / salmon). Any column holding either class set works.
#' @param displacement Optional displacement layer for the red hatch: a
#'   data.frame (or `.parquet`/`.rds` path) with `GEOID`/`geoid` and either
#'   `dis_group` (hatched where non-`NA` -- any income group at risk, the HPRM
#'   convention) or `dis_value` (hatched where `< 0`).
#' @param colors Three fill colors, in class order good / middle / bad
#'   (default the report palette: green `#74b56e`, pale yellow `#fbf2b4`,
#'   salmon `#f4a582` -- shared by all three panes).
#' @param legend_title Legend title; default `"verdict for a <tier> household"`
#'   (`"entry verdict ..."` when `verdict_col = "entry_verdict"`).
#' @param height Optional map height in CSS pixels.
#' @param declutter Hide the basemap's decorative land tints (parks,
#'   landcover, hillshade) that wash out the fills -- see
#'   [nt_declutter_basemap()]. Default `TRUE`.
#' @param ... Passed to [nt_add_choropleth()] via [nt_map()] (e.g. `popup`,
#'   `tooltip`, `opacity`; `before_id` defaults to `"water"` so fills sit
#'   under the basemap's water and labels).
#' @return A `maplibregl` htmlwidget -- composable with [nt_map_sync()].
#' @family ern_maps
#' @seealso [afford_verdict()], [afford_capacity()], [afford_caption()],
#'   [nt_map_sync()]
#' @examples \dontrun{
#' idx <- afford_index("53", "033", 2024, tenure = "rent", geometry = TRUE) |>
#'   afford_verdict()
#' afford_map(idx)                                  # VLI verdict, King County
#' afford_map(idx, displacement = "~/data/.../hprm_v5_full_2022.parquet")
#' htmltools::browsable(htmltools::tagList(
#'   afford_map(idx), afford_caption("VLI")))       # map + its locked reading
#' }
#' @export
afford_map <- function(x, tier = "VLI", verdict_col = "verdict",
                       displacement = NULL,
                       colors = c("#74b56e", "#fbf2b4", "#f4a582"),
                       legend_title = NULL, height = NULL, declutter = TRUE,
                       ...) {
  if (!inherits(x, "sf"))
    stop("`x` must be an sf object -- run afford_index(..., geometry = TRUE).",
         call. = FALSE)
  stopifnot(is.character(verdict_col), length(verdict_col) == 1L)
  if (!verdict_col %in% names(x)) {
    if (verdict_col == "verdict" &&
        all(c("supply", "supply_stretch") %in% names(x))) {
      x <- afford_verdict(x)
    } else {
      stop("`x` has no `", verdict_col, "` column -- see afford_verdict()",
           if (verdict_col == "entry_verdict") " / afford_entry()"
           else if (verdict_col == "stable_cat") " / afford_stability()", ".",
           call. = FALSE)
    }
  }
  if (!"ami_tier" %in% names(x))
    stop("`x` needs an `ami_tier` column (an afford_index() result).", call. = FALSE)
  stopifnot(length(colors) == 3L)
  tier <- as.character(tier)[1]
  v <- x[!is.na(x[[verdict_col]]) & as.character(x$ami_tier) == tier, ]
  if (nrow(v) == 0L)
    stop("No rows for ami_tier = \"", tier, "\" with a non-missing `",
         verdict_col, "`.", call. = FALSE)

  vals <- stats::na.omit(unique(as.character(v[[verdict_col]])))
  lmap <- if (all(vals %in% names(.afi_verdict_labels))) .afi_verdict_labels
          else if (all(vals %in% names(.afi_stability_labels))) .afi_stability_labels
          else stop("`", verdict_col, "` must hold verdict classes (affordable /",
                    " roughly affordable / not affordable) or stability classes",
                    " (stable / elevated / precarious).", call. = FALSE)
  lv <- unname(lmap)
  v$verdict <- factor(unname(lmap[as.character(v[[verdict_col]])]), levels = lv)
  # keep each class's color even when a class is absent from this region
  present <- lv[lv %in% unique(as.character(v$verdict))]
  cols <- unname(colors)[match(present, lv)]
  v <- v[, intersect(c("GEOID", "county", "verdict", "supply", "supply_stretch"),
                     names(v))]

  if (is.null(legend_title))
    legend_title <-
      if (identical(unname(lmap), unname(.afi_stability_labels)))
        "stability verdict (can they stay?)"
      else sprintf(
        if (verdict_col == "entry_verdict") "entry verdict for a %s household"
        else "verdict for a %s household", tier)
  dots <- list(...)
  if (!"before_id" %in% names(dots)) dots$before_id <- "water"
  m <- do.call(nt_map, c(list(v, color = "verdict", type = "categorical",
                              colors = cols, legend_title = legend_title),
                         dots))

  if (!is.null(displacement)) {
    d <- .afi_read_tabular(displacement)
    gcol <- if ("GEOID" %in% names(d)) "GEOID"
      else if ("geoid" %in% names(d)) "geoid"
      else stop("`displacement` needs a GEOID/geoid column.", call. = FALSE)
    at_risk <- if ("dis_group" %in% names(d)) !is.na(d$dis_group)
      else if ("dis_value" %in% names(d)) !is.na(d$dis_value) & d$dis_value < 0
      else stop("`displacement` needs a `dis_group` or `dis_value` column.",
                call. = FALSE)
    hg <- v[v$GEOID %in% as.character(d[[gcol]])[at_risk], ]
    if (nrow(hg) > 0L)
      m <- mapgl::add_line_layer(m, id = "displacement_hatch",
        source = .nt_hatch_lines(hg), line_color = "#c1272d",
        line_width = 0.9, line_opacity = 0.85)
  }
  if (isTRUE(declutter)) m <- nt_declutter_basemap(m)
  if (!is.null(height)) m$height <- height
  m
}

#' The standard reading and "what ifs" for the verdict maps
#'
#' @description
#' The caption block that must travel with [afford_map()], per pane:
#'
#' **`pane = "affordability"`** (pane 1): the simple cost reading (*"this is
#' how much a \code{tier} household will pay if they can move in there"*), the
#' locked red-class descriptor (*"even at the rents current tenants pay, a
#' \code{tier} family would give up more than half their income in these
#' neighborhoods -- and entry prices run higher still"*), and the three
#' assumptions: **if they can move in** (price-only), **at incumbent prices**
#' (entry runs higher), **at the top of the tier's income range**.
#'
#' **`pane = "availability"`** (pane 2, [afford_entry()]): the entry reading
#' (*"this is how much a \code{tier} household will pay to move into this
#' neighborhood today"*) and its assumptions: **if a unit opens and they are
#' accepted**, **asking rents are estimated** (ZIP index shifted to tracts),
#' and **competition is not yet netted out** (the CHAS adjustment).
#'
#' **`pane = "stability"`** (pane 3, [afford_stability()]): the staying
#' reading (*"this is whether a \code{tier} household that gets in can
#' stay"*, on HPRM's published displacement + eviction anchors) and its
#' assumptions: **place-level risk, not a personal guarantee**, **a 2022
#' model against 2024 prices**, and **California's eviction signal is
#' out-of-sample** (lean on the displacement side there).
#' @param tier Tier label used in the text (default `"VLI"`).
#' @param pane `"affordability"` (default), `"availability"`, or
#'   `"stability"`.
#' @return An `htmltools` tag; place it under the map (e.g.
#'   `htmltools::tagList(afford_map(x), afford_caption())`).
#' @seealso [afford_map()], [afford_entry()]
#' @export
afford_caption <- function(tier = "VLI",
                           pane = c("affordability", "availability",
                                    "stability")) {
  tier <- as.character(tier)[1]
  pane <- match.arg(pane)
  li <- function(b, ...) htmltools::tags$li(htmltools::tags$b(b), ...)
  wrap <- function(...) htmltools::div(
    style = "font-size:0.85rem; line-height:1.45; max-width:62em; color:#333;", ...)
  bq <- function(txt) htmltools::tags$blockquote(
    style = "border-left:3px solid #c1272d; margin:6px 0; padding:2px 10px; color:#7a1f1a;",
    htmltools::tags$b(txt))
  ul <- function(...) htmltools::tags$ul(
    style = "margin:4px 0 0 0; padding-left:1.2em;", ...)

  if (pane == "affordability") {
    return(wrap(
      htmltools::tags$p(
        htmltools::tags$b(sprintf(
          "This is how much a %s household will pay if they can move in there:",
          tier)),
        " green \u2014 the typical unit takes no more than 30% of their income;",
        " orange \u2014 30\u201350% (attainable only by stretching);",
        " red \u2014 more than half their income."),
      bq(sprintf(
        paste0("Even at the rents current tenants pay, a %s family would give ",
               "up more than half their income in these neighborhoods \u2014 ",
               "and entry prices run higher still."), tier)),
      ul(
        li("\u201cIf they can move in.\u201d",
           " The map is price-only: it says nothing about whether a unit is open",
           " (availability) or whether the applicant is accepted (screening)."),
        li("At incumbent prices.",
           " Classes are priced at what current tenants pay (gross rent,",
           " utilities included); asking rents on the units that open run",
           " higher, so the map understates the entry barrier."),
        li(sprintf("At the top of the %s range.", tier),
           sprintf(" The share is computed at the %s income ceiling;", tier),
           " households deeper in the band give up a larger share of income for",
           " the same unit."))))
  }

  if (pane == "availability") {
    return(wrap(
      htmltools::tags$p(
        htmltools::tags$b(sprintf(
          "This is how much a %s household will pay to move into this neighborhood today:",
          tier)),
        " the same three classes as the affordability verdict, priced at",
        " today's asking rents instead of what sitting tenants pay."),
      bq(paste0("The door costs more than the rent roll: where the affordability",
                " map is green and this one is not, the neighborhood is",
                " affordable only to those already in it.")),
      ul(
        li("If a unit opens and they are accepted.",
           " Openings (turnover/vacancy) are reported alongside;",
           " screening on records, credit, and vouchers is still excluded."),
        li("Asking rents are estimated.",
           " A ZIP-level asking index (typical listing, smoothed) shifted onto",
           " tracts by housing-unit weights; tracts with thin listing coverage",
           " fall back to the area's median premium."),
        li("Competition is not yet netted out.",
           " An affordable unit occupied by a higher-income household still",
           " counts (the CHAS adjustment is the next layer), so these are",
           " ceilings."))))
  }

  wrap(
    htmltools::tags$p(
      htmltools::tags$b(sprintf(
        "This is whether a %s household that gets in can stay:", tier)),
      " green \u2014 stable (no measurable displacement and below-average eviction",
      " risk, on HPRM's published anchors); orange \u2014 elevated risk;",
      " red \u2014 precarious (High/Extreme displacement or eviction pressure)."),
    bq("A place you can afford and enter but cannot keep is not a destination."),
    ul(
      li("Place-level risk, not a personal guarantee.",
         " HPRM is built from individual-level records but published as tract",
         " rates; it describes the neighborhood's pressure, not any one lease."),
      li("A 2022 model against 2024 prices.",
         " Stability is HPRM v5 (2022); the price panes use 2020\u201324 ACS",
         " and current asking rents \u2014 a two-year vintage gap."),
      li("California's eviction signal is out-of-sample.",
         " EER training excluded CA (flagged per tract); the displacement",
         " side (EDR) is the robust read there.")))
}
