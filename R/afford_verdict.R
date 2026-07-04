# =============================================================================
# The free-will readout: the verdict and the capacity arithmetic.
#
# The question the index exists to answer: if everyone could move freely,
# paying no more than 30% (or, stretching, 50%) of income in rent -- WHERE are
# those places, how many UNITS are available, and how many PEOPLE could move
# there? afford_index() computes the ingredients; these two functions turn them
# into the answer:
#   * afford_verdict()  -- the WHERE, as a three-class map verdict
#     (affordable / roughly affordable / not affordable). Public-facing maps
#     must classify, not shade: a continuous gradient that renders a place
#     "mid" is read as "there isn't a problem."
#   * afford_capacity() -- the UNITS and PEOPLE: affordable units per 100 tier
#     households, the openings per year, and the regional shortfall.
# See dev/affordability-deep-dive.md for the design and the v1 autopsy.
# =============================================================================

#' @title The three-class affordability verdict (affordable / roughly / not)
#' @description
#' Classifies each tract x tenure x tier row of an [afford_index()] result into
#' a plain-language verdict for the tier's households:
#' \itemize{
#'   \item **affordable** -- most of the stock is within reach at the standard
#'     burden (default 30% of income);
#'   \item **roughly affordable** -- not that, but most of the stock is within
#'     reach by *stretching* to the `stretch` burden (default 50%, HUD's
#'     severe-burden threshold);
#'   \item **not affordable** -- even at the stretch line, most of the stock is
#'     out of reach.
#' }
#' This is the headline map layer: policy audiences read maps by color, so the
#' end product renders verdicts (e.g. green / orange / red), with continuous
#' shares demoted to tooltips or appendices.
#' @details
#' At the default `share_cut = 0.5` (a tract-majority rule) each class is a
#' **median-unit sentence**: "the typical rental in this tract would cost a
#' household at this income no more than 30% of income" (affordable), "30-50%"
#' (roughly), "more than half their income" (not affordable).
#'
#' The verdict is a **price statement** (Gate 1). It says nothing about whether
#' an affordable unit is open (`available_*`, Gate 2), whether the household
#' would be accepted (Gate 3), or whether it could *stay*
#' ([afford_stability()]) -- read it alongside those layers, and see
#' [afford_capacity()] for the units-and-people arithmetic.
#' @param x An [afford_index()] result with `supply` and `supply_stretch`
#'   (i.e. run with a non-`NULL` `stretch`; `0.50` is the default).
#' @param share_cut Share of a tract's stock that must clear a burden line for
#'   the verdict (default `0.5` -- the majority / median-unit rule).
#' @return `x` with a `verdict` factor
#'   (`affordable` / `roughly affordable` / `not affordable`; `NA` where the
#'   tract has no priced stock). For maps, pair the levels with unambiguous
#'   traffic-light colors, e.g. `#1a9850` / `#fdae61` / `#d73027`.
#' @seealso [afford_index()], [afford_capacity()], [afford_stability()]
#' @examples \dontrun{
#' idx <- afford_index("53", "033", 2024, tenure = "rent")
#' v   <- afford_verdict(idx)
#' table(v$verdict[v$ami_tier == "VLI"])
#' }
#' @export
afford_verdict <- function(x, share_cut = 0.5) {
  req <- c("supply", "supply_stretch")
  if (!is.data.frame(x) || !all(req %in% names(x)))
    stop("`x` must be an afford_index() result with `supply` and `supply_stretch` ",
         "columns -- run afford_index() with a non-NULL `stretch` ",
         "(0.50 is the default).", call. = FALSE)
  stopifnot(is.numeric(share_cut), length(share_cut) == 1,
            share_cut > 0, share_cut <= 1)
  x$verdict <- factor(
    ifelse(is.na(x$supply) | is.na(x$supply_stretch), NA_character_,
    ifelse(x$supply >= share_cut, "affordable",
    ifelse(x$supply_stretch >= share_cut, "roughly affordable",
           "not affordable"))),
    levels = c("affordable", "roughly affordable", "not affordable"))
  x
}

#' @title Free-will capacity: units and people, per tenure and tier
#' @description
#' The units-and-people arithmetic for an [afford_index()] result: per tenure x
#' tier, how many units the tier's households could rent/own at the standard
#' burden (`affordable_units`, and per 100 tier households), how many of those
#' actually open up (`open_per_year` from turnover, `open_now` from vacancy),
#' and the regional **shortfall** (tier households minus affordable units --
#' the deficit that remains *even if everyone could re-sort freely*). This is
#' the NLIHC-Gap-style readout ("X affordable homes per 100 households"),
#' before any competition adjustment.
#' @details
#' Demand (`tier_households` = `reg_hh_tier`) is **regional**: if you filter
#' `x` to a subset of tracts first, the result reads "the tracts kept could
#' absorb X per 100 of the *region's* tier households," which is usually what
#' you want (e.g. capacity of the stable set). Counts are not yet
#' competition-adjusted -- units affordable to the tier but occupied by
#' higher-income households still count, so these are ceilings (NLIHC's
#' "affordable and available" adjustment is the planned refinement).
#' @param x An [afford_index()] result (one row per tract x tenure x tier;
#'   `sf` geometry is dropped). Works on banded ([afford_bands()]) input too,
#'   where each row set is a non-overlapping slice.
#' @return A tibble, one row per tenure x tier: `n_tracts`, `tier_households`,
#'   `affordable_units`, `per100_affordable`, `shortfall` (positive =
#'   shortage), plus -- when the columns are present -- the stretch-line
#'   (`affordable_stretch_units`, `per100_stretch`), availability
#'   (`open_per_year`, `per100_open_year`, `open_now`, `per100_open_now`,
#'   `tracts_lt1_open`), and entry-price ([afford_entry()]:
#'   `affordable_entry_units`, `per100_entry`, `open_entry_per_year`,
#'   `per100_open_entry_year`) readouts.
#' @seealso [afford_index()], [afford_verdict()]
#' @examples \dontrun{
#' idx <- afford_index("53", "033", 2024, tenure = "rent")
#' afford_capacity(idx)                                  # region-wide, all tiers
#' afford_capacity(idx[idx$ami_tier == "VLI", ])         # the pilot group
#' }
#' @export
afford_capacity <- function(x) {
  req <- c("GEOID", "tenure", "ami_tier", "accessible", "reg_hh_tier")
  if (!is.data.frame(x) || !all(req %in% names(x)))
    stop("`x` must be an afford_index() result (missing columns).", call. = FALSE)
  if (inherits(x, "sf")) x <- sf::st_drop_geometry(x)
  if (anyDuplicated(x[, c("GEOID", "tenure", "ami_tier")]))
    warning("duplicate GEOID x tenure x ami_tier rows -- unit sums will double-count.",
            call. = FALSE)
  has_st <- "accessible_stretch" %in% names(x)
  has_av <- all(c("available_turnover", "available_vacancy") %in% names(x))
  has_en <- "accessible_entry" %in% names(x)
  has_eo <- "available_entry_turnover" %in% names(x)
  has_sd <- "stable_dest" %in% names(x)
  has_es <- all(c("available_entry_turnover", "stability") %in% names(x))

  out <- dplyr::summarize(dplyr::group_by(x, tenure, ami_tier),
    n_tracts         = dplyr::n_distinct(GEOID),
    tier_households  = dplyr::first(reg_hh_tier),
    affordable_units = sum(accessible, na.rm = TRUE),
    affordable_stretch_units =
      if (has_st) sum(accessible_stretch, na.rm = TRUE) else NA_real_,
    open_per_year = if (has_av) sum(available_turnover, na.rm = TRUE) else NA_real_,
    open_now      = if (has_av) sum(available_vacancy,  na.rm = TRUE) else NA_real_,
    tracts_lt1_open =
      if (has_av) sum(available_turnover < 1, na.rm = TRUE) else NA_integer_,
    affordable_entry_units =
      if (has_en) sum(accessible_entry, na.rm = TRUE) else NA_real_,
    open_entry_per_year =
      if (has_eo) sum(available_entry_turnover, na.rm = TRUE) else NA_real_,
    stable_dest_per_year =
      if (has_sd) sum(stable_dest, na.rm = TRUE) else NA_real_,
    open_entry_stable_per_year =
      if (has_es) sum(available_entry_turnover * stability, na.rm = TRUE)
      else NA_real_,
    .groups = "drop")

  per100 <- function(units) ifelse(out$tier_households > 0,
                                   units / out$tier_households * 100, NA_real_)
  out$per100_affordable <- per100(out$affordable_units)
  out$per100_stretch    <- per100(out$affordable_stretch_units)
  out$per100_open_year  <- per100(out$open_per_year)
  out$per100_open_now   <- per100(out$open_now)
  out$per100_entry      <- per100(out$affordable_entry_units)
  out$per100_open_entry_year <- per100(out$open_entry_per_year)
  out$per100_stable_dest <- per100(out$stable_dest_per_year)
  out$per100_open_entry_stable <- per100(out$open_entry_stable_per_year)
  out$shortfall         <- out$tier_households - out$affordable_units

  keep <- c("tenure", "ami_tier", "n_tracts", "tier_households",
    "affordable_units", "per100_affordable", "shortfall",
    if (has_st) c("affordable_stretch_units", "per100_stretch"),
    if (has_av) c("open_per_year", "per100_open_year",
                  "open_now", "per100_open_now", "tracts_lt1_open"),
    if (has_en) c("affordable_entry_units", "per100_entry"),
    if (has_eo) c("open_entry_per_year", "per100_open_entry_year"),
    if (has_sd) c("stable_dest_per_year", "per100_stable_dest"),
    if (has_es) c("open_entry_stable_per_year", "per100_open_entry_stable"))
  out[, keep]
}
