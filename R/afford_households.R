# =============================================================================
# Tract-level households by AMI tier -- the demand side at tract grain.
#
# afford_index() compares tract supply to REGIONAL tier households; nothing in
# the engine said how many tier households live in each tract. Consumers
# reimplemented that locally (hprm_sanmateo code/f_ami_tiers.R was the worked
# copy); this promotes it, with the two disciplines that build earned:
#   * open-top honesty: a cutoff falling inside B25118's open $150,000+
#     bracket snaps DOWN to the bracket floor and says so -- the data cannot
#     resolve a line drawn inside an unbounded bracket, and interpolating
#     against an invented ceiling fabricates households (the exact bug the
#     2026-09-04 afford-prototype comparison caught: an invented $300k cap
#     conjured 8-12% of the top bracket into the LI tier in four Bay Area
#     counties). Modeling the tail (e.g. Pareto) is a choice needing sign-off,
#     not a silent default.
#   * margins of error on every count, same RSS construction as afford_index().
#
# Feeds afford_typology() / afford_mismatch() (where they are vs where the
# openings are) and per-tract popups.
# =============================================================================

# ---- internal: snap county cutoffs down to the open-top bracket floor --------
# cuts is an ami_cutoffs() frame; tiers whose cut_<T> exceeds top_floor get the
# floor instead. Returns the cuts frame with cut_<T>_eff / cut_<T>_snapped
# alongside each original column. A non-finite top_floor (no open bracket)
# makes this a no-op.
.afi_snap_cuts <- function(cuts, tier_names, top_floor) {
  for (ti in tier_names) {
    cc <- cuts[[paste0("cut_", ti)]]
    snapped <- is.finite(top_floor) & cc > top_floor
    cuts[[paste0("cut_", ti, "_eff")]] <- ifelse(snapped, top_floor, cc)
    cuts[[paste0("cut_", ti, "_snapped")]] <- snapped
  }
  cuts
}

# ---- internal: per-tract cumulative tier households from a bracket frame -----
# br: GEOID, county, tenure_grp, lo, hi, n, m. cuts: one row per county with
# cut_<T>_eff / cut_<T>_snapped columns. Pure frame -> frame, so tests need no
# API. Returns long: one row per GEOID x tenure_grp x tier.
.afi_hh_from_brackets <- function(br, cuts, tier_names) {
  totals <- dplyr::summarize(dplyr::group_by(br, GEOID, tenure_grp),
                             hh_total = sum(n, na.rm = TRUE), .groups = "drop")
  per_tier <- lapply(tier_names, function(ti) {
    cc <- cuts[, c("county", paste0("cut_", ti), paste0("cut_", ti, "_eff"),
                   paste0("cut_", ti, "_snapped"))]
    names(cc) <- c("county", "income_cutoff", "income_cutoff_eff", "cutoff_snapped")
    d <- dplyr::left_join(br, cc, by = "county")
    g <- dplyr::summarize(dplyr::group_by(d, GEOID, county, tenure_grp),
      hh     = .afi_interp_le(lo, hi, n, dplyr::first(income_cutoff_eff)),
      hh_moe = .afi_interp_le_moe(lo, hi, m, dplyr::first(income_cutoff_eff)),
      income_cutoff     = dplyr::first(income_cutoff),
      income_cutoff_eff = dplyr::first(income_cutoff_eff),
      cutoff_snapped    = dplyr::first(cutoff_snapped),
      .groups = "drop")
    g$ami_tier <- ti
    g
  })
  out <- dplyr::left_join(dplyr::bind_rows(per_tier), totals,
                          by = c("GEOID", "tenure_grp"))
  out$ami_tier <- factor(out$ami_tier, levels = tier_names)
  out
}

#' @title Tract-level households by AMI tier (the demand side at tract grain)
#' @description
#' For each census tract, the number of households at or below each AMI tier's
#' income ceiling, by tenure, from `B25118` (tenure x household income)
#' interpolated within brackets against [ami_cutoffs()] -- the same income
#' definitions [afford_index()] prices supply with, so the two sides of any
#' comparison cut at the same dollars. This is the engine home for what
#' consumers previously reimplemented locally for map popups.
#' @details
#' **Tiers are cumulative ceilings**, like [afford_index()]'s: the `VLI` row
#' counts every household at or below 50% AMI. `bands = TRUE` differences them
#' into non-overlapping slices (the 30-50% band, and so on), MOEs differenced
#' by the same disjoint-bracket identity [afford_bands()] uses; households
#' above the top tier are `hh_total` minus the top cumulative row.
#'
#' **Open-top honesty.** `B25118`'s top bracket is unbounded ("$150,000 or
#' more"). A tier cutoff that lands inside it cannot be interpolated without
#' inventing a bracket width, which fabricates households -- so the cutoff
#' snaps down to the bracket floor, `cutoff_snapped` flags the rows, a message
#' names the counties, and `income_cutoff_eff` records the dollars actually
#' applied next to the HUD `income_cutoff` they stand in for. In high-cost
#' counties the LI (80%) line commonly snaps; report the understatement rather
#' than hiding it. Counts are estimates on a uniform-within-bracket
#' assumption; tract MOEs are wide -- read distributions, not single counts.
#' @param state State (FIPS like `"06"` or name/abbreviation).
#' @param counties County or counties (3-digit FIPS or names).
#' @param year ACS 5-year endpoint year (default `2024`).
#' @param ami_source,ami_tiers,hud_hh_size Passed to [ami_cutoffs()]; same
#'   defaults as [afford_index()].
#' @param tenure `"renter"` (default), `"owner"`, or both.
#' @param bands `FALSE` (default) for cumulative tiers; `TRUE` for
#'   non-overlapping bands.
#' @return A long tibble, one row per tract x tenure x tier: `GEOID`,
#'   `county`, `tenure_grp`, `ami_tier`, `hh`, `hh_moe`, `hh_total` (the
#'   tenure universe), `income_cutoff` (HUD dollars), `income_cutoff_eff`
#'   (dollars applied; differs only when snapped), `cutoff_snapped`, `year`,
#'   `ami_source`.
#' @seealso [afford_index()], [afford_typology()], [afford_mismatch()],
#'   [ami_cutoffs()]
#' @examples \dontrun{
#' hh <- afford_households("06", "081", 2024)          # San Mateo renters
#' hh[hh$ami_tier == "ELI", c("GEOID", "hh", "hh_moe")]
#'
#' # Non-overlapping bands for a popup table
#' afford_households("06", "081", 2024, bands = TRUE)
#' }
#' @export
afford_households <- function(state, counties, year = 2024,
                              ami_source = c("auto", "hud", "hud_acs",
                                             "acs_fmr", "acs"),
                              ami_tiers = c(ELI = 0.30, VLI = 0.50,
                                            LI = 0.80, MI = 1.20),
                              hud_hh_size = 4,
                              tenure = c("renter", "owner"),
                              bands = FALSE) {
  ami_source <- match.arg(ami_source)
  tenure <- match.arg(tenure, several.ok = TRUE)
  state <- .afi_norm_state(state)
  counties <- .afi_norm_counties(state, counties)

  cuts <- ami_cutoffs(state, counties, year, ami_source, ami_tiers, hud_hh_size)
  if (!is.null(attr(cuts, "ami_source"))) ami_source <- attr(cuts, "ami_source")
  cuts$county <- cuts$GEOID
  tier_names <- names(ami_tiers)

  br <- .afi_get_tenure_brackets("tract", state, counties, year)
  br <- br[br$tenure_grp %in% tenure, , drop = FALSE]
  br$county <- substr(br$GEOID, 1, 5)

  top_floor <- suppressWarnings(max(br$lo[is.infinite(br$hi)], na.rm = TRUE))
  cuts <- .afi_snap_cuts(cuts, tier_names, top_floor)
  for (ti in tier_names) {
    sn <- cuts$county[cuts[[paste0("cut_", ti, "_snapped")]]]
    if (length(sn))
      message(sprintf(paste0(
        "afford_households(): %s limit falls inside the open $%s+ bracket in ",
        "%s -- ceiling drawn at the bracket floor (see income_cutoff_eff)."),
        ti, format(top_floor, big.mark = ","), paste(sn, collapse = ", ")))
  }

  out <- .afi_hh_from_brackets(br, cuts, tier_names)
  out$year <- year
  out$ami_source <- ami_source

  if (isTRUE(bands)) {
    out <- dplyr::arrange(dplyr::group_by(out, GEOID, tenure_grp), ami_tier)
    out <- dplyr::mutate(out,
      hh_moe = sqrt(pmax(hh_moe^2 - dplyr::lag(hh_moe, default = 0)^2, 0)),
      hh     = hh - dplyr::lag(hh, default = 0))
    out <- dplyr::ungroup(out)
  }
  dplyr::as_tibble(out[order(out$GEOID, out$tenure_grp, out$ami_tier), ])
}
