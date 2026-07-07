# =============================================================================
# Stable-destination layer (Gate 3: "Allowed/stay") -- joins HPRM housing
# stability onto an afford_index() result and completes the
# afford -> available -> stable chain.
#
# A good destination is not just affordable, and not just open: it is one a
# household can actually STAY in. This layer reads displacement (EDR) and
# eviction (EER) risk from ERN's HPRM, maps each to a 0-1 stability score on
# HPRM's own published anchors, and weights the (afford x available) attainable
# set by it. See dev/good-place-design.md and hprm/manuscripts/a-good-place.md.
# =============================================================================

# ---- internal: read/normalize HPRM stability inputs (GEOID, dis_value, ev_value)
.afi_hprm_fields <- function(hprm) {
  if (is.data.frame(hprm)) {
    d <- hprm
  } else if (is.character(hprm) && length(hprm) == 1L) {
    if (grepl("\\.parquet$", hprm, ignore.case = TRUE)) {
      if (!requireNamespace("arrow", quietly = TRUE))
        stop("Reading an HPRM .parquet path needs the 'arrow' package ",
             "(install.packages('arrow')) -- or pass a data.frame.", call. = FALSE)
      d <- as.data.frame(arrow::read_parquet(hprm))
    } else if (grepl("\\.(rds|qs2?)$", hprm, ignore.case = TRUE) &&
               grepl("\\.rds$", hprm, ignore.case = TRUE)) {
      d <- as.data.frame(readRDS(hprm))
    } else stop("`hprm` path must be a .parquet or .rds file.", call. = FALSE)
  } else stop("`hprm` must be a data.frame or a path to a .parquet/.rds file.", call. = FALSE)

  nm <- names(d)
  gcol <- if ("GEOID" %in% nm) "GEOID" else if ("geoid" %in% nm) "geoid" else
    stop("`hprm` needs a GEOID/geoid column.", call. = FALSE)
  for (f in c("dis_value", "ev_value"))
    if (!f %in% nm) stop("`hprm` is missing the '", f, "' column.", call. = FALSE)
  data.frame(GEOID = as.character(d[[gcol]]),
             dis_value = as.numeric(d$dis_value),
             ev_value  = as.numeric(d$ev_value),
             stringsAsFactors = FALSE)
}

#' @title Stable-destination layer: weight affordable, available supply by HPRM stability
#' @description
#' Joins ERN's Housing Precarity Risk Model (HPRM) onto an [afford_index()]
#' result and completes the destination chain **afford -> available -> stable**:
#' of the units a group can afford ([afford_index()] Gate 1) and that are
#' actually open ([afford_index()] Gate 2 `available_*`), how many are in tracts
#' a low-income household can plausibly **stay** in (low displacement + eviction
#' risk, Gate 3)? Stability is a *place-level risk descriptor*, not an
#' individual-outcome claim (the EDR/EER signals are built from individual data
#' but published as tract rates).
#' @details
#' Two HPRM fields drive stability, each mapped to 0-1 on HPRM's **published risk
#' anchors**:
#' \itemize{
#'   \item **Displacement** `dis_value` (EDR; net out-migration of low-income
#'     renters -- households *leaving*): `s_edr = clamp01((dis_value + 300)/350)`
#'     (`+50 -> 1.0`, `-300` Extreme `-> 0`).
#'   \item **Eviction** `ev_value` (EER; filing-rate ratio, the added precarity of
#'     the formal-eviction channel): `s_eer = clamp01((2 - ev_value)/1.2)`
#'     (`0.8 -> 1.0`, `2.0` Extreme `-> 0`).
#' }
#' `stability_source` chooses which to use -- these are *different harms* and can
#' diverge (HPRM's policy-substitution effect). `"edr"` (displacement only) is
#' the robust choice where EER is out-of-sample: HPRM's EER training excluded
#' California, so `ev_value` is out-of-sample there (flagged in
#' `eer_out_of_sample`). The attainable set entering the stability gate is the
#' chosen `availability` count; `stable_dest = attainable * stability`.
#'
#' @section Why stability, not an "opportunity" score:
#' The conventional third layer would be an opportunity index (the Opportunity
#' Atlas, the Child Opportunity Index, California's siting maps). This layer
#' deliberately measures **housing stability** instead, for four reasons
#' developed in `vignette("three-panes")` and, in full, in
#' `dev/good-place-design.md`:
#'
#' 1. Opportunity scores aggregate *individual outcomes* to tracts; the
#'    authors of the Opportunity Atlas themselves attribute only about three
#'    fifths of its tract-to-tract variation to causal neighborhood effects,
#'    the rest to sorting (Chetty et al. 2018, rev. 2025). The forces that
#'    decide who lives where are structural -- income inequality, wage
#'    stagnation, labor-market restructuring (Chapple 2017) -- so this product
#'    measures the *structural constraint on the choice set* (afford, enter,
#'    stay), not aggregated outcomes read as a property of land.
#' 2. "High-opportunity" encodes a contested, suburban-and-white definition of
#'    good (Goetz 2018); LIHTC residents' own assessments and employment
#'    outcomes track the California opportunity domains weakly or not at all,
#'    and what residents value is affordability, predictability, and the
#'    ability to stay (Reid 2019). Stable, affordable housing is itself the
#'    platform for mobility.
#' 3. Individual relocation to "opportunity" tracts leaves the sorted
#'    metropolitan structure intact (Sampson 2008), and steering households to
#'    designated tracts risks recreating segregation -- hence the headline is
#'    the size of the constrained choice set, never a prescribed destination.
#' 4. Displacement measurement is proxy-dependent; tract-composition proxies
#'    repeatedly miss it (Carlson 2020). EDR/EER are estimated from
#'    household-level panel and court-record data (the individual approach),
#'    then published as tract rates -- which is why the output here is a
#'    place-level risk descriptor, never an individual prediction.
#'
#' @references
#' Carlson, H. J. (2020). Measuring Displacement: Assessing Proxies for
#' Involuntary Residential Mobility. *City & Community*, 19(3), 573-592.
#' \doi{10.1111/cico.12482}
#'
#' Chapple, K. (2017). Income Inequality and Urban Displacement: The New
#' Gentrification. *New Labor Forum*, 26(1), 84-93.
#' \doi{10.1177/1095796016682018}
#'
#' Chetty, R., Friedman, J. N., Hendren, N., Jones, M. R., & Porter, S. R.
#' (2018; revised 2025). The Opportunity Atlas: Mapping the Childhood Roots of
#' Social Mobility. NBER Working Paper 25147.
#'
#' Goetz, E. G. (2018). *The One-Way Street of Integration: Fair Housing and
#' the Pursuit of Racial Justice in American Cities*. Cornell University Press.
#'
#' Reid, C. K. (2019). Rethinking "Opportunity" in the Siting of Affordable
#' Housing in California: Resident Perspectives on the Low-Income Housing Tax
#' Credit. *Housing Policy Debate*, 29(4), 645-669.
#' \doi{10.1080/10511482.2019.1582549}
#'
#' Sampson, R. J. (2008). Moving to Inequality: Neighborhood Effects and
#' Experiments Meet Social Structure. *American Journal of Sociology*, 114(1),
#' 189-231. \doi{10.1086/589843}
#' @param x An [afford_index()] result (must have `GEOID`; for the default
#'   `availability`, the `available_turnover`/`available_vacancy` columns from
#'   `afford_index(..., availability = TRUE)`).
#' @param hprm The HPRM data: a data.frame with `GEOID`/`geoid`, `dis_value`,
#'   `ev_value`, or a path to a `.parquet` (needs the `arrow` package) or `.rds`.
#' @param stability_source `"both"` (default; weighted average), `"edr"`
#'   (displacement only), or `"eer"` (eviction only).
#' @param edr_weight Weight on EDR when `stability_source = "both"` (default
#'   `0.5`; EER gets `1 - edr_weight`).
#' @param availability Which afford_index availability measure feeds the chain:
#'   `"turnover"` (default, `available_turnover`), `"vacancy"`
#'   (`available_vacancy`), or `"none"` (afford-only -- uses `accessible`,
#'   skipping Gate 2).
#' @param stable_cut Stability at/above which a tract counts as a stable
#'   destination (default `0.8`, ~ HUD "At Risk or better").
#' @param caution_cut Stability at/above which a tract is "elevated" rather than
#'   "precarious" (default `0.5`).
#' @return `x` with added columns: `dis_value`, `ev_value`, `s_edr`, `s_eer`,
#'   `stability`, `eer_out_of_sample`, `attainable` (the availability count fed
#'   to the gate), `stable_dest` (`attainable * stability` -- the continuous
#'   headline), `stable` (logical hard filter), and `stable_cat` (a
#'   stable/elevated/precarious factor). Re-running on a result that already
#'   carries these columns simply recomputes them (idempotent).
#' @seealso [afford_index()], [afford_bands()]
#' @examples \dontrun{
#' idx <- afford_index("53", "033", 2024, tenure = "rent")        # King County
#' sd  <- afford_stability(idx, hprm = "~/data/.../hprm_v5_full_2022.parquet")
#' # the stable-destination set for VLI renters (continuous, availability folded in):
#' sd[sd$ami_tier == "VLI", c("GEOID", "supply", "stability", "stable_dest", "stable_cat")]
#' # displacement-only stability (robust where EER is out-of-sample, e.g. CA):
#' afford_stability(idx, hprm = hprm_df, stability_source = "edr")
#' }
#' @export
afford_stability <- function(x, hprm,
                             stability_source = c("both", "edr", "eer"),
                             edr_weight = 0.5,
                             availability = c("turnover", "vacancy", "none"),
                             stable_cut = 0.8, caution_cut = 0.5) {
  stability_source <- match.arg(stability_source)
  availability <- match.arg(availability)
  if (!is.data.frame(x) || !"GEOID" %in% names(x))
    stop("`x` must be an afford_index() result (with a GEOID column).", call. = FALSE)
  stopifnot(edr_weight >= 0, edr_weight <= 1, caution_cut <= stable_cut)

  # idempotent: re-running on a prior result (or any frame that already carries
  # stability columns) replaces them instead of creating .x/.y join collisions
  stale <- intersect(names(x), c("dis_value", "ev_value", "s_edr", "s_eer",
                                 "stability", "eer_out_of_sample", "attainable",
                                 "stable_dest", "stable", "stable_cat"))
  if (length(stale)) x <- x[, setdiff(names(x), stale)]

  h <- .afi_hprm_fields(hprm)
  clamp01 <- function(v) pmin(pmax(v, 0), 1)
  h$s_edr <- clamp01((h$dis_value + 300) / 350)   # +50 -> 1, -300 (Extreme) -> 0
  h$s_eer <- clamp01((2 - h$ev_value) / 1.2)        # 0.8 -> 1, 2.0 (Extreme) -> 0
  h$stability <- switch(stability_source,
    both = edr_weight * h$s_edr + (1 - edr_weight) * h$s_eer,
    edr  = h$s_edr,
    eer  = h$s_eer)
  h$eer_out_of_sample <- grepl("^06", h$GEOID)      # CA excluded from EER training

  out <- dplyr::left_join(x, h, by = "GEOID")

  avail_col <- switch(availability, turnover = "available_turnover",
                      vacancy = "available_vacancy", none = NA_character_)
  if (!is.na(avail_col) && !avail_col %in% names(out))
    stop("availability = \"", availability, "\" needs the `", avail_col,
         "` column -- run afford_index(..., availability = TRUE).", call. = FALSE)
  out$attainable <- if (is.na(avail_col)) out$accessible else out[[avail_col]]

  out$stable_dest <- out$attainable * out$stability        # afford x available x stable
  out$stable <- !is.na(out$stability) & out$attainable > 0 & out$stability >= stable_cut
  out$stable_cat <- factor(
    ifelse(is.na(out$stability), NA_character_,
    ifelse(out$stability >= stable_cut, "stable",
    ifelse(out$stability >= caution_cut, "elevated", "precarious"))),
    levels = c("stable", "elevated", "precarious"))
  out
}
