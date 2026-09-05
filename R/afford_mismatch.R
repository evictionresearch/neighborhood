# =============================================================================
# Where they are vs where they could go -- the mismatch readout.
#
# The funnel says how many units a tier can afford, enter, and keep
# (afford_index -> afford_entry -> afford_stability). These two functions ask
# the geographic question the funnel leaves open: do the tier's households
# LIVE where its openings ARE?
#   * afford_typology()  -- a four-cell tract classification: many households
#     with many openings (stable), many with few (lock-in risk), few with many
#     (latent option), few of both. One legend, the whole mismatch story.
#   * afford_mismatch()  -- the numbers: a dissimilarity index between the
#     tier's household and openings distributions, and exposure statistics
#     (what kind of tract the households sit in, vs what kind the openings
#     would hand a mover -- the counterfactual where moves follow the market
#     alone). If the counterfactual exposure is no better than the observed,
#     prices by themselves reproduce the segregated geography; if it is worse,
#     protections and subsidies are doing the holding, and anyone forced to
#     move through the market lands somewhere worse.
#
# Folded in from the standalone afford prototype, 2026-09-04. Both functions
# are frame -> frame (no API), so they test offline.
# =============================================================================

# ---- internal: two-group dissimilarity index --------------------------------
# 0.5 * sum |a_i/sum(a) - b_i/sum(b)|; the share of either group that would
# have to relocate for the two distributions to match. NA counts contribute
# nothing; a group with no mass has no distribution to compare (NA).
.afi_dissimilarity <- function(a, b) {
  a <- ifelse(is.na(a), 0, a)
  b <- ifelse(is.na(b), 0, b)
  if (sum(a) <= 0 || sum(b) <= 0) return(NA_real_)
  0.5 * sum(abs(a / sum(a) - b / sum(b)))
}

# ---- internal: resolve which openings column drives the comparison ----------
.afi_openings_col <- function(x, openings) {
  cols <- c(entry_turnover = "available_entry_turnover",
            turnover = "available_turnover",
            vacancy = "available_vacancy",
            accessible = "accessible")
  if (openings == "auto") {
    hit <- cols[cols %in% names(x)][1]
    if (is.na(hit)) stop("no openings column found -- run afford_index() ",
                         "(availability = TRUE) first.", call. = FALSE)
    message("afford: openings = \"auto\" using `", hit, "`.")
    return(unname(hit))
  }
  col <- cols[[openings]]
  if (!col %in% names(x))
    stop("openings = \"", openings, "\" needs the `", col, "` column -- ",
         "run the producing step first.", call. = FALSE)
  col
}

#' @title Tract typology: households vs openings, four cells
#' @description
#' Classifies each rental tract x tier row of an [afford_index()] result by
#' whether the tier's **households** are there and whether its **openings**
#' are there, split at the study-area medians: `stable` (many households,
#' many openings), `lockin` (many households, few openings -- the tier lives
#' there but could not re-enter at today's churn; displacement risk), `latent`
#' (few households, many openings -- where they could go), `exclusion` (few of
#' both). This is the one-legend map of the mismatch between where a tier
#' lives and where the market would admit it.
#' @details
#' Households come from [afford_households()] (cumulative tiers, so pass its
#' default output -- bands would change the question). Openings default to the
#' most door-priced column available (`available_entry_turnover`, then
#' `available_turnover`, then bare `accessible`, with a message). Tracts with
#' fewer than `min_renters` renter households are left unclassified rather
#' than letting a handful of households swing a cell. Medians are computed per
#' tier over the classified tracts and ride along as `typology_med_hh` /
#' `typology_med_open`, so a map legend can state its own cutpoints.
#' Non-rent rows pass through with `NA` typology columns.
#' @param x An [afford_index()] result (rent rows; run with
#'   `availability = TRUE` for openings-based cells).
#' @param households An [afford_households()] result for the same
#'   state/counties/year/tiers (renter rows are used).
#' @param openings Which column counts as openings: `"auto"` (default),
#'   `"entry_turnover"`, `"turnover"`, `"vacancy"`, or `"accessible"`
#'   (price-only, no churn).
#' @param min_renters Minimum renter households for a tract to be classified
#'   (default `50`).
#' @return `x` plus `hh`, `hh_moe`, `hh_total` (from `households`),
#'   `openings`, `typology_med_hh`, `typology_med_open`, and `cell` (factor
#'   `stable` / `lockin` / `latent` / `exclusion`; `NA` under `min_renters`
#'   or off-tenure).
#' @seealso [afford_households()], [afford_mismatch()], [afford_index()]
#' @examples \dontrun{
#' idx <- afford_index("06", "081", 2024, tenure = "rent")
#' hh  <- afford_households("06", "081", 2024)
#' ty  <- afford_typology(idx, hh)
#' table(ty$cell[ty$ami_tier == "ELI"])
#' }
#' @export
afford_typology <- function(x, households,
                            openings = c("auto", "entry_turnover", "turnover",
                                         "vacancy", "accessible"),
                            min_renters = 50) {
  openings <- match.arg(openings)
  req <- c("GEOID", "tenure", "ami_tier")
  if (!is.data.frame(x) || !all(req %in% names(x)))
    stop("`x` must be an afford_index() result (missing: ",
         paste(setdiff(req, names(x)), collapse = ", "), ").", call. = FALSE)
  hreq <- c("GEOID", "tenure_grp", "ami_tier", "hh", "hh_moe", "hh_total")
  if (!is.data.frame(households) || !all(hreq %in% names(households)))
    stop("`households` must come from afford_households().", call. = FALSE)
  stopifnot(is.numeric(min_renters), length(min_renters) == 1, min_renters >= 0)

  ocol <- .afi_openings_col(x, openings)

  h <- households[households$tenure_grp == "renter",
                  c("GEOID", "ami_tier", "hh", "hh_moe", "hh_total")]
  h$ami_tier <- as.character(h$ami_tier)
  out <- x
  out$.tier_chr <- as.character(out$ami_tier)
  out <- dplyr::left_join(out, h,
                          by = c("GEOID", ".tier_chr" = "ami_tier"))
  out$.tier_chr <- NULL
  not_rent <- out$tenure != "rent"
  if (any(not_rent)) {
    out$hh[not_rent] <- NA; out$hh_moe[not_rent] <- NA
    out$hh_total[not_rent] <- NA
    message("afford_typology(): households are renter households -- ",
            "non-rent rows get NA typology columns.")
  }
  out$openings <- out[[ocol]]
  out$openings[not_rent] <- NA

  out$typology_med_hh <- NA_real_
  out$typology_med_open <- NA_real_
  out$cell <- NA_character_
  for (ti in unique(out$ami_tier[!not_rent])) {
    i <- which(!not_rent & out$ami_tier == ti)
    live <- i[!is.na(out$hh_total[i]) & out$hh_total[i] >= min_renters &
              !is.na(out$hh[i]) & !is.na(out$openings[i])]
    if (length(live) < 4) next            # a median over 3 tracts is not a split
    med_hh <- stats::median(out$hh[live])
    med_op <- stats::median(out$openings[live])
    out$typology_med_hh[i] <- med_hh
    out$typology_med_open[i] <- med_op
    hi_h <- out$hh[live] >= med_hh
    hi_o <- out$openings[live] >= med_op
    out$cell[live] <- ifelse(hi_h & hi_o, "stable",
                      ifelse(hi_h, "lockin",
                      ifelse(hi_o, "latent", "exclusion")))
  }
  out$cell <- factor(out$cell,
                     levels = c("stable", "lockin", "latent", "exclusion"))
  out
}

#' @title Mismatch statistics: dissimilarity and exposure, observed vs market
#' @description
#' For each tenure x tier of an [afford_typology()] result, how far "where the
#' tier's households are" sits from "where its openings are": a dissimilarity
#' index between the two distributions, and -- when tract `context` is
#' supplied -- exposure statistics with a counterfactual reading. If ELI
#' households were redistributed across tracts in proportion to ELI openings
#' (moves following the market alone), what tract poverty rate would they
#' face, against what they face now and what the average renter faces?
#' @details
#' The dissimilarity index is the share of either distribution that would
#' have to relocate for the two to match (0 = the tier lives exactly where
#' its openings are; 1 = fully separate). For each numeric column `v` of
#' `context` three weighted means are returned: `v_hh` (weighted by tier
#' households -- observed exposure), `v_open` (weighted by openings -- the
#' market counterfactual), and `v_all` (weighted by all renter households --
#' the benchmark). `v_open` worse than `v_hh` says non-market mechanisms
#' (tenure protections, subsidies, long tenancies) are holding observed
#' exposure down, and a household forced to move through the market lands
#' somewhere worse -- a displacement reading, not just a segregation one.
#' @param x An [afford_typology()] result (needs `hh`, `hh_total`,
#'   `openings`).
#' @param context Optional data.frame: `GEOID` plus numeric tract columns
#'   (poverty rate, an HPRM stability score, a racial-composition share).
#'   Build one with tidycensus, e.g. B17001 poverty below /
#'   universe per tract.
#' @return A tibble, one row per tenure x tier: `n_tracts`,
#'   `tier_households` (sum of tract-level `hh`), `openings_total`,
#'   `dissimilarity`, and the three exposure columns per context variable.
#' @references
#' Bruch, E., and J. Swait. 2019. Choice Set Formation in Residential
#' Mobility and Its Implications for Segregation Dynamics. *Demography*
#' 56(5):1665-1692. \doi{10.1007/s13524-019-00810-5}
#'
#' Kang, S., J. S. Jeon, and W. Airgood-Obrycki. 2024. Exploring mismatch in
#' within-metropolitan affordable housing in the United States. *Urban
#' Studies* 61(2):231-253. \doi{10.1177/00420980231180490}
#' @seealso [afford_typology()], [afford_capacity()] (units-and-people
#'   arithmetic without the geography)
#' @examples \dontrun{
#' ty <- afford_typology(afford_index("06", "081", 2024, tenure = "rent"),
#'                       afford_households("06", "081", 2024))
#' pov <- tidycensus::get_acs("tract", variables = c(below = "B17001_002",
#'          uni = "B17001_001"), state = "06", county = "081", year = 2024,
#'          survey = "acs5", output = "wide")
#' pov <- data.frame(GEOID = pov$GEOID, poverty = pov$belowE / pov$uniE)
#' afford_mismatch(ty, context = pov)
#' }
#' @export
afford_mismatch <- function(x, context = NULL) {
  req <- c("GEOID", "tenure", "ami_tier", "hh", "hh_total", "openings")
  if (!is.data.frame(x) || !all(req %in% names(x)))
    stop("`x` must be an afford_typology() result (missing: ",
         paste(setdiff(req, names(x)), collapse = ", "), ").", call. = FALSE)
  if (inherits(x, "sf")) x <- sf::st_drop_geometry(x)
  ctx_vars <- character(0)
  if (!is.null(context)) {
    stopifnot("`context` needs a GEOID column" = "GEOID" %in% names(context))
    ctx_vars <- setdiff(names(context), "GEOID")
    stopifnot("`context` variables must be numeric" =
                all(vapply(context[ctx_vars], is.numeric, logical(1))))
    x <- dplyr::left_join(x, context, by = "GEOID")
  }

  wmean <- function(v, w) {
    ok <- !is.na(v) & !is.na(w) & w > 0
    if (!any(ok)) return(NA_real_)
    sum(v[ok] * w[ok]) / sum(w[ok])
  }
  out <- dplyr::summarize(dplyr::group_by(x, tenure, ami_tier),
    n_tracts        = dplyr::n_distinct(GEOID),
    tier_households = sum(hh, na.rm = TRUE),
    openings_total  = sum(openings, na.rm = TRUE),
    dissimilarity   = .afi_dissimilarity(hh, openings),
    .groups = "drop")
  for (v in ctx_vars) {
    x$.ctx <- x[[v]]
    ex <- dplyr::summarize(dplyr::group_by(x, tenure, ami_tier),
      ctx_hh   = wmean(.ctx, hh),
      ctx_open = wmean(.ctx, openings),
      ctx_all  = wmean(.ctx, hh_total),
      .groups = "drop")
    names(ex)[match(c("ctx_hh", "ctx_open", "ctx_all"), names(ex))] <-
      paste0(v, c("_hh", "_open", "_all"))
    out <- dplyr::left_join(out, ex, by = c("tenure", "ami_tier"))
  }
  out
}
