# =============================================================================
# Affordability index v2 -- AMI tier cutoffs (the income-ceiling engine)
#
# Per county, the income ceilings for the standardized affordability tiers
# (ELI/VLI/LI/MI), from one of several sources -- most exact first:
#
#   "hud"     Official published HUD income limits (bundled snapshot first, then
#             the live HUD API). EXACT -- recommended for reports. Durable
#             offline once a snapshot is bundled (data-raw/build_hud_il.R).
#   "hud_acs" Census-only reproduction of HUD's cascade (median FAMILY income
#             B19113 trended from FY-2 by the CBO factor, FMR high-cost bump,
#             ELI poverty floor, LI US-median cap, HUD family-size factors).
#             Reproduces HUD in NON-high-cost areas. In high-cost areas where
#             HUD's year-over-year increase cap binds it OVERSTATES the limit:
#             that cap chains back to HUD's own publication history and cannot
#             be reproduced from public data (e.g. San Diego FY2024 VLI(4) is
#             68,900 x 1.10 = 75,750, NOT the FMR formula's ~82,600). Longevity
#             insurance for years HUD never published. Validated, divergence
#             documented -- not for verbatim-HUD reports in high-cost regions.
#   "acs_fmr" Naive ACS fractions PLUS the FMR high-cost bump (HUD-like cascade
#             on current-year ACS; no trend, no YoY cap). Census + live FMR.
#   "acs"     Pure fractions of county median FAMILY income (B19113), Census
#             only and fully transparent. Understates thresholds in high-cost
#             areas (no high-cost bump); see dev/hud-income-limits-architecture.md.
#   "auto"    (default) the most exact source available at run time: hud
#             (snapshot -> API if a HUD_API_KEY is set) -> hud_acs -> acs_fmr
#             -> acs, emitting a message when it is not the exact "hud" source.
#
# Vintage discipline ("all data relevant to `year`"): acs / acs_fmr read the
# `year` ACS5; hud_acs reads the FY-2 ACS5 and trends it forward, reproducing
# how HUD builds the FY-`year` limits; hud reads the FY-`year` HUD limits.
#
# Shared low-level helpers (.afi_norm_state/.afi_norm_counties/.afi_hh_size_factor)
# live in R/afford_index.R. See dev/hud-income-limits-architecture.md and
# dev/affordability-index-design.md.
# =============================================================================

# ---- verified HUD cascade constants (provenance: dev/ doc + primary sources) -
# Income-trend factor: HUD trends the FY-2 ACS forward to the limit year.
# FY<=2024 = CBO CPI-U forecast; FY>=2025 = CBO per-capita wage-growth forecast.
# Verified: FY2024 = 1.062 (HUD FY24 IL/Medians methodology); FY2025 = 1.08
# (CBO wages 47,460 / 2023 wages 43,920). Only verified years listed; an
# unlisted year makes hud_acs error (so "auto" falls through to acs_fmr/acs).
.afi_income_trend <- c(`2024` = 1.062, `2025` = 1.08)

# US median family income -- the 4-person LI ceiling, waived under high housing
# cost. Verified FY2024 = 97,800 (HUD FY24 methodology). Extend as verified.
.afi_us_median_fi <- c(`2024` = 97800)

# HHS poverty guideline, 48 contiguous states, by household size 1-8 (the ELI
# dollar floor). Verified 2024 (aspe.hhs.gov). Keyed by year; extend as verified.
.afi_poverty_guideline <- list(
  `2024` = c(`1` = 15060, `2` = 20440, `3` = 25820, `4` = 31200,
             `5` = 36580, `6` = 41960, `7` = 47340, `8` = 52720)
)

# High-housing-cost factor: annualized 2BR FMR -> implied VLI (= 12 * 0.85 / 0.35
# = 29.142857). This is HUD's high-cost *trigger/estimate*; HUD then applies YoY
# caps (not public-reproducible), so it is an upper estimate in capped areas.
.afi_fmr_vli_factor <- 12 * 0.85 / 0.35

# ---- internal: HUD rounding rules -------------------------------------------
.afi_round100 <- function(x) round(x / 100) * 100   # area median -> nearest $100
.afi_round50  <- function(x) ceiling(x / 50) * 50   # published limit -> up to $50

# ---- internal: area 2-bedroom Fair Market Rent (high-cost bump input) --------
# Named numeric GEOID -> monthly 2BR FMR via the HUD API; NA where unavailable.
.afi_fmr_2br <- function(state, counties, year) {
  ids <- paste0(state, counties)
  if (!requireNamespace("hudr", quietly = TRUE) || !nzchar(Sys.getenv("HUD_API_KEY")))
    return(stats::setNames(rep(NA_real_, length(ids)), ids))
  out <- vapply(counties, function(co) {
    r <- tryCatch(hudr::get_hud_fmr_data(paste0(state, co, "99999"), as.character(year)),
                  error = function(e) NULL)
    if (!is.list(r)) r <- NULL   # hudr returns an error STRING (not a condition) on API failure
    bd <- r$basicdata
    if (is.null(bd) || is.null(bd$two_bedroom)) return(NA_real_)
    msa <- suppressWarnings(as.numeric(bd$two_bedroom[bd$zip_code == "MSA level"]))
    v <- if (length(msa) && !all(is.na(msa))) msa else suppressWarnings(as.numeric(bd$two_bedroom))
    v[1]
  }, numeric(1))
  stats::setNames(out, ids)
}

# ---- internal: county median FAMILY income (B19113) at a given ACS vintage ----
.afi_acs_mfi <- function(state, counties, vintage_year) {
  m <- tidycensus::get_acs(geography = "county",
                           variables = c(mfi = "B19113_001"),
                           state = state, county = counties,
                           year = vintage_year, survey = "acs5")
  data.frame(GEOID = m$GEOID, NAME = m$NAME, mfi = m$estimate,
             stringsAsFactors = FALSE)
}

# ---- internal: build cut_<TIER> columns from a 4-person area median + VLI -----
# Implements HUD's cascade for one county: VLI may already carry the FMR bump.
# LI = 1.6*VLI (capped at US median unless high-cost, when apply_li_cap=TRUE);
# ELI = min(VLI, max(0.6*VLI, poverty)); derived tiers (e.g. MI=120%) are
# fractions of the area median. Family-size factors + $50 rounding applied last.
.afi_cascade_row <- function(geoid, name, mfi4, vli4, year, size, ami_tiers,
                             apply_li_cap, round_out = TRUE) {
  yk  <- as.character(year)
  pov <- .afi_poverty_guideline[[yk]]
  high_cost <- vli4 > 0.50 * mfi4 + 1e-6
  li4 <- 1.6 * vli4
  if (apply_li_cap && !high_cost && !is.na(.afi_us_median_fi[yk]))
    li4 <- min(li4, unname(.afi_us_median_fi[yk]))
  sf  <- unname(.afi_hh_size_factor[as.character(size)])
  r   <- if (round_out) .afi_round50 else identity
  pov_s <- if (!is.null(pov)) unname(pov[as.character(size)]) else 0
  vli <- r(vli4 * sf)
  li  <- r(li4  * sf)
  eli <- r(min(vli4 * sf, max(0.60 * vli4 * sf, pov_s)))
  out <- data.frame(GEOID = geoid, NAME = name, ami = mfi4, stringsAsFactors = FALSE)
  for (ti in names(ami_tiers)) {
    f <- ami_tiers[[ti]]
    out[[paste0("cut_", ti)]] <-
      if (isTRUE(all.equal(f, 0.30))) eli
      else if (isTRUE(all.equal(f, 0.50))) vli
      else if (isTRUE(all.equal(f, 0.80))) li
      else r(f * mfi4 * sf)                       # derived (e.g. MI = 120%)
  }
  out
}

# ---- internal: ACS-based cutoffs (pure fractions, or HUD-like FMR cascade) ----
# fmr_bump = FALSE  -> "acs"      : straight fractions of B19113 at `year`, no
#                                   poverty floor / cap / rounding (transparent).
# fmr_bump = TRUE   -> "acs_fmr"  : VLI bumped by FMR, HUD-style cascade on the
#                                   `year` ACS (no trend, no LI cap, no YoY cap).
# mfi_vintage / trend / apply_li_cap let hud_acs reuse this with FY-2 + trend.
.afi_acs_cutoffs <- function(state, counties, year, ami_tiers, hud_hh_size,
                             fmr_bump = FALSE, mfi_vintage = year, trend = 1,
                             apply_li_cap = FALSE) {
  size <- if (is.null(hud_hh_size)) 4L else as.integer(hud_hh_size)
  m <- .afi_acs_mfi(state, counties, mfi_vintage)
  m$county <- m$GEOID
  fmr <- if (fmr_bump) .afi_fmr_2br(state, counties, year) else NULL
  if (fmr_bump && !is.null(fmr) && all(is.na(fmr)))
    warning("2BR FMR unavailable (HUD API down?) -- the high-cost VLI bump is ",
            "skipped, so VLI may be understated in high-cost areas.", call. = FALSE)

  if (!fmr_bump) {                                  # pure "acs": straight fractions
    sf <- unname(.afi_hh_size_factor[as.character(size)])
    out <- data.frame(GEOID = m$GEOID, NAME = m$NAME, ami = m$mfi,
                      stringsAsFactors = FALSE)
    for (ti in names(ami_tiers))
      out[[paste0("cut_", ti)]] <- m$mfi * ami_tiers[[ti]] * sf
    return(dplyr::as_tibble(out))
  }

  rows <- lapply(seq_len(nrow(m)), function(i) {
    mfi4 <- .afi_round100(m$mfi[i] * trend)
    vli4 <- 0.50 * mfi4
    f2 <- if (!is.null(fmr)) unname(fmr[m$GEOID[i]]) else NA_real_
    if (!is.na(f2)) vli4 <- max(vli4, f2 * .afi_fmr_vli_factor)
    .afi_cascade_row(m$GEOID[i], m$NAME[i], mfi4, vli4, year, size, ami_tiers,
                     apply_li_cap = apply_li_cap)
  })
  dplyr::as_tibble(do.call(rbind, rows))
}

# ---- internal: hud_acs -- full Census-only reproduction of HUD's cascade ------
.afi_hud_acs_cutoffs <- function(state, counties, year, ami_tiers, hud_hh_size) {
  yk <- as.character(year)
  trend <- .afi_income_trend[yk]
  if (is.na(trend))
    stop("ami_source = 'hud_acs': no verified income-trend factor for FY", year,
         ". Add it to .afi_income_trend (see dev/hud-income-limits-architecture.md) ",
         "or use ami_source = 'hud' / 'acs_fmr'.", call. = FALSE)
  .afi_acs_cutoffs(state, counties, year, ami_tiers, hud_hh_size,
                   fmr_bump = TRUE, mfi_vintage = year - 2,
                   trend = unname(trend), apply_li_cap = TRUE)
}

# ---- internal: read a bundled HUD income-limit snapshot, if present ----------
# Returns the data.frame in inst/extdata/hud_il_<year>.rds if bundled, else NULL.
# Stored as extdata (not data/) so year-versioned, partial lookup snapshots do
# not pollute the user-facing data namespace. Build them with
# data-raw/build_hud_il.R (longevity / offline fallback).
.afi_hud_snapshot <- function(year) {
  f <- system.file("extdata", paste0("hud_il_", year, ".rds"),
                   package = "neighborhood")
  if (!nzchar(f) || !file.exists(f)) return(NULL)
  tryCatch(readRDS(f), error = function(e) NULL)
}

# ---- internal: official HUD income-limit cutoffs (snapshot first, then API) --
# ELI/VLI/LI come straight from HUD's published per-household-size limits (so
# caps/floors/high-cost are all baked in); a non-30/50/80 tier (e.g. MI = 120%)
# is derived as fraction x area median x family-size factor (NOT a HUD limit).
.afi_hud_cutoffs <- function(state, counties, year, ami_tiers, hud_hh_size) {
  size <- if (is.null(hud_hh_size)) 4L else as.integer(hud_hh_size)
  if (!size %in% 1:8) stop("hud_hh_size must be an integer 1-8.", call. = FALSE)
  sf <- unname(.afi_hh_size_factor[as.character(size)])

  snap <- .afi_hud_snapshot(year)
  need_api <- is.null(snap) || !all(paste0(state, counties) %in% snap$GEOID)
  if (need_api) {
    if (!requireNamespace("hudr", quietly = TRUE))
      stop("ami_source = 'hud' needs a bundled snapshot or the 'hudr' package ",
           "(install.packages('hudr')) plus a HUD_API_KEY.", call. = FALSE)
    if (!nzchar(Sys.getenv("HUD_API_KEY")))
      stop("ami_source = 'hud' needs a bundled snapshot or a HUD_API_KEY ",
           "(register free at huduser.gov; e.g. in ~/.Renviron).", call. = FALSE)
  }

  # search every sub-list for a field (robust to named OR positional API lists)
  pick <- function(il, field) {
    for (el in il) if (is.list(el) && field %in% names(el)) return(el[[field]][1])
    NA
  }
  one <- function(co) {
    geoid <- paste0(state, co)
    if (!is.null(snap) && geoid %in% snap$GEOID) {            # snapshot hit
      r <- snap[snap$GEOID == geoid, , drop = FALSE]
      med <- as.numeric(r$median_income); nm <- as.character(r$area_name)
      eli <- as.numeric(r[[paste0("il30_p", size)]])
      vli <- as.numeric(r[[paste0("il50_p", size)]])
      li  <- as.numeric(r[[paste0("il80_p", size)]])
    } else {                                                  # live HUD API
      il <- tryCatch(hudr::get_hud_il_data(paste0(geoid, "99999"), as.character(year)),
        error = function(e)        # suppress original msg: it can echo the key
          stop("HUD API call failed for county ", geoid, " (", year,
               "); original error hidden to avoid leaking the API key.", call. = FALSE))
      if (!is.list(il))            # hudr returns an error STRING on API failure
        stop("HUD API returned no usable data for county ", geoid, " (", year,
             ") -- the API may be down. Use a year covered by the bundled ",
             "snapshot, or retry later.", call. = FALSE)
      med <- as.numeric(pick(il, "median_income")); nm <- as.character(pick(il, "area_name"))
      eli <- as.numeric(pick(il, paste0("il30_p", size)))
      vli <- as.numeric(pick(il, paste0("il50_p", size)))
      li  <- as.numeric(pick(il, paste0("il80_p", size)))
    }
    out <- data.frame(GEOID = geoid,
                      NAME = if (length(nm) && !is.na(nm)) nm else NA_character_,
                      ami = med, stringsAsFactors = FALSE)
    for (ti in names(ami_tiers)) {
      f <- ami_tiers[[ti]]
      out[[paste0("cut_", ti)]] <-
        if (isTRUE(all.equal(f, 0.30))) eli
        else if (isTRUE(all.equal(f, 0.50))) vli
        else if (isTRUE(all.equal(f, 0.80))) li
        else f * med * sf                  # derived (e.g. MI = 120%); not a HUD limit
    }
    out
  }
  dplyr::as_tibble(do.call(rbind, lapply(counties, one)))
}

#' @title Area Median Income tier cutoffs by county
#' @description
#' Returns each study county's Area Median Income (AMI) and the income ceilings
#' for the standardized affordability tiers (ELI/VLI/LI/MI), from one of several
#' sources. AMI is anchored on county median **family** income (ACS `B19113`,
#' HUD's anchor).
#' @details
#' **Choosing a source.** Every source returns the same shape; they differ only
#' in how the tier ceilings are set:
#' \itemize{
#'   \item `"hud"` -- HUD's official published limits. Use for any report that
#'     must match HUD verbatim. Needs a bundled snapshot, or `hudr` plus a
#'     `HUD_API_KEY`. The live HUD API is rate-limited, so bundle a snapshot
#'     (`data-raw/build_hud_il.R`) for production use.
#'   \item `"hud_acs"` -- a Census-only reproduction of HUD's cascade. Close to
#'     HUD in ordinary areas, but **overstates the limits in high-cost areas**
#'     where HUD's year-over-year increase cap binds: that cap depends on HUD's
#'     own publication history and cannot be reproduced from public data (e.g.
#'     San Diego FY2024 VLI is 75,750 from HUD vs ~82,600 here). It also uses
#'     *county* median income, so it diverges from HUD in *multi-county metros*
#'     (HUD uses the metro-wide median). A fallback for years/areas HUD has not
#'     published -- not for high-cost reports.
#'   \item `"acs_fmr"` -- naive fractions plus HUD's FMR high-cost bump (no
#'     trend, no cap). Census + live FMR.
#'   \item `"acs"` -- pure fractions of `B19113`. Fully transparent and
#'     Census-only, but understates thresholds in high-cost areas (no bump).
#'   \item `"auto"` (default) -- the most exact available, in order
#'     `hud -> hud_acs -> acs_fmr -> acs`; prints which it used when it is not
#'     the exact HUD source.
#' }
#' **Vintage discipline.** `acs`/`acs_fmr` read the `year` ACS 5-year data;
#' `hud_acs` reads the FY-2 ACS and trends it forward (how HUD builds the
#' FY-`year` limits); `hud` reads HUD's FY-`year` limits. `hud_acs`'s trend
#' factor is bundled only for verified years (currently 2024-2025); other years
#' error so `auto` falls back. **Tiers are cumulative ceilings** -- a VLI
#' household can use anything affordable at or below 50% AMI.
#' @param state State (FIPS code like `"06"` or a name/abbreviation like `"CA"`).
#' @param counties County or counties (3-digit FIPS like `"075"` or names).
#' @param year Limit year (the ACS5 endpoint for `acs`/`acs_fmr`; the HUD fiscal
#'   year for `hud`/`hud_acs`).
#' @param ami_source One of `"auto"` (default; most-exact available: hud ->
#'   hud_acs -> acs_fmr -> acs), `"hud"` (official published limits, snapshot
#'   then API), `"hud_acs"` (Census-only HUD-cascade reproduction; overstates in
#'   high-cost capped areas), `"acs_fmr"` (ACS + FMR high-cost bump), or `"acs"`
#'   (pure fractions of B19113, Census-only).
#' @param ami_tiers Named numeric vector of tier fractions of AMI. Default
#'   `c(ELI = .30, VLI = .50, LI = .80, MI = 1.20)`.
#' @param hud_hh_size Household size (1-8) for the HUD family-size adjustment;
#'   default `4`. `NULL` leaves AMI on the 4-person basis.
#' @return A tibble with one row per county: `GEOID`, `NAME`, `ami`, and one
#'   `cut_<TIER>` income-ceiling column per tier.
#' @seealso [afford_index()]
#' @examples \dontrun{
#' # Default "auto": exact HUD limits when reachable, else Census fallback
#' ami_cutoffs("06", c("073", "075"), 2024)
#'
#' # Force exact HUD limits (a report that must match HUD verbatim)
#' ami_cutoffs("06", "073", 2024, ami_source = "hud")
#'
#' # Fully reproducible, Census-only (understates high-cost areas)
#' ami_cutoffs("06", "073", 2024, ami_source = "acs")
#'
#' # 2-person household basis instead of the 4-person default
#' ami_cutoffs("06", "073", 2024, hud_hh_size = 2)
#'
#' # Names work too; custom tiers
#' ami_cutoffs("CA", "San Diego", 2024, ami_tiers = c(ELI = .30, VLI = .50))
#' }
#' @export
ami_cutoffs <- function(state, counties, year = 2024,
                        ami_source = c("auto", "hud", "hud_acs", "acs_fmr", "acs"),
                        ami_tiers = c(ELI = 0.30, VLI = 0.50, LI = 0.80, MI = 1.20),
                        hud_hh_size = 4) {
  ami_source <- match.arg(ami_source)
  state <- .afi_norm_state(state)
  counties <- .afi_norm_counties(state, counties)
  if (!is.null(hud_hh_size) && !as.character(hud_hh_size) %in% names(.afi_hh_size_factor))
    stop("hud_hh_size must be 1-8 (or NULL).", call. = FALSE)

  resolve <- function(src) switch(src,
    hud     = .afi_hud_cutoffs(state, counties, year, ami_tiers, hud_hh_size),
    hud_acs = .afi_hud_acs_cutoffs(state, counties, year, ami_tiers, hud_hh_size),
    acs_fmr = .afi_acs_cutoffs(state, counties, year, ami_tiers, hud_hh_size,
                               fmr_bump = TRUE),
    acs     = .afi_acs_cutoffs(state, counties, year, ami_tiers, hud_hh_size,
                               fmr_bump = FALSE))

  if (ami_source != "auto") return(resolve(ami_source))

  for (src in c("hud", "hud_acs", "acs_fmr", "acs")) {       # most-exact first
    res <- tryCatch(resolve(src), error = function(e) NULL)
    if (!is.null(res)) {
      if (src != "hud")
        message("ami_cutoffs(): ami_source='auto' used '", src,
                "' (exact HUD limits unavailable). Pass ami_source explicitly to override.")
      return(res)
    }
  }
  stop("ami_cutoffs(): no AMI source succeeded under 'auto'.", call. = FALSE)
}
