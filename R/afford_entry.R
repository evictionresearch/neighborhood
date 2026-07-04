# =============================================================================
# Pane 2 -- Availability, component B: the ENTRY PRICE layer.
#
# Pane 1 (afford_index / afford_verdict) prices tracts at what current tenants
# pay (ACS standing gross rents). Movers face asking rents. This layer measures
# the wedge -- the tract's ENTRY PREMIUM = asking / standing, from Zillow's
# ZIP-level Observed Rent Index brought onto tracts with block-exact
# housing-unit weights -- and re-scores affordability at the ticket window:
# entry_supply, entry_verdict, and openings-at-entry counts. Design + verified
# feasibility: dev/availability-design.md.
# =============================================================================

.afi_zori_url <- paste0("https://files.zillowstatic.com/research/public_csvs/",
                        "zori/Zip_zori_uc_sfrcondomfr_sm_month.csv")

# ---- internal: tidy a raw ZORI frame to one row per ZIP for one month --------
.afi_tidy_zori <- function(z, month = NULL) {
  mo <- names(z)[grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", names(z))]
  if (!length(mo))
    stop("No month columns found -- is this the ZORI csv?", call. = FALSE)
  month <- if (is.null(month)) mo[length(mo)] else as.character(month)
  if (!month %in% mo)
    stop("Month \"", month, "\" not in the ZORI file (it runs ",
         mo[1], " to ", mo[length(mo)], ").", call. = FALSE)
  keep <- data.frame(
    zip         = sprintf("%05d", as.integer(as.character(z$RegionName))),
    state       = as.character(z$State),
    city        = as.character(z$City),
    metro       = as.character(z$Metro),
    county_name = as.character(z$CountyName),
    zori        = as.numeric(z[[month]]),
    month       = month,
    stringsAsFactors = FALSE)
  keep[!is.na(keep$zori), , drop = FALSE]
}

#' Download and tidy Zillow's ZIP-level asking-rent index (ZORI)
#'
#' @description
#' Fetches (and caches) Zillow's Observed Rent Index — the smoothed typical
#' **asking rent** of listed units, ZIP level, monthly — and returns one row
#' per ZIP for one month. This is the market-entry price signal behind
#' [afford_entry()]: pane 1 prices tracts at what sitting tenants pay; ZORI is
#' what the door costs.
#' @param month Month column to keep (e.g. `"2026-05-31"`); `NULL` (default)
#'   uses the latest in the file.
#' @param path Optional path to an already-downloaded ZORI csv. Default caches
#'   under `tools::R_user_dir("neighborhood", "cache")`.
#' @param refresh Re-download even if the cache exists (default `FALSE`; the
#'   file updates monthly).
#' @param quiet Passed to [utils::download.file()].
#' @return A data.frame: `zip`, `state`, `city`, `metro`, `county_name`,
#'   `zori` (typical asking rent, $/month), `month`.
#' @seealso [afford_entry()], [nt_zcta_weights()]
#' @examples \dontrun{
#' z <- afford_zori()
#' median(z$zori[z$county_name == "King County" & z$state == "WA"])
#' }
#' @export
afford_zori <- function(month = NULL, path = NULL, refresh = FALSE,
                        quiet = FALSE) {
  if (is.null(path)) {
    dir <- tools::R_user_dir("neighborhood", "cache")
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    path <- file.path(dir, "zori_zip_month.csv")
  }
  if (!file.exists(path) || isTRUE(refresh)) {
    utils::download.file(.afi_zori_url, path, mode = "wb", quiet = quiet)
  }
  .afi_tidy_zori(utils::read.csv(path, check.names = FALSE), month)
}

# ---- internal: national ZCTA median gross rent (standing), one ACS vintage ---
.afi_zcta_rent <- function(year) {
  d <- tidycensus::get_acs(geography = "zcta",
                           variables = c(standing = "B25064_001"),
                           year = year, survey = "acs5", cache_table = TRUE)
  data.frame(zcta = as.character(d$GEOID), standing = as.numeric(d$estimate),
             stringsAsFactors = FALSE)
}

# ---- internal: HU-weighted tract premium from ZIP premiums -------------------
# w: zcta/tract_geoid/frac_hu; zp: zip/premium. Coverage = the share of the
# tract's housing units in ZIPs that carry a premium; tracts under `cover_min`
# fall back to the study-area median premium.
.afi_tract_premium <- function(w, zp, cover_min = 0.25) {
  j <- dplyr::left_join(w, zp, by = c("zcta" = "zip"))
  tr <- dplyr::summarize(dplyr::group_by(j, tract_geoid),
    coverage = sum(frac_hu[!is.na(premium)], na.rm = TRUE),
    premium  = if (any(!is.na(premium)))
                 stats::weighted.mean(premium, frac_hu * !is.na(premium),
                                      na.rm = TRUE) else NA_real_,
    .groups = "drop")
  fallback <- stats::median(zp$premium, na.rm = TRUE)
  low <- is.na(tr$premium) | tr$coverage < cover_min
  tr$premium[low] <- fallback
  tr$premium_fallback <- low
  tr
}

#' Re-score affordability at entry prices (pane 2: availability)
#'
#' @description
#' Pane 1 ([afford_index()] + [afford_verdict()]) prices each tract at what
#' **current tenants** pay. `afford_entry()` re-scores it at what **entering**
#' the tract costs: each tract gets an **entry premium** (asking / standing
#' rent — Zillow ZORI over ACS ZIP standing rents, brought to tracts with
#' block-exact housing-unit weights), the rent distribution is shifted by it,
#' and the same 30%/50% verdict grammar is applied at the door price:
#' `entry_supply`, `entry_verdict`, and — when Gate-2 availability columns are
#' present — `available_entry_turnover`/`available_entry_vacancy` (units that
#' both open up *and* are affordable at entry prices).
#' @details
#' Mechanics: a unit is entry-affordable when `standing_rent * premium <=
#' income_cutoff * burden / 12`, evaluated on the tract's `B25063` rent
#' brackets by interpolation (equivalently, the pane-1 cost line divided by
#' the premium). The premium mixes the true mover premium with drift between
#' the ACS 5-year window and the ZORI month — both push entry costs up, which
#' is exactly the correction pane 1 needs; disentangling them (ACS
#' recent-mover rents) is a documented validation step. Rental rows only
#' (`tenure == "rent"`); other tenures pass through with `NA` entry columns.
#' Tracts whose ZIPs lack ZORI coverage below `cover_min` (HU-share) fall back
#' to the study-area median premium and are flagged `premium_fallback`.
#'
#' **Not yet in this layer:** competition (an affordable unit occupied by a
#' higher-income household — the CHAS adjustment) and screening. Counts are
#' ceilings.
#' @param x An [afford_index()] result (any tiers; `rent` rows get entry
#'   columns). Must carry `GEOID`, `county`, `year`, `ami_tier`,
#'   `income_cutoff`, `total`.
#' @param zori A frame from [afford_zori()] (or one with `zip` + `zori`).
#' @param weights A crosswalk from [nt_zcta_weights()] for the same counties
#'   (`zcta`, `tract_geoid`, `frac_hu`). `NULL` builds it (downloads blocks +
#'   the national ZCTA file on first use — slow once, cached after).
#' @param burden,stretch The two burden lines (defaults `0.30`/`0.50`) — match
#'   what you passed to [afford_index()].
#' @param cover_min Minimum HU-share of a tract in ZORI-covered ZIPs before
#'   the fallback premium is used (default `0.25`).
#' @return `x` plus: `entry_premium`, `premium_coverage`, `premium_fallback`,
#'   `accessible_entry`, `accessible_entry_stretch`, `supply_entry`,
#'   `supply_entry_stretch`, `entry_verdict` (same three classes as
#'   [afford_verdict()]), and `available_entry_turnover` /
#'   `available_entry_vacancy` when availability columns are present.
#' @seealso [afford_zori()], [nt_zcta_weights()], [afford_capacity()] (gains
#'   per-100-at-entry columns), [afford_map()] (`verdict_col =
#'   "entry_verdict"`)
#' @examples \dontrun{
#' idx <- afford_index("53", "033", 2024, tenure = "rent", geometry = TRUE) |>
#'   afford_verdict()
#' ent <- afford_entry(idx, afford_zori())
#' afford_capacity(ent)                       # per-100 at standing vs entry
#' afford_map(ent, verdict_col = "entry_verdict")
#' }
#' @export
afford_entry <- function(x, zori, weights = NULL,
                         burden = 0.30, stretch = 0.50, cover_min = 0.25) {
  req <- c("GEOID", "county", "year", "ami_tier", "income_cutoff", "total",
           "tenure")
  if (!is.data.frame(x) || !all(req %in% names(x)))
    stop("`x` must be an afford_index() result (missing: ",
         paste(setdiff(req, names(x)), collapse = ", "), ").", call. = FALSE)
  if (!is.data.frame(zori) || !all(c("zip", "zori") %in% names(zori)))
    stop("`zori` must come from afford_zori() (zip + zori columns).",
         call. = FALSE)
  stopifnot(is.numeric(burden), burden > 0, burden < 1,
            is.numeric(stretch), stretch > burden, stretch < 1)

  year <- unique(x$year)[1]
  state <- unique(substr(x$county, 1, 2))
  if (length(state) != 1)
    stop("`x` spans multiple states; run afford_entry() per state.", call. = FALSE)
  counties <- unique(substr(x$county, 3, 5))

  if (is.null(weights)) weights <- nt_zcta_weights(state, counties)
  if (!all(c("zcta", "tract_geoid", "frac_hu") %in% names(weights)))
    stop("`weights` must come from nt_zcta_weights().", call. = FALSE)

  # ZIP premium = asking (ZORI) / standing (ACS ZCTA median gross rent)
  standing <- .afi_zcta_rent(year)
  zp <- dplyr::inner_join(zori[, c("zip", "zori")], standing,
                          by = c("zip" = "zcta"))
  zp <- zp[!is.na(zp$zori) & !is.na(zp$standing) & zp$standing > 0, ]
  zp$premium <- zp$zori / zp$standing
  zp <- zp[is.finite(zp$premium) & zp$premium > 0, c("zip", "premium")]
  zp <- zp[zp$zip %in% weights$zcta, , drop = FALSE]
  if (nrow(zp) == 0)
    stop("No ZORI-covered ZIPs intersect the study counties.", call. = FALSE)

  tp <- .afi_tract_premium(weights[, c("zcta", "tract_geoid", "frac_hu")],
                           zp, cover_min = cover_min)
  if (any(tp$premium_fallback))
    warning(sum(tp$premium_fallback), " tract(s) below ", cover_min,
            " ZORI HU-coverage use the study-area median premium.",
            call. = FALSE)

  # entry-affordable units: standing brackets against the deflated cost line
  br <- .afi_get_brackets("tract", "B25063", state, counties, year)
  key <- unique(sf::st_drop_geometry(x[x$tenure == "rent",
           c("GEOID", "ami_tier", "income_cutoff")]))
  key <- dplyr::left_join(key, tp, by = c("GEOID" = "tract_geoid"))
  j <- dplyr::inner_join(br, key, by = "GEOID",
                         relationship = "many-to-many")
  ent <- dplyr::summarize(dplyr::group_by(j, GEOID, ami_tier),
    entry_premium    = dplyr::first(premium),
    premium_coverage = dplyr::first(coverage),
    premium_fallback = dplyr::first(premium_fallback),
    accessible_entry = .afi_interp_le(
      lo, hi, n, dplyr::first(income_cutoff) * burden / 12 / dplyr::first(premium)),
    accessible_entry_stretch = .afi_interp_le(
      lo, hi, n, dplyr::first(income_cutoff) * stretch / 12 / dplyr::first(premium)),
    .groups = "drop")

  out <- dplyr::left_join(x, ent, by = c("GEOID", "ami_tier"))
  not_rent <- out$tenure != "rent"
  if (any(not_rent)) {
    for (cl in c("entry_premium", "premium_coverage", "premium_fallback",
                 "accessible_entry", "accessible_entry_stretch"))
      out[[cl]][not_rent] <- NA
    message("afford_entry(): entry pricing is a rental concept -- ",
            "non-rent rows get NA entry columns.")
  }
  out$supply_entry <- ifelse(out$total > 0, out$accessible_entry / out$total,
                             NA_real_)
  out$supply_entry_stretch <- ifelse(out$total > 0,
    out$accessible_entry_stretch / out$total, NA_real_)
  out$entry_verdict <- factor(
    ifelse(is.na(out$supply_entry) | is.na(out$supply_entry_stretch), NA_character_,
    ifelse(out$supply_entry >= 0.5, "affordable",
    ifelse(out$supply_entry_stretch >= 0.5, "roughly affordable",
           "not affordable"))),
    levels = c("affordable", "roughly affordable", "not affordable"))
  if ("turnover_rate" %in% names(out))
    out$available_entry_turnover <- out$accessible_entry * out$turnover_rate
  if ("vacancy_rate" %in% names(out))
    out$available_entry_vacancy <- out$accessible_entry * out$vacancy_rate
  out
}
