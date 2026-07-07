# =============================================================================
# Affordability index v2 -- Phase 1 (supply engine / Gate 1: "Afford")
#
# Rebuilds the tract-level affordability index on a defensible footing:
#   * AMI from county median FAMILY income (B19113), per HUD's anchor
#   * income brackets parsed from ACS variable LABELS (not positional rep())
#   * within-bracket linear interpolation (no closest()-snapping)
#   * rent + both owner cost bases (A = current monthly cost, B = cost to buy in)
#   * standardized ELI/VLI/LI/MI tiers, all household sizes (default 4)
#   * two burden lines: the 30% standard plus a 50% "stretch" (severe-burden)
#     line, so "roughly affordable" is measurable (see afford_verdict())
#
# See dev/affordability-index-design.md and dev/hud-income-limits-architecture.md.
# This is Gate 1 of the three-gate "choice" funnel; availability and barrier
# gates arrive in later phases.
# =============================================================================

# ---- internal: AMI tier fractions -------------------------------------------
# ELI/VLI/LI/MI as cumulative ceilings (a VLI household can use anything
# affordable at <= 50% AMI). HI is everything above MI and is not a ceiling.
.afi_default_tiers <- c(ELI = 0.30, VLI = 0.50, LI = 0.80, MI = 1.20)

# ---- internal: HUD family-size adjustment factors (1..8 persons) -------------
.afi_hh_size_factor <- c(`1` = 0.70, `2` = 0.80, `3` = 0.90, `4` = 1.00,
                         `5` = 1.08, `6` = 1.16, `7` = 1.24, `8` = 1.32)

# ---- internal: rough default 30-yr mortgage rate by year (own_buyin) ---------
# Used only when interest_rate is not supplied. Approximate annual averages;
# override with `interest_rate` for precision.
.afi_default_rate <- function(year) {
  tbl <- c(`2015` = 0.039, `2016` = 0.037, `2017` = 0.040, `2018` = 0.045,
           `2019` = 0.040, `2020` = 0.031, `2021` = 0.030, `2022` = 0.054,
           `2023` = 0.068, `2024` = 0.069, `2025` = 0.067, `2026` = 0.066)
  k <- as.character(year)
  if (k %in% names(tbl)) unname(tbl[k]) else 0.068
}

# ---- internal: annual mortgage constant (payment per $ of loan, per year) ----
.afi_mortgage_constant <- function(interest_rate, term_years = 30) {
  i <- interest_rate / 12
  n <- term_years * 12
  if (i <= 0) return(1 / term_years)
  12 * (i / (1 - (1 + i)^(-n)))
}

# ---- internal: price -> required-income multiplier (own_buyin, basis B) ------
# required_income = price * factor, where the household spends <= 30% of income
# on (mortgage P&I on the financed portion) + (taxes & insurance on full price).
.afi_price_income_factor <- function(interest_rate, term_years = 30,
                                     down_pct = 0.10, tax_ins_rate = 0.0125,
                                     burden = 0.30) {
  amc <- .afi_mortgage_constant(interest_rate, term_years)
  ((1 - down_pct) * amc + tax_ins_rate) / burden
}

# ---- internal: interpolated count of units with cost <= cutoff ---------------
# Uniform-within-bracket assumption. For an open-topped bracket (hi = Inf) with a
# finite cutoff above its floor, (cutoff - lo) / (hi - lo) -> 0, so a partial
# open-top contributes nothing -- we never fabricate an infinite width. A fully
# open cutoff (Inf) still counts the open-top in full.
.afi_interp_le <- function(lo, hi, n, cutoff) {
  frac <- ifelse(hi <= cutoff, 1,
          ifelse(lo >= cutoff, 0, (cutoff - lo) / (hi - lo)))
  sum(n * frac, na.rm = TRUE)
}

# ---- internal: parse one ACS bracket label into [lo, hi) dollar bounds -------
# Returns c(lo, hi); c(NA, NA) for totals / subtotals / non-dollar leaves
# (e.g. "Total:", "With cash rent:", "No cash rent").
.afi_parse_label <- function(label) {
  leaf   <- sub(".*!!", "", label)
  leaf_l <- tolower(leaf)
  nums   <- as.numeric(gsub(",", "",
              regmatches(leaf, gregexpr("[0-9][0-9,]*", leaf))[[1]]))
  if (grepl("total", leaf_l) || length(nums) == 0) return(c(NA, NA))
  if (grepl("less than|under", leaf_l))             return(c(0, nums[1]))
  if (grepl("or more|or above|and more|\\+", leaf_l)) return(c(nums[1], Inf))
  if (length(nums) >= 2)                            return(c(nums[1], nums[2]))
  c(NA, NA)
}

# ---- internal: pull an ACS bracket-count table, labelled with [lo, hi) -------
.afi_get_brackets <- function(geography, table, state, counties, year) {
  vars <- tidycensus::load_variables(year, "acs5", cache = TRUE)
  raw <- tidycensus::get_acs(geography = geography, table = table,
                             state = state, county = counties,
                             year = year, survey = "acs5", cache_table = TRUE)
  raw <- dplyr::left_join(raw, vars, by = c("variable" = "name"))
  bounds <- t(vapply(raw$label, .afi_parse_label, numeric(2)))
  raw$lo <- bounds[, 1]
  raw$hi <- bounds[, 2]
  raw <- raw[!is.na(raw$lo), c("GEOID", "NAME", "lo", "hi", "estimate")]
  names(raw)[names(raw) == "estimate"] <- "n"
  raw
}

# ---- internal: FIPS normalization (accepts FIPS or names) --------------------
.afi_norm_state <- function(state) {
  s <- as.character(state)
  if (grepl("^[0-9]{2}$", s)) return(s)
  fc <- tidycensus::fips_codes
  m <- unique(fc$state_code[tolower(fc$state) == tolower(s) |
                            tolower(fc$state_name) == tolower(s)])
  if (length(m) == 0) stop("Unrecognized state: ", state, call. = FALSE)
  m[1]
}

.afi_norm_counties <- function(state_fips, counties) {
  fc <- tidycensus::fips_codes
  sub <- fc[fc$state_code == state_fips, ]
  out <- vapply(as.character(counties), function(x) {
    if (grepl("^[0-9]{3}$", x)) return(x)
    m <- sub$county_code[tolower(sub$county) == tolower(x) |
                         tolower(sub$county) == tolower(paste(x, "County"))]
    if (length(m) == 0)
      m <- sub$county_code[grepl(tolower(x), tolower(sub$county), fixed = TRUE)]
    if (length(m) == 0) stop("Unrecognized county: ", x, call. = FALSE)
    m[1]
  }, character(1))
  unname(out)
}

# ---- internal: B25118 tenure x income brackets (owner/renter demand) ---------
# Like .afi_get_brackets but for the tenure-split table B25118: it keeps an
# owner/renter `tenure_grp` (parsed from the label) so the two universes are not
# merged, and drops the "Owner/Renter occupied:" subtotals and the grand total.
.afi_get_tenure_brackets <- function(geography, state, counties, year) {
  vars <- tidycensus::load_variables(year, "acs5", cache = TRUE)
  raw <- tidycensus::get_acs(geography = geography, table = "B25118",
                             state = state, county = counties,
                             year = year, survey = "acs5", cache_table = TRUE)
  raw <- dplyr::left_join(raw, vars, by = c("variable" = "name"))
  raw$tenure_grp <- ifelse(grepl("Owner occupied", raw$label), "owner",
                    ifelse(grepl("Renter occupied", raw$label), "renter", NA_character_))
  bounds <- t(vapply(raw$label, .afi_parse_label, numeric(2)))
  raw$lo <- bounds[, 1]; raw$hi <- bounds[, 2]
  raw <- raw[!is.na(raw$lo) & !is.na(raw$tenure_grp),
             c("GEOID", "tenure_grp", "lo", "hi", "estimate")]
  names(raw)[names(raw) == "estimate"] <- "n"
  raw
}

# ---- internal: regional cumulative households per tier (interpolated) --------
# `hh` has columns county, lo, hi, n. Returns a named vector (tier -> regional
# count of households with income <= the tier's cutoff), within-bracket
# interpolated and summed across counties.
.afi_demand_vec <- function(hh, cuts, tier_names) {
  vapply(tier_names, function(ti) {
    cc <- cuts[, c("county", paste0("cut_", ti))]
    names(cc)[2] <- "inc_cut"
    d <- dplyr::left_join(hh, cc, by = "county")
    per_co <- dplyr::summarize(dplyr::group_by(d, county),
      tier_hh = .afi_interp_le(lo, hi, n, dplyr::first(inc_cut)), .groups = "drop")
    sum(per_co$tier_hh, na.rm = TRUE)
  }, numeric(1))
}

# ---- internal: tract availability rates (Gate 2: vacancy + turnover) ---------
# Per tract, the share of stock that is OPEN now (point-in-time vacancy, B25004)
# and the share that TURNS OVER per year (B07013 past-year movers), separately
# for rental and owner stock. NA where the denominator is zero.
.afi_availability <- function(state, counties, year) {
  vars <- c(own_occ = "B25003_002", rent_occ = "B25003_003",
            for_rent = "B25004_002", for_sale = "B25004_004",
            mov_own_tot = "B07013_002", mov_rent_tot = "B07013_003",
            mov_own_same = "B07013_005", mov_rent_same = "B07013_006")
  d <- tidycensus::get_acs(geography = "tract", variables = vars, state = state,
                           county = counties, year = year, survey = "acs5",
                           output = "wide")
  g <- function(nm) as.numeric(d[[paste0(nm, "E")]])
  rate <- function(num, den) ifelse(den > 0, num / den, NA_real_)
  data.frame(
    GEOID          = d$GEOID,
    vac_rate_rent  = rate(g("for_rent"), g("rent_occ") + g("for_rent")),
    vac_rate_own   = rate(g("for_sale"), g("own_occ")  + g("for_sale")),
    turn_rate_rent = rate(g("mov_rent_tot") - g("mov_rent_same"), g("mov_rent_tot")),
    turn_rate_own  = rate(g("mov_own_tot")  - g("mov_own_same"),  g("mov_own_tot")),
    stringsAsFactors = FALSE)
}


#' @title Tract-level affordability index (Gate 1: supply by income tier)
#' @description
#' For each census tract, counts the housing units affordable to households at
#' each AMI tier (ELI/VLI/LI/MI), by tenure, and compares that supply to the
#' region's share of households in the tier. This is the rebuilt successor to
#' [afford()]; it fixes the AMI basis (county median family income), uses
#' label-parsed income brackets with within-bracket interpolation, and treats
#' rental and owner affordability with explicit, documented cost models.
#' @details
#' Affordability uses a `burden`-of-income rule throughout (default 30%, the
#' HUD standard). For each tenure a tract's housing-cost brackets (parsed from
#' ACS labels) are compared to the tier's income ceiling:
#' \itemize{
#'   \item \code{"rent"}: gross rent (\code{B25063}); affordable if
#'     \code{rent <= income_cutoff * burden / 12}.
#'   \item \code{"own_current"} (basis A, default): current selected monthly
#'     owner cost (\code{B25094}); same 30% rule as rent. Describes the
#'     affordability of the existing owned stock as currently financed.
#'   \item \code{"own_buyin"} (basis B): home price (\code{B25075} owner value by
#'     default, or \code{B25085} for-sale price asked -- see \code{buyin_stock})
#'     converted to the income needed to \emph{buy in today} via a mortgage model
#'     (\code{interest_rate}, \code{term_years}, \code{down_pct},
#'     \code{tax_ins_rate}). Answers "could a mover afford to buy here now?".
#' }
#' Counts use linear interpolation within brackets; open-topped brackets count
#' only when fully below the cutoff. Per tract/tenure/tier the function returns
#' \code{supply} (accessible / total), \code{ratio} (supply divided by the
#' region's share of tier households -- a location quotient), and \code{rate}
#' (accessible units per 100,000 regional tier households).
#'
#' A second, higher burden line (`stretch`, default 50% -- HUD's severe-burden
#' threshold) is computed alongside the standard: `accessible_stretch` /
#' `supply_stretch` count what the tier could reach by *stretching* to that
#' share of income. The two lines together support the three-class verdict
#' (affordable / roughly affordable / not affordable) -- see [afford_verdict()].
#' @param state State (FIPS like `"06"` or name/abbreviation like `"CA"`).
#' @param counties County or counties (3-digit FIPS or names).
#' @param year ACS 5-year endpoint year (default `2024`).
#' @param tenure Any of `"rent"`, `"own_current"` (A), `"own_buyin"` (B).
#' @param demand Which household universe the affordable supply is compared to
#'   (sets `reg_hh_tier`/`reg_hh_total` and thus `ratio`/`rate`). One of:
#'   `"matched"` (default; rent supply vs **renter** households, ownership supply
#'   vs **owner** households -- from `B25118`), `"all"` (all households, `B19001`
#'   -- everyone as a potential mover), `"renter"` / `"owner"` (that universe for
#'   every tenure), or `"likelihood"` (all households split into rent/own demand
#'   by the ACS tenure propensity per income tier; with realized ACS shares this
#'   tracks `"matched"` closely and is the hook for modeled cross-tenure mobility
#'   -- an owner who could rent, and vice versa). Default `"matched"` is what the
#'   deployed SLC/San Diego reports assume.
#' @param ami_source `"auto"` (default; most-exact available: hud -> hud_acs ->
#'   acs_fmr -> acs), `"hud"` (official HUD limits; snapshot then API), `"hud_acs"`
#'   (Census-only HUD-cascade reproduction; overstates in high-cost capped areas),
#'   `"acs_fmr"` (ACS + FMR high-cost bump), or `"acs"` (pure B19113 fractions);
#'   see [ami_cutoffs()].
#' @param ami_tiers Named tier fractions of AMI (default ELI/VLI/LI/MI).
#' @param hud_hh_size Household size (1-8) for AMI's family-size adjustment
#'   (default `4`; `NULL` for unadjusted).
#' @param burden Share of income treated as affordable housing cost (default
#'   `0.30`, the HUD 30%-of-income standard). Drives `accessible`/`supply` and
#'   everything downstream; for `"own_buyin"` it is the payment share inside the
#'   mortgage model.
#' @param stretch Optional second, higher burden line (default `0.50` -- HUD's
#'   severe-burden threshold). Adds `accessible_stretch`/`supply_stretch`: what
#'   the tier could reach by stretching to this share of income (the "roughly
#'   affordable" band in [afford_verdict()]). `NULL` to skip.
#' @param interest_rate Annual mortgage rate for `"own_buyin"`; `NULL` (default)
#'   uses a built-in by-year approximation.
#' @param term_years,down_pct,tax_ins_rate `"own_buyin"` mortgage assumptions
#'   (default 30 years, 10% down, 1.25% combined tax+insurance of value/year).
#' @param buyin_stock Price universe for `"own_buyin"`: `"owned_value"` (default,
#'   `B25075` value of owner-occupied homes -- the full distribution of what
#'   homes are worth here, stable at tract level) or `"for_sale"` (`B25085`
#'   price asked on vacant-for-sale units -- closer to "what a mover could buy
#'   now" but small-count and noisy). Availability is handled separately by the
#'   Gate-2 vacancy/turnover measures.
#' @param availability If `TRUE` (default), attach Gate-2 availability: per-tract
#'   `vacancy_rate` (point-in-time openings, `B25004`) and `turnover_rate`
#'   (past-year move-outs, `B07013`), tenure-matched, plus `available_vacancy`
#'   and `available_turnover` (= `accessible` x each rate -- affordable units
#'   that are actually open / turning over). Set `FALSE` for price-only Gate 1.
#' @param geometry If `TRUE`, attach tract polygons (an `sf` tibble). With the
#'   long output this duplicates polygons across tenure/tier; filter to one
#'   tenure and tier before mapping.
#' @return A long tibble, one row per tract x tenure x tier, with `accessible`,
#'   `total`, `supply` (and, when `stretch` is set, `accessible_stretch` /
#'   `supply_stretch`), `reg_hh_tier`, `reg_hh_total`, `class_prop`, `ratio`,
#'   `rate`, the county `ami`, and the tier `income_cutoff`. When
#'   `availability = TRUE`, also `vacancy_rate`, `turnover_rate`,
#'   `available_vacancy`, and `available_turnover` (Gate 2).
#' @seealso [ami_cutoffs()], [afford_bands()], [afford_verdict()],
#'   [afford_capacity()], [afford()] (legacy)
#' @examples \dontrun{
#' # San Francisco rental affordability, default matched (vs renters) demand
#' sf <- afford_index("06", "075", 2024, tenure = "rent")
#' sf[sf$ami_tier == "VLI", c("GEOID", "supply", "ratio", "available_vacancy")]
#'
#' # Which tracts can a VLI renter actually move into? (affordable AND open)
#' sf[sf$ami_tier == "VLI" & sf$available_turnover > 0, ]
#'
#' # Non-overlapping bands (the 30-50%, 50-80% slices) instead of cumulative
#' afford_bands(sf)
#'
#' # Owner "buy-in" affordability off the for-sale flow, exact HUD limits
#' afford_index("06", "075", 2024, tenure = "own_buyin",
#'              buyin_stock = "for_sale", ami_source = "hud")
#'
#' # Compare demand universes: renters vs all households
#' afford_index("06", "075", 2024, tenure = "rent", demand = "all")
#' }
#' @export
afford_index <- function(state, counties, year = 2024,
                         tenure = c("rent", "own_current", "own_buyin"),
                         demand = c("matched", "all", "renter", "owner", "likelihood"),
                         ami_source = c("auto", "hud", "hud_acs", "acs_fmr", "acs"),
                         ami_tiers = c(ELI = 0.30, VLI = 0.50, LI = 0.80, MI = 1.20),
                         hud_hh_size = 4,
                         burden = 0.30, stretch = 0.50,
                         interest_rate = NULL, term_years = 30,
                         down_pct = 0.10, tax_ins_rate = 0.0125,
                         buyin_stock = c("owned_value", "for_sale"),
                         availability = TRUE,
                         geometry = FALSE) {
  ami_source <- match.arg(ami_source)
  tenure <- match.arg(tenure, several.ok = TRUE)
  demand <- match.arg(demand)
  buyin_stock <- match.arg(buyin_stock)
  stopifnot(is.logical(availability), length(availability) == 1,
            is.numeric(burden), length(burden) == 1, burden > 0, burden < 1)
  if (!is.null(stretch))
    stopifnot(is.numeric(stretch), length(stretch) == 1,
              stretch > burden, stretch < 1)
  state <- .afi_norm_state(state)
  counties <- .afi_norm_counties(state, counties)

  # AMI tier cutoffs by county (also validates ami_source). Record the source
  # ami_cutoffs actually resolved, not the request ("auto" is not a provenance).
  cuts <- ami_cutoffs(state, counties, year, ami_source, ami_tiers, hud_hh_size)
  if (!is.null(attr(cuts, "ami_source"))) ami_source <- attr(cuts, "ami_source")
  cuts$county <- cuts$GEOID
  tier_names <- names(ami_tiers)

  if (is.null(interest_rate)) interest_rate <- .afi_default_rate(year)
  price_factor <- .afi_price_income_factor(interest_rate, term_years,
                                           down_pct, tax_ins_rate, burden)
  price_factor_stretch <- if (is.null(stretch)) NULL else
    .afi_price_income_factor(interest_rate, term_years, down_pct,
                             tax_ins_rate, stretch)

  # -- Demand: regional households per tier, by universe (interpolated) --------
  # Universes: all households (B19001); renter & owner (B25118 tenure x income).
  # Each is the regional count with income <= the tier cutoff. `demand_for()`
  # maps a supply row's (tenure, tier) to the right (count, total) for `demand`.
  d_all <- d_ren <- d_own <- NULL
  if (demand %in% c("all", "likelihood")) {
    hh_all <- .afi_get_brackets("county", "B19001", state, counties, year)
    hh_all$county <- hh_all$GEOID
    d_all <- list(tier = .afi_demand_vec(hh_all, cuts, tier_names),
                  total = sum(hh_all$n, na.rm = TRUE))
  }
  if (demand %in% c("matched", "renter", "owner", "likelihood")) {
    tb <- .afi_get_tenure_brackets("county", state, counties, year)
    tb$county <- tb$GEOID
    ren <- tb[tb$tenure_grp == "renter", , drop = FALSE]
    own <- tb[tb$tenure_grp == "owner",  , drop = FALSE]
    d_ren <- list(tier = .afi_demand_vec(ren, cuts, tier_names), total = sum(ren$n, na.rm = TRUE))
    d_own <- list(tier = .afi_demand_vec(own, cuts, tier_names), total = sum(own$n, na.rm = TRUE))
  }
  # tenure -> demand universe for the default "matched" mode: rent vs renters,
  # ownership vs owners. (own_current and own_buyin are both ownership.)
  demand_for <- function(ten, ti) {           # returns c(reg_hh_tier, reg_hh_total)
    rent_like <- ten == "rent"
    safe <- function(num, den) if (is.finite(den) && den > 0) num / den else 0
    switch(demand,
      all     = c(d_all$tier[[ti]], d_all$total),
      renter  = c(d_ren$tier[[ti]], d_ren$total),
      owner   = c(d_own$tier[[ti]], d_own$total),
      matched = if (rent_like) c(d_ren$tier[[ti]], d_ren$total)
                else            c(d_own$tier[[ti]], d_own$total),
      likelihood = {        # all households split by the ACS tenure propensity
        num_t <- if (rent_like) d_ren$tier[[ti]] else d_own$tier[[ti]]
        num_T <- if (rent_like) d_ren$total      else d_own$total
        c(d_all$tier[[ti]] * safe(num_t, d_ren$tier[[ti]] + d_own$tier[[ti]]),
          d_all$total      * safe(num_T, d_ren$total      + d_own$total))
      })
  }

  # -- Supply: per tract, per requested tenure, per tier ----------------------
  # own_buyin reads the chosen price universe: owner-occupied value (B25075,
  # default) or the for-sale flow (B25085, vacant-for-sale price asked).
  tenure_table <- c(rent = "B25063", own_current = "B25094",
                    own_buyin = if (buyin_stock == "for_sale") "B25085" else "B25075")
  supply_one <- function(ten) {
    tb <- .afi_get_brackets("tract", tenure_table[[ten]], state, counties, year)
    tb$county <- substr(tb$GEOID, 1, 5)
    totals <- dplyr::summarize(dplyr::group_by(tb, GEOID),
                               total = sum(n, na.rm = TRUE), .groups = "drop")
    per_tier <- lapply(tier_names, function(ti) {
      cc <- cuts[, c("county", paste0("cut_", ti))]
      names(cc)[2] <- "inc_cut"
      d <- dplyr::left_join(tb, cc, by = "county")
      d$cost_cut <- if (ten == "own_buyin") d$inc_cut / price_factor
                    else d$inc_cut * burden / 12
      if (!is.null(stretch))
        d$cost_cut_stretch <- if (ten == "own_buyin") d$inc_cut / price_factor_stretch
                              else d$inc_cut * stretch / 12
      g <- dplyr::summarize(dplyr::group_by(d, GEOID, county),
        accessible   = .afi_interp_le(lo, hi, n, dplyr::first(cost_cut)),
        accessible_stretch = if (is.null(stretch)) NA_real_ else
          .afi_interp_le(lo, hi, n, dplyr::first(cost_cut_stretch)),
        income_cutoff = dplyr::first(inc_cut), .groups = "drop")
      g$ami_tier <- ti
      g
    })
    out <- dplyr::left_join(dplyr::bind_rows(per_tier), totals, by = "GEOID")
    out$tenure <- ten
    out
  }
  supply <- dplyr::bind_rows(lapply(tenure, supply_one))

  # -- Assemble: join demand, county AMI, derive supply/ratio/rate -------------
  dm <- do.call(rbind, lapply(seq_len(nrow(supply)), function(i)
    demand_for(supply$tenure[i], as.character(supply$ami_tier[i]))))
  supply$reg_hh_tier  <- dm[, 1]
  supply$reg_hh_total <- dm[, 2]
  supply <- dplyr::left_join(supply, cuts[, c("county", "ami")], by = "county")
  supply <- dplyr::mutate(supply,
    class_prop = reg_hh_tier / reg_hh_total,
    supply     = ifelse(total > 0, accessible / total, NA_real_),
    supply_stretch = ifelse(total > 0, accessible_stretch / total, NA_real_),
    ratio      = ifelse(class_prop > 0, supply / class_prop, NA_real_),
    rate       = ifelse(reg_hh_tier > 0, accessible / reg_hh_tier * 1e5, NA_real_),
    ami_tier   = factor(ami_tier, levels = tier_names),
    year       = year, ami_source = ami_source)

  out_cols <- c("GEOID", "county", "year", "ami_source", "tenure", "ami_tier",
    "ami", "income_cutoff", "accessible", "total", "supply",
    if (!is.null(stretch)) c("accessible_stretch", "supply_stretch"),
    "reg_hh_tier", "reg_hh_total", "class_prop", "ratio", "rate")

  # -- Gate 2: availability -- affordable AND open / turning over --------------
  # vacancy = point-in-time openings (B25004); turnover = annual move-outs
  # (B07013). available_* = accessible * rate, assuming open/turnover units share
  # the occupied affordability mix (report alongside the price-only `accessible`).
  if (isTRUE(availability)) {
    av <- .afi_availability(state, counties, year)
    supply <- dplyr::left_join(supply, av, by = "GEOID")
    is_rent <- supply$tenure == "rent"
    supply$vacancy_rate       <- ifelse(is_rent, supply$vac_rate_rent,  supply$vac_rate_own)
    supply$turnover_rate      <- ifelse(is_rent, supply$turn_rate_rent, supply$turn_rate_own)
    supply$available_vacancy  <- supply$accessible * supply$vacancy_rate
    supply$available_turnover <- supply$accessible * supply$turnover_rate
    out_cols <- c(out_cols, "vacancy_rate", "turnover_rate",
                  "available_vacancy", "available_turnover")
  }

  supply <- dplyr::as_tibble(
    supply[order(supply$GEOID, supply$tenure, supply$ami_tier), out_cols])

  if (isTRUE(geometry)) {
    geo <- tigris::tracts(state = state, county = counties, cb = TRUE, year = year)
    geo <- sf::st_transform(geo, crs = 4326)[, "GEOID"]
    supply <- sf::st_as_sf(dplyr::left_join(geo, supply, by = "GEOID"))
  }
  supply
}

#' @title Convert cumulative AMI tiers to non-overlapping bands
#' @description
#' [afford_index()] reports **cumulative** tiers (ELI = at/below 30% AMI, VLI =
#' at/below 50%, ...). `afford_bands()` differences them into **non-overlapping
#' bands** (the 30-50% slice, the 50-80% slice, ...) within each tract x tenure,
#' so the tiers partition the stock instead of nesting. The lowest tier is
#' unchanged; `supply`/`class_prop`/`ratio`/`rate` (and any availability or
#' stretch columns) are recomputed on the banded counts. Tier *labels* keep their names (the `VLI`
#' row now holds the 30-50% slice). Assumes tiers are ascending in AMI fraction
#' (the default ELI/VLI/LI/MI order).
#' @param x A tibble returned by [afford_index()].
#' @return `x` with `accessible`, `reg_hh_tier`, and any `available_*` /
#'   `accessible_stretch` columns differenced to bands, and
#'   `supply`/`class_prop`/`ratio`/`rate` (and `supply_stretch`) recomputed.
#' @seealso [afford_index()]
#' @examples \dontrun{
#' idx   <- afford_index("06", "075", 2024, tenure = "rent")   # cumulative
#' bands <- afford_bands(idx)                                   # 30-50, 50-80, ...
#' }
#' @export
afford_bands <- function(x) {
  req <- c("GEOID", "tenure", "ami_tier", "accessible", "total",
           "reg_hh_tier", "reg_hh_total")
  if (!is.data.frame(x) || !all(req %in% names(x)))
    stop("`x` must be an afford_index() result (missing columns).", call. = FALSE)
  has_av <- all(c("vacancy_rate", "turnover_rate") %in% names(x))
  has_st <- "accessible_stretch" %in% names(x)
  x <- dplyr::arrange(dplyr::group_by(x, GEOID, tenure), ami_tier)
  x <- dplyr::mutate(x,
    accessible  = accessible  - dplyr::lag(accessible,  default = 0),
    reg_hh_tier = reg_hh_tier - dplyr::lag(reg_hh_tier, default = 0))
  if (has_st) x <- dplyr::mutate(x,
    accessible_stretch = accessible_stretch - dplyr::lag(accessible_stretch, default = 0))
  x <- dplyr::ungroup(x)
  x <- dplyr::mutate(x,
    supply     = ifelse(total > 0, accessible / total, NA_real_),
    class_prop = ifelse(reg_hh_total > 0, reg_hh_tier / reg_hh_total, NA_real_),
    ratio      = ifelse(class_prop > 0, supply / class_prop, NA_real_),
    rate       = ifelse(reg_hh_tier > 0, accessible / reg_hh_tier * 1e5, NA_real_))
  if (has_st) x <- dplyr::mutate(x,
    supply_stretch = ifelse(total > 0, accessible_stretch / total, NA_real_))
  if (has_av) x <- dplyr::mutate(x,
    available_vacancy  = accessible * vacancy_rate,
    available_turnover = accessible * turnover_rate)
  x
}
