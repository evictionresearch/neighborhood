# =============================================================================
# Affordability index v2 -- Phase 1 (supply engine / Gate 1: "Afford")
#
# Rebuilds the tract-level affordability index on a defensible footing:
#   * AMI from county median FAMILY income (B19113), per HUD's anchor
#   * income brackets parsed from ACS variable LABELS (not positional rep())
#   * within-bracket linear interpolation (no closest()-snapping)
#   * rent + both owner cost bases (A = current monthly cost, B = cost to buy in)
#   * standardized ELI/VLI/LI/MI tiers, all household sizes (default 4)
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

# ---- internal: read a bundled HUD income-limit snapshot, if present ----------
# Returns the data.frame `hud_il_<year>` if it is bundled with the package, else
# NULL. Snapshots are the longevity fallback for when HUD's API is unreachable;
# build them with data-raw/build_hud_il.R.
.afi_hud_snapshot <- function(year) {
  nm <- paste0("hud_il_", year)
  items <- tryCatch(utils::data(package = "neighborhood")$results[, "Item"],
                    error = function(e) character(0))
  if (!nm %in% items) return(NULL)
  env <- new.env()
  tryCatch({
    utils::data(list = nm, package = "neighborhood", envir = env)
    get(nm, envir = env)
  }, error = function(e) NULL)
}

# ---- internal: official HUD income-limit cutoffs (snapshot first, then API) --
# Returns the same shape as the "acs" path. ELI/VLI/LI come straight from HUD's
# published limits for the requested family size (so caps/floors/high-cost
# adjustments are baked in); any non-30/50/80 tier (e.g. MI = 120%) is *derived*
# as fraction x area median x family-size factor and is not a HUD-published limit.
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

  pick <- function(il, field) {        # robust to which sub-table holds a field
    for (el in il) if (field %in% names(el)) return(el[[field]][1])
    NA
  }
  one <- function(co) {
    geoid <- paste0(state, co)
    if (!is.null(snap) && geoid %in% snap$GEOID) {        # snapshot hit
      r <- snap[snap$GEOID == geoid, , drop = FALSE]
      med <- as.numeric(r$median_income); nm <- as.character(r$area_name)
      eli <- as.numeric(r[[paste0("il30_p", size)]])
      vli <- as.numeric(r[[paste0("il50_p", size)]])
      li  <- as.numeric(r[[paste0("il80_p", size)]])
    } else {                                              # live HUD API
      il <- tryCatch(hudr::get_hud_il_data(paste0(geoid, "99999"), as.character(year)),
        error = function(e)            # suppress original msg: it can echo the key
          stop("HUD API call failed for county ", geoid, " (", year,
               "); original error hidden to avoid leaking the API key.", call. = FALSE))
      med <- as.numeric(pick(il, "median_income")); nm <- as.character(pick(il, "area_name"))
      eli <- as.numeric(il$extremely_low[[paste0("il30_p", size)]])
      vli <- as.numeric(il$very_low[[paste0("il50_p", size)]])
      li  <- as.numeric(il$low[[paste0("il80_p", size)]])
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
        else f * med * sf              # derived (e.g. MI = 120%); not a HUD limit
    }
    out
  }
  dplyr::as_tibble(do.call(rbind, lapply(counties, one)))
}

#' @title Area Median Income tier cutoffs by county
#' @description
#' Returns each study county's Area Median Income (AMI) and the income ceilings
#' for the standardized affordability tiers (ELI/VLI/LI/MI). In the default
#' `"acs"` source, AMI is the county median **family** income (ACS `B19113`,
#' HUD's anchor) and tiers are simple fractions of it, optionally adjusted for
#' household size with HUD's family-size factors.
#' @param state State (FIPS code like `"06"` or a name/abbreviation like `"CA"`).
#' @param counties County or counties (3-digit FIPS like `"075"` or names).
#' @param year ACS 5-year endpoint year.
#' @param ami_source One of `"acs"` (default; Census only), `"hud"` (official
#'   published HUD limits via the HUD API; requires the `hudr` package and a
#'   `HUD_API_KEY`), or `"hud_acs"` (full HUD-cascade reproduction; not yet
#'   implemented). With `"hud"`, ELI/VLI/LI come straight from HUD's per-
#'   household-size limits; tiers without a HUD limit (e.g. MI = 120%) are
#'   derived as a fraction of the area median.
#' @param ami_tiers Named numeric vector of tier fractions of AMI. Default
#'   `c(ELI = .30, VLI = .50, LI = .80, MI = 1.20)`.
#' @param hud_hh_size Household size (1-8) for the HUD family-size adjustment;
#'   default `4`. Use `NULL` to leave AMI unadjusted (4-person basis).
#' @return A tibble with one row per county: `GEOID`, `NAME`, `ami`, and one
#'   `cut_<TIER>` income-ceiling column per tier.
#' @seealso [afford_index()]
#' @examples \dontrun{
#' ami_cutoffs("06", c("075", "081"), 2024)
#' }
#' @export
ami_cutoffs <- function(state, counties, year = 2024,
                        ami_source = c("acs", "hud", "hud_acs"),
                        ami_tiers = c(ELI = 0.30, VLI = 0.50, LI = 0.80, MI = 1.20),
                        hud_hh_size = 4) {
  ami_source <- match.arg(ami_source)
  state <- .afi_norm_state(state)
  counties <- .afi_norm_counties(state, counties)

  size_factor <- if (is.null(hud_hh_size)) 1 else {
    k <- as.character(hud_hh_size)
    if (!k %in% names(.afi_hh_size_factor))
      stop("hud_hh_size must be 1-8 (or NULL).", call. = FALSE)
    unname(.afi_hh_size_factor[k])
  }

  if (ami_source == "hud")
    return(.afi_hud_cutoffs(state, counties, year, ami_tiers, hud_hh_size))
  if (ami_source == "hud_acs")
    stop("ami_source = 'hud_acs' (full HUD-cascade reproduction) is not yet ",
         "implemented; use 'hud' (live API) or 'acs'. See ",
         "dev/hud-income-limits-architecture.md.", call. = FALSE)

  # ami_source == "acs": county median FAMILY income (B19113); `ami` is the
  # 4-person area median, family-size adjustment applied to the tier cutoffs.
  ami <- tidycensus::get_acs(geography = "county",
                             variables = c(ami = "B19113_001"),
                             state = state, county = counties,
                             year = year, survey = "acs5")
  out <- data.frame(GEOID = ami$GEOID, NAME = ami$NAME,
                    ami = ami$estimate, stringsAsFactors = FALSE)
  for (ti in names(ami_tiers))
    out[[paste0("cut_", ti)]] <- out$ami * ami_tiers[[ti]] * size_factor
  dplyr::as_tibble(out)
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
#' Affordability uses a 30%-of-income rule throughout. For each tenure a tract's
#' housing-cost brackets (parsed from ACS labels) are compared to the tier's
#' income ceiling:
#' \itemize{
#'   \item \code{"rent"}: gross rent (\code{B25063}); affordable if
#'     \code{rent <= income_cutoff * 0.30 / 12}.
#'   \item \code{"own_current"} (basis A, default): current selected monthly
#'     owner cost (\code{B25094}); same 30% rule as rent. Describes the
#'     affordability of the existing owned stock as currently financed.
#'   \item \code{"own_buyin"} (basis B): home value (\code{B25075}) converted to
#'     the income needed to \emph{buy in today} via an explicit mortgage model
#'     (\code{interest_rate}, \code{term_years}, \code{down_pct},
#'     \code{tax_ins_rate}). Answers "could a mover afford to buy here now?".
#' }
#' Counts use linear interpolation within brackets; open-topped brackets count
#' only when fully below the cutoff. Per tract/tenure/tier the function returns
#' \code{supply} (accessible / total), \code{ratio} (supply divided by the
#' region's share of tier households -- a location quotient), and \code{rate}
#' (accessible units per 100,000 regional tier households).
#' @param state State (FIPS like `"06"` or name/abbreviation like `"CA"`).
#' @param counties County or counties (3-digit FIPS or names).
#' @param year ACS 5-year endpoint year (default `2024`).
#' @param tenure Any of `"rent"`, `"own_current"` (A), `"own_buyin"` (B).
#' @param ami_source `"acs"` (default), `"hud"` (official HUD limits; needs
#'   `hudr` + `HUD_API_KEY`), or `"hud_acs"` (not yet implemented); see
#'   [ami_cutoffs()].
#' @param ami_tiers Named tier fractions of AMI (default ELI/VLI/LI/MI).
#' @param hud_hh_size Household size (1-8) for AMI's family-size adjustment
#'   (default `4`; `NULL` for unadjusted).
#' @param interest_rate Annual mortgage rate for `"own_buyin"`; `NULL` (default)
#'   uses a built-in by-year approximation.
#' @param term_years,down_pct,tax_ins_rate `"own_buyin"` mortgage assumptions
#'   (default 30 years, 10% down, 1.25% combined tax+insurance of value/year).
#' @param geometry If `TRUE`, attach tract polygons (an `sf` tibble). With the
#'   long output this duplicates polygons across tenure/tier; filter to one
#'   tenure and tier before mapping.
#' @return A long tibble, one row per tract x tenure x tier, with `accessible`,
#'   `total`, `supply`, `reg_hh_tier`, `reg_hh_total`, `class_prop`, `ratio`,
#'   `rate`, the county `ami`, and the tier `income_cutoff`.
#' @seealso [ami_cutoffs()], [afford()] (legacy)
#' @examples \dontrun{
#' sf_idx <- afford_index("06", "075", 2024)                 # San Francisco
#' sf_idx[sf_idx$tenure == "rent" & sf_idx$ami_tier == "VLI", ]
#' }
#' @export
afford_index <- function(state, counties, year = 2024,
                         tenure = c("rent", "own_current", "own_buyin"),
                         ami_source = c("acs", "hud", "hud_acs"),
                         ami_tiers = c(ELI = 0.30, VLI = 0.50, LI = 0.80, MI = 1.20),
                         hud_hh_size = 4,
                         interest_rate = NULL, term_years = 30,
                         down_pct = 0.10, tax_ins_rate = 0.0125,
                         geometry = FALSE) {
  ami_source <- match.arg(ami_source)
  tenure <- match.arg(tenure, several.ok = TRUE)
  state <- .afi_norm_state(state)
  counties <- .afi_norm_counties(state, counties)

  # AMI tier cutoffs by county (also validates ami_source)
  cuts <- ami_cutoffs(state, counties, year, ami_source, ami_tiers, hud_hh_size)
  cuts$county <- cuts$GEOID
  tier_names <- names(ami_tiers)

  if (is.null(interest_rate)) interest_rate <- .afi_default_rate(year)
  price_factor <- .afi_price_income_factor(interest_rate, term_years,
                                           down_pct, tax_ins_rate)

  # -- Demand: regional households per tier (B19001, interpolated, summed) -----
  hh <- .afi_get_brackets("county", "B19001", state, counties, year)
  hh$county <- hh$GEOID
  reg_hh_total <- sum(hh$n, na.rm = TRUE)
  demand <- vapply(tier_names, function(ti) {
    cc <- cuts[, c("county", paste0("cut_", ti))]
    names(cc)[2] <- "inc_cut"
    d <- dplyr::left_join(hh, cc, by = "county")
    per_co <- dplyr::summarize(dplyr::group_by(d, county),
      tier_hh = .afi_interp_le(lo, hi, n, dplyr::first(inc_cut)), .groups = "drop")
    sum(per_co$tier_hh, na.rm = TRUE)
  }, numeric(1))

  # -- Supply: per tract, per requested tenure, per tier ----------------------
  tenure_table <- c(rent = "B25063", own_current = "B25094", own_buyin = "B25075")
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
                    else d$inc_cut * 0.30 / 12
      g <- dplyr::summarize(dplyr::group_by(d, GEOID, county),
        accessible   = .afi_interp_le(lo, hi, n, dplyr::first(cost_cut)),
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
  supply$reg_hh_tier  <- demand[supply$ami_tier]
  supply$reg_hh_total <- reg_hh_total
  supply <- dplyr::left_join(supply, cuts[, c("county", "ami")], by = "county")
  supply <- dplyr::mutate(supply,
    class_prop = reg_hh_tier / reg_hh_total,
    supply     = ifelse(total > 0, accessible / total, NA_real_),
    ratio      = ifelse(class_prop > 0, supply / class_prop, NA_real_),
    rate       = ifelse(reg_hh_tier > 0, accessible / reg_hh_tier * 1e5, NA_real_),
    ami_tier   = factor(ami_tier, levels = tier_names),
    year       = year, ami_source = ami_source)
  supply <- dplyr::as_tibble(supply[order(supply$GEOID, supply$tenure, supply$ami_tier),
    c("GEOID", "county", "year", "ami_source", "tenure", "ami_tier", "ami",
      "income_cutoff", "accessible", "total", "supply",
      "reg_hh_tier", "reg_hh_total", "class_prop", "ratio", "rate")])

  if (isTRUE(geometry)) {
    geo <- tigris::tracts(state = state, county = counties, cb = TRUE, year = year)
    geo <- sf::st_transform(geo, crs = 4326)[, "GEOID"]
    supply <- sf::st_as_sf(dplyr::left_join(geo, supply, by = "GEOID"))
  }
  supply
}
