#' @importFrom dplyr %>%
#' @importFrom dplyr .data
NULL

utils::globalVariables(c(
  "AsianE", "BlackE", "COUNTYFP", "GEOID", "LatineE", "NeighType", "STATEFP",
  "WhiteE", "county", "county_code", "estimate", "limit", "n", "nt_conc",
  "reg_class_pop", "reg_total_pop", "state", "state_code", "state_name",
  "totraceE", "tr_own_accessible", "tr_own_rate", "tr_own_supply",
  "tr_own_total", "tr_rent_accessible", "tr_rent_rate", "tr_rent_supply",
  "tr_rent_total", "variable",
  # afford_index() / afford_bands() NSE columns
  "lo", "hi", "inc_cut", "cost_cut", "accessible", "income_cutoff", "total",
  "ami_tier", "reg_hh_tier", "reg_hh_total", "class_prop", "supply", "ratio",
  "rate", "ami", "tier_hh", "tenure", "tenure_grp", "vacancy_rate",
  "turnover_rate", "available_vacancy", "available_turnover",
  "accessible_stretch", "supply_stretch", "cost_cut_stretch",
  # afford_entry() / nt_zcta_weights() NSE columns
  "zcta", "premium", "coverage", "premium_fallback", "accessible_entry",
  "available_entry_turnover", "stable_dest", "stability",
  # nt_areal_weight() NSE columns
  "NAME", "GEOID20", "POP20", "HOUSING20", "ALAND20", "block", "tract_geoid",
  "pop", "hu", "area", "place_name", "place_geoid", "pop_in", "hu_in",
  "area_in", "pop_t", "hu_t", "area_t", "frac_pop", "frac_hu", "frac_area",
  # nt_erase_water() NSE columns
  "AWATER"
))
