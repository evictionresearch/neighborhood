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
  # afford_index() NSE columns
  "lo", "hi", "inc_cut", "cost_cut", "accessible", "income_cutoff", "total",
  "ami_tier", "reg_hh_tier", "reg_hh_total", "class_prop", "supply", "ratio",
  "rate", "ami", "tier_hh"
))
