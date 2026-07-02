# How wrong was v1? Legacy afford() vs afford_index() (v2/HUD) head-to-head on
# King County WA -- VLI renters (the pilot group), ACS 2020-2024 / HUD FY2024.
# Produces every number cited in dev/affordability-deep-dive.md:
#   (1) the income-cutoff distortion (median-of-medians + snapping vs HUD VLI),
#   (2) per-tract supply distortion and verdict flips,
#   (3) the free-will quantities: units affordable / open per year / open now,
#       per 100 VLI renter households, and the structural deficit,
#   (4) a demo 3-class verdict (affordable / roughly / not) at 30% and 50% burden,
#   (5) the ownership factor: 0.188 vs the rate-aware 2024 factor.
#
#   Rscript dev/afford_v1_gap.R      # from the repo root
#
# Needs dev/afford_qa_data.rds (built by dev/afford_qa_data.R). ACS pulls use
# cache_table = TRUE, so re-runs are mostly offline.

suppressPackageStartupMessages({ library(dplyr); library(sf) })
pkgload::load_all(".", quiet = TRUE)

qa <- readRDS("dev/afford_qa_data.rds")
k2 <- qa$baseline$king |> sf::st_drop_geometry() |>
  filter(tenure == "rent", ami_tier == "VLI")
stopifnot(nrow(k2) > 300, "income_cutoff" %in% names(k2))

# ---- v1 run (legacy engine, ami_limit = 0.5 "half of AMI") -------------------
v1 <- suppressMessages(afford("53", "033", 0.5, 2024))

# v1's AMI = median of tract median household incomes (B19013), cutoff snapped
med_inc <- suppressMessages(tidycensus::get_acs(
  geography = "tract", variables = "B19013_001",
  state = "53", county = "033", year = 2024, cache_table = TRUE))
ami_v1 <- stats::median(med_inc$estimate, na.rm = TRUE)
income_limit <- c(0, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000,
                  50000, 60000, 75000, 100000, 125000, 150000, 200000, Inf)
closest <- function(x, limits) limits[which.min(abs(limits - x))]
cut_v1_raw     <- 0.5 * ami_v1
cut_v1_snapped <- closest(cut_v1_raw, income_limit)
cut_v2_hud     <- unique(k2$income_cutoff)   # HUD VLI, 4-person, King FY24

# ---- per-tract supply comparison ---------------------------------------------
j <- inner_join(
  v1 |> select(GEOID, v1_supply = tr_rent_supply,
               v1_acc = tr_rent_accessible, v1_tot = tr_rent_total),
  k2 |> select(GEOID, v2_supply = supply, accessible, total,
               reg_hh_tier, reg_hh_total, ratio,
               available_turnover, available_vacancy),
  by = "GEOID")
stopifnot(nrow(j) > 300)

supply_cmp <- list(
  n_tracts        = nrow(j),
  cor             = cor(j$v1_supply, j$v2_supply, use = "complete.obs"),
  median_v1       = median(j$v1_supply, na.rm = TRUE),
  median_v2       = median(j$v2_supply, na.rm = TRUE),
  median_diff_pp  = median(j$v1_supply - j$v2_supply, na.rm = TRUE) * 100,
  mean_diff_pp    = mean(j$v1_supply - j$v2_supply, na.rm = TRUE) * 100,
  share_off_10pp  = mean(abs(j$v1_supply - j$v2_supply) > 0.10, na.rm = TRUE),
  share_off_20pp  = mean(abs(j$v1_supply - j$v2_supply) > 0.20, na.rm = TRUE))

# ---- 30% vs 50% burden supply + 3-class verdict demo (v2 cutoff) -------------
# Majority rule: supply share >= 0.5 is equivalent to a median-unit statement
# ("the typical rental here costs a VLI household <= 30% / <= 50% / > 50%").
br <- .afi_get_brackets("tract", "B25063", "53", "033", 2024)
sup <- br |> group_by(GEOID) |> summarize(
  total    = sum(n, na.rm = TRUE),
  supply30 = .afi_interp_le(lo, hi, n, cut_v2_hud * 0.30 / 12) / total,
  supply50 = .afi_interp_le(lo, hi, n, cut_v2_hud * 0.50 / 12) / total,
  .groups = "drop") |> filter(total > 0)

# internal consistency: supply30 must reproduce the packaged v2 supply
chk <- inner_join(sup, k2 |> select(GEOID, v2_supply = supply), by = "GEOID")
stopifnot(cor(chk$supply30, chk$v2_supply, use = "complete.obs") > 0.999)

verdict <- case_when(sup$supply30 >= 0.5 ~ "affordable",
                     sup$supply50 >= 0.5 ~ "roughly affordable",
                     TRUE ~ "not affordable")
verdict_tab <- table(factor(verdict,
  levels = c("affordable", "roughly affordable", "not affordable")))

# same majority rule applied to v1's supply (its own cutoff): verdict flips
jv <- inner_join(sup, j, by = "GEOID")
v1_class30 <- ifelse(jv$v1_supply >= 0.5, "affordable", "lower")
v2_class30 <- ifelse(jv$supply30  >= 0.5, "affordable", "lower")
flip_30 <- mean(v1_class30 != v2_class30, na.rm = TRUE)

# paper-vs-real: "affordable" tracts where <1 affordable unit opens per year
aff_geo <- sup$GEOID[verdict == "affordable"]
open_lt1 <- j |> filter(GEOID %in% aff_geo) |>
  summarize(n = sum(available_turnover < 1, na.rm = TRUE),
            n_na = sum(is.na(available_turnover)))

# ---- free-will capacity arithmetic (King, VLI renters) -----------------------
vli_renters <- unique(k2$reg_hh_tier)
capacity <- list(
  vli_renter_households   = vli_renters,
  affordable_units        = sum(j$accessible, na.rm = TRUE),
  structural_deficit      = vli_renters - sum(j$accessible, na.rm = TRUE),
  open_per_year_turnover  = sum(j$available_turnover, na.rm = TRUE),
  open_now_vacancy        = sum(j$available_vacancy, na.rm = TRUE),
  per100_affordable       = sum(j$accessible, na.rm = TRUE) / vli_renters * 100,
  per100_open_per_year    = sum(j$available_turnover, na.rm = TRUE) / vli_renters * 100,
  per100_open_now         = sum(j$available_vacancy, na.rm = TRUE) / vli_renters * 100,
  tracts_lt1_open_yr      = sum(j$available_turnover < 1, na.rm = TRUE))

# v1's demand universe vs v2's (all households at snapped cutoff vs VLI renters)
demand_cmp <- list(v1_class_pop = unique(v1$reg_class_pop),
                   v1_total_pop = unique(v1$reg_total_pop),
                   v2_vli_renters = vli_renters,
                   v2_renter_total = unique(k2$reg_hh_total))

# ---- ownership factor: 0.188 vs rate-aware 2024 ------------------------------
fac24 <- .afi_price_income_factor(.afi_default_rate(2024))
own_cmp <- list(v1_factor = 0.188, v2_factor_2024 = fac24,
                required_income_ratio = fac24 / 0.188,
                v1_implied_price_to_income = 1 / 0.188,
                v2_implied_price_to_income = 1 / fac24)

results <- list(
  cutoffs = list(ami_v1_median_of_medians = ami_v1,
                 cut_v1_raw = cut_v1_raw, cut_v1_snapped = cut_v1_snapped,
                 cut_v2_hud_vli4 = cut_v2_hud,
                 cutoff_gap_dollars = cut_v2_hud - cut_v1_snapped,
                 cutoff_gap_pct = cut_v2_hud / cut_v1_snapped - 1),
  supply_cmp = supply_cmp,
  verdict_tab = verdict_tab,
  verdict_shares = round(prop.table(verdict_tab), 3),
  flip_share_majority_rule_30 = flip_30,
  affordable_tracts_lt1_opening = open_lt1,
  capacity = capacity,
  demand_cmp = demand_cmp,
  own_cmp = own_cmp)
print(results)
