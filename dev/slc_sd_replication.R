# Does the published v1 finding survive the corrected engine?
#
# The Salt Lake City (Wasatch) and San Diego EDR reports, built on legacy
# afford(), headline: "no 'more affordable' neighborhoods outside
# displacement-risk areas" -- affordable tracts were (nearly) a subset of
# displacement-pressure tracts. The King/Bay walkthrough on v2 shows a softer
# picture (strong affordability-instability gradient, cor ~ -0.49, but most of
# the affordable half stable) -- different REGIONS, though, so nothing has been
# replicated or contradicted yet. This script runs the published regions
# through BOTH engines and crosses each affordability class with HPRM
# displacement bands (dis_value: <= -300 Extreme, (-300,-100] Elevated,
# (-100,0) outflow, >= 0 none) -- the direct test.
#
# Regions (constrained by the 2026-07-02 HUD API outage to snapshot-exact
# counties): San Diego County 06073 = the exact published region; Salt Lake
# County 49035 = the core county of the 4-county Wasatch region (Weber/Davis
# need the live API). EER (ev_value) is not used: it is out-of-sample for CA,
# and the published claim is about DISPLACEMENT risk -- dis_value only.
#
#   Rscript dev/slc_sd_replication.R      # from the repo root

suppressPackageStartupMessages({ library(dplyr); library(sf); library(arrow) })
pkgload::load_all(".", quiet = TRUE)

h <- read_parquet("/Users/buffalo/data/evictionresearch/hprm/hprm_v5_full_2022.parquet") |>
  transmute(GEOID = geoid, dis_value = as.numeric(dis_value))

income_limit <- c(0, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000,
                  50000, 60000, 75000, 100000, 125000, 150000, 200000, Inf)
closest <- function(x, limits) limits[which.min(abs(limits - x))]
band <- function(dv) cut(dv, c(-Inf, -300, -100, 0, Inf), right = FALSE,
  labels = c("Extreme (<=-300)", "Elevated (-300,-100]",
             "Outflow (-100,0)", "No outflow (>=0)"))

run_region <- function(label, state, county) {
  v2 <- suppressMessages(afford_index(state, county, 2024, tenure = "rent",
                                      ami_source = "hud")) |> afford_verdict()
  v1 <- suppressMessages(afford(state, county, 0.5, 2024))

  vli <- v2 |> filter(ami_tier == "VLI") |> left_join(h, by = "GEOID")
  j1  <- v1 |> select(GEOID, v1_rate = tr_rent_rate, v1_supply = tr_rent_supply) |>
    left_join(h, by = "GEOID")

  # v1's cutoff distortion, localized to this region
  med_inc <- suppressMessages(tidycensus::get_acs(geography = "tract",
    variables = "B19013_001", state = state, county = county,
    year = 2024, cache_table = TRUE))
  ami_v1 <- stats::median(med_inc$estimate, na.rm = TRUE)
  cut_v1 <- closest(0.5 * ami_v1, income_limit)
  cut_v2 <- unique(vli$income_cutoff)

  # published-style v1 classes (Not / Less / More Affordable on the rate)
  v1_cls <- cut(j1$v1_rate, c(-Inf, 100, 200, Inf),
                labels = c("Not (<=100)", "Less (100-200)", "More (>200)"))
  # relative split used in the King/Bay walkthrough
  half <- factor(ifelse(vli$supply > median(vli$supply, na.rm = TRUE),
                        "more-affordable half", "less-affordable half"))

  aff <- vli$verdict == "affordable"
  list(label = label,
    cut_v1_snapped = cut_v1, cut_v2_hud_vli4 = cut_v2,
    cut_gap_pct = cut_v2 / cut_v1 - 1,
    verdict_counts = table(vli$verdict, useNA = "ifany"),
    v1_class_x_disp = table(v1_cls, band(j1$dis_value)),
    v2_verdict_x_disp = table(vli$verdict, band(vli$dis_value)),
    v2_half_x_disp = table(half, band(vli$dis_value)),
    # the published claim, tested on v2: affordable tracts with NO outflow
    v2_affordable_no_outflow = sum(aff & vli$dis_value >= 0, na.rm = TRUE),
    v2_affordable_outflow    = sum(aff & vli$dis_value < 0,  na.rm = TRUE),
    share_affordable_under_displacement =
      mean(vli$dis_value[aff] < 0, na.rm = TRUE),
    share_rest_under_displacement =
      mean(vli$dis_value[!aff & !is.na(vli$verdict)] < 0, na.rm = TRUE),
    med_dv_affordable = median(vli$dis_value[aff], na.rm = TRUE),
    med_dv_rest = median(vli$dis_value[!aff], na.rm = TRUE),
    cor_supply_dis = cor(vli$supply, vli$dis_value, use = "complete.obs"))
}

slc <- run_region("Salt Lake County (Wasatch core)", "49", "035")
sd  <- run_region("San Diego County", "06", "073")
print(slc); print(sd)
saveRDS(list(slc = slc, sd = sd),
  "/private/tmp/claude-501/-Users-buffalo-git-evictionresearch-neighborhood/fece173f-44e2-4643-a35a-5414bacdc86b/scratchpad/slc_sd_replication.rds")
