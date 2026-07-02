# Precompute for the P1 stable-destination walkthrough (dev/p1_eval.Rmd).
#   Rscript dev/p1_eval_data.R
# Joins afford_index() (Gate 1-2) with HPRM stability (Gate 3) via afford_stability(),
# and attaches revealed low-income in-migration via inflow_index(), for King County WA
# and the 9-county San Francisco Bay Area.

suppressPackageStartupMessages({library(arrow); library(dplyr); library(sf)})
pkgload::load_all(".", quiet = TRUE)
options(tigris_use_cache = TRUE, tigris_progress = FALSE)
clamp01 <- function(x) pmin(pmax(x, 0), 1)
retry <- function(expr, n = 4) {  # live HUD FMR pulls can rate-limit (cf. afford_qa_data.R)
  for (i in seq_len(n)) {
    r <- tryCatch(force(expr), error = function(e) NULL)
    if (!is.null(r)) return(r)
    Sys.sleep(5)
  }
  stop("failed after ", n, " attempts", call. = FALSE)
}

# -- HPRM fields: dis_value + ev_value (stability), nmr_pred_* (tier-specific
#    inflow), plus display-only dis_group / hprm_score. Positive dis_value =
#    low-income GROWTH (codebook: "backfilled with raw sum ... = low-income growth"). --
hprm_path <- "/Users/buffalo/data/evictionresearch/hprm/hprm_v5_full_2022.parquet"
h_full <- read_parquet(hprm_path) |>
  transmute(GEOID = geoid, dis_value, ev_value, nmr_pred_el, nmr_pred_vl, nmr_pred_l,
            dis_group, hprm_score)

# -- afford_stability (Gate 3) + inflow_index (revealed destinations) for a region --
build_region <- function(state, counties, ami_source) {
  idx <- suppressMessages(afford_index(state, counties, 2024, tenure = "rent",
                                       ami_source = ami_source, geometry = TRUE)) |>
    afford_verdict()   # 3-class verdict off the 30% + 50% (stretch) burden lines
  out <- afford_stability(idx, hprm = h_full, stability_source = "both",
                          availability = "turnover") |>
    left_join(dplyr::select(h_full, GEOID, dis_group, hprm_score), by = "GEOID")
  list(data = inflow_index(h_full, tier = "overall", x = out),  # + inflow/growth/inflow_cat
       capacity = afford_capacity(idx))   # units-and-people arithmetic, all tiers
}

# King County, WA -- exact HUD snapshot (all in inst/extdata) --
king_l <- retry(build_region("53", "033", "hud"))
king <- king_l$data; cap_king <- king_l$capacity
# 9-county Bay Area -- hud_acs: 3 counties (Napa/Solano/Sonoma) are not in the
# offline HUD snapshot, so use the validated ACS-based HUD approximation for all 9. --
bay_l <- retry(build_region("06", c("001","013","041","055","075","081","085","095","097"), "hud_acs"))
bay <- bay_l$data; cap_bay <- bay_l$capacity

# -- stability mapping curves (for the "how it's measured" section) --
fn_edr <- tibble(dis_value = seq(-400, 120, 4),  s_edr = clamp01((dis_value + 300) / 350))
fn_eer <- tibble(ev_value  = seq(0.4, 2.6, 0.02), s_eer = clamp01((2 - ev_value) / 1.2))

# -- traffic-light affordability x stability category (VLI, per-tract) --
bivar_cat <- function(v) factor(dplyr::case_when(
    v$supply <  median(v$supply, na.rm = TRUE) ~ "Less affordable",
    v$stability >= 0.8                          ~ "Affordable & stable (>= 0.8)",
    v$stability >= 0.5                          ~ "Affordable, elevated risk (0.5-0.8)",
    TRUE                                        ~ "Affordable, high precarity (< 0.5)"),
  levels = c("Affordable & stable (>= 0.8)", "Affordable, elevated risk (0.5-0.8)",
             "Affordable, high precarity (< 0.5)", "Less affordable"))

# -- summary numbers for one region's VLI subset (dis_value is per-tract) --
summarize_region <- function(region) {
  v <- region[region$ami_tier == "VLI", ]
  aff <- v$supply > median(v$supply, na.rm = TRUE)
  dv  <- v$dis_value
  nc  <- table(bivar_cat(v))
  nv  <- table(v$verdict)
  list(n_tracts = nrow(v), join_matched = sum(!is.na(v$stability)), join_total = nrow(v),
       v_aff = as.integer(nv[["affordable"]]),
       v_rough = as.integer(nv[["roughly affordable"]]),
       v_not = as.integer(nv[["not affordable"]]),
       med_stab_aff = median(v$stability[aff], na.rm = TRUE),
       med_stab_less = median(v$stability[!aff], na.rm = TRUE),
       cor_aff_stab = suppressWarnings(cor(v$supply, v$stability, use = "complete.obs")),
       dv_min = min(dv, na.rm = TRUE), dv_max = max(dv, na.rm = TRUE),
       n_growth = sum(v$growth, na.rm = TRUE), n_disp = sum(dv < 0, na.rm = TRUE),
       n_even = sum(dv >= 0 & dv <= 50, na.rm = TRUE),
       nc_green = as.integer(nc[1]), nc_orange = as.integer(nc[2]),
       nc_red = as.integer(nc[3]), nc_less = as.integer(nc[4]))
}
summ <- summarize_region(king); summ_bay <- summarize_region(bay)

saveRDS(list(king = king, bay = bay, fn_edr = fn_edr, fn_eer = fn_eer,
             summ = summ, summ_bay = summ_bay,
             cap_king = cap_king, cap_bay = cap_bay,
             anchors = list(edr = c(stable = 50, extreme = -300),
                            eer = c(stable = 0.8, extreme = 2.0)),
             built = format(Sys.time(), "%Y-%m-%d %H:%M")),
        "dev/p1_eval_data.rds")
message("saved dev/p1_eval_data.rds",
        " | verdict a/r/n King ", summ$v_aff, "/", summ$v_rough, "/", summ$v_not,
        " Bay ", summ_bay$v_aff, "/", summ_bay$v_rough, "/", summ_bay$v_not,
        " | King join ", summ$join_matched, "/", summ$join_total,
        " cor=", round(summ$cor_aff_stab, 2), " growth=", summ$n_growth, " disp=", summ$n_disp,
        " | Bay join ", summ_bay$join_matched, "/", summ_bay$join_total,
        " cor=", round(summ_bay$cor_aff_stab, 2), " growth=", summ_bay$n_growth,
        " disp=", summ_bay$n_disp)
