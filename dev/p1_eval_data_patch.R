# One-off patch, 2026-07-02: the HUD API (IL + FMR endpoints) is down (the
# longevity scenario in dev/hud-income-limits-architecture.md), so the full
# rebuild in dev/p1_eval_data.R cannot re-pull the Bay's hud_acs cutoffs.
# The rds on disk (built 2026-07-01, API healthy) already holds the correct
# FMR-bumped income cutoffs per tract row -- so this augments it IN PLACE with
# what the verdict build added, all computable from cached ACS + saved cutoffs:
#   * accessible_stretch / supply_stretch  (B25063 at income_cutoff x 50% / 12)
#   * verdict                              (afford_verdict)
#   * cap_king / cap_bay                   (afford_capacity)
#   * v_aff / v_rough / v_not in summ / summ_bay
# Semantically identical to a fresh run: on King County the live rebuilt engine
# gives verdicts 139 / 315 / 40 (checked 2026-07-02), which this must match.
# Re-run dev/p1_eval_data.R proper once the HUD API is back.
#
#   Rscript dev/p1_eval_data_patch.R      # from the repo root

suppressPackageStartupMessages({ library(dplyr); library(sf) })
pkgload::load_all(".", quiet = TRUE)

d <- readRDS("dev/p1_eval_data.rds")
stopifnot(is.null(d$cap_king))   # refuse to double-patch

augment <- function(reg, state, counties) {
  br <- .afi_get_brackets("tract", "B25063", state, counties, 2024)
  cuts <- reg |> sf::st_drop_geometry() |>
    distinct(GEOID, ami_tier, income_cutoff)
  sup <- br |>
    inner_join(cuts, by = "GEOID", relationship = "many-to-many") |>
    group_by(GEOID, ami_tier) |>
    summarize(accessible_stretch =
                .afi_interp_le(lo, hi, n, dplyr::first(income_cutoff) * 0.50 / 12),
              .groups = "drop")
  reg <- left_join(reg, sup, by = c("GEOID", "ami_tier"))
  reg$supply_stretch <- ifelse(reg$total > 0,
                               reg$accessible_stretch / reg$total, NA_real_)
  afford_verdict(reg)
}
add_verdict_counts <- function(s, reg) {
  nv <- table(reg$verdict[reg$ami_tier == "VLI"])
  s$v_aff   <- as.integer(nv[["affordable"]])
  s$v_rough <- as.integer(nv[["roughly affordable"]])
  s$v_not   <- as.integer(nv[["not affordable"]])
  s
}

d$king <- augment(d$king, "53", "033")
d$bay  <- augment(d$bay, "06",
                  c("001","013","041","055","075","081","085","095","097"))

# verification gate: must reproduce the live rebuilt engine's King verdicts
kv <- table(d$king$verdict[d$king$ami_tier == "VLI"])
stopifnot(kv[["affordable"]] == 139, kv[["roughly affordable"]] == 315,
          kv[["not affordable"]] == 40)

d$summ     <- add_verdict_counts(d$summ, d$king)
d$summ_bay <- add_verdict_counts(d$summ_bay, d$bay)
d$cap_king <- afford_capacity(d$king)
d$cap_bay  <- afford_capacity(d$bay)
d$built    <- paste0(d$built, " (patched ", format(Sys.time(), "%Y-%m-%d %H:%M"),
                     ": stretch/verdict/capacity)")

saveRDS(d, "dev/p1_eval_data.rds")
message("patched dev/p1_eval_data.rds",
        " | verdict a/r/n King ", d$summ$v_aff, "/", d$summ$v_rough, "/", d$summ$v_not,
        " Bay ", d$summ_bay$v_aff, "/", d$summ_bay$v_rough, "/", d$summ_bay$v_not)
