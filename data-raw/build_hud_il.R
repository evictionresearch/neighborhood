# Build a bundled county-level snapshot of HUD Income Limits, for longevity.
#
# Once committed as data/hud_il_<year>.rda, afford_index(ami_source = "hud")
# reads it directly (offline, deterministic) and only falls back to the live
# HUD API for counties/years not in the snapshot. This is the insurance against
# HUD deprecating its API or taking the data offline -- see
# dev/hud-income-limits-architecture.md.
#
# Usage:
#   Rscript data-raw/build_hud_il.R 2024
#
# Requires the 'hudr' package and a HUD_API_KEY in the environment. Runs one API
# call per county (~3,100 calls), so expect this to take a while.

suppressPackageStartupMessages({
  library(hudr)
  library(dplyr)
  library(tidycensus)
})

args <- commandArgs(trailingOnly = TRUE)
yr <- if (length(args) >= 1) args[[1]] else "2024"
if (!nzchar(Sys.getenv("HUD_API_KEY"))) stop("HUD_API_KEY not set.")

# Every county / county-equivalent in the 50 states + DC (drop territories;
# the HUD API covers PR separately if you want it -- add 72 below).
counties <- tidycensus::fips_codes %>%
  dplyr::filter(as.integer(state_code) <= 56) %>%
  dplyr::transmute(GEOID = paste0(state_code, county_code),
                   eid   = paste0(state_code, county_code, "99999")) %>%
  dplyr::distinct()

pick <- function(il, f) { for (el in il) if (f %in% names(el)) return(el[[f]][1]); NA }
grab <- function(il, sub, pre)
  vapply(1:8, function(p) as.numeric(il[[sub]][[paste0(pre, p)]]), numeric(1))

rows <- vector("list", nrow(counties))
for (i in seq_len(nrow(counties))) {
  il <- tryCatch(hudr::get_hud_il_data(counties$eid[i], yr), error = function(e) NULL)
  if (is.null(il)) { message("  skip ", counties$GEOID[i]); next }
  rows[[i]] <- data.frame(
    GEOID = counties$GEOID[i],
    area_name = as.character(pick(il, "area_name")),
    median_income = as.numeric(pick(il, "median_income")),
    setNames(as.list(grab(il, "extremely_low", "il30_p")), paste0("il30_p", 1:8)),
    setNames(as.list(grab(il, "very_low",      "il50_p")), paste0("il50_p", 1:8)),
    setNames(as.list(grab(il, "low",           "il80_p")), paste0("il80_p", 1:8)),
    year = as.character(yr), stringsAsFactors = FALSE)
  if (i %% 200 == 0) message(format(Sys.time(), "%H:%M:%S"), "  ", i, "/", nrow(counties))
}

hud_il <- dplyr::bind_rows(rows)
message("Built ", nrow(hud_il), " / ", nrow(counties), " counties for FY", yr)

assign(paste0("hud_il_", yr), hud_il)
do.call(usethis::use_data,
        list(as.name(paste0("hud_il_", yr)), overwrite = TRUE, compress = "xz"))
message("Saved data/hud_il_", yr, ".rda")
