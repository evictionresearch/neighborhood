# Build a bundled county-level snapshot of HUD Income Limits, for longevity.
#
# Once committed as inst/extdata/hud_il_<year>.rds, afford_index(ami_source =
# "hud") and the default "auto" read it directly (offline, deterministic, no
# rate limits) and only fall back to the live HUD API for counties/years not in
# the snapshot. Insurance against HUD deprecating its API or taking the data
# offline -- see dev/hud-income-limits-architecture.md.
#
# Usage:
#   Rscript data-raw/build_hud_il.R 2024                  # all US counties (~3,100)
#   Rscript data-raw/build_hud_il.R 2024 CA               # one state
#   Rscript data-raw/build_hud_il.R 2024 CA 073,075,081   # specific counties
#
# The result MERGES into any existing data/hud_il_<year>.rda (dedupe by GEOID),
# so you can build incrementally -- your study counties first, the nation later.
# Requires the 'hudr' package and a HUD_API_KEY in the environment.

suppressPackageStartupMessages({
  library(hudr); library(dplyr); library(tidycensus)
})

args <- commandArgs(trailingOnly = TRUE)
yr        <- if (length(args) >= 1) args[[1]] else "2024"
state_in  <- if (length(args) >= 2) args[[2]] else NULL
county_in <- if (length(args) >= 3) strsplit(args[[3]], ",")[[1]] else NULL
if (!nzchar(Sys.getenv("HUD_API_KEY"))) stop("HUD_API_KEY not set.")

fc <- tidycensus::fips_codes
counties <- dplyr::filter(fc, as.integer(state_code) <= 56)   # 50 states + DC
if (!is.null(state_in)) {
  sc <- if (grepl("^[0-9]{2}$", state_in)) state_in else
    unique(fc$state_code[tolower(fc$state) == tolower(state_in) |
                         tolower(fc$state_name) == tolower(state_in)])[1]
  counties <- dplyr::filter(counties, state_code == sc)
  if (!is.null(county_in))
    counties <- dplyr::filter(counties, county_code %in% sprintf("%03s", county_in))
}
counties <- counties %>%
  dplyr::transmute(GEOID = paste0(state_code, county_code),
                   eid   = paste0(state_code, county_code, "99999")) %>%
  dplyr::distinct()
message("Targeting ", nrow(counties), " counties for FY", yr)

# Robust to named OR positional HUD API list elements.
pick <- function(il, f) { for (el in il) if (is.list(el) && f %in% names(el)) return(el[[f]][1]); NA }
grab <- function(il, pre) vapply(1:8, function(p) {
  f <- paste0(pre, p)
  for (el in il) if (is.list(el) && f %in% names(el)) return(as.numeric(el[[f]]))
  NA_real_
}, numeric(1))
get_il <- function(eid) {           # one county, with a few retries (rate limits)
  for (try in 1:4) {
    il <- tryCatch(hudr::get_hud_il_data(eid, yr), error = function(e) NULL)
    if (is.list(il) && !is.na(pick(il, "median_income"))) return(il)
    Sys.sleep(3)
  }
  NULL
}

rows <- vector("list", nrow(counties))
for (i in seq_len(nrow(counties))) {
  il <- get_il(counties$eid[i])
  if (is.null(il)) { message("  skip ", counties$GEOID[i], " (no data)"); next }
  rows[[i]] <- data.frame(
    GEOID = counties$GEOID[i],
    area_name = as.character(pick(il, "area_name")),
    median_income = as.numeric(pick(il, "median_income")),
    setNames(as.list(grab(il, "il30_p")), paste0("il30_p", 1:8)),
    setNames(as.list(grab(il, "il50_p")), paste0("il50_p", 1:8)),
    setNames(as.list(grab(il, "il80_p")), paste0("il80_p", 1:8)),
    year = as.character(yr), stringsAsFactors = FALSE)
  Sys.sleep(0.2)                                   # be gentle on the API
  if (i %% 100 == 0) message(format(Sys.time(), "%H:%M:%S"), "  ", i, "/", nrow(counties))
}
new <- dplyr::bind_rows(rows)
message("Pulled ", nrow(new), " / ", nrow(counties), " counties for FY", yr)

# Merge with any existing snapshot (new rows win on GEOID collision), then save
# to inst/extdata/hud_il_<year>.rds (read by .afi_hud_snapshot via system.file).
rds <- file.path("inst", "extdata", paste0("hud_il_", yr, ".rds"))
if (file.exists(rds)) {
  old <- readRDS(rds)
  new <- dplyr::bind_rows(new, dplyr::filter(old, !GEOID %in% new$GEOID))
  message("Merged with existing snapshot -> ", nrow(new), " counties total")
}
if (!dir.exists(dirname(rds))) dir.create(dirname(rds), recursive = TRUE)
saveRDS(new[order(new$GEOID), ], rds, compress = "xz")
message("Saved ", rds, " (", nrow(new), " counties)")
