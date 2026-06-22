# Build the bundled `mn_evictions` dataset: county-level monthly eviction
# filings for Minnesota, the aggregated data behind the public state profile at
# https://evictionresearch.net/minnesota/.
#
# Developer/build script (data-raw/ is build-ignored). Source is the Minnesota
# project's county aggregate parquet. Re-run when the underlying data refreshes.
#
#   Rscript data-raw/build_mn_evictions.R

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
})

src <- "../minnesota/data/d3_county_agg_202612.parquet"
stopifnot(file.exists(src))

raw <- read_parquet(src)

# Match the live page's "hold-out rule": drop the newest LSC month (2026-04),
# whose cohorts are still being expunged downward. Keep 2017-01 .. 2026-03.
mn_evictions <- raw |>
  filter(year < 2026 | (year == 2026 & month <= 3)) |>
  transmute(
    geoid          = county_code,           # 5-digit county FIPS
    county,
    year           = as.integer(year),
    month          = as.integer(month),
    filings        = as.integer(filings),   # monthly eviction filings (LSC court records)
    renters        = as.numeric(co_renters),# renter households (ACS; constant per county)
    rate           = filings_rate,          # raw monthly proportion = filings / renters
    # race-imputed filings + matching renter denominators (for race-trend charts)
    filings_black  = filings_black,
    filings_white  = filings_white,
    filings_latine = filings_latine,
    filings_other  = filings_other,
    renters_black  = as.numeric(co_black_renters),
    renters_white  = as.numeric(co_white_renters),
    renters_latine = as.numeric(co_latine_renters),
    renters_other  = as.numeric(co_other_renters)
  ) |>
  arrange(geoid, year, month)

stopifnot(
  nrow(mn_evictions) > 0,
  length(unique(mn_evictions$geoid)) == 87,
  max(mn_evictions$year * 100 + mn_evictions$month) == 202603
)

message(sprintf("mn_evictions: %d rows, %d counties, %d-%02d .. %d-%02d",
                nrow(mn_evictions), length(unique(mn_evictions$geoid)),
                min(mn_evictions$year), min(mn_evictions$month[mn_evictions$year == min(mn_evictions$year)]),
                max(mn_evictions$year), 3L))

if (requireNamespace("usethis", quietly = TRUE)) {
  usethis::use_data(mn_evictions, overwrite = TRUE, compress = "xz")
} else {
  save(mn_evictions, file = "data/mn_evictions.rda", compress = "xz", version = 3)
}
