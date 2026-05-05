# neighborhood 1.0.6

## Bug fixes

* Fix `devtools::install_github()` failing with
  `Error in read.table(...) : more columns than column names` (#10).
  R's lazy-data loader was attempting to parse `data/*.csv.bz2` files via
  `read.table()` and tripping on commas embedded inside quoted `NAME` values.
  Bundled data is now shipped as `.rda`, the canonical R package-data format.

## Bundled data

* The package now ships a single bundled dataset, `us_nt_tracts2024`
  (full-US tract-level racial typologies from the 2020-2024 ACS 5-year),
  built directly from `ntdf()` so the bundled output matches what users get
  when they run the function themselves.
* `LazyDataCompression: xz` set for smaller install size.
* Build script lives at `data-raw/build_full_us.R` and takes a year as a CLI
  argument so the dataset can be re-cut for any ACS endpoint.

## Cleanup

* `afford()`: default `year` is now `2024` (was an unusable `NULL`); placeholder
  defaults removed from `state` / `counties` / `ami_limit` so they are required
  arguments. Bare `case_when()` calls are now `dplyr::case_when()`.
* `get_co_puma()` references `tidycensus::fips_codes` explicitly.
* Replaced blanket `@import` directives with targeted `@importFrom`.
* Fixed `License: MIT + file LICENSE` DCF stub; full license text moved to
  `LICENSE.md`.
* Added `tests/testthat/` with smoke tests for the bundled dataset and
  `nt_pal()`.
