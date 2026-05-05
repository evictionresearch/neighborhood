# neighborhood 1.0.6

## Bug fixes

* Fix `devtools::install_github()` failing with
  `Error in read.table(...) : more columns than column names` (#10).
  R's lazy-data loader was attempting to parse `data/*.csv.bz2` files via
  `read.table()` and tripping on commas embedded inside quoted `NAME` values.
  Bundled datasets are now shipped as `.rda`, which is the canonical R
  package-data format.

## Cleanup

* Bundled datasets (`us_nt_tracts2019`, `us_nt_tracts2021`, `us_nt_tracts2022`)
  are now documented and exposed via lazy data.
* `us_nt_tracts2021` schema harmonized with 2019/2022: `LatinxE`/`pLatinx`
  renamed to `LatineE`/`pLatine`, extra county/state columns dropped, and the
  `NeighType` strings rewritten from "Latinx" to "Latine" so they match the
  `nt_conc` factor levels. The previous build silently produced 21,184 `NA`
  `nt_conc` values nationwide because the original "Latinx-*" strings did not
  match any factor level. All three datasets now share the same 16 columns in
  the same order, and `ntdf()` reproduces the bundled output exactly (verified
  for Baltimore City across 2019, 2021, 2022).
* `LazyDataCompression: xz` set; bundled data shrinks from ~12.8MB to ~10.4MB.
* Build script for bundled datasets moved to `data-raw/` and made parameterized
  via the `NEIGHBORHOOD_SRC_DIR` environment variable.
* `afford()` no longer relies on `dplyr` being attached: bare `case_when()`
  calls are now `dplyr::case_when()`. Default `year` is now `2022` (was an
  unusable `NULL`); placeholder defaults removed from `state` / `counties` /
  `ami_limit` so they are required arguments.
* `get_co_puma()` now references `tidycensus::fips_codes` explicitly.
* Replaced blanket `@import` directives with targeted `@importFrom`.
* Fixed `License: MIT + file LICENSE` DCF stub; full license text moved to
  `LICENSE.md`.
* Added `tests/testthat/` with smoke tests for bundled datasets and `nt_pal()`.
