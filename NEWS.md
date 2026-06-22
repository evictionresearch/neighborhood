# neighborhood 1.1.0 (development version)

## New features

* **Bundled dataset.** `mn_evictions` (Minnesota county-month eviction filings,
  the public data behind the state profile, with race breakdowns) — documented
  and ready to plot.

* **`nt_chart(yaxis = )`.** A new option controlling the y-axis: `"always"`
  (default), `"hover"` (the ERN newspaper style — the y-axis labels and dashed
  gridlines are hidden until the reader hovers the chart, then appear), or
  `"none"`.

* **Editorial chart styling.** `nt_chart()` gains the touches that reproduce the
  published profile plates: `end_label` writes a direct label at the end of each
  line (e.g. `"Black · 33.5"`, the alternative to a legend); `line_styles` varies
  the dash pattern per series for print/grayscale legibility; `source` prints a
  small attribution in the lower-right; `value_fmt = "multiple"` formats a ratio
  as `1.52×` (for parity/disparity charts); and `highlight_last` now accents the
  leading series on a grouped chart.

* **`nt_add_choropleth(fill_color = )`.** Pass a MapLibre paint expression to
  take full control of a layer's coloring (e.g. a flag-based `case` wrapping an
  `interpolate`), while still getting the popup/tooltip/legend/visibility wiring.
  This makes it possible to reproduce bespoke, multi-layer maps exactly.

* New articles: `interactive-charts` (refreshed — replicates the Minnesota
  profile charts from `mn_evictions`, demonstrates `yaxis = "hover"` and the
  editorial end-label/line-style/source touches, and pulls Apartment List rent
  data from a URL to chart regional rents) and `precarity-mapping` (an advanced
  tutorial that builds a precarity-style interactive map — switchable risk
  layers, a threshold slider, and rich popups — entirely from public data).

* **Interactive MapLibre mapping.** A new, deterministic toolkit for building
  interactive MapLibre GL maps of neighborhood data — the same kind of maps used
  in the Eviction Research Network state profiles (HPRM, Minnesota, Washington),
  with choropleths, legends, labels, popups, and PMTiles vector tiles for
  large/full-US data. The API scales from a one-liner to full composition:
    - `nt_map()` — one-line map of an `sf` layer.
    - `nt_maplibre()` — start an ERN-styled map (OpenFreeMap basemap, control
      set with address search, fit-to-data).
    - `nt_add_choropleth()` — the workhorse: colors features by a column
      (categorical typologies use the `nt_pal()` key automatically; numeric
      columns use the ERN sequential ramp) and adds a matching legend, hover
      tooltip, and click popup. Draws inline for small layers and switches to
      PMTiles for large ones.
    - `nt_add_labels()` — text labels at interior points.
    - `nt_popup()` — deterministic HTML popups, from a simple field table up to
      the HPRM-style race bars / bubble tracks (see the tutorial).
    - `nt_pmtiles()` — build a PMTiles archive from `sf` via `tippecanoe`.

  Built on [mapgl](https://walker-data.com/mapgl/) by Kyle Walker; every `nt_*`
  function returns an ordinary `mapgl` map, so you can drop to raw `mapgl` at any
  point. Maps are `htmlwidgets` and save to standalone HTML with
  `htmlwidgets::saveWidget()`. `mapgl` is now a hard dependency (`Imports`);
  `tippecanoe` is an optional system requirement used only for PMTiles.

  For transparency, this mapping toolkit was designed and implemented with the
  assistance of **Claude Opus 4.8** (Anthropic). The functions are deterministic
  and require no AI to run.

* **Interactive charts.** `nt_chart()` and `nt_spark()` build the editorial
  "newspaper graphic" charts used in the state profiles — trend lines, bars,
  multi-series, and sparklines — where **hovering reveals the y-axis** (a
  crosshair prints the value on the axis at the cursor) plus a tooltip, with
  one-argument support for a dashed baseline, a shaded band (e.g. a moratorium
  window), and a highlighted latest point. Built on
  [echarts4r](https://echarts4r.john-coene.com/) (John Coene) / Apache ECharts
  (now an `Imports` dependency); returns the raw `echarts4r` widget so you can
  keep piping `e_*` functions. Also developed with the assistance of Claude Opus
  4.8.

* The mapping functions now accept **user-supplied spatial inputs** beyond a
  pre-built `sf` object: a path to a shapefile / GeoJSON / GeoPackage, a bare
  `sfc` geometry, or an `sp`/`terra` object — all normalized to `sf` (the
  canonical representation) and re-projected to WGS84 as needed.

* **Multiple layers on one map.** `nt_add_choropleth()` gains a `visible`
  argument, and the new `nt_layers_control()` adds a radio switcher that shows
  one layer at a time and reveals only that layer's legend — e.g. neighborhood
  typology plus the share of each demographic group on a single map. Legends
  from repeated `nt_add_choropleth()` calls now append and tie to their layer.
  (`nt_add_choropleth()` also now strips a name attribute from `column`, so
  passing a named element like `demos[i]` in a loop no longer corrupts the
  layer.)

* New vignettes: `mapping-with-maplibre` (a full tutorial from a one-line map up
  to HPRM-style rich popups and full-US PMTiles), `interactive-charts`, and
  `neighborhood-typologies` (the `ntdf()` -> `ntcheck()` -> map workflow).

* `nt_pal()` now reads from a single shared internal color key, so the leaflet
  palette and the MapLibre typology colors can never drift apart. `ntcheck()`
  documentation expanded.

* `afford_index()` — rebuilt tract-level affordability index (Phase 1, "Afford"
  gate). Successor to `afford()` with the methodology fixes from the review:
  AMI from county median **family** income (`B19113`); income brackets parsed
  from ACS variable **labels** (not positional `rep()`); **within-bracket linear
  interpolation** (no `closest()` snapping); standardized ELI/VLI/LI/MI tiers and
  HUD family-size adjustment (1–8, default 4); FIPS **or** name inputs; and three
  owner/renter cost models — `rent` (gross rent), `own_current` (basis A, current
  monthly owner cost), and `own_buyin` (basis B, income needed to buy in today
  via an explicit, parameterized mortgage model). Returns a tidy long table with
  `supply`, `ratio` (location quotient), and `rate` per tract × tenure × tier.
* `ami_cutoffs()` — county AMI and tier income ceilings (the demand-side engine).
* `ami_source = "hud"` pulls official HUD income limits (validated cell-by-cell
  against the live API: e.g. San Diego FY2024 VLI(4) = $75,750, ELI(4) = $45,450).
  ELI/VLI/LI use HUD's per-household-size limits (caps/floors/high-cost baked in);
  the path is snapshot-first with live-API fallback and never leaks the API key
  on error. Needs the `hudr` package (now in Suggests) + `HUD_API_KEY`.
* `data-raw/build_hud_il.R` builds a bundled national `hud_il_<year>` snapshot so
  the `"hud"` path works offline once committed (longevity insurance).
* `ami_source = "hud_acs"` (full HUD-cascade reproduction) still fails fast with
  guidance; arrives next (see `dev/hud-income-limits-architecture.md`).

## Documentation

* New vignette `affordability-index` documenting the `afford()` methodology,
  output schema, and known limitations.
* `afford()` roxygen rewritten: corrected `@return` (a data frame is returned
  unless `geometry = TRUE`), documented every output column, the 30%-rule and
  AMI threshold math, and the major caveats (the `0.188` ownership factor's
  embedded interest-rate assumption; median-of-tract-medians AMI; occupied-stock
  vs. availability; FIPS-only inputs).
* Added `dev/affordability-index-review.md`: a full methodological review of the
  affordability index with a prioritized remediation plan and a proposed R
  package design. (Build-ignored; not shipped.)

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
