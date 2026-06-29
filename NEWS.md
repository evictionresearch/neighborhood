# neighborhood 1.1.0 (development version)

## New features

* **A gallery of editorial chart types.** Seven new `echarts4r`-backed functions
  extend the "newspaper graphic" look (minimal axes, `yaxis = "hover"`, accent
  highlight, direct labels, dark tooltip, `source` credit) beyond trend lines to
  the rest of the forms the Eviction Research Network publishes — with a bias
  toward the racial-disparity comparisons that are the org's specialty:
    - `nt_bar()` — ranked / horizontal bars (top-N counties by rate), with
      `diverging` (above/below a reference) and `group` (dodged) variants.
    - `nt_lollipop()` — the leaner dot-plot ranking (e.g. rates indexed to the
      white rate), with an optional stem.
    - `nt_dumbbell()` — two dots joined by a connector: the core disparity idiom
      (Black vs. white rate by county; 2019 vs. 2024), sorted by the gap.
    - `nt_range()` — ranked estimate + error-bar (a dot with a whisker for an ACS
      margin of error or explicit low/high bounds), so survey-based disparity
      comparisons show the interval, not just the point.
    - `nt_stacked_bar()` — composition / 100%-stacked bars (tenure by race,
      rent-burden by AMI, racial composition) with in-bar share labels.
    - `nt_slope()` — slope chart for before/after change, with direct end labels.
    - `nt_scatter()` — scatter with an optional linear trend line.
    - `nt_waffle()` — a unit / "data portrait" chart in the spirit of W.E.B.
      Du Bois (one square per share of the whole).

* **A ggplot2 kit for the static (print/PDF) artifacts.** `theme_ern()` plus
  `scale_color_ern()` / `scale_fill_ern()` (discrete; `palette = "typology"` for
  the 19 neighborhood-typology colors), their `_c` continuous counterparts, and
  `ern_palette()` to expose the house colors — so the same design language
  reproduces the report and paper figures, not just the interactive widgets.
  `ggplot2` is now an `Imports` dependency.

* **Honest, consistent charts.** Filled bars (`nt_bar`, `nt_stacked_bar`) now
  always start at zero, `nt_range` extends its axis so confidence whiskers can't
  be clipped, and composition palettes scale to 6–7 categories without recycling
  colors. The chart family shares one argument vocabulary — `palette` for colors,
  `value_labels` for direct labels, identical `value_fmt` options — with
  deprecated aliases (`nt_dumbbell(colors=)`, `nt_waffle(n=)`) that warn.
  `nt_chart()` now stacks areas (not just bars), currency formats as `-$100`, and
  `nt_scatter()` accepts a `Date` x (time axis). Map coloring degrades gracefully
  on low-variance / all-missing columns instead of erroring cryptically.

* **Exhaustive vignettes.** All six articles were expanded to document every
  function from the basic one-call form up through each argument, with
  per-function reference tables — including `afford_index()`/`ami_cutoffs()` and
  `get_co_puma()`, which previously appeared in no tutorial.

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
  data from a URL to chart regional rents); `chart-gallery` (the ranked,
  disparity, and composition chart forms — `nt_bar`/`nt_lollipop`/`nt_dumbbell`/
  `nt_range`/`nt_stacked_bar`/`nt_slope`/`nt_scatter`/`nt_waffle` plus the
  `theme_ern` static kit — all built from `mn_evictions`); and `precarity-mapping`
  (an advanced tutorial that builds a precarity-style interactive map —
  switchable risk layers, a threshold slider, and rich popups — entirely from
  public data).

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
* `data-raw/build_hud_il.R` builds a bundled `hud_il_<year>` snapshot (in
  `inst/extdata/`) so the `"hud"` path works offline, exact, with no rate limits.
  It now takes an optional state / county subset and **merges** into any existing
  snapshot, so you can capture your study counties first and the nation later
  (longevity insurance).
* `ami_source` now offers `"auto"` (default; most-exact available, in order
  hud → hud_acs → acs_fmr → acs), `"acs_fmr"` (ACS fractions + HUD's FMR
  high-cost bump), and `"hud_acs"` — the Census-only HUD-cascade reproduction
  (median family income trended from the FY−2 ACS, FMR bump, ELI poverty floor,
  LI US-median cap, HUD family-size factors). `hud_acs` reproduces HUD in
  ordinary areas but **overstates limits in high-cost areas** where HUD's
  year-over-year increase cap binds (that cap depends on HUD's own publication
  history and is not reproducible from public data — e.g. San Diego FY2024 VLI
  75,750 vs ~82,600), and it uses county—not metro—median income; use `"hud"`
  for high-cost reports. The AMI engine now lives in `R/ami_cutoffs.R`. See
  `dev/hud-income-limits-architecture.md`.
* `afford_index(demand = ...)` chooses the household universe the supply is
  compared to: `"matched"` (default — rent vs **renters**, ownership vs
  **owners**, from `B25118`, matching the deployed reports), `"all"` (everyone,
  `B19001`), `"renter"`/`"owner"`, or `"likelihood"` (split by ACS tenure
  propensity per income tier). This fixes the prior all-households denominator
  that overstated rental adequacy (e.g. SF VLI rental ratio 0.90 vs renters,
  not 1.08 vs all households).
* `afford_index(buyin_stock = ...)` picks the `own_buyin` price universe:
  `"owned_value"` (`B25075`, default) or `"for_sale"` (`B25085`, the for-sale
  flow).
* `afford_index(availability = TRUE)` adds **Gate 2** of the choice funnel:
  per-tract `vacancy_rate` (`B25004`) and `turnover_rate` (`B07013`), plus
  `available_vacancy`/`available_turnover` (affordable units that are actually
  open / turning over) — affordable *and* attainable, not just affordable.
* `afford_bands()` converts the cumulative tiers (≤30 / ≤50 / ≤80%) into
  non-overlapping bands (the 30–50%, 50–80% slices), recomputing
  `supply`/`ratio`/`rate` on the banded counts.

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
