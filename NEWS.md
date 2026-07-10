# neighborhood 1.1.0 (development version)

## Bug fixes

* `nt_chart()` now caps the value axis at 100% for normalized (100%) stacked
  percent bars, instead of letting ECharts round up to a phantom 120% ceiling.
  A new `y_max` argument overrides the cap (or caps any chart); percent charts
  that legitimately exceed 100% are left alone. The same auto-cap now also pins
  the baseline to 0 (the value axis carries `scale = TRUE`, which otherwise let
  an all-high stacked bar — e.g. every value 80–98% — start at 78% instead of
  0); a new `y_min` argument overrides that floor.

## New features

* State-profile builders (`nt_profile_*`) — the inline-SVG charts and
  interactive shells of the ERN state-profile pages
  (evictionresearch.net/washington/ is the reference implementation), as
  raw-HTML string builders for the profiles' Quarto template:
  `nt_profile_trend_svg()` (monthly trend line with moratoria band +
  baseline), `nt_profile_yearly_svg()` (yearly bars with SARIMA projection
  overlay), `nt_profile_project_year()` (the year-end SARIMA projection
  itself), `nt_profile_fourps_svg()` (the "four P's" policy-lever diagram),
  `nt_profile_county_trend()` / `nt_profile_county_trend_data()` (the
  county-trend chart shell + its embedded data payload),
  `nt_profile_map_explorer()` (the MapLibre + PMTiles county explorer with
  time slider and readout card), `nt_profile_county_table()` (the
  searchable/sortable all-counties table), `nt_profile_monthly_js()` (the
  Figure 1 data payload), and `nt_raw_html()` (emit any of the above from a
  Quarto chunk). Output is byte-compatible with the shipped Washington
  profile; the interaction JS lives with the page template, not the package.

* `nt_apartmentlist_rents()` — Apartment List's monthly *Rent Estimates*
  release (metro/county/city median new-lease rents back to 2017), always the
  newest vintage: checks the research page for a newer monthly file, keeps
  dated CSVs in a local folder (default `~/data/apartment_list`), and falls
  back to the newest local copy when offline (`refresh = "never"` skips the
  check; `"force"` re-downloads). Replaces the hand-rolled link scrape in
  `vignette("interactive-charts")` Part IV, which is now a two-metro
  comparison.

* `nt_chart()` grows a second round of levers, all demoed in
  `vignette("interactive-charts")`: `digits` (one rounding override for
  tooltip, crosshair, axis ticks, and bar labels), `tooltip_count` +
  `tooltip_count_suffix` (append the raw count behind a plotted rate to the
  tooltip), `tooltip_trigger = "item"` (per-series tooltips for many-line
  charts), `isolate` (double-click a series to isolate it and dim the rest),
  `bar_labels` (in-bar values with a fit-aware overflow rule), `flip`
  (engine-aware horizontal bars — prefer over piping `e_flip_coords()`), and
  `legend_pos`/`legend_nudge` (bottom-row legend placement with pixel
  fine-tuning).

* `nt_sync_maps()` (in development) — a standalone flex-row builder for saved
  HTML pages: synchronized MapLibre panels with per-panel titles and a shared
  cross-panel layer switcher. Overlaps `nt_map_sync()` in the synced-row role;
  the switcher is the new capability.

* Backfilled entries for utilities that shipped quietly earlier in 1.1.0:
  `inflow_index()` (**experimental** revealed-destination layer — where
  low-income households appear to be moving; trust its displacement/negative
  tail only), `nt_areal_weight()` (block-exact tract → place weights),
  `nt_erase_water()` (display-only shoreline clipping for tract maps), and
  `nt_tract_interpolate()` (weight tract-level estimates onto arbitrary
  boundaries).

* **The affordability verdict and the free-will capacity readout.**
  `afford_index()` gains explicit burden lines — `burden` (default 0.30, the
  HUD standard) and `stretch` (default 0.50, HUD's severe-burden threshold) —
  emitting `accessible_stretch`/`supply_stretch` alongside the standard counts
  (`afford_bands()` differences them too). Two new functions turn the engine's
  output into the question it exists to answer (*with free choice at ≤30% /
  ≤50% of income: where, how many units, how many people?*):
    - `afford_verdict()` — the three-class map verdict (**affordable / roughly
      affordable / not affordable**), a tract-majority rule equivalent to a
      median-unit statement. Headline maps classify rather than shade:
      continuous gradients read as "no problem here" to policy audiences.
    - `afford_capacity()` — units-and-people arithmetic per tenure × tier:
      affordable units per 100 tier households, openings per year (turnover)
      and now (vacancy), and the regional **shortfall** that remains even
      under perfect free sorting. NLIHC-Gap-style, pre-competition-adjustment.
    - `afford_map()` + `afford_caption()` — pane 1 (of affordability →
      availability → stability) as a product: the verdict choropleth in the
      report palette (plain-language legend, optional HPRM displacement
      hatch, colors stable when a class is absent), and the locked reading
      that must travel with it — *"this is how much a \[tier\] household will
      pay if they can move in there"*, the red-class descriptor (*"...and
      entry prices run higher still"*), and the three "what ifs" (price-only;
      incumbent prices; the tier's income ceiling).
    - `hud_ami()` — the standalone AMI puller: one row per county × year with
      the AMI level HUD anchors its limits on, vectorized over `years`, with
      an `ami_source` provenance column (under `"auto"` each year resolves
      independently, so multi-year pulls can mix sources).

* **Pane 2 — availability at entry prices.** Pane 1 prices tracts at what
  sitting tenants pay; movers face asking rents. Three new functions measure
  the wedge and re-score at the door: `afford_zori()` (Zillow's ZIP-level
  asking-rent index, downloaded once and cached), `nt_zcta_weights()`
  (block-exact ZCTA → tract housing-unit crosswalk: 2020 blocks nest in both
  2020 tracts and ZCTAs), and `afford_entry()` — each tract gets an **entry
  premium** (asking ÷ standing rent, HU-weighted from its ZIPs, with a
  coverage fallback), the rent distribution shifts by it, and the same 30/50
  verdict grammar applies at entry (`entry_verdict`), plus
  openings-at-entry counts. `afford_capacity()` gains the per-100-at-entry
  columns, `afford_map(verdict_col = "entry_verdict")` maps pane 2, and
  `afford_caption(pane = "availability")` carries its reading and what-ifs.
  Competition (CHAS) and screening remain the documented next layers.

* **Pane 3 wired — the full funnel.** `afford_map(verdict_col = "stable_cat")`
  renders the stability verdict (stable / elevated / precarious, same three
  colors) from `afford_stability()`, `afford_caption(pane = "stability")`
  carries its reading (*"a place you can afford and enter but cannot keep is
  not a destination"*) and caveats (place-level risk; 2022 model vs 2024
  prices; CA eviction signal out-of-sample), and `afford_capacity()` gains
  the funnel's last columns: `per100_stable_dest` and
  `per100_open_entry_stable` (openings that are entry-affordable **and**
  stability-weighted).

* **The HPRM jurisdiction map.** `nt_hprm_map()` renders scored HPRM tract
  data as the report-page embed: switchable choropleth layers for displacement
  risk (EDR), eviction risk (EER), the 0–8 composite, and divergence
  (convergent / EDR-dominant / EER-dominant), each with its legend and rich
  per-tract popup, the jurisdiction boundary framed on top, water erased and
  the basemap decluttered by default. `nt_hprm_pal()` exposes the canonical
  tier colors so interactive maps and static exports read identically.

* **`nt_declutter_basemap()`** — hides the basemap's decorative land tints
  (parks, landcover, hillshade, ...) that wash out semi-transparent
  choropleth fills, leaving roads, water, and labels intact. On by default in
  `afford_map()`; pipe any other map through it.

* **`nt_map_sync()` — synchronized side-by-side MapLibre maps.** Lay two or
  more maps (`nt_map()` / `nt_maplibre()` / any `mapgl::maplibre()` pipeline)
  in a row with their views locked together: pan, zoom, or rotate one and the
  others follow, so the same area reads across layers at once (before/after,
  two engines, afford × stability). Optional per-panel `titles`/`subtitles`,
  auto or shared sync `group`s (same `group` syncs maps in different rows).
  Client-side `jumpTo` with a re-entrancy guard; proven in the v1-vs-v2
  report-style comparison before promotion.

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

* **A full brand palette system, WCAG-verified.** `ern_palette()` grows from 4
  palettes to 11, organized by what the data means and led by the 2022 brand
  triad (dark navy / blue navy / brand red):
    - *Qualitative:* `"qualitative"` (rebuilt house default — blue navy, brand
      red, chart steel, teal, then gold/purple/green/slate; every fill clears
      the WCAG 1.4.11 3:1 non-text minimum on white, and the first four are the
      most colorblind-separable subset), `"neutral"` (red-free, for categories
      where red would read as "bad"), and `"legacy"` (the pre-1.1.0 vector,
      verbatim, for reproducing published figures).
    - *Sequential:* `"blues"` (neutral magnitude), `"reds"` (brand-red
      intensity for filings/displacement counts), `"greens"` (more = good),
      alongside the existing `"ramp"` risk ramp (now documented as
      risk-only).
    - *Diverging:* `"div_brand"` (navy ↔ brand red, valenced — red marks the
      bad pole) and `"div_gold"` (blue ↔ gold, neutral), via new
      `scale_color_ern_div()` / `scale_fill_ern_div()` with a `midpoint`
      argument; `scale_*_ern_c()` gains a `palette` argument (default
      unchanged).
    - **The red rule.** Brand red `#F9322B` is verified WCAG-compliant as a
      *graphic* (3.8:1 on white ≥ the 3:1 non-text minimum) and stays the
      chart accent; red rendered as *text* uses the canonical `#CC2118`
      (5.5:1, AA). `ern_palette("brand")` tokens sync to the canonical brand
      guide: `muted` is now `#586573` (was deprecated `#6c7a89`),
      `accent_deep` is `#CC2118` (was `#D6231C`), and `accent_deeper`
      (`#B01D16`), `steel_chart` (`#7B96B5`, brand steel darkened to clear
      3:1 as a fill), and `tint` (`#e8eef4`) are new.
    - **`ern_swatch()`** writes and opens a self-contained HTML swatch sheet
      generated from the live palette definitions — every palette titled,
      labeled with its exact `ern_palette()` string, and badged with computed
      WCAG contrast — so choosing a scheme is reading a page.
    - Documented end-to-end in `vignette("ern-palettes")`, with live figures
      built from the bundled `mn_evictions` data.

* **Partial-dependence plots for the HPRM BART models.** `nt_pd_plot()` draws a
  partial-dependence curve (posterior mean + credible ribbon) over a frequency
  histogram of the predictor — the "PD curve with a frequency table underneath"
  figure used for the EDR/EER driver plots — in the `theme_ern()` house style.
  It is model-agnostic (takes a tidy PD table plus the observed values), so it
  runs with no Java. `nt_pd_bart()` is the `bartMachine` bridge that computes
  that table from a fitted model (server-only: needs `bartMachine` + a Java
  runtime). Reimplements CHEST-Lab's `pdPlotGG.R` (Scarpone, Brinkmann et al.
  2020) and the bartMachine `pd_plot()` workflow from the original UDP HPRM work
  (Ramiller & Thomas), adding the frequency panel and decoupling the plotting
  from the Java computation. `patchwork` is now an `Imports` dependency.

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

## Bug fixes

* `afford_index()` and `ami_cutoffs()` now record the AMI source **actually
  resolved**, not the request: under `ami_source = "auto"` the output's
  `ami_source` column (and a new `"ami_source"` attribute on `ami_cutoffs()`
  results) reports `"hud"`, `"hud_acs"`, `"acs_fmr"`, or `"acs"` — never
  `"auto"`, which is a request, not a provenance.

## Documentation

* The stability rationale is now in the package docs, with verified sources:
  `vignette("three-panes")` gains *Why stability — and not an "opportunity"
  score* (the Opportunity Atlas demoted to a labeled, contested secondary
  lens; Reid 2019, Carlson 2020, Chapple 2017, Goetz 2018, Sampson 2008, and
  Chetty et al. w25147 cited, DOIs included), and `?afford_stability` carries
  the condensed argument with full references.
* `vignette("affordability-index")` gains a worked **two-region walkthrough**
  (San Francisco vs. Salt Lake County, VLI renters, computed 2026-07-05):
  income ceilings first, engine output second, verdict and capacity third —
  the high-cost-adjustment and `ami_source` lessons shown with real dollars.
* Every vignette now walks through at least one real-place example, and the
  comparative ones read two or more regions against each other: two-metro
  Apartment List rents in `interactive-charts`, CA vs. MD typology structure
  in `neighborhood-typologies`, a two-city block-exact crosswalk with real
  boundary-straddling weights in `geography-utilities`, a two-city layout
  pattern in `mapping-with-maplibre`, and a port-the-recipe-to-Fulton-County
  section in `precarity-mapping`.

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
