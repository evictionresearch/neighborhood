<!-- Read this: 
https://www.r-bloggers.com/2022/09/the-package-learning-how-to-build-an-r-package/

library(pkgdown)
usethis::use_pkgdown()
pkgdown::build_site()
usethis::use_pkgdown_github_pages() -->
# The Neighborhood Package

[![DOI](https://zenodo.org/badge/300691126.svg)](https://zenodo.org/doi/10.5281/zenodo.10642018)


An R package with various functions to study neighborhood dynamics. 

## Install

``` r
devtools::install_github("evictionresearch/neighborhood", ref = "main")
```

## Function reference (quick)

*A terse index of everything exported; details in the vignettes and help pages.*

**Affordability index** — three panes: affordability → availability → stability

| Function | What it does |
|---|---|
| `afford_index()` | The engine: tract-level affordable supply by AMI tier (30%/50% burden lines, vacancy/turnover availability, demand universes) |
| `afford_bands()` | Difference cumulative tiers into non-overlapping bands (30–50%, 50–80%, …) |
| `afford_verdict()` | The three-class verdict: affordable / roughly affordable / not affordable |
| `afford_capacity()` | Units-and-people arithmetic: affordable per 100 tier households, shortfall, openings (standing and at entry) |
| `afford_map()` | The verdict map (pane 1, or pane 2 via `verdict_col = "entry_verdict"`), optional displacement hatch |
| `afford_caption()` | The locked reading + "what ifs" block that travels with each pane's map |
| `afford_entry()` | Pane 2: entry premium (asking ÷ standing rent) re-scores each tract at the door price |
| `afford_zori()` | Zillow ZIP-level asking-rent index, downloaded once and cached |
| `afford_stability()` | Pane 3: weight the attainable set by HPRM displacement + eviction stability |
| `inflow_index()` | Revealed low-income net migration from HPRM (experimental; displacement read only) |
| `ami_cutoffs()` | AMI tier income ceilings by county (source ladder: HUD snapshot/API → Census reproductions) |
| `hud_ami()` | Standalone AMI level by county × year, with an `ami_source` provenance column |
| `afford()` | Legacy v1 engine (superseded; kept for old maps) |

**Neighborhood racial typologies**

| Function | What it does |
|---|---|
| `ntdf()` | Build the tract typology data (optionally with geometry) |
| `ntcheck()` | Typology counts, to decide concatenation |
| `nt_pal()` | The canonical typology palette |

**Interactive maps (MapLibre)**

| Function | What it does |
|---|---|
| `nt_map()` | One-line choropleth of an `sf` layer |
| `nt_map_sync()` | Two-plus maps side by side with locked (synchronized) views |
| `nt_maplibre()` | Start a map (basemap, controls, fit to data) |
| `nt_add_choropleth()` | Add a colored layer + legend + popup/tooltip |
| `nt_add_labels()` | Add text labels |
| `nt_layers_control()` | Layer switcher for multi-layer maps |
| `nt_popup()` | Deterministic HTML popups |
| `nt_pmtiles()` | PMTiles vector tiles for large/full-US layers |
| `nt_declutter_basemap()` | Hide park/landcover/hillshade basemap tints under fills |

**Interactive charts (echarts4r)**

| Function | What it does |
|---|---|
| `nt_chart()` | Editorial trend/bar/area chart (hover y-axis, baselines, end labels) |
| `nt_spark()` | Inline sparkline |
| `nt_bar()` / `nt_lollipop()` | Ranked bars / dot rankings (diverging + grouped variants) |
| `nt_dumbbell()` / `nt_range()` | Two-point disparity dots / estimate with error whisker |
| `nt_stacked_bar()` | Composition / 100%-stacked bars |
| `nt_slope()` / `nt_scatter()` / `nt_waffle()` | Before-after slopes / scatter + trend / unit ("Du Bois") chart |

**Static charts (ggplot2)**

| Function | What it does |
|---|---|
| `theme_ern()` | The house ggplot theme |
| `scale_color_ern()` / `scale_fill_ern()` | Discrete house scales (`_c` continuous, `_div` diverging; `colour` aliases) |
| `ern_palette()` | The brand palettes (qualitative, sequential, diverging) |
| `ern_swatch()` | Open the palette sheet with WCAG contrast receipts |

**HPRM maps & model plots**

| Function | What it does |
|---|---|
| `nt_hprm_map()` | The jurisdiction-report HPRM map (switchable EDR / EER / composite / divergence layers, boundary framing) |
| `nt_hprm_pal()` | Canonical colors for the scored HPRM tier vocabularies |
| `nt_pd_plot()` | Partial-dependence curve + predictor histogram (BART driver plots) |
| `nt_pd_bart()` | Compute the PD table from a fitted `bartMachine` model |

**Geography utilities & crosswalks**

| Function | What it does |
|---|---|
| `get_co_puma()` | County ↔ PUMA crosswalk |
| `nt_areal_weight()` | Block-exact tract ↔ place population/housing weights |
| `nt_zcta_weights()` | Block-exact ZCTA → tract housing-unit weights (the ZIP→tract crosswalk) |
| `nt_erase_water()` | Clip water out of tract geometries for cleaner maps |

**Bundled data:** `us_nt_tracts2024` (typologies for every US tract), `mn_evictions` (MN county-month filings).

## Neighborhood Racial Typologies Function

The Neighborhood Racial Typologies function is a descriptive, categorical tool to help identify racial and ethnic divides within a region (e.g. city, county, state, etc.). Traditional segregation measures, such as the [disimilarity index](https://en.wikipedia.org/wiki/Index_of_dissimilarity), provide a single measure for a large geographical area. This descripitve function is useful for mapping tract level, small area (e.g. neighborhood level) divisions between ethnic and racial groups.  

The neighborhood typology is designated when a tract's racial/ethnic group share is more than 10%. For example, if a tract is 60% white, 30% Asian, 5% Black, and 5% Latinx then that tract will be called a majority Asian-White tract. This definition can be found in the `NeighType` field.  

Because there are so many different combinations, I created the `nt_conc` field to show concatinated fields of 1, 2, or more groups. 

#### Credits
This function is based off this paper:  
[Hall, Matthew, Kyle Crowder, and Amy Spring. 2015. “Neighborhood Foreclosures, Racial/Ethnic Transitions, and Residential Segregation.” American Sociological Review 80:526–549.](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4479290/)

1. `nt`: Neighborhood Racial Typologies
2. `ntdf`: create the dataframe for nt
3. `ntcheck`: identify counts that can be concatenated

### Example
You can view an interactive map using this function [here](https://evictionresearch.net/maryland/maps/baltimore.html). Choose the "Neighborhood Segregation" layer

``` r
Baltimore_nt <- ntdf(state = "MD", county = "Baltimore City", geometry = TRUE)
cal <- ntdf(state = "CA")
ny <- ntdf(state = "NY")
glimpse(Baltimore_nt)

ps_nt <- ntdf(state = "WA", county = c("Snohomish", "King", "Pierce"), geometry = TRUE)


# Check to see if there are duplicate tract assumptions. 
Baltimore_nt %>% 
st_set_geometry(NULL) %>% 
mutate(val = 1) %>%
spread(NeighType, val, fill = 0) %>% 
mutate_at(vars(`All Black`:`White-Shared`), list(as.numeric)) %>% 
select(13:ncol(.)) %>% 
mutate(rowsum = rowSums(.)) %>% 
filter(rowsum > 1) %>% 
glimpse()
```

### Concatenation
After running the above code, look at the counts and consider concatenating and/or reducing outlying (small count) neighborhood types. 

``` r
ntcheck(Baltimore_nt)
ntcheck(cal)
ntcheck(ny)
ntcheck(ps_nt)
```

The `nt_conc` field concatenates the `NeighType` field automatically and may satisfy most people. 

## Get County and PUMA cross sections
The `get_co_puma` function defines the county associated with a PUMA. In some cases, multiple PUMAs fall within one county, such as in urban areas. In other situations, multiple counties may fall within one PUMA, such as in rural situations. Tracts nest within PUMAs so this crosswalk lets you aggregate tract- or PUMA-level data to counties.

## Interactive maps (MapLibre)

The package includes a small, customizable toolkit for building interactive
[MapLibre GL](https://maplibre.org/) maps of neighborhood data — the same kind
of maps used in the Eviction Research Network state profiles (HPRM, Minnesota,
Washington), with legends, labels, popups, and (for large data) PMTiles vector
tiles. It is a deterministic tool: plain R, no AI required to run it.

A map is one line:

``` r
library(neighborhood)

balt <- ntdf(state = "MD", county = "Baltimore City", geometry = TRUE)
nt_map(balt)   # typologies, colored, with legend + popup + controls
```

![A choropleth map of Baltimore City census tracts colored by neighborhood typology, with a legend, basemap, and address search box.](man/figures/README-map.png)

…and composes up to whatever you need:

``` r
nt_maplibre(balt) |>
  nt_add_choropleth(balt, "nt_conc", legend = "interactive") |>
  nt_add_labels(balt, "NAME", min_zoom = 11)
```

| Function | Purpose |
|---|---|
| `nt_map()` | One-line map of an `sf` layer |
| `nt_maplibre()` | Start a map (basemap, controls, fit to data) |
| `nt_add_choropleth()` | Add a colored layer + legend + popup/tooltip |
| `nt_add_labels()` | Add text labels |
| `nt_popup()` | Build deterministic HTML popups |
| `nt_pmtiles()` | Build PMTiles for large/full-US data |

Maps accept an `sf` object **or** a path to a shapefile / GeoJSON / GeoPackage
(or an `sfc` / `sp` / `terra` object) — everything is normalized to `sf`.

See `vignette("mapping-with-maplibre")` for the full tutorial (from a one-liner
to HPRM-style rich popups) and `vignette("neighborhood-typologies")` for the
typology workflow.

## Interactive charts (echarts)

`nt_chart()` builds the editorial trend/bar charts from the state profiles, where
**hovering reveals the y-axis** (a crosshair prints the value on the axis at the
cursor) plus a tooltip — with one-argument baselines, shaded bands, and a
highlighted latest point. For the published-plate look it also does direct
end-of-line labels (`"Black · 33.5"`), print-friendly per-series line styles, and
a source-credit footer; `yaxis = "hover"` makes the y-axis appear only on hover.
`nt_spark()` makes inline sparklines.

``` r
filings <- data.frame(month = seq(as.Date("2019-01-01"), by = "month", length.out = 72),
                      filings = round(1000 + 250 * sin(seq_len(72) / 6)))
nt_chart(filings, "month", "filings", type = "line",
         baseline = mean(filings$filings[1:12]), baseline_label = "2019 avg",
         highlight_last = TRUE)
```

![An editorial trend line shown mid-hover: a red crosshair tracks the cursor and prints the value on the y-axis, alongside a tooltip, the dashed baseline, and the accented latest point.](man/figures/README-chart.png)

See `vignette("interactive-charts")` for the full tour, which also replicates the
Minnesota state-profile charts and pulls live Apartment List rent data from a URL.

## Bundled data

| Dataset | What |
|---|---|
| `us_nt_tracts2024` | Neighborhood racial typologies for every US tract (2020–2024 ACS) |
| `mn_evictions` | Minnesota county-month eviction filings (the public state-profile data, with race breakdowns) |

The advanced `vignette("precarity-mapping")` walks through building a precarity-style
interactive map — switchable risk layers, a threshold slider, and rich popups (race
bars and tract-vs-county-vs-US "bubble tracks") — entirely from public data.

## Credits

Census data come from [**tidycensus**](https://walker-data.com/tidycensus/) and
the interactive maps are built on [**mapgl**](https://walker-data.com/mapgl/),
both by [Kyle Walker](https://walker-data.com/). His book,
[*Analyzing US Census Data*](https://walker-data.com/census-r/), is an excellent
companion. PMTiles are built with
[tippecanoe](https://github.com/felt/tippecanoe). The interactive charts are
built on [**echarts4r**](https://echarts4r.john-coene.com/) by
[John Coene](https://john-coene.com/) (bundling Apache ECharts).

For transparency: the MapLibre mapping functions (`nt_map()`, `nt_maplibre()`,
`nt_add_choropleth()`, `nt_add_labels()`, `nt_popup()`, `nt_pmtiles()`) and the
chart functions (`nt_chart()`, `nt_spark()`) were designed and implemented with
the assistance of **Claude Opus 4.8** (Anthropic). They are deterministic R code
and require no AI to operate.
