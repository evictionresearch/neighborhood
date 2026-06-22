# Generate the static example figures embedded in the vignettes.
#
# This is a developer/build script (data-raw/ is build-ignored), not part of the
# package. It renders real nt_map()/nt_chart() outputs to PNGs under
# vignettes/figures/ so the tutorials can show example output without requiring
# a browser, a Census API key, or tippecanoe at vignette-build time.
#
# Requirements: webshot2 (+ chromote) and a Chromium-family browser, plus network
# access (OpenFreeMap basemap tiles + tigris TIGER geometry). Re-run after
# changing a function's visual output.
#
#   Rscript data-raw/make_vignette_figures.R

suppressPackageStartupMessages({
  # Load the current source (so figures reflect the working tree) when run from
  # the package root; fall back to the installed package otherwise.
  if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")) {
    pkgload::load_all(".", quiet = TRUE)
  } else {
    library(neighborhood)
  }
  library(sf)
  library(dplyr)
  library(htmlwidgets)
  library(webshot2)
})

# Point chromote at an available Chromium-family browser if Chrome isn't default.
if (Sys.getenv("CHROMOTE_CHROME") == "") {
  brave <- "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"
  if (file.exists(brave)) Sys.setenv(CHROMOTE_CHROME = brave)
}

fig_dir <- "vignettes/figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# Save a widget to a temp HTML file, then screenshot it to a PNG.
shot <- function(widget, file, delay = 2, vwidth = 1000, vheight = 560) {
  tmp <- tempfile(fileext = ".html")
  saveWidget(widget, tmp, selfcontained = TRUE)
  webshot2::webshot(tmp, file.path(fig_dir, file),
                    delay = delay, vwidth = vwidth, vheight = vheight)
  message("wrote ", file.path(fig_dir, file))
}

# Auto-open a chart's tooltip after render so the screenshot captures the
# signature hover state: the red crosshair + the value printed on the y-axis.
# Also fires a `mouseenter` so the yaxis = "hover" reveal (labels + gridlines)
# is captured.
with_hover <- function(widget, data_index, series_index = 0) {
  htmlwidgets::onRender(widget, sprintf(
    "function(el, x){
       setTimeout(function(){
         el.dispatchEvent(new MouseEvent('mouseenter', {bubbles:false}));
         var inst = (window.echarts && echarts.getInstanceByDom) ? echarts.getInstanceByDom(el) : null;
         if(!inst){ var c = el.querySelector('div'); if(c) inst = echarts.getInstanceByDom(c); }
         if(inst){ inst.dispatchAction({type:'showTip', seriesIndex:%d, dataIndex:%d}); }
       }, 900);
     }", series_index, data_index))
}

# ---- Maps (real Baltimore City geometry via tigris + bundled typologies) ----
bc <- tigris::tracts(state = "MD", county = "510", cb = TRUE, year = 2022,
                     progress_bar = FALSE) |>
  st_transform(4326) |>
  select(GEOID)
md <- left_join(bc, filter(us_nt_tracts2024, state == "MD"), by = "GEOID") |>
  filter(!is.na(nt_conc))

md$popup <- nt_popup(md, title = "GEOID",
                     fields = c("Neighborhood type" = "nt_conc",
                                "Population" = "totraceE", "Share Black" = "pBlack"))

shot(nt_map(md, popup = "popup", legend_title = "Neighborhood type"),
     "map-typologies.png", delay = 7, vheight = 620)        # basemap tiles need time

shot(nt_map(md, color = "pBlack", legend_title = "Share Black"),
     "map-numeric.png", delay = 7, vheight = 620)

# Multi-layer switcher: typology + the share of each demographic group.
demos <- c(White = "pWhite", Black = "pBlack", Asian = "pAsian", Latine = "pLatine")
m_layers <- nt_maplibre(md) |>
  nt_add_choropleth(md, "nt_conc", id = "Typology", legend_title = "Neighborhood type")
for (i in seq_along(demos)) {
  m_layers <- nt_add_choropleth(m_layers, md, demos[[i]], id = names(demos)[i],
                                legend_title = paste("Share", names(demos)[i]),
                                visible = FALSE)
}
m_layers <- nt_layers_control(m_layers, layers = c("Typology", names(demos)),
                              title = "Show layer")
shot(m_layers, "map-layers.png", delay = 7, vheight = 620)

# ---- Minnesota charts from the bundled mn_evictions (yaxis = "hover") ----
statewide <- mn_evictions |>
  group_by(year, month) |>
  summarise(filings = sum(filings), .groups = "drop") |>
  mutate(date = as.Date(sprintf("%d-%02d-01", year, month))) |>
  filter(date >= as.Date("2019-01-01"))
prepan <- mn_evictions |>
  filter(year <= 2019) |>
  group_by(year, month) |>
  summarise(filings = sum(filings), .groups = "drop") |>
  pull(filings) |>
  mean()
shot(with_hover(
  nt_chart(statewide, "date", "filings", type = "line", yaxis = "hover",
           baseline = prepan, baseline_label = "2017-2019 avg",
           band = c(as.Date("2020-03-01"), as.Date("2021-06-01")), band_label = "Moratorium",
           highlight_last = TRUE, height = "440px"),
  data_index = 70), "chart-mn-trend.png", delay = 3, vheight = 500)

yearly <- mn_evictions |> group_by(year) |> summarise(filings = sum(filings), .groups = "drop")
shot(with_hover(
  nt_chart(yearly, "year", "filings", type = "bar", yaxis = "hover",
           highlight_last = TRUE, height = "440px"),
  data_index = 8), "chart-mn-bars.png", delay = 3, vheight = 500)

roll12 <- function(x) as.numeric(stats::filter(x, rep(1, 12), sides = 1))
race <- mn_evictions |>
  arrange(year, month) |>
  group_by(year, month) |>
  summarise(across(c(filings_black, filings_white, filings_latine, filings_other,
                     renters_black, renters_white, renters_latine, renters_other), sum),
            .groups = "drop") |>
  mutate(date = as.Date(sprintf("%d-%02d-01", year, month)),
         Black  = 1000 * roll12(filings_black)  / renters_black,
         White  = 1000 * roll12(filings_white)  / renters_white,
         Latine = 1000 * roll12(filings_latine) / renters_latine,
         Other  = 1000 * roll12(filings_other)  / renters_other) |>
  tidyr::pivot_longer(c(Black, White, Latine, Other), names_to = "race", values_to = "rate") |>
  filter(!is.na(rate))
shot(with_hover(
  nt_chart(race, "date", "rate", group = "race", type = "line", yaxis = "hover",
           end_label = TRUE, highlight_last = TRUE,
           line_styles = c(Black = "solid", Latine = "dotted",
                           Other = "dashed", White = "solid"),
           palette = c(Black = "#19222C", Latine = "#19222C",
                       Other = "#3f4b57", White = "#9fb6c4"),
           source = "lsc · acs · wru v2.0 · birdie", height = "440px"),
  data_index = 80), "chart-mn-race.png", delay = 3, vheight = 500)

# Plate VI: the disparity ratio (each group's rate / the white rate).
ratio <- race |>
  dplyr::select(date, race, rate) |>
  tidyr::pivot_wider(names_from = race, values_from = rate) |>
  mutate(Black = Black / White, Latine = Latine / White, Other = Other / White) |>
  tidyr::pivot_longer(c(Black, Latine, Other), names_to = "race", values_to = "ratio") |>
  filter(!is.na(ratio))
shot(with_hover(
  nt_chart(ratio, "date", "ratio", group = "race", type = "line", yaxis = "hover",
           value_fmt = "multiple", end_label = TRUE, highlight_last = TRUE,
           baseline = 1, baseline_label = "parity with white renters",
           line_styles = c(Black = "solid", Latine = "dotted", Other = "dashed"),
           palette = c(Black = "#19222C", Latine = "#19222C", Other = "#9fb6c4"),
           source = "lsc · acs · wru v2.0 · birdie", height = "440px"),
  data_index = 80), "chart-mn-ratio.png", delay = 3, vheight = 500)

# ---- Chart gallery: ranked / disparity / composition from mn_evictions ----
# All from the bundled data (no network), so these always render. The latest
# complete 12-month window drives the cross-sectional charts.
last_date <- mn_evictions |>
  mutate(date = as.Date(sprintf("%d-%02d-01", year, month))) |>
  summarise(m = max(date)) |> pull(m)
cutoff <- seq(last_date, by = "-12 months", length.out = 2)[2]
win <- mn_evictions |>
  mutate(date = as.Date(sprintf("%d-%02d-01", year, month))) |>
  filter(date > cutoff)

county12 <- win |>
  group_by(county) |>
  summarise(filings = sum(filings), renters = dplyr::first(renters),
            fb = sum(filings_black), fw = sum(filings_white),
            rb = dplyr::first(renters_black), rw = dplyr::first(renters_white),
            .groups = "drop") |>
  mutate(rate  = 1000 * filings / renters,
         black = 1000 * fb / rb, white = 1000 * fw / rw,
         share_black = rb / renters) |>
  filter(is.finite(rate))

top <- county12 |> arrange(dplyr::desc(rate)) |> head(10)

# 1. Ranked horizontal bars: top-10 counties by 12-month filing rate.
shot(with_hover(
  nt_bar(top, "county", "rate", value_fmt = "comma", yaxis = "hover",
         source = "lsc · acs", height = "440px"), data_index = 0),
  "chart-rank-counties.png", delay = 3, vheight = 500)

# 2. Disparity dumbbell: Black vs white 12-month rate, widest gaps.
gap <- county12 |> filter(is.finite(black), is.finite(white)) |>
  mutate(g = black - white) |> arrange(dplyr::desc(g)) |> head(10)
shot(nt_dumbbell(gap, "county", low = "white", high = "black",
                 low_label = "White", high_label = "Black",
                 source = "lsc · acs · wru v2.0 · birdie", height = "460px"),
     "chart-dumbbell-race.png", delay = 3, vheight = 520)

# 3. Lollipop: statewide rate by race, indexed to the white rate.
sw <- win |>
  summarise(across(c(filings_black, filings_white, filings_latine, filings_other,
                     renters_black, renters_white, renters_latine, renters_other), sum)) |>
  mutate(Black = 1000 * filings_black / renters_black,
         White = 1000 * filings_white / renters_white,
         Latine = 1000 * filings_latine / renters_latine,
         Other = 1000 * filings_other / renters_other)
idx <- data.frame(race = c("Black", "Latine", "Other", "White"),
                  index = c(sw$Black, sw$Latine, sw$Other, sw$White) / sw$White)
shot(nt_lollipop(idx, "race", "index", value_fmt = "multiple", highlight = "Black",
                 baseline = 1, baseline_label = "parity with white",
                 source = "lsc · acs · wru v2.0 · birdie", height = "360px"),
     "chart-lollipop-race.png", delay = 3, vheight = 400)

# 4. Composition (100% stacked): renter racial composition, top-6 counties.
sc <- win |> group_by(county) |>
  summarise(Black = dplyr::first(renters_black), White = dplyr::first(renters_white),
            Latine = dplyr::first(renters_latine), Other = dplyr::first(renters_other),
            .groups = "drop") |>
  filter(county %in% top$county[1:6]) |>
  tidyr::pivot_longer(c(Black, White, Latine, Other), names_to = "race", values_to = "renters")
shot(nt_stacked_bar(sc, "county", "renters", group = "race", height = "420px",
                    palette = c(White = "#9fb6c4", Black = "#19222C",
                                Latine = "#F9322B", Other = "#6c7a89"),
                    source = "acs"), "chart-stacked-renters.png", delay = 3, vheight = 460)

# 5. Slope: 2019 -> 2024 annual filing rate per 1,000, biggest movers.
ann <- mn_evictions |> filter(year %in% c(2019, 2024)) |>
  group_by(county, year) |>
  summarise(filings = sum(filings), renters = dplyr::first(renters), .groups = "drop") |>
  mutate(rate = 1000 * filings / renters) |> filter(is.finite(rate))
mov <- ann |>
  tidyr::pivot_wider(id_cols = county, names_from = year, values_from = rate,
                     names_prefix = "y") |>
  mutate(d = y2024 - y2019) |> arrange(dplyr::desc(abs(d))) |> head(6)
shot(nt_slope(filter(ann, county %in% mov$county), "year", "rate", group = "county",
              value_fmt = "comma", source = "lsc · acs", height = "440px"),
     "chart-slope-counties.png", delay = 3, vheight = 500)

# 6. Scatter: county filing rate vs. share of renters who are Black (+ trend).
sca <- county12 |> filter(is.finite(share_black))
shot(with_hover(
  nt_scatter(sca, "share_black", "rate", trend = TRUE, x_fmt = "percent",
             value_fmt = "comma", yaxis = "hover", source = "lsc · acs",
             height = "440px"), data_index = 0),
  "chart-scatter-share.png", delay = 3, vheight = 500)

# 7. Waffle: statewide composition of filings by race (12-month). Pass the race
# palette so the focus group (Black) carries the accent and White stays light.
race_pal <- c(Black = "#F9322B", White = "#aab4be", Latine = "#223754", Other = "#6c7a89")
comp <- c(Black = sw$filings_black, White = sw$filings_white,
          Latine = sw$filings_latine, Other = sw$filings_other)
shot(nt_waffle(comp, palette = race_pal,
               title = "Share of eviction filings by race, last 12 months",
               source = "lsc · acs · wru v2.0 · birdie", height = "440px"),
     "chart-waffle-race.png", delay = 2, vheight = 480)

# 7b. Estimate + error bar: top counties by 12-month rate with a count-based
# (Poisson) 95% interval, so the uncertainty in low-filing counties is visible.
ci <- top |>
  mutate(se = 1000 * sqrt(filings) / renters,
         lo = pmax(0, rate - 1.96 * se), hi = rate + 1.96 * se)
shot(with_hover(
  nt_range(ci, "county", value = "rate", low = "lo", high = "hi",
           value_fmt = "comma", yaxis = "hover", source = "lsc · acs",
           height = "440px"), data_index = 0),
  "chart-range-counties.png", delay = 3, vheight = 500)

# 8. Static (ggplot2) version of the ranked bar in the ERN look — no browser
#    needed, so this one renders even without a Chromium-family browser.
{
  library(ggplot2)
  gg <- top |> mutate(hl = rate == max(rate)) |>
    ggplot(aes(stats::reorder(county, rate), rate, fill = hl)) +
    geom_col(width = 0.68) +
    geom_text(aes(label = round(rate, 1)), hjust = -0.15, size = 3.3,
              color = ern_palette("brand")[["navy"]]) +
    scale_fill_manual(values = c(`FALSE` = ern_palette("brand")[["navy"]],
                                 `TRUE` = ern_palette("brand")[["accent"]]),
                      guide = "none") +
    coord_flip(clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(title = "Eviction filing rate by county",
         subtitle = "Filings per 1,000 renter households · latest 12 months",
         x = NULL, y = NULL, caption = "Source: ERN · LSC court records") +
    theme_ern(grid = "x") +
    theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank())
  ggsave(file.path(fig_dir, "chart-ggtheme.png"), gg, width = 8, height = 4.2, dpi = 140)
  message("wrote ", file.path(fig_dir, "chart-ggtheme.png"))
}

# ---- Apartment List: pull from URL, Minneapolis metro median rent ----
# Scrape the current link off the data page; fall back to a pinned vintage if
# the page serves bot-filtered content to readLines().
apt_ok <- tryCatch({
  page <- "https://www.apartmentlist.com/research/category/data-rent-estimates"
  html <- tryCatch(paste(readLines(url(page), warn = FALSE), collapse = "\n"), error = function(e) "")
  csv_url <- regmatches(html, regexpr(
    "https://assets\\.ctfassets\\.net/[^\"\\\\]*Apartment_List_Rent_Estimates_2[0-9_]+\\.csv", html))[1]
  if (is.na(csv_url)) {
    csv_url <- paste0("https://assets.ctfassets.net/jeox55pd4d8n/2nz8cro1T1bopNPDG6Sbsf/",
                      "54e7e1745db655de15aaa99c5570e6ea/Apartment_List_Rent_Estimates_2026_05.csv")
  }
  rent <- readr::read_csv(csv_url, col_types = readr::cols(location_fips_code = readr::col_character()),
                          show_col_types = FALSE)
  mpls <- rent |>
    filter(location_type == "Metro", location_fips_code == "33460", bed_size == "overall") |>
    tidyr::pivot_longer(matches("^\\d{4}_\\d{2}$"), names_to = "month", values_to = "rent") |>
    mutate(date = lubridate::ym(month)) |>
    filter(!is.na(rent))
  shot(with_hover(
    nt_chart(mpls, "date", "rent", type = "line", value_fmt = "currency", yaxis = "hover",
             title = "Median new-lease rent - Minneapolis metro", height = "440px"),
    data_index = nrow(mpls) - 6), "chart-apartmentlist.png", delay = 3, vheight = 500)
  TRUE
}, error = function(e) { message("Apartment List figure skipped: ", conditionMessage(e)); FALSE })

# ---- Precarity map (illustrative composite from public ACS data) ----
# Build a transparent renter-precarity composite for Hennepin County, MN, the
# same way the precarity-mapping vignette does. Needs a Census API key.
prec_ok <- tryCatch({
  acs_vars <- c(
    renters = "B25003_003", occ_hh = "B25003_001",
    rb_30_35 = "B25070_007", rb_35_40 = "B25070_008",
    rb_40_50 = "B25070_009", rb_50p = "B25070_010",
    rb_total = "B25070_001", rb_na = "B25070_011",
    med_rent = "B25064_001", med_inc = "B19013_001", pop = "B01003_001")
  tr <- tidycensus::get_acs(geography = "tract", state = "MN", county = "Hennepin",
                            variables = acs_vars, year = 2022, output = "wide",
                            geometry = TRUE, progress_bar = FALSE) |>
    st_transform(4326)
  pctl <- function(x) dplyr::percent_rank(x) * 100
  prec <- tr |>
    transmute(GEOID, pop = popE, med_rent = med_rentE, med_inc = med_incE,
              renter_share = rentersE / occ_hhE,
              rent_burden = (rb_30_35E + rb_35_40E + rb_40_50E + rb_50pE) /
                pmax(rb_totalE - rb_naE, 1)) |>
    filter(pop >= 500, !is.na(renter_share), !is.na(rent_burden)) |>
    mutate(precarity = rowMeans(cbind(pctl(rent_burden), pctl(renter_share), pctl(-med_inc))))
  ramp5 <- c("#ffffcc", "#fed976", "#fd8d3c", "#f03b20", "#bd0026")
  m_prec <- nt_maplibre(prec) |>
    nt_add_choropleth(prec, "precarity", id = "Precarity", breaks = c(20, 40, 60, 80),
                      colors = ramp5, labels = c("0-20", "20-40", "40-60", "60-80", "80-100"),
                      legend_title = "Precarity percentile", opacity = 0.85) |>
    nt_add_choropleth(prec, "rent_burden", id = "Rent burden",
                      breaks = c(0.35, 0.45, 0.55, 0.65), colors = ramp5,
                      labels = c("<35%", "35-45%", "45-55%", "55-65%", "65%+"),
                      legend_title = "Renters paying 30%+", opacity = 0.85, visible = FALSE) |>
    nt_add_choropleth(prec, "renter_share", id = "Renter share", breaks = c(0.25, 0.5, 0.75),
                      labels = c("<25%", "25-50%", "50-75%", "75%+"),
                      legend_title = "Renter share", opacity = 0.85, visible = FALSE)
  m_prec <- nt_layers_control(m_prec, layers = c("Precarity", "Rent burden", "Renter share"),
                              title = "Measure")
  shot(m_prec, "map-precarity.png", delay = 7, vheight = 620)
  TRUE
}, error = function(e) { message("Precarity figure skipped: ", conditionMessage(e)); FALSE })

# ---- Affordability index map (afford_index ratio, for affordability-index.Rmd) ----
# VLI rental supply ratio by tract, San Francisco. Needs a Census API key (the
# pure-ACS AMI source); skipped cleanly if unavailable.
afford_ok <- tryCatch({
  idx <- afford_index("06", "075", geometry = TRUE)
  vli_rent <- idx[idx$tenure == "rent" & idx$ami_tier == "VLI" & !is.na(idx$ratio), ]
  m_aff <- nt_maplibre(vli_rent) |>
    nt_add_choropleth(vli_rent, "ratio", type = "continuous",
                      legend_title = "VLI rental supply ratio")
  shot(m_aff, "map-afford-ratio.png", delay = 7, vheight = 620)
  TRUE
}, error = function(e) { message("Affordability figure skipped: ", conditionMessage(e)); FALSE })

# Remove orphaned figures from earlier revisions.
unlink(file.path(fig_dir, c("chart-trend.png", "chart-race.png", "chart-bars.png",
                            "map-hprm.png")))

message("Done. Figures in ", fig_dir)
