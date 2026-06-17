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
with_hover <- function(widget, data_index, series_index = 0) {
  htmlwidgets::onRender(widget, sprintf(
    "function(el, x){
       setTimeout(function(){
         var inst = (window.echarts && echarts.getInstanceByDom) ? echarts.getInstanceByDom(el) : null;
         if(!inst){ var c = el.querySelector('div'); if(c) inst = echarts.getInstanceByDom(c); }
         if(inst){ inst.dispatchAction({type:'showTip', seriesIndex:%d, dataIndex:%d}); }
       }, 700);
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

# ---- Charts (synthetic editorial data; hover state captured) ----
filings <- data.frame(
  month   = seq(as.Date("2019-01-01"), by = "month", length.out = 72),
  filings = round(1000 + 250 * sin(seq_len(72) / 6) + seq_len(72) * 3)
)
shot(with_hover(
  nt_chart(filings, "month", "filings", type = "line",
           baseline = mean(filings$filings[1:12]), baseline_label = "2019 avg",
           band = c(as.Date("2020-03-01"), as.Date("2021-06-01")), band_label = "Moratorium",
           highlight_last = TRUE, height = "440px"),
  data_index = 60), "chart-trend.png", delay = 3, vheight = 500)

race <- expand.grid(year = 2016:2025, race = c("Black", "Latine", "White"))
race$rate <- c(seq(28, 40, length = 10), seq(22, 31, length = 10), seq(12, 15, length = 10))
shot(with_hover(
  nt_chart(race, "year", "rate", group = "race", type = "line",
           baseline = 20, baseline_label = "State avg",
           y_title = "per 1,000 renters", height = "440px"),
  data_index = 7), "chart-race.png", delay = 3, vheight = 500)

yearly <- data.frame(year = 2019:2025,
                     filings = c(13200, 9800, 8100, 11500, 13900, 14600, 6200))
shot(with_hover(
  nt_chart(yearly, "year", "filings", type = "bar", highlight_last = TRUE,
           height = "440px"),
  data_index = 4), "chart-bars.png", delay = 3, vheight = 500)

message("Done. Figures in ", fig_dir)
