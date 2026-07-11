# nt_profile_* — the state-profile inline-SVG/HTML builders.
# These are contract tests: the profile pages' interaction JS binds to the
# ids, classes, and data-* attributes asserted here, so a failure means the
# shipped pages' hover/tooltip/table/map behavior would break.

monthly_demo <- function(n = 64, start = "2019-01-01") {
  data.frame(date    = seq(as.Date(start), by = "month", length.out = n),
             filings = round(1500 + 400 * sin(seq_len(n) / 5)))
}

test_that("nt_profile_trend_svg emits the Figure 1 contract", {
  m <- monthly_demo()
  svg <- nt_profile_trend_svg(m$date, m$filings, baseline = 1378,
                              band_start = as.Date("2020-03-01"),
                              band_end   = as.Date("2021-11-01"))
  expect_match(svg, '<svg id="chart-monthly-svg" viewBox="0 0 1000 320"', fixed = TRUE)
  expect_match(svg, 'class="moratoria-band"')
  expect_match(svg, '<path class="data" d="M 40.0 ')
  expect_match(svg, 'class="focal"')
  expect_match(svg, 'id="chart-monthly-indicator"')
  # baseline y follows the data-driven ceiling: max 1900 -> ceiling 2000
  y_max <- max(500, ceiling(max(c(m$filings, 1378)) / 500) * 500)
  y_expect <- sprintf('y1="%.1f"', 298 - 1378 * 220 / y_max)
  expect_match(svg, paste0('<line class="baseline" x1="40" ', y_expect))
  # gridline labels always reach the ceiling
  expect_match(svg, format(y_max, big.mark = ","), fixed = TRUE)
})

test_that("nt_profile_monthly_js carries the series + scale constants", {
  js <- nt_profile_monthly_js(c(10L, 20L, 30L), year_base = 2019, y_div = 2500)
  expect_match(js, "window.__waMonthly = { filings: [10, 20, 30], yearBase: 2019, monthBase: 0, yDiv: 2500 };",
               fixed = TRUE)
})

test_that("nt_profile_yearly_svg accents the partial year and overlays a projection", {
  yrs <- 2016:2022
  vals <- c(18157, 17682, 16938, 15190, 4748, 3986, 6200)
  svg <- nt_profile_yearly_svg(yrs, vals, baseline = 16532, latest_month = 4,
                               projection = list(total = 19500, lo = 17000, hi = 22000))
  expect_match(svg, 'class="ybar focal" data-key="2022" data-label="2022 (Jan–Apr)"', fixed = TRUE)
  expect_match(svg, 'data-proj-total="19,500" data-proj-lo="17,000" data-proj-hi="22,000"', fixed = TRUE)
  expect_match(svg, 'class="yproj-pi"')
  expect_match(svg, "~19,500 projected", fixed = TRUE)
  # complete year: no accented bar, no projection elements even if one is
  # passed (the <style> block always defines the classes; check elements)
  svg2 <- nt_profile_yearly_svg(yrs, vals, baseline = 16532, latest_month = 12,
                                projection = list(total = 19500, lo = 17000, hi = 22000))
  expect_no_match(svg2, 'class="ybar focal"')
  expect_no_match(svg2, '<rect class="yproj')
  expect_no_match(svg2, "projected")
})

test_that("nt_profile_project_year returns NULL when there is nothing to project", {
  m <- monthly_demo(48, "2020-01-01")   # ends December -> year complete
  expect_null(nt_profile_project_year(m$date, m$filings))
  short <- monthly_demo(10, "2025-01-01")  # too few training months
  expect_null(nt_profile_project_year(short$date, short$filings, fit_start = "2025-01-01"))
})

test_that("nt_profile_project_year fits and sums a partial year", {
  skip_if_not_installed("forecast")
  m <- monthly_demo(52, "2022-01-01")   # ends April of year 5
  p <- nt_profile_project_year(m$date, m$filings, fit_start = "2022-01-01")
  expect_type(p, "list")
  expect_equal(p$h, 8L)
  expect_equal(p$ytd, sum(tail(m$filings, 4)))
  expect_gt(p$hi, p$total); expect_lt(p$lo, p$total)
  expect_match(p$order, "^SARIMA")
})

test_that("nt_profile_county_trend_data flags record counties and orders by record size", {
  ca <- expand.grid(month = 1:12, year = 2016:2020,
                    county_code = c("53001", "53002"), stringsAsFactors = FALSE)
  ca$county <- ifelse(ca$county_code == "53001", "Alpha", "Beta")
  # Alpha's record year is the latest full year; Beta peaked early and bigger
  ca$filings <- ifelse(ca$county_code == "53001",
                       ifelse(ca$year == 2020, 30L, 10L),
                       ifelse(ca$year == 2016, 50L, 5L))
  js <- nt_profile_county_trend_data(ca)
  expect_match(js, "window.__waCountyTrendData", fixed = TRUE)
  expect_match(js, '"name":"Alpha","rec":true', fixed = TRUE)
  expect_match(js, '"name":"Beta","rec":false', fixed = TRUE)
  # Beta's record (600/yr total) beats Alpha's (360) -> Beta listed first
  expect_lt(regexpr("Beta", js), regexpr("Alpha", js))
})

test_that("nt_profile_county_table flags the leader and keeps the sort/search shell", {
  co <- data.frame(county  = c("Clark", "King"),
                   renters = c(65952, 419249),
                   filings = c(2192, 8850))
  co$rate <- round(1000 * co$filings / co$renters, 1)
  html <- nt_profile_county_table(co[order(-co$rate), ],
                                  caption_desc = "d", caption_src = "s")
  expect_match(html, 'window.__waCountyTable', fixed = TRUE)
  expect_match(html, '{ county: "Clark", renters: 65952, filings: 2192, rate: 33.2, hi: true }', fixed = TRUE)
  expect_no_match(html, 'King[^}]*hi: true')
  for (id in c("county-search", "county-count", "county-table", "county-tbody", "th-filings"))
    expect_match(html, sprintf('id="%s"', id), fixed = TRUE)
})

test_that("nt_profile_map_explorer wires the map config into data-* attributes", {
  html <- nt_profile_map_explorer(
    county_json = "data/c.json?v=1", acs_json = "data/a.json?v=1",
    pmtiles = "assets/maps/x.pmtiles?v=1",
    center = c(-120.74, 47.55), zoom = 6,
    bbox = c(-124.85, 45.5, -116.92, 49.05),
    state_name = "Testonia", state_abbr = "TS", n_counties = 7)
  expect_match(html, 'data-center-lon="-120.740000"', fixed = TRUE)
  expect_match(html, "data-breaks='[0,15,25,35]'", fixed = TRUE)
  expect_match(html, "data-bbox='[-124.85,45.5,-116.92,49.05]'", fixed = TRUE)
  expect_match(html, "Search address or place in Testonia", fixed = TRUE)
  expect_match(html, "any TS address or place", fixed = TRUE)
  expect_match(html, "rank by rate (of 7 counties)", fixed = TRUE)
  for (id in c("state-map", "map-legend", "explorer-slider", "explorer-card", "tile-spark"))
    expect_match(html, sprintf('id="%s"', id), fixed = TRUE)
})

test_that("nt_profile_fourps_svg reproduces the shipped diagram's contract", {
  svg <- nt_profile_fourps_svg()
  expect_match(svg, '<div class="plate6-svg-wrap"><svg id="plate6-svg"', fixed = TRUE)
  expect_match(svg, 'viewBox="0 4 100 69.67"', fixed = TRUE)
  expect_match(svg, "var(--sx, 8.2)", fixed = TRUE)   # constant-px fallback — never 1
  expect_match(svg, "STRONGEST TOGETHER")
  expect_match(svg, "①  PREVENT")
})

test_that("nt_raw_html wraps output in a pandoc raw-html fence", {
  out <- capture.output(nt_raw_html("<p>x</p>"))
  expect_equal(out, c("```{=html}", "<p>x</p>", "```"))
})
