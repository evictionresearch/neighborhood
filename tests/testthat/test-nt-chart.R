# Object-shape tests for the interactive chart wrappers. They inspect the
# echarts4r option payload rather than rendering in a browser, so they run on CI.
# Guarded on echarts4r being installed.

trend_df <- function() {
  data.frame(
    month   = seq(as.Date("2019-01-01"), by = "month", length.out = 36),
    filings = round(1000 + 200 * sin(seq_len(36) / 6))
  )
}

race_df <- function() {
  d <- expand.grid(year = 2016:2024, race = c("Black", "Latine", "White"),
                   stringsAsFactors = FALSE)
  d$rate <- round(runif(nrow(d), 10, 40), 1)
  d
}

test_that("nt_chart returns an echarts htmlwidget", {
  skip_if_not_installed("echarts4r")
  m <- nt_chart(trend_df(), "month", "filings", type = "line")
  expect_s3_class(m, "echarts4r")
  expect_s3_class(m, "htmlwidget")
  expect_length(m$x$opts$series, 1L)
})

test_that("nt_chart enables the crosshair axisPointer on hover", {
  skip_if_not_installed("echarts4r")
  m <- nt_chart(trend_df(), "month", "filings", crosshair = TRUE)
  ap <- m$x$opts$tooltip$axisPointer
  expect_identical(ap$type, "cross")
  # red dashed guide, ERN accent
  expect_match(ap$lineStyle$color, "F9322B", ignore.case = TRUE)
})

test_that("nt_chart can disable the crosshair", {
  skip_if_not_installed("echarts4r")
  m <- nt_chart(trend_df(), "month", "filings", crosshair = FALSE)
  expect_null(m$x$opts$tooltip$axisPointer)
})

test_that("grouped data yields one series per group", {
  skip_if_not_installed("echarts4r")
  m <- nt_chart(race_df(), "year", "rate", group = "race", type = "line")
  expect_length(m$x$opts$series, 3L)
})

test_that("numeric (year) x becomes a category axis, not a 0-based value axis", {
  skip_if_not_installed("echarts4r")
  m <- nt_chart(race_df(), "year", "rate", group = "race")
  expect_identical(m$x$opts$xAxis[[1]]$type, "category")
})

test_that("date x stays a time axis", {
  skip_if_not_installed("echarts4r")
  m <- nt_chart(trend_df(), "month", "filings")
  expect_identical(m$x$opts$xAxis[[1]]$type, "time")
})

test_that("baseline adds a dashed mark line", {
  skip_if_not_installed("echarts4r")
  m <- nt_chart(trend_df(), "month", "filings",
                baseline = 1000, baseline_label = "avg")
  expect_false(is.null(m$x$opts$series[[1]]$markLine))
})

test_that("bar type and stacking work", {
  skip_if_not_installed("echarts4r")
  m <- nt_chart(race_df(), "year", "rate", group = "race",
                type = "bar", stack = "total")
  expect_length(m$x$opts$series, 3L)
  expect_identical(m$x$opts$series[[1]]$stack, "total")
})

test_that("nt_chart validates inputs", {
  skip_if_not_installed("echarts4r")
  expect_error(nt_chart(trend_df(), "nope", "filings"), "not found")
  expect_error(nt_chart(list(a = 1), "a", "b"), "data frame")
})

test_that("percent value_fmt scales by 100 so axis and tooltip agree", {
  skip_if_not_installed("echarts4r")
  # The y-axis uses Intl percent (x100); the crosshair/tooltip JS must match.
  js <- neighborhood:::.nt_js_number_expr("percent", "v")
  expect_match(js, "100", fixed = TRUE)
  # comma/currency must NOT scale
  expect_false(grepl("100", neighborhood:::.nt_js_number_expr("comma", "v"), fixed = TRUE))
})

test_that("band edges are coerced to match a category (numeric-year) axis", {
  skip_if_not_installed("echarts4r")
  df <- data.frame(year = 2016:2025, filings = seq_len(10) * 1000)
  m <- nt_chart(df, "year", "filings", type = "bar", band = c(2020, 2022))
  ma <- m$x$opts$series[[1]]$markArea
  expect_type(ma$data[[1]][[1]]$xAxis, "character")
  expect_identical(ma$data[[1]][[1]]$xAxis, "2020")
})

test_that("band stays a Date on a time axis", {
  skip_if_not_installed("echarts4r")
  m <- nt_chart(trend_df(), "month", "filings",
                band = c(as.Date("2020-03-01"), as.Date("2021-06-01")))
  ma <- m$x$opts$series[[1]]$markArea
  expect_false(is.null(ma))
})

test_that("yaxis = 'hover' hides the y-axis labels and attaches a reveal hook", {
  skip_if_not_installed("echarts4r")
  m <- nt_chart(trend_df(), "month", "filings", yaxis = "hover")
  expect_false(isTRUE(m$x$opts$yAxis[[1]]$axisLabel$show))
  expect_false(is.null(m$jsHooks$render))
  expect_match(paste(unlist(m$jsHooks$render), collapse = " "), "setOption")
})

test_that("yaxis = 'always' (default) shows labels with no reveal hook", {
  skip_if_not_installed("echarts4r")
  m <- nt_chart(trend_df(), "month", "filings")
  expect_true(isTRUE(m$x$opts$yAxis[[1]]$axisLabel$show))
  expect_null(m$jsHooks$render)
})

test_that("end_label attaches a direct end label per series and hides the legend", {
  skip_if_not_installed("echarts4r")
  m <- nt_chart(race_df(), "year", "rate", group = "race", type = "line",
                end_label = TRUE)
  shows <- vapply(m$x$opts$series, function(s) isTRUE(s$endLabel$show), logical(1))
  expect_true(all(shows))
  expect_false(isTRUE(m$x$opts$legend$show))   # labels replace the legend
})

test_that("line_styles set the dash pattern per series, matched by name", {
  skip_if_not_installed("echarts4r")
  m <- nt_chart(race_df(), "year", "rate", group = "race", type = "line",
                line_styles = c(Black = "solid", Latine = "dotted", White = "dashed"))
  byname <- vapply(m$x$opts$series, function(s) s$lineStyle$type %||% NA_character_,
                   character(1))
  names(byname) <- vapply(m$x$opts$series, function(s) s$name, character(1))
  expect_identical(byname[["Black"]], "solid")
  expect_identical(byname[["Latine"]], "dotted")
  expect_identical(byname[["White"]], "dashed")
})

test_that("a named palette colors series by group name", {
  skip_if_not_installed("echarts4r")
  m <- nt_chart(race_df(), "year", "rate", group = "race", type = "line",
                palette = c(Black = "#111111", Latine = "#222222", White = "#cccccc"))
  cols <- vapply(m$x$opts$series, function(s) s$lineStyle$color %||% NA_character_,
                 character(1))
  names(cols) <- vapply(m$x$opts$series, function(s) s$name, character(1))
  expect_identical(cols[["White"]], "#cccccc")
})

test_that("source adds a lower-right attribution title", {
  skip_if_not_installed("echarts4r")
  m <- nt_chart(trend_df(), "month", "filings", source = "src · credit")
  titles <- m$x$opts$title
  texts <- vapply(titles, function(t) t$text %||% "", character(1))
  expect_true("src · credit" %in% texts)
})

test_that("value_fmt = 'multiple' formats values with a multiplication sign", {
  skip_if_not_installed("echarts4r")
  js <- neighborhood:::.nt_js_number_expr("multiple", "v")
  expect_match(js, "toFixed", fixed = TRUE)
  expect_match(js, "u00d7", fixed = TRUE)   # the × glyph
})

test_that("highlight_last accents the leading series on a grouped chart", {
  skip_if_not_installed("echarts4r")
  d <- data.frame(
    year = rep(2016:2020, 2),
    race = rep(c("Black", "White"), each = 5),
    rate = c(10, 12, 14, 16, 30,    9, 9, 9, 9, 12)   # Black leads at the end
  )
  m <- nt_chart(d, "year", "rate", group = "race", type = "line",
                highlight_last = TRUE)
  mp <- vapply(m$x$opts$series, function(s) !is.null(s$markPoint), logical(1))
  names(mp) <- vapply(m$x$opts$series, function(s) s$name, character(1))
  expect_true(mp[["Black"]])
  expect_false(mp[["White"]])
})

test_that("nt_spark returns an echarts htmlwidget", {
  skip_if_not_installed("echarts4r")
  s <- nt_spark(c(1, 3, 2, 5, 4), type = "bar")
  expect_s3_class(s, "echarts4r")
  expect_s3_class(s, "htmlwidget")
})
