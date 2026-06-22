# Regression tests locking in the second-pass deep-review fixes.

# ---- honest bars + ranged whiskers ---------------------------------------

test_that("nt_bar / nt_stacked_bar value axis starts at zero", {
  skip_if_not_installed("echarts4r")
  b <- nt_bar(data.frame(c = c("a", "b", "c"), v = c(102, 100, 101)), "c", "v")
  expect_false(isTRUE(b$x$opts$xAxis[[1]]$scale))     # not free-scaled away from 0
  expect_equal(b$x$opts$xAxis[[1]]$min, 0)
  bv <- nt_bar(data.frame(c = c("a", "b"), v = c(5, 9)), "c", "v",
               orientation = "vertical")
  expect_equal(bv$x$opts$yAxis[[1]]$min, 0)
  s <- nt_stacked_bar(data.frame(x = rep(c("A", "B"), each = 2),
                                 g = rep(c("o", "r"), 2), v = c(2, 3, 4, 1)),
                      "x", "v", group = "g")
  expect_equal(s$x$opts$xAxis[[1]]$min, 0)
  expect_equal(s$x$opts$xAxis[[1]]$max, 1)
})

test_that("dot/whisker charts keep scale=TRUE (dots need not touch zero)", {
  skip_if_not_installed("echarts4r")
  l <- nt_lollipop(data.frame(c = c("a", "b"), v = c(100, 102)), "c", "v")
  expect_true(isTRUE(l$x$opts$xAxis[[1]]$scale))
})

test_that("nt_range extends the value axis to contain the whiskers", {
  skip_if_not_installed("echarts4r")
  d <- data.frame(g = c("a", "b", "c"), v = c(50, 51, 52), moe = c(2, 40, 3))
  e <- nt_range(d, "g", value = "v", error = "moe")
  expect_lte(e$x$opts$xAxis[[1]]$min, min(d$v - d$moe))
  expect_gte(e$x$opts$xAxis[[1]]$max, max(d$v + d$moe))
})

# ---- composition palette depth -------------------------------------------

test_that("composition charts give 6-7 categories distinct colors", {
  skip_if_not_installed("echarts4r")
  s6 <- nt_stacked_bar(data.frame(p = rep("A", 6), grp = letters[1:6], v = 1:6),
                       "p", "v", group = "grp")
  expect_length(unique(unlist(s6$x$opts$color)), 6L)
  w7 <- nt_waffle(stats::setNames(1:7, LETTERS[1:7]))
  expect_length(unique(unlist(w7$x$opts$color)), 7L)
})

# ---- cross-chart API consistency -----------------------------------------

test_that("value_fmt whitelists are identical across the chart family", {
  want <- c("comma", "percent", "currency", "multiple", "none")
  for (f in list(nt_chart, nt_bar, nt_lollipop, nt_dumbbell, nt_range,
                 nt_slope, nt_scatter, nt_stacked_bar)) {
    expect_identical(eval(formals(f)$value_fmt), want)
  }
})

test_that("the color argument is named `palette` across the suite", {
  for (f in list(nt_bar, nt_lollipop, nt_dumbbell, nt_range, nt_slope,
                 nt_scatter, nt_stacked_bar, nt_waffle)) {
    expect_true("palette" %in% names(formals(f)))
  }
})

test_that("renamed args keep deprecated aliases that warn", {
  skip_if_not_installed("echarts4r")
  d <- data.frame(c = c("A", "B"), lo = c(1, 2), hi = c(3, 4))
  expect_warning(nt_dumbbell(d, "c", "lo", "hi", colors = c("#111111", "#222222")),
                 "deprecated")
  expect_warning(nt_waffle(c(A = 1, B = 1), n = 20), "deprecated")
  expect_true("squares" %in% names(formals(nt_waffle)))
  expect_true("value_labels" %in% names(formals(nt_stacked_bar)))
})

test_that("grouped nt_bar legend can be turned off", {
  skip_if_not_installed("echarts4r")
  g <- expand.grid(c = c("A", "B"), r = c("x", "y"), stringsAsFactors = FALSE)
  g$v <- 1:4
  expect_false(isTRUE(nt_bar(g, "c", "v", group = "r", legend = FALSE)$x$opts$legend$show))
  expect_true(isTRUE(nt_bar(g, "c", "v", group = "r")$x$opts$legend$show))
})

# ---- nt_chart stack + currency -------------------------------------------

test_that("nt_chart threads stack into the area series", {
  skip_if_not_installed("echarts4r")
  d <- data.frame(yr = rep(2019:2021, 2), g = rep(c("a", "b"), each = 3), v = 1:6)
  e <- nt_chart(d, "yr", "v", group = "g", type = "area", stack = "g")
  expect_identical(e$x$opts$series[[1]]$stack, "g")
})

test_that("currency formatter puts the minus sign before the dollar", {
  expr <- neighborhood:::.nt_js_number_expr("currency", "v")
  expect_match(expr, "Math.abs", fixed = TRUE)
})

# ---- nt_scatter axis types -----------------------------------------------

test_that("nt_scatter uses a time axis for Date x and rejects non-numeric x", {
  skip_if_not_installed("echarts4r")
  sd <- data.frame(d = as.Date("2020-01-01") + 0:9, y = 1:10)
  expect_identical(nt_scatter(sd, "d", "y")$x$opts$xAxis[[1]]$type, "time")
  expect_error(nt_scatter(data.frame(x = letters[1:3], y = 1:3), "x", "y"),
               "numeric or Date")
})

# ---- ggplot kit ----------------------------------------------------------

test_that("qualitative ggplot scales set the brand NA color", {
  skip_if_not_installed("ggplot2")
  expect_identical(scale_fill_ern()$na.value, neighborhood:::.nt_na_color)
  expect_identical(scale_color_ern()$na.value, neighborhood:::.nt_na_color)
})

test_that("theme_ern legend= controls legend.position", {
  skip_if_not_installed("ggplot2")
  expect_identical(theme_ern(legend = "none")$legend.position, "none")
  expect_identical(theme_ern()$legend.position, "bottom")
})

# ---- map degenerate-data guards ------------------------------------------

test_that("low-variance numeric choropleth still derives distinct breaks", {
  d <- data.frame(GEOID = as.character(1:20), v = seq(0.010, 0.014, length.out = 20))
  fs <- neighborhood:::.nt_fill_spec(d, "v", type = "step")
  expect_length(fs$breaks, 3L)
  expect_length(unique(fs$breaks), 3L)
})

test_that("all-NA numeric column errors clearly", {
  expect_error(neighborhood:::.nt_fill_spec(data.frame(GEOID = "1", v = NA_real_), "v"),
               "no non-missing")
})

test_that("all-NA categorical column errors clearly", {
  d <- data.frame(GEOID = c("1", "2"), g = c(NA_character_, NA_character_))
  expect_error(neighborhood:::.nt_fill_spec(d, "g", type = "categorical"),
               "no non-NA categories")
})

test_that("unsorted user breaks error clearly", {
  d <- data.frame(GEOID = as.character(1:20), v = seq(0, 1, length.out = 20))
  expect_error(
    neighborhood:::.nt_fill_spec(d, "v", type = "step", breaks = c(0.6, 0.2, 0.4)),
    "strictly increasing")
})
