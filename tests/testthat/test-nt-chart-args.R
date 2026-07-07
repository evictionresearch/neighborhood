# Object-shape tests for the second round of nt_chart() levers (digits,
# tooltip_count, tooltip_trigger, isolate, bar_labels, flip, legend_pos,
# legend_nudge). Same approach as test-nt-chart.R: inspect the echarts4r
# option payload, never render.

cat_df <- function() {
  data.frame(cat = c("a", "b", "c"), val = c(10, 20, 30), n = c(100, 200, 300))
}

two_series_df <- function() {
  data.frame(x = rep(1:3, 2), y = 1:6, g = rep(c("A", "B"), each = 3))
}

axis_type <- function(ax) if (!is.null(ax$type)) ax$type else ax[[1]]$type

test_that("flip moves the category axis to y", {
  skip_if_not_installed("echarts4r")
  flat <- nt_chart(cat_df(), "cat", "val", type = "bar")
  flip <- nt_chart(cat_df(), "cat", "val", type = "bar", flip = TRUE)
  expect_identical(axis_type(flat$x$opts$xAxis), "category")
  expect_identical(axis_type(flip$x$opts$yAxis), "category")
})

test_that("bar_labels turns on in-bar series labels (bars only)", {
  skip_if_not_installed("echarts4r")
  off <- nt_chart(cat_df(), "cat", "val", type = "bar")
  on  <- nt_chart(cat_df(), "cat", "val", type = "bar", bar_labels = TRUE)
  expect_false(isTRUE(off$x$opts$series[[1]]$label$show))
  expect_true(isTRUE(on$x$opts$series[[1]]$label$show))
})

test_that("legend_pos = 'bottom' anchors the legend below, centered", {
  skip_if_not_installed("echarts4r")
  top <- nt_chart(cat_df(), "cat", "val", type = "bar", legend = TRUE)
  bot <- nt_chart(cat_df(), "cat", "val", type = "bar", legend = TRUE,
                  legend_pos = "bottom")
  expect_null(top$x$opts$legend$bottom)
  expect_true(is.numeric(bot$x$opts$legend$bottom))
  expect_identical(bot$x$opts$legend$left, "center")
})

test_that("legend_nudge shifts the bottom legend by the given pixels", {
  skip_if_not_installed("echarts4r")
  b0 <- nt_chart(cat_df(), "cat", "val", type = "bar", legend = TRUE,
                 legend_pos = "bottom")$x$opts$legend$bottom
  b1 <- nt_chart(cat_df(), "cat", "val", type = "bar", legend = TRUE,
                 legend_pos = "bottom", legend_nudge = -6)$x$opts$legend$bottom
  expect_equal(abs(b1 - b0), 6)
  expect_error(
    nt_chart(cat_df(), "cat", "val", legend_nudge = c(1, 2, 3)),
    "legend_nudge"
  )
})

test_that("tooltip_trigger switches between axis and item", {
  skip_if_not_installed("echarts4r")
  ax <- nt_chart(cat_df(), "cat", "val")
  it <- nt_chart(cat_df(), "cat", "val", tooltip_trigger = "item")
  expect_identical(ax$x$opts$tooltip$trigger, "axis")
  expect_identical(it$x$opts$tooltip$trigger, "item")
})

test_that("digits and tooltip_count reach the tooltip formatter", {
  skip_if_not_installed("echarts4r")
  m <- nt_chart(cat_df(), "cat", "val", value_fmt = "percent", digits = 4,
                tooltip_count = "n", tooltip_count_suffix = " households")
  f <- as.character(m$x$opts$tooltip$formatter)
  expect_true(grepl("households", f, fixed = TRUE))
  expect_true(grepl("4", f, fixed = TRUE))
  expect_error(
    nt_chart(cat_df(), "cat", "val", tooltip_count = "nope"),
    "not found"
  )
})

test_that("isolate installs the double-click render hook", {
  skip_if_not_installed("echarts4r")
  m <- nt_chart(two_series_df(), "x", "y", group = "g", type = "line",
                isolate = TRUE)
  hooks <- paste(unlist(m$jsHooks$render), collapse = "\n")
  expect_true(grepl("dblclick", hooks, ignore.case = TRUE))
})
