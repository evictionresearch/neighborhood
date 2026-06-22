# Object-shape tests for the ranked categorical charts (nt_bar, nt_lollipop,
# nt_dumbbell). They inspect the echarts4r option payload, so they run on CI.

counties <- function() {
  data.frame(county = c("Hennepin", "Ramsey", "Anoka", "Dakota", "Olmsted"),
             rate   = c(42.3, 38.1, 19.4, 22.7, 15.2),
             stringsAsFactors = FALSE)
}

test_that("nt_bar returns an echarts htmlwidget", {
  skip_if_not_installed("echarts4r")
  e <- nt_bar(counties(), "county", "rate")
  expect_s3_class(e, "echarts4r")
  expect_s3_class(e, "htmlwidget")
  expect_length(e$x$opts$series, 1L)
})

test_that("horizontal nt_bar flips: categories on y, value axis on x with a formatter", {
  skip_if_not_installed("echarts4r")
  e <- nt_bar(counties(), "county", "rate")            # horizontal is the default
  expect_identical(e$x$opts$yAxis[[1]]$type, "category")
  expect_false(is.null(e$x$opts$xAxis[[1]]$axisLabel$formatter))
})

test_that("ranked nt_bar accents the leader (the max) by default", {
  skip_if_not_installed("echarts4r")
  e <- nt_bar(counties(), "county", "rate", sort = "desc")
  # first data element corresponds to the first (largest) row after sorting
  expect_identical(e$x$opts$series[[1]]$data[[1]]$itemStyle$color,
                   neighborhood:::.ern_brand$accent)
  expect_identical(e$x$opts$series[[1]]$data[[2]]$itemStyle$color,
                   neighborhood:::.ern_brand$navy)
})

test_that("nt_bar highlight by name accents named categories", {
  skip_if_not_installed("echarts4r")
  e <- nt_bar(counties(), "county", "rate", sort = "none", highlight = "Anoka")
  cols <- vapply(e$x$opts$series[[1]]$data, function(d) d$itemStyle$color, character(1))
  # Anoka is row 3 in the unsorted data
  expect_identical(cols[[3]], neighborhood:::.ern_brand$accent)
  expect_true(all(cols[-3] == neighborhood:::.ern_brand$navy))
})

test_that("nt_bar top-n keeps only n categories", {
  skip_if_not_installed("echarts4r")
  e <- nt_bar(counties(), "county", "rate", n = 3)
  expect_length(e$x$opts$series[[1]]$data, 3L)
})

test_that("diverging nt_bar colors by sign and draws a reference line", {
  skip_if_not_installed("echarts4r")
  d <- data.frame(g = letters[1:4], chg = c(-3, 2, -1, 5))
  e <- nt_bar(d, "g", "chg", orientation = "vertical", sort = "none",
              diverging = TRUE, baseline = 0)
  cols <- vapply(e$x$opts$series[[1]]$data, function(z) z$itemStyle$color, character(1))
  expect_identical(cols[[1]], neighborhood:::.ern_brand$navy_soft)  # negative
  expect_identical(cols[[2]], neighborhood:::.ern_brand$accent)     # positive
  expect_false(is.null(e$x$opts$series[[1]]$markLine))
})

test_that("grouped nt_bar yields one series per group and shows the legend", {
  skip_if_not_installed("echarts4r")
  g <- expand.grid(county = c("A", "B"), race = c("Black", "White"),
                   stringsAsFactors = FALSE)
  g$rate <- c(20, 25, 10, 12)
  e <- nt_bar(g, "county", "rate", group = "race")
  expect_length(e$x$opts$series, 2L)
  expect_true(isTRUE(e$x$opts$legend$show))
})

test_that("nt_bar yaxis='hover' attaches an x-axis reveal hook (horizontal)", {
  skip_if_not_installed("echarts4r")
  e <- nt_bar(counties(), "county", "rate", yaxis = "hover")
  expect_false(is.null(e$jsHooks$render))
  expect_match(paste(unlist(e$jsHooks$render), collapse = " "), "xAxis")
})

test_that("nt_bar validates inputs", {
  skip_if_not_installed("echarts4r")
  expect_error(nt_bar(counties(), "nope", "rate"), "not found")
  expect_error(nt_bar(list(a = 1), "a", "b"), "data frame")
})

test_that("nt_lollipop draws a stem + a dot (and just a dot when stem=FALSE)", {
  skip_if_not_installed("echarts4r")
  disp <- data.frame(race = c("Black", "Latine", "White"), index = c(3.1, 1.8, 1.0))
  e <- nt_lollipop(disp, "race", "index", value_fmt = "multiple",
                   highlight = "Black", baseline = 1)
  expect_length(e$x$opts$series, 2L)                 # stem (bar) + dot (scatter)
  expect_false(is.null(e$x$opts$series[[2]]$markLine) ||
               is.null(e$x$opts$series[[1]]$markLine))
  e2 <- nt_lollipop(disp, "race", "index", stem = FALSE)
  expect_length(e2$x$opts$series, 1L)
})

test_that("nt_dumbbell makes two ends, a connector per row, and labelled legend", {
  skip_if_not_installed("echarts4r")
  gap <- data.frame(county = c("Hennepin", "Ramsey", "Dakota"),
                    white = c(12, 14, 9), black = c(41, 38, 22))
  e <- nt_dumbbell(gap, "county", low = "white", high = "black",
                   low_label = "White", high_label = "Black")
  expect_length(e$x$opts$series, 2L)
  expect_length(e$x$opts$series[[1]]$markLine$data, nrow(gap))   # one segment/row
  expect_identical(e$x$opts$legend$data, list("White", "Black"))
})

test_that("value labels + tooltips read the value index per orientation (not the category)", {
  skip_if_not_installed("echarts4r")
  # echarts serializes a categorical datum as [category, value] vertically and
  # [value, category] after a horizontal flip, so the formatter index must flip.
  ev <- nt_bar(counties(), "county", "rate", orientation = "vertical")
  eh <- nt_bar(counties(), "county", "rate", orientation = "horizontal")
  expect_match(as.character(ev$x$opts$series[[1]]$label$formatter), "raw[1]", fixed = TRUE)
  expect_match(as.character(eh$x$opts$series[[1]]$label$formatter), "raw[0]", fixed = TRUE)
  expect_match(as.character(ev$x$opts$tooltip$formatter), "raw[1]", fixed = TRUE)
  expect_match(as.character(eh$x$opts$tooltip$formatter), "raw[0]", fixed = TRUE)
})

test_that("ranked charts reject duplicate categories with a clear message", {
  skip_if_not_installed("echarts4r")
  d <- data.frame(c = c("A", "A", "B"), v = c(1, 2, 3))
  expect_error(nt_bar(d, "c", "v"), "duplicate categories")
  expect_error(nt_lollipop(d, "c", "v"), "duplicate categories")
  expect_error(nt_dumbbell(data.frame(c = c("A", "A"), lo = 1:2, hi = 3:4),
                           "c", "lo", "hi"), "duplicate categories")
})

test_that("nt_range draws estimate dots with one whisker per category and accents the leader", {
  skip_if_not_installed("echarts4r")
  inc <- data.frame(race = c("White", "Black", "Latine"),
                    med = c(78000, 51000, 56000), moe = c(1800, 3200, 2900))
  e <- nt_range(inc, "race", value = "med", error = "moe", value_fmt = "currency")
  expect_s3_class(e, "echarts4r")
  expect_length(e$x$opts$series, 1L)
  expect_length(e$x$opts$series[[1]]$markLine$data, 3L)   # one whisker per row
  # default sort = desc -> White (max) is row 1 -> accented
  expect_identical(e$x$opts$series[[1]]$data[[1]]$itemStyle$color,
                   neighborhood:::.ern_brand$accent)
})

test_that("nt_range accepts low/high bounds and validates the error spec", {
  skip_if_not_installed("echarts4r")
  d <- data.frame(g = c("a", "b"), v = c(5, 9), lo = c(4, 7), hi = c(6, 11))
  e <- nt_range(d, "g", value = "v", low = "lo", high = "hi")
  expect_length(e$x$opts$series[[1]]$markLine$data, 2L)
  expect_error(nt_range(d, "g", value = "v"), "either")
  expect_error(nt_range(d, "g", value = "v", error = "lo", low = "lo", high = "hi"),
               "not both")
})

test_that("nt_dumbbell sorts by gap so the widest disparity comes first", {
  skip_if_not_installed("echarts4r")
  gap <- data.frame(county = c("Small", "Big"), white = c(10, 10), black = c(12, 40))
  e <- nt_dumbbell(gap, "county", low = "white", high = "black", sort = "gap",
                   orientation = "vertical")
  # vertical keeps categories on x in display order; "Big" (gap 30) comes first
  expect_identical(levels(e$x$opts$xAxis[[1]]$data)[1], "Big")
})
