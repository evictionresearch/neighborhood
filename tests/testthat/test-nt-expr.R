# The fill-spec builder turns (data, column, type) into a mapgl paint expression
# plus a matching legend spec. These tests check the structural decisions
# (categorical vs step vs continuous, break handling, legend pairing) without a
# browser.

test_that("auto type picks categorical for factors, step for numerics", {
  d <- nt_test_sf()
  expect_identical(.nt_fill_spec(d, "nt_conc", type = "auto")$type, "categorical")
  expect_identical(.nt_fill_spec(d, "pBlack",  type = "auto")$type, "step")
})

test_that("categorical legend pairs present levels with palette colors", {
  d <- nt_test_sf()
  fs <- .nt_fill_spec(d, "nt_conc")
  expect_identical(fs$legend_type, "categorical")
  expect_length(fs$legend_values, length(fs$legend_colors))
  expect_true("Diverse" %in% fs$legend_values)
})

test_that("step uses ERN labels for a 4-class ramp and computes quantile breaks", {
  d <- nt_test_sf()
  fs <- .nt_fill_spec(d, "pBlack", type = "step")
  expect_identical(fs$legend_values, c("Lower", "Moderate", "High", "Extreme"))
  expect_length(fs$legend_colors, 4L)
  expect_length(fs$breaks, 3L)              # n_classes - 1 interior breaks
})

test_that("explicit floor break is dropped to match the color count", {
  d <- nt_test_sf()
  fs <- .nt_fill_spec(d, "pBlack", type = "step", breaks = c(0, 0.25, 0.5, 0.75))
  expect_identical(fs$breaks, c(0.25, 0.5, 0.75))
})

test_that("wrong number of breaks errors clearly", {
  d <- nt_test_sf()
  expect_error(
    .nt_fill_spec(d, "pBlack", type = "step", breaks = c(0.25, 0.5)),
    "interior break"
  )
})

test_that("continuous type yields a continuous legend", {
  d <- nt_test_sf()
  fs <- .nt_fill_spec(d, "pBlack", type = "continuous", breaks = c(0.25, 0.5, 0.75))
  expect_identical(fs$legend_type, "continuous")
})
