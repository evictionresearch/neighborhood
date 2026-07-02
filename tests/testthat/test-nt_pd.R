# Tests for the partial-dependence plotter. nt_pd_plot() is pure ggplot2 +
# patchwork (no Java / bartMachine), so it is fully testable here; nt_pd_bart()
# can only be smoke-tested for its guard rails without a fitted model.

test_that("nt_pd_plot stacks a PD curve over a frequency panel", {
  skip_if_not_installed("patchwork")
  pd <- data.frame(x = 1:10, mean = (1:10) / 10,
                   lower = (1:10) / 10 - 0.1, upper = (1:10) / 10 + 0.1)
  p <- nt_pd_plot(pd, observed = rep(1:10, times = 3:12), var_label = "rent")
  expect_s3_class(p, "patchwork")
})

test_that("nt_pd_plot without `observed` returns a single ggplot", {
  pd <- data.frame(x = 1:10, mean = (1:10) / 10,
                   lower = (1:10) / 10 - 0.1, upper = (1:10) / 10 + 0.1)
  expect_s3_class(nt_pd_plot(pd), "ggplot")
})

test_that("nt_pd_plot validates the PD table", {
  expect_error(nt_pd_plot(data.frame(a = 1:3)), "data frame with columns")
  expect_error(
    nt_pd_plot(data.frame(x = 1, mean = 1, lower = 0, upper = 2)),
    "two evaluation points")
})

test_that("the probit label flows through to the y-axis", {
  pd <- data.frame(x = 1:5, mean = 1:5, lower = 0:4, upper = 2:6)
  p <- nt_pd_plot(pd, effect = "probit")
  expect_identical(p$labels$y, "Partial effect (probit)")
})

test_that("nt_pd_bart fails fast without a fitted bartMachine model", {
  # Errors whether or not bartMachine/Java are installed (missing-package guard
  # or the class check), and never silently proceeds.
  expect_error(nt_pd_bart(list(), "x"))
})
