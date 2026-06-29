# Unit tests for the affordability-index helpers that need no Census API.

test_that(".afi_interp_le interpolates and handles open-top brackets", {
  lo <- c(0, 100, 200); hi <- c(100, 200, Inf); n <- c(10, 10, 10)
  # cutoff mid-second-bracket: 10 + 5 + 0 = 15
  expect_equal(neighborhood:::.afi_interp_le(lo, hi, n, 150), 15)
  # cutoff on a boundary: first bracket only
  expect_equal(neighborhood:::.afi_interp_le(lo, hi, n, 100), 10)
  # cutoff inside the open-top bracket: partial open-top contributes 0 -> 20
  expect_equal(neighborhood:::.afi_interp_le(lo, hi, n, 250), 20)
  # fully open cutoff counts everything, open-top included
  expect_equal(neighborhood:::.afi_interp_le(lo, hi, n, Inf), 30)
  # below everything
  expect_equal(neighborhood:::.afi_interp_le(lo, hi, n, 0), 0)
})

test_that(".afi_mortgage_constant matches the closed-form annuity", {
  # Correct closed-form annual mortgage constant 12 * (i / (1 - (1 + i)^-n)) for
  # i = 0.06/12, n = 360 is 0.0719461; the prior literal 0.071935 was imprecise
  # and exceeded the 1e-4 relative tolerance under testthat edition 3.
  expect_equal(neighborhood:::.afi_mortgage_constant(0.06, 30), 0.0719461, tolerance = 1e-4)
  # zero-interest fallback is 1/term
  expect_equal(neighborhood:::.afi_mortgage_constant(0, 30), 1/30)
})

test_that(".afi_price_income_factor composes mortgage + taxes over the burden", {
  f <- neighborhood:::.afi_price_income_factor(0.06, 30, down_pct = 0.20,
                                               tax_ins_rate = 0.0125, burden = 0.30)
  expect_equal(f, ((0.80 * 0.0719461) + 0.0125) / 0.30, tolerance = 1e-4)
  # higher rates -> need more income per dollar of price
  expect_gt(neighborhood:::.afi_price_income_factor(0.07),
            neighborhood:::.afi_price_income_factor(0.04))
})

test_that(".afi_parse_label reads ACS bracket labels and rejects subtotals", {
  expect_equal(unname(neighborhood:::.afi_parse_label("Estimate!!Total:!!$500 to $549")), c(500, 549))
  expect_equal(unname(neighborhood:::.afi_parse_label("Estimate!!Total:!!Less than $100")), c(0, 100))
  expect_equal(unname(neighborhood:::.afi_parse_label("Estimate!!Total:!!$3,500 or more")), c(3500, Inf))
  expect_equal(unname(neighborhood:::.afi_parse_label("Estimate!!Total:!!$1,000 to $1,249")), c(1000, 1249))
  # totals / subtotals / non-dollar leaves -> NA (excluded)
  expect_true(all(is.na(neighborhood:::.afi_parse_label("Estimate!!Total:"))))
  expect_true(all(is.na(neighborhood:::.afi_parse_label("Estimate!!Total:!!With cash rent:"))))
  expect_true(all(is.na(neighborhood:::.afi_parse_label("Estimate!!Total:!!No cash rent"))))
})

test_that("HUD family-size factors are the standard table", {
  expect_equal(unname(neighborhood:::.afi_hh_size_factor["4"]), 1.00)
  expect_equal(unname(neighborhood:::.afi_hh_size_factor["1"]), 0.70)
  expect_equal(unname(neighborhood:::.afi_hh_size_factor["8"]), 1.32)
})

test_that("hud_acs errors (no API call) for a year with no verified trend factor", {
  expect_error(ami_cutoffs("06", "075", 1990, ami_source = "hud_acs"),
               "income-trend factor")
})

test_that("ami_source is validated", {
  expect_error(ami_cutoffs("06", "075", 2024, ami_source = "nonsense"))
})

test_that("afford_bands differences cumulative tiers into non-overlapping slices", {
  x <- data.frame(
    GEOID        = rep("06075000100", 4),
    tenure       = rep("rent", 4),
    ami_tier     = factor(c("ELI", "VLI", "LI", "MI"),
                          levels = c("ELI", "VLI", "LI", "MI")),
    accessible   = c(10, 25, 60, 90),       # cumulative
    total        = rep(100, 4),
    reg_hh_tier  = c(100, 200, 320, 400),   # cumulative
    reg_hh_total = rep(1000, 4),
    vacancy_rate = rep(0.05, 4),
    turnover_rate = rep(0.20, 4))
  b <- afford_bands(x)
  expect_equal(b$accessible,  c(10, 15, 35, 30))    # tier minus the tier below
  expect_equal(b$reg_hh_tier, c(100, 100, 120, 80))
  expect_equal(b$supply,      c(.10, .15, .35, .30))
  expect_equal(b$class_prop,  c(.10, .10, .12, .08))
  expect_equal(b$available_vacancy, c(10, 15, 35, 30) * 0.05)
})

test_that("afford_bands rejects non-afford_index input", {
  expect_error(afford_bands(data.frame(a = 1)), "afford_index")
})

# Live end-to-end check (needs a Census API key + network); skipped otherwise.
test_that("afford_index runs end-to-end and is internally consistent (live ACS)", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  testthat::skip_if(!nzchar(Sys.getenv("CENSUS_API_KEY")), "no CENSUS_API_KEY")
  idx <- afford_index("06", "075", 2024, tenure = "rent", ami_source = "acs")
  expect_true(all(c("supply", "ratio", "rate", "vacancy_rate",
                    "available_vacancy", "available_turnover") %in% names(idx)))
  expect_true(all(idx$supply >= 0 & idx$supply <= 1, na.rm = TRUE))
  expect_true(all(idx$available_vacancy <= idx$accessible + 1e-6, na.rm = TRUE))
  # cumulative tiers: accessible is non-decreasing ELI -> MI within a tract
  one <- idx[idx$GEOID == idx$GEOID[1], ]
  one <- one[order(one$ami_tier), ]
  expect_false(is.unsorted(one$accessible))
})
