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
  expect_equal(neighborhood:::.afi_mortgage_constant(0.06, 30), 0.071935, tolerance = 1e-4)
  # zero-interest fallback is 1/term
  expect_equal(neighborhood:::.afi_mortgage_constant(0, 30), 1/30)
})

test_that(".afi_price_income_factor composes mortgage + taxes over the burden", {
  f <- neighborhood:::.afi_price_income_factor(0.06, 30, down_pct = 0.20,
                                               tax_ins_rate = 0.0125, burden = 0.30)
  expect_equal(f, ((0.80 * 0.071935) + 0.0125) / 0.30, tolerance = 1e-4)
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

test_that("hud_acs source fails fast (not yet implemented, no API call)", {
  expect_error(ami_cutoffs("06", "075", 2024, ami_source = "hud_acs"),
               "not yet implemented")
})
