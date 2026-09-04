# afford_safmr_check() internals and guards, offline. The live SAFMR path
# needs hudr + HUD_API_KEY and is exercised in dev builds.

test_that(".afi_bracket_quantile inverts the distribution and floors the open top", {
  lo <- c(0, 100, 200); hi <- c(100, 200, Inf); n <- c(10, 10, 10)
  expect_equal(neighborhood:::.afi_bracket_quantile(lo, hi, n, 0.5), 150)
  # quantile landing in the open-top bracket returns its floor, never a point
  # inside an invented width
  expect_equal(neighborhood:::.afi_bracket_quantile(lo, hi, n, 0.9), 200)
  expect_true(is.na(neighborhood:::.afi_bracket_quantile(lo, hi, c(0, 0, 0), 0.4)))
  # unsorted input sorts itself
  expect_equal(neighborhood:::.afi_bracket_quantile(rev(lo), rev(hi), rev(n), 0.5), 150)
})

test_that("afford_safmr_check guards its input", {
  expect_error(afford_safmr_check(data.frame(GEOID = "a")), "afford_entry")
  bad_band <- data.frame(GEOID = "a", county = "06081", ami_tier = "ELI",
                         tenure = "rent", entry_premium = 1.2)
  expect_error(afford_safmr_check(bad_band, band = c(2, 1)))
})
