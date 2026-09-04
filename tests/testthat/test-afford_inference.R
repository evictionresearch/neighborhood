# MOE propagation and EB shrinkage internals, plus the verdict fragility flag
# and afford_bands() MOE differencing -- all offline.

test_that(".afi_interp_le_moe RSS-combines included brackets", {
  lo <- c(0, 100, 200); hi <- c(100, 200, Inf); m <- c(3, 4, 12)
  # full first + half of the second: sqrt(3^2 + (4 * 0.5)^2)
  expect_equal(neighborhood:::.afi_interp_le_moe(lo, hi, m, 150), sqrt(13))
  # boundary cutoff: first bracket only
  expect_equal(neighborhood:::.afi_interp_le_moe(lo, hi, m, 100), 3)
  # partial open-top contributes nothing (same rule as .afi_interp_le)
  expect_equal(neighborhood:::.afi_interp_le_moe(lo, hi, m, 250), 5)
  # fully open cutoff includes the open-top in full: sqrt(9 + 16 + 144)
  expect_equal(neighborhood:::.afi_interp_le_moe(lo, hi, m, Inf), 13)
  # NA and negative-sentinel MOEs carry no sampling error
  expect_equal(neighborhood:::.afi_interp_le_moe(lo, hi, c(NA, -555555555, 4), 200), 0)
})

test_that(".afi_eb_shrink pulls noisy rates toward the pool, leaves precise ones", {
  g <- rep("a", 6)
  rate <- c(0.10, 0.20, 0.30, 0.10, 0.20, 0.90)
  se   <- c(0.001, 0.001, 0.001, 0.001, 0.001, 0.50)
  out <- neighborhood:::.afi_eb_shrink(rate, se, g)
  # precise tracts keep their rates
  expect_equal(out[1:5], rate[1:5], tolerance = 1e-2)
  # the noisy outlier is pulled well toward the pool
  expect_lt(out[6], 0.5)
  # a group under min_n is left untouched
  expect_equal(neighborhood:::.afi_eb_shrink(rate[1:3], se[1:3], g[1:3]),
               rate[1:3])
  # NAs pass through untouched
  out2 <- neighborhood:::.afi_eb_shrink(c(rate, NA), c(se, 0.1), rep("a", 7))
  expect_true(is.na(out2[7]))
})

test_that("afford_bands differences MOEs by the disjoint-bracket identity", {
  tb <- data.frame(
    GEOID = "a", tenure = "rent",
    ami_tier = factor(c("ELI", "VLI"), levels = c("ELI", "VLI")),
    accessible = c(10, 30), accessible_moe = c(3, 5),
    total = 100, supply = c(0.1, 0.3), supply_moe = c(0.03, 0.05),
    reg_hh_tier = c(100, 250), reg_hh_total = 1000,
    stringsAsFactors = FALSE)
  b <- afford_bands(tb)
  # band MOE = sqrt(cum_hi^2 - cum_lo^2): sqrt(25 - 9) = 4
  expect_equal(b$accessible_moe[b$ami_tier == "VLI"], 4)
  expect_equal(b$accessible_moe[b$ami_tier == "ELI"], 3)   # lowest tier unchanged
  expect_equal(b$supply_moe[b$ami_tier == "VLI"], 0.04)
})

test_that("afford_verdict flags fragile classifications", {
  toy <- data.frame(
    GEOID = c("a", "b", "c"), tenure = "rent", ami_tier = "VLI",
    supply = c(0.52, 0.10, 0.49), supply_moe = c(0.05, 0.02, 0.10),
    supply_stretch = c(0.90, 0.20, 0.80),
    supply_stretch_moe = c(0.05, 0.02, 0.10),
    stringsAsFactors = FALSE)
  v <- afford_verdict(toy)
  # a: affordable with the 30% line 0.02 over the cut, inside its 0.05 MOE
  # b: not affordable with both lines far from the cut
  # c: roughly affordable one point under the majority rule -- the case the
  #    flag exists for
  expect_identical(v$verdict_fragile, c(TRUE, FALSE, TRUE))
  expect_identical(as.character(v$verdict[3]), "roughly affordable")
  # without MOE columns the flag is absent, not invented
  v2 <- afford_verdict(
    toy[, setdiff(names(toy), c("supply_moe", "supply_stretch_moe"))])
  expect_false("verdict_fragile" %in% names(v2))
})
