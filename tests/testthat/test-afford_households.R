# afford_households() internals -- the open-top snap and the bracket core,
# offline. The live pull rides the same tidycensus path afford_index() uses.

test_that(".afi_snap_cuts snaps only cutoffs inside the open top and flags them", {
  cuts <- data.frame(county = c("06081", "06095"),
                     cut_ELI = c(58750, 39200),
                     cut_LI  = c(156650, 104500))
  s <- neighborhood:::.afi_snap_cuts(cuts, c("ELI", "LI"), 150000)
  expect_equal(s$cut_LI_eff, c(150000, 104500))
  expect_identical(s$cut_LI_snapped, c(TRUE, FALSE))
  # lines inside closed brackets are untouched
  expect_equal(s$cut_ELI_eff, cuts$cut_ELI)
  expect_identical(s$cut_ELI_snapped, c(FALSE, FALSE))
  # no open bracket (top_floor -Inf) is a no-op
  s2 <- neighborhood:::.afi_snap_cuts(cuts, "LI", -Inf)
  expect_equal(s2$cut_LI_eff, cuts$cut_LI)
})

test_that(".afi_hh_from_brackets interpolates per tract with MOEs", {
  br <- data.frame(
    GEOID = rep(c("t1", "t2"), each = 2), county = "06081",
    tenure_grp = "renter",
    lo = rep(c(0, 100), 2), hi = rep(c(100, Inf), 2),
    n = c(10, 10, 40, 0), m = c(3, 4, 6, 8),
    stringsAsFactors = FALSE)
  cuts <- data.frame(county = "06081", cut_ELI = 50, cut_ELI_eff = 50,
                     cut_ELI_snapped = FALSE)
  out <- neighborhood:::.afi_hh_from_brackets(br, cuts, "ELI")
  t1 <- out[out$GEOID == "t1", ]
  expect_equal(t1$hh, 5)          # half the first bracket
  expect_equal(t1$hh_moe, 1.5)    # 3 * 0.5, RSS of one term
  expect_equal(t1$hh_total, 20)
  expect_equal(out$hh[out$GEOID == "t2"], 20)
  expect_identical(as.character(unique(out$ami_tier)), "ELI")
  expect_identical(unique(out$cutoff_snapped), FALSE)
})
