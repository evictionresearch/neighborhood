# afford_typology() + afford_mismatch() on toy frames -- offline. Five tracts:
# a (many households, many openings), b (many, few), c (few, many), d (few,
# few), e (under the renter floor, unclassified).

toy_idx <- data.frame(
  GEOID = c("a", "b", "c", "d", "e"), county = "06081", tenure = "rent",
  ami_tier = "ELI", accessible = c(50, 40, 30, 20, 10),
  available_turnover = c(10, 2, 8, 6, 3), stringsAsFactors = FALSE)
toy_hh <- data.frame(
  GEOID = c("a", "b", "c", "d", "e"), tenure_grp = "renter", ami_tier = "ELI",
  hh = c(100, 80, 10, 5, 50), hh_moe = 5,
  hh_total = c(200, 160, 100, 60, 40), stringsAsFactors = FALSE)

test_that("afford_typology assigns the four cells at tier medians", {
  ty <- suppressMessages(afford_typology(toy_idx, toy_hh, min_renters = 50))
  # live set is a-d (e has 40 renter households): hh median 45, openings 7
  expect_identical(as.character(ty$cell),
                   c("stable", "lockin", "latent", "exclusion", NA))
  expect_equal(unique(ty$typology_med_hh[1:4]), 45)
  expect_equal(unique(ty$typology_med_open[1:4]), 7)
  expect_true(is.na(ty$cell[ty$GEOID == "e"]))
  # the explicit openings choice is honored
  ty2 <- afford_typology(toy_idx, toy_hh, openings = "accessible",
                         min_renters = 50)
  expect_equal(ty2$openings, toy_idx$accessible)
})

test_that("afford_typology guards its inputs", {
  expect_error(afford_typology(toy_idx[, 1:2], toy_hh), "afford_index")
  expect_error(afford_typology(toy_idx, toy_hh[, 1:3]), "afford_households")
  expect_error(afford_typology(toy_idx, toy_hh, openings = "entry_turnover"),
               "available_entry_turnover")
})

test_that("afford_mismatch computes D and the three exposures", {
  ty <- suppressMessages(afford_typology(toy_idx, toy_hh, min_renters = 50))
  ctx <- data.frame(GEOID = c("a", "b", "c", "d", "e"),
                    pov = c(0.30, 0.20, 0.05, 0.10, 0.50))
  mm <- afford_mismatch(ty, context = ctx)
  expect_equal(nrow(mm), 1)
  expect_equal(mm$tier_households, sum(toy_hh$hh))
  expect_equal(mm$openings_total, sum(toy_idx$available_turnover))
  hh_sh <- toy_hh$hh / sum(toy_hh$hh)
  op_sh <- toy_idx$available_turnover / sum(toy_idx$available_turnover)
  expect_equal(mm$dissimilarity, 0.5 * sum(abs(hh_sh - op_sh)))
  expect_equal(mm$pov_hh,   weighted.mean(ctx$pov, toy_hh$hh))
  expect_equal(mm$pov_open, weighted.mean(ctx$pov, toy_idx$available_turnover))
  expect_equal(mm$pov_all,  weighted.mean(ctx$pov, toy_hh$hh_total))
  # the displacement reading this exists for: observed vs market exposure
  expect_true(mm$pov_open != mm$pov_hh)
})

test_that(".afi_dissimilarity handles NA and empty distributions", {
  expect_equal(neighborhood:::.afi_dissimilarity(c(1, 1), c(1, 1)), 0)
  expect_equal(neighborhood:::.afi_dissimilarity(c(1, 0), c(0, 1)), 1)
  expect_equal(neighborhood:::.afi_dissimilarity(c(1, NA), c(1, 1)), 0.5)
  expect_true(is.na(neighborhood:::.afi_dissimilarity(c(0, 0), c(1, 1))))
})
