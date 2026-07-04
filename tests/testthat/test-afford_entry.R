# afford_entry() internals and guards, offline. The full pipeline (blocks,
# ZCTAs, ZORI, ACS) is exercised live by the King County build in
# dev/report_style_maps.Rmd.

test_that(".afi_tidy_zori tidies to one row per zip for one month", {
  z <- data.frame(RegionID = 1:3, SizeRank = 1:3, RegionName = c(2134, 98101, 98199),
                  RegionType = "zip", StateName = c("MA", "WA", "WA"),
                  State = c("MA", "WA", "WA"), City = "x", Metro = "m",
                  CountyName = c("Suffolk County", "King County", "King County"),
                  `2026-04-30` = c(2900, 2100, NA), `2026-05-31` = c(3000, 2200, NA),
                  check.names = FALSE)
  out <- .afi_tidy_zori(z)                       # default = latest month
  expect_identical(out$zip, c("02134", "98101"))  # zero-padded, NA row dropped
  expect_equal(out$zori, c(3000, 2200))
  expect_identical(unique(out$month), "2026-05-31")
  out4 <- .afi_tidy_zori(z, month = "2026-04-30")
  expect_equal(out4$zori, c(2900, 2100))
  expect_error(.afi_tidy_zori(z, month = "2020-01-31"), "not in the ZORI file")
  expect_error(.afi_tidy_zori(data.frame(RegionName = 1)), "month columns")
})

test_that(".afi_tract_premium HU-weights and falls back on thin coverage", {
  w <- data.frame(
    zcta = c("98101", "98102", "98102", "98999"),
    tract_geoid = c("t1", "t1", "t2", "t3"),
    frac_hu = c(0.5, 0.5, 1, 1))
  zp <- data.frame(zip = c("98101", "98102"), premium = c(1.2, 1.4))
  tr <- .afi_tract_premium(w, zp, cover_min = 0.25)
  expect_equal(tr$premium[tr$tract_geoid == "t1"], 1.3)   # 0.5*1.2 + 0.5*1.4
  expect_equal(tr$premium[tr$tract_geoid == "t2"], 1.4)
  expect_false(any(tr$premium_fallback[tr$tract_geoid %in% c("t1", "t2")]))
  # t3's only zip has no premium -> coverage 0 -> fallback to median (1.3)
  expect_true(tr$premium_fallback[tr$tract_geoid == "t3"])
  expect_equal(tr$premium[tr$tract_geoid == "t3"], 1.3)
})

test_that("afford_entry validates its inputs", {
  x <- data.frame(GEOID = "t", county = "53033", year = 2024,
                  ami_tier = "VLI", income_cutoff = 75000, total = 100,
                  tenure = "rent")
  z <- data.frame(zip = "98101", zori = 2200)
  expect_error(afford_entry(x[, -1], z), "missing")
  expect_error(afford_entry(x, data.frame(a = 1)), "afford_zori")
  x2 <- rbind(x, transform(x, county = "06073"))
  expect_error(afford_entry(x2, z), "multiple states")
  expect_error(afford_entry(x, z, weights = data.frame(a = 1)),
               "nt_zcta_weights")
})