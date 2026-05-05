target_cols <- c(
  "GEOID", "NAME", "totraceE", "WhiteE", "BlackE", "AsianE", "LatineE",
  "pWhite", "pAsian", "pBlack", "pLatine", "pOther",
  "NeighType", "nt_conc", "state", "year"
)

test_that("us_nt_tracts2024 has expected schema and rows", {
  expect_true(nrow(us_nt_tracts2024) > 0)
  expect_identical(names(us_nt_tracts2024), target_cols)
  expect_s3_class(us_nt_tracts2024$nt_conc, "factor")
  expect_true(all(us_nt_tracts2024$year == 2024))
  expect_false(any(is.na(us_nt_tracts2024$nt_conc)))
})
