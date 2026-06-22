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

test_that("mn_evictions has the documented schema and a sane rate", {
  expect_setequal(
    names(mn_evictions),
    c("geoid", "county", "year", "month", "filings", "renters", "rate",
      "filings_black", "filings_white", "filings_latine", "filings_other",
      "renters_black", "renters_white", "renters_latine", "renters_other")
  )
  expect_equal(length(unique(mn_evictions$geoid)), 87)
  expect_true(max(mn_evictions$year * 100 + mn_evictions$month) == 202603)  # hold-out
  expect_type(mn_evictions$geoid, "character")
  # rate is the raw proportion filings/renters
  ok <- with(mn_evictions, abs(rate - filings / renters) < 1e-6 | renters == 0)
  expect_true(all(ok, na.rm = TRUE))
  # statewide annualized per-1,000 over the last 12 months is ~24
  last12 <- mn_evictions[(mn_evictions$year == 2025 & mn_evictions$month >= 4) |
                           mn_evictions$year == 2026, ]
  denom <- sum(unique(mn_evictions[, c("geoid", "renters")])$renters)
  ann <- 1000 * sum(last12$filings) * 12 / 12 / denom
  expect_true(ann > 20 && ann < 28)
})
