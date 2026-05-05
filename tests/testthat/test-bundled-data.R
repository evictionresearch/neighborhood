target_cols <- c(
  "GEOID", "NAME", "totraceE", "WhiteE", "BlackE", "AsianE", "LatineE",
  "pWhite", "pAsian", "pBlack", "pLatine", "pOther",
  "NeighType", "nt_conc", "state", "year"
)

for (ds in c("us_nt_tracts2019", "us_nt_tracts2021", "us_nt_tracts2022")) {
  test_that(paste(ds, "has expected schema and rows"), {
    df <- get(ds)
    expect_true(nrow(df) > 0)
    expect_identical(names(df), target_cols)
    expect_s3_class(df$nt_conc, "factor")
  })
}

test_that("year column matches dataset name", {
  expect_true(all(us_nt_tracts2019$year == 2019))
  expect_true(all(us_nt_tracts2021$year == 2021))
  expect_true(all(us_nt_tracts2022$year == 2022))
})
