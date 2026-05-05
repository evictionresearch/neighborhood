test_that("nt_pal returns a leaflet color function", {
  df <- head(us_nt_tracts2022, 50)
  pal <- nt_pal(df)
  expect_type(pal, "closure")
  expect_match(pal(df$nt_conc[[1]]), "^#")
})
