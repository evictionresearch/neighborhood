# Offline tests for nt_apartmentlist_rents(): a temp folder of fake
# vintage-named CSVs and refresh = "never", so no network is touched.

al_dir <- function(vintages = character()) {
  d <- tempfile("al")
  dir.create(d)
  for (v in vintages) {
    utils::write.csv(
      data.frame(location_name = "Metroville", location_fips_code = "33460",
                 `2024_01` = 1500, check.names = FALSE),
      file.path(d, sprintf("Apartment_List_Rent_Estimates_%s.csv", v)),
      row.names = FALSE
    )
  }
  d
}

test_that("an empty folder with refresh = 'never' errors usefully", {
  d <- al_dir()
  on.exit(unlink(d, recursive = TRUE))
  expect_error(nt_apartmentlist_rents(d, refresh = "never"),
               "no Apartment List file")
})

test_that("the newest local vintage is read, with the vintage attribute", {
  d <- al_dir(c("2025_01", "2025_02"))
  on.exit(unlink(d, recursive = TRUE))
  out <- nt_apartmentlist_rents(d, refresh = "never", quiet = TRUE)
  expect_s3_class(out, "tbl_df")
  expect_identical(attr(out, "vintage"), "2025_02")
  # month columns keep their raw names (check.names = FALSE on read)
  expect_true("2024_01" %in% names(out))
  expect_identical(out$location_fips_code, "33460")
})

test_that("non-release files in the folder are ignored", {
  d <- al_dir("2025_03")
  on.exit(unlink(d, recursive = TRUE))
  file.create(file.path(d, "Apartment_List_Rent_Estimates_Summary.txt"))
  file.create(file.path(d, "notes.csv"))
  out <- nt_apartmentlist_rents(d, refresh = "never", quiet = TRUE)
  expect_identical(attr(out, "vintage"), "2025_03")
})
