# The map functions should accept user-supplied spatial inputs, normalizing
# everything to sf: an sf object, a path to a shapefile/GeoJSON/GeoPackage, a
# bare sfc geometry, etc.

test_that(".nt_as_sf passes through sf unchanged", {
  d <- nt_test_sf()
  expect_identical(.nt_as_sf(d), d)
})

test_that(".nt_as_sf reads a GeoJSON path", {
  d <- nt_test_sf()
  f <- tempfile(fileext = ".geojson")
  on.exit(unlink(f), add = TRUE)
  sf::st_write(d, f, quiet = TRUE)
  got <- .nt_as_sf(f)
  expect_s3_class(got, "sf")
  expect_equal(nrow(got), nrow(d))
  expect_true("nt_conc" %in% names(got))
})

test_that(".nt_as_sf reads a shapefile path", {
  d <- nt_test_sf()
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  shp <- file.path(dir, "tracts.shp")
  suppressWarnings(sf::st_write(d, shp, quiet = TRUE))  # shp truncates field names
  got <- .nt_as_sf(shp)
  expect_s3_class(got, "sf")
  expect_equal(nrow(got), nrow(d))
})

test_that(".nt_as_sf wraps a bare sfc geometry", {
  g <- sf::st_geometry(nt_test_sf())
  got <- .nt_as_sf(g)
  expect_s3_class(got, "sf")
  expect_equal(nrow(got), length(g))
})

test_that(".nt_as_sf errors helpfully on a non-spatial data frame", {
  expect_error(.nt_as_sf(sf::st_drop_geometry(nt_test_sf())),
               "not spatial|could not be converted")
})

test_that(".nt_as_sf errors on a non-path string", {
  expect_error(.nt_as_sf("not a file"), "not a readable spatial file")
})

test_that("nt_map accepts a GeoJSON path end to end", {
  skip_if_not_installed("mapgl")
  d <- nt_test_sf()
  f <- tempfile(fileext = ".geojson")
  on.exit(unlink(f), add = TRUE)
  sf::st_write(d, f, quiet = TRUE)
  m <- nt_map(f, color = "nt_conc")
  expect_s3_class(m, "htmlwidget")
  expect_identical(m$x$layers[[1]]$id, "nt_nt_conc")
})
