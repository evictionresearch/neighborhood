# PMTiles tests need the tippecanoe command-line tool, so they are skipped where
# it is not installed (e.g. most CI).

test_that("nt_pmtiles writes a valid PMTiles archive", {
  skip_if_not_installed("mapgl")
  skip_if(Sys.which("tippecanoe") == "", "tippecanoe not installed")
  d <- nt_test_sf()
  path <- tempfile(fileext = ".pmtiles")
  out <- nt_pmtiles(d, path = path, quiet = TRUE)
  expect_identical(out, path)
  expect_true(file.exists(path))
  expect_gt(file.size(path), 0)
  # PMTiles files begin with the ASCII magic "PMTiles"
  magic <- rawToChar(readBin(path, "raw", 7L))
  expect_identical(magic, "PMTiles")
})

test_that("forcing tiles = 'pmtiles' wires up a vector-tile layer", {
  skip_if_not_installed("mapgl")
  skip_if(Sys.which("tippecanoe") == "", "tippecanoe not installed")
  d <- nt_test_sf()
  m <- nt_maplibre(d) |> nt_add_choropleth(d, "nt_conc", tiles = "pmtiles")
  expect_identical(m$x$layers[[1]]$source_layer, "nt")
})
