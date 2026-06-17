# Object-shape tests for the MapLibre wrappers. They inspect the serialized
# widget payload (layers, legend, controls) rather than rendering in a browser,
# so they run on CI. Guarded on mapgl being installed.

test_that("nt_maplibre returns a maplibre htmlwidget with the control set", {
  skip_if_not_installed("mapgl")
  d <- nt_test_sf()
  m <- nt_maplibre(d)
  expect_s3_class(m, "maplibregl")
  expect_s3_class(m, "htmlwidget")
  expect_true(all(c("navigation_control", "fullscreen_control",
                    "geolocate_control", "geocoder_control") %in% names(m$x)))
})

test_that("nt_maplibre can drop controls and search", {
  skip_if_not_installed("mapgl")
  m <- nt_maplibre(nt_test_sf(), controls = TRUE, search = FALSE)
  expect_false("geocoder_control" %in% names(m$x))
  expect_true("navigation_control" %in% names(m$x))
})

test_that("nt_add_choropleth adds a fill layer with popup, tooltip, and legend", {
  skip_if_not_installed("mapgl")
  d <- nt_test_sf()
  m <- nt_maplibre(d) |> nt_add_choropleth(d, "nt_conc")
  expect_length(m$x$layers, 1L)
  lyr <- m$x$layers[[1]]
  expect_identical(lyr$id, "nt_nt_conc")
  expect_identical(lyr$type, "fill")
  expect_false(is.null(lyr$popup))
  expect_identical(lyr$tooltip, "nt_conc")
  expect_gt(nchar(paste(m$x$legend_html, collapse = "")), 0L)
})

test_that("nt_map one-liner maps nt_conc by default", {
  skip_if_not_installed("mapgl")
  m <- nt_map(nt_test_sf())
  expect_s3_class(m, "htmlwidget")
  expect_identical(m$x$layers[[1]]$id, "nt_nt_conc")
})

test_that("numeric column maps as a binned layer with a chosen id", {
  skip_if_not_installed("mapgl")
  d <- nt_test_sf()
  m <- nt_maplibre(d) |>
    nt_add_choropleth(d, "pBlack", breaks = c(0.25, 0.5, 0.75), id = "afford")
  expect_identical(m$x$layers[[1]]$id, "afford")
})

test_that("nt_add_labels adds a symbol layer", {
  skip_if_not_installed("mapgl")
  d <- nt_test_sf()
  m <- nt_maplibre(d) |>
    nt_add_choropleth(d, "nt_conc") |>
    nt_add_labels(d, "NAME")
  types <- vapply(m$x$layers, function(z) z$type, character(1))
  expect_true("symbol" %in% types)
})

test_that("popup accepts an existing column name", {
  skip_if_not_installed("mapgl")
  d <- nt_test_sf()
  d$mypopup <- nt_popup(d, title = "GEOID", fields = c("Type" = "nt_conc"))
  m <- nt_maplibre(d) |> nt_add_choropleth(d, "nt_conc", popup = "mypopup")
  expect_identical(m$x$layers[[1]]$popup, "mypopup")
})

test_that("map functions reject non-spatial data with guidance", {
  d <- sf::st_drop_geometry(nt_test_sf())
  expect_error(nt_map(d), "not spatial|could not be converted")
  expect_error(nt_maplibre(d), "not spatial|could not be converted")
})
