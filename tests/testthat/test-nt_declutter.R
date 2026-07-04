# nt_declutter_basemap(): structural tests on the emitted hook.

skip_if_not_installed("mapgl")

test_that("nt_declutter_basemap attaches the hide hook", {
  m <- nt_declutter_basemap(mapgl::maplibre())
  html <- htmltools::renderTags(m)$html
  expect_true(grepl("setLayoutProperty", html, fixed = TRUE))
  expect_true(grepl("park|landcover", html))
})

test_that("custom pattern and input validation", {
  m <- nt_declutter_basemap(mapgl::maplibre(), pattern = "hillshade")
  expect_true(grepl("hillshade", htmltools::renderTags(m)$html, fixed = TRUE))
  expect_error(nt_declutter_basemap(data.frame()), "MapLibre")
})
