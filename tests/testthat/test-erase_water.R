# Unit tests for nt_erase_water()'s internals on synthetic geometry — no Census
# API. The public function pulls + crops water per county, then:
#   .ew_prep_water() — drop small bodies, crop to the area of interest (+buffer)
#   .ew_cut()        — union the prepared water and erase it from x

.poly <- function(x0, y0, x1, y1) {
  sf::st_polygon(list(rbind(c(x0, y0), c(x1, y0), c(x1, y1),
                            c(x0, y1), c(x0, y0))))
}
.rbox <- sf::st_bbox(c(xmin = -1, ymin = -1, xmax = 11, ymax = 11),
                     crs = sf::st_crs(3310))

test_that(".ew_prep_water keeps large nearby water and returns geometry", {
  w <- sf::st_sf(AWATER = 1e9,
                 geometry = sf::st_sfc(.poly(5, 0, 15, 10), crs = 3310))
  g <- neighborhood:::.ew_prep_water(w, .rbox, area_min = 5e5)
  expect_s3_class(g, "sfc")
  expect_length(g, 1L)
})

test_that(".ew_prep_water drops bodies below the area threshold", {
  w <- sf::st_sf(AWATER = 1,           # 1 m^2, well under the 5e5 default
                 geometry = sf::st_sfc(.poly(5, 0, 6, 1), crs = 3310))
  expect_null(neighborhood:::.ew_prep_water(w, .rbox, area_min = 5e5))
})

test_that(".ew_prep_water crops away water outside the area of interest", {
  # a large body, but far from the region box -> nothing to erase (lean scoping)
  w <- sf::st_sf(AWATER = 1e9,
                 geometry = sf::st_sfc(.poly(100, 100, 110, 110), crs = 3310))
  expect_null(neighborhood:::.ew_prep_water(w, .rbox, area_min = 5e5))
})

test_that(".ew_cut erases water from x and preserves attributes", {
  x     <- sf::st_sf(id = "T",
                     geometry = sf::st_sfc(.poly(0, 0, 10, 10), crs = 3310))
  water <- sf::st_sfc(.poly(5, 0, 15, 10), crs = 3310)   # covers x's right half
  res   <- neighborhood:::.ew_cut(x, water)

  expect_equal(as.numeric(sf::st_area(res)), 50, tolerance = 1e-6)  # half remains
  expect_equal(res$id, "T")                                          # attrs kept
  expect_true(all(sf::st_geometry_type(res) %in% c("POLYGON", "MULTIPOLYGON")))
})

test_that("prep + cut compose to the expected land footprint", {
  x <- sf::st_sf(id = 1,
                 geometry = sf::st_sfc(.poly(0, 0, 10, 10), crs = 3310))
  w <- sf::st_sf(AWATER = 1e9,
                 geometry = sf::st_sfc(.poly(5, 0, 15, 10), crs = 3310))
  g <- neighborhood:::.ew_prep_water(w, .rbox, area_min = 5e5)
  res <- neighborhood:::.ew_cut(x, g)
  expect_equal(as.numeric(sf::st_area(res)), 50, tolerance = 1e-6)
})

test_that("nt_erase_water guards inputs before any network call", {
  x <- sf::st_sf(id = 1, geometry = sf::st_sfc(.poly(0, 0, 1, 1), crs = 3310))
  expect_error(nt_erase_water(data.frame(a = 1), "CA"), "sf object")
  expect_error(nt_erase_water(x, c("CA", "OR")), "single state")
})
