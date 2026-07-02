# Unit tests for nt_areal_weight()'s internals on synthetic geometry — no Census
# API. The public function is a thin wrapper of two helpers:
#   .aw_resolve_places()  — LSAD/name filter + uniqueness/empty guards
#   .aw_block_fractions() — the weighting math (block -> place -> share of tract)

# square polygon from corner coords
.poly <- function(x0, y0, x1, y1) {
  sf::st_polygon(list(rbind(c(x0, y0), c(x1, y0), c(x1, y1),
                            c(x0, y1), c(x0, y0))))
}
# small square centred on (cx, cy) — its point-on-surface is (cx, cy)
.cell <- function(cx, cy, h = 0.4) .poly(cx - h, cy - h, cx + h, cy + h)

# raw places layer: two incorporated cities (LSAD 25) named Alpha & Beta, plus a
# CDP (LSAD 57) that also happens to be named "Alpha"
.places_raw <- function() {
  sf::st_sf(
    NAME = c("Alpha", "Beta", "Alpha"),
    GEOID = c("0600001", "0600002", "0600003"),
    LSAD = c("25", "25", "57"),
    geometry = sf::st_sfc(.poly(0, 0, 10, 10), .poly(10, 0, 20, 10),
                          .poly(0, 20, 10, 30)),
    crs = 3310
  )
}

# resolved places (Alpha = [0,10]^2, Beta = [10,20]x[0,10])
.places_resolved <- function() {
  sf::st_sf(
    place_name = c("Alpha", "Beta"), place_geoid = c("1", "2"),
    geometry = sf::st_sfc(.poly(0, 0, 10, 10), .poly(10, 0, 20, 10)),
    crs = 3310
  )
}

# blocks engineered to exercise each case
.blocks_fixture <- function() {
  b <- data.frame(
    tract = c("000100", "000100", "000200", "000200",
              "000300", "000300", "000400"),
    blk   = c("1001", "1002", "2001", "2002", "3001", "3002", "4001"),
    cx    = c(2, 4,  2,  2,  5, 15,  5),
    cy    = c(2, 2,  5, 25,  5,  5,  9),
    pop   = c(100, 100, 100, 100, 100, 100, 0),
    hu    = c(40, 40, 40, 40, 40, 40, 0),
    area  = c(1000, 1000, 1000, 1000, 1000, 1000, 0),
    stringsAsFactors = FALSE
  )
  geom <- sf::st_sfc(lapply(seq_len(nrow(b)), function(i) .cell(b$cx[i], b$cy[i])),
                     crs = 3310)
  sf::st_sf(
    block = paste0("06081", b$tract, b$blk),
    tract_geoid = paste0("06081", b$tract),
    pop = b$pop, hu = b$hu, area = b$area, geometry = geom
  )
}

# helper to read one fraction out of the result
.frac <- function(res, place, tract, col = "frac_pop") {
  res[[col]][res$place_name == place & res$tract_geoid == paste0("06081", tract)]
}

test_that(".aw_resolve_places keeps incorporated places and drops the CDP", {
  pl <- neighborhood:::.aw_resolve_places(.places_raw(), NULL, "25")
  expect_setequal(pl$place_name, c("Alpha", "Beta"))
  expect_equal(nrow(pl), 2L)
  expect_true(all(c("place_name", "place_geoid") %in% names(pl)))
})

test_that(".aw_resolve_places errors when a name resolves to >1 polygon", {
  # lsad = NULL keeps both the city Alpha and the CDP Alpha -> ambiguous
  expect_error(
    neighborhood:::.aw_resolve_places(.places_raw(), "Alpha", NULL),
    "multiple polygons"
  )
})

test_that(".aw_resolve_places warns on a missing name and errors on empty set", {
  expect_warning(
    neighborhood:::.aw_resolve_places(.places_raw(), c("Alpha", "Ghost"), "25"),
    "not found"
  )
  expect_error(
    suppressWarnings(
      neighborhood:::.aw_resolve_places(.places_raw(), "Ghost", "25")),
    "no places matched"
  )
})

test_that(".aw_block_fractions weights each tract by its in-place share", {
  res <- neighborhood:::.aw_block_fractions(.blocks_fixture(), .places_resolved())

  # one row per (place, tract) overlap: Alpha touches 4 tracts, Beta touches 1
  expect_equal(nrow(res), 5L)

  # fully-inside tract -> 1; straddling tract (half its pop unincorporated) -> .5
  expect_equal(.frac(res, "Alpha", "000100"), 1)
  expect_equal(.frac(res, "Alpha", "000200"), 0.5)  # denominator incl. unincorp.

  # a tract split across two cities appears once per city, summing to <= 1
  expect_equal(.frac(res, "Alpha", "000300"), 0.5)
  expect_equal(.frac(res, "Beta",  "000300"), 0.5)
  expect_equal(sum(res$frac_pop[res$tract_geoid == "06081000300"]), 1)

  # zero-population (water) tract -> 0, never NaN from a divide-by-zero
  expect_equal(.frac(res, "Alpha", "000400"), 0)
  expect_false(any(is.nan(res$frac_pop)))
})

test_that("nt_areal_weight guards state arity before any network call", {
  expect_error(nt_areal_weight(c("CA", "OR"), "x"), "single state")
})
