# nt_map_sync(): structural tests on the emitted HTML (no browser needed).

skip_if_not_installed("mapgl")

sync_html <- function(s) htmltools::renderTags(s)$html
groups_in <- function(html) unique(unlist(regmatches(
  html, gregexpr("ntsync-[0-9]+", html))))

test_that("attaches one shared sync group across the maps", {
  s <- nt_map_sync(mapgl::maplibre(), mapgl::maplibre())
  html <- sync_html(s)
  # each map's hook mentions the group twice (registry init + lookup)
  expect_equal(length(gregexpr("_ntSync\\['ntsync-", html)[[1]]), 4L)
  expect_length(groups_in(html), 1L)
})

test_that("auto groups are unique per call; explicit group is honored", {
  s1 <- nt_map_sync(mapgl::maplibre(), mapgl::maplibre())
  s2 <- nt_map_sync(mapgl::maplibre(), mapgl::maplibre())
  expect_false(identical(groups_in(sync_html(s1)), groups_in(sync_html(s2))))
  s3 <- nt_map_sync(mapgl::maplibre(), mapgl::maplibre(), group = "abc")
  expect_true(grepl("_ntSync\\['abc'\\]", sync_html(s3)))
})

test_that("list input, labels, and height are applied", {
  s <- nt_map_sync(list(mapgl::maplibre(), mapgl::maplibre()),
                   titles = c("A", "B"), subtitles = "same", height = 300)
  html <- sync_html(s)
  expect_true(grepl(">A</div>", html) && grepl(">B</div>", html))
  expect_equal(length(gregexpr(">same</div>", html)[[1]]), 2L)  # recycled
  expect_equal(length(gregexpr("height:300px", html)[[1]]), 2L)
})

test_that("input validation", {
  expect_error(nt_map_sync(mapgl::maplibre()), "at least two")
  expect_error(nt_map_sync(mapgl::maplibre(), data.frame()), "MapLibre")
  expect_error(nt_map_sync(mapgl::maplibre(), mapgl::maplibre(),
                           titles = c("a", "b", "c")), "one entry per map")
})
