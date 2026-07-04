# afford_map() + afford_caption(): structural tests (no browser needed).

skip_if_not_installed("mapgl")

sq <- function(x0) sf::st_polygon(list(rbind(
  c(x0, 0), c(x0 + 1, 0), c(x0 + 1, 1), c(x0, 1), c(x0, 0))))
toy_sf <- sf::st_sf(
  GEOID = c("a", "b", "c"), ami_tier = "VLI",
  verdict = factor(c("affordable", "roughly affordable", "not affordable"),
    levels = c("affordable", "roughly affordable", "not affordable")),
  supply = c(0.6, 0.4, 0.1), supply_stretch = c(0.9, 0.6, 0.3),
  geometry = sf::st_sfc(sq(0), sq(1), sq(2), crs = 4326))
wtags <- function(m) htmltools::renderTags(m)$html

test_that("afford_map builds the verdict choropleth with plain labels", {
  m <- afford_map(toy_sf)
  expect_s3_class(m, "maplibregl")
  html <- wtags(m)
  expect_true(grepl("Affordable (≤30% of income)", html, fixed = TRUE))
  expect_true(grepl("verdict for a VLI household", html, fixed = TRUE))
})

test_that("displacement hatch is added from GEOID + dis_group", {
  hz <- data.frame(GEOID = c("a", "b", "c"), dis_group = c("VLI", NA, NA))
  m <- afford_map(toy_sf, displacement = hz)
  expect_true(grepl("displacement_hatch", wtags(m), fixed = TRUE))
  # dis_value fallback: hatch where negative
  hz2 <- data.frame(GEOID = c("a", "b", "c"), dis_value = c(-120, 30, NA))
  expect_true(grepl("displacement_hatch",
                    wtags(afford_map(toy_sf, displacement = hz2)), fixed = TRUE))
})

test_that("an absent class does not shift the remaining colors", {
  t2 <- toy_sf[toy_sf$verdict != "roughly affordable", ]
  html <- wtags(afford_map(t2))
  expect_true(grepl("#f4a582", html, fixed = TRUE))    # 'not' keeps salmon
  expect_false(grepl("#fbf2b4", html, fixed = TRUE))   # yellow not reassigned
})

test_that("verdict is derived when missing; errors are clear", {
  t3 <- toy_sf
  t3$verdict <- NULL
  expect_s3_class(afford_map(t3), "maplibregl")
  expect_error(afford_map(toy_sf, tier = "ELI"), "No rows for ami_tier")
  expect_error(afford_map(sf::st_drop_geometry(toy_sf)), "sf")
})

test_that("verdict_col switches panes; declutter hook is on by default", {
  t4 <- toy_sf
  t4$entry_verdict <- factor(
    c("not affordable", "roughly affordable", "affordable"),
    levels = levels(toy_sf$verdict))
  m <- afford_map(t4, verdict_col = "entry_verdict")
  html <- wtags(m)
  expect_true(grepl("entry verdict for a VLI household", html, fixed = TRUE))
  expect_true(grepl("setLayoutProperty", html, fixed = TRUE))
  m2 <- afford_map(t4, declutter = FALSE)
  expect_false(grepl("setLayoutProperty", wtags(m2), fixed = TRUE))
  expect_error(afford_map(toy_sf, verdict_col = "entry_verdict"),
               "entry_verdict")
})

test_that("afford_caption has an availability pane", {
  html <- as.character(afford_caption("VLI", pane = "availability"))
  expect_true(grepl("move into this neighborhood today", html, fixed = TRUE))
  expect_true(grepl("Competition is not yet netted out", html, fixed = TRUE))
  expect_true(grepl("If a unit opens and they are accepted", html, fixed = TRUE))
})

test_that("afford_caption carries the locked descriptor and the what-ifs", {
  html <- as.character(afford_caption("VLI"))
  expect_true(grepl("entry prices run higher still", html, fixed = TRUE))
  expect_true(grepl("VLI family", html, fixed = TRUE))
  expect_true(grepl("If they can move in", html, fixed = TRUE))
  expect_true(grepl("At incumbent prices", html, fixed = TRUE))
  html2 <- as.character(afford_caption("ELI"))
  expect_true(grepl("ELI family", html2, fixed = TRUE))
})
