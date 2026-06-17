test_that("nt_popup returns one HTML string per row", {
  d <- nt_test_sf()
  p <- nt_popup(d, title = "GEOID", fields = c("Type" = "nt_conc"))
  expect_type(p, "character")
  expect_length(p, nrow(d))
  expect_match(p[1], "<b>GEOID: 06001000001</b>")
  expect_match(p[1], "Type: Mostly White")
})

test_that("nt_popup formats numerics: percent for [0,1], commas otherwise", {
  d <- nt_test_sf()
  p <- nt_popup(d, fields = c("Pct Black" = "pBlack", "Pop" = "totraceE"))
  expect_match(p[1], "Pct Black: 5.0%")     # 0.05 -> percent
  expect_match(p[1], "Pop: 4,231")          # count -> comma
})

test_that("nt_popup is deterministic (byte-identical across runs)", {
  d <- nt_test_sf()
  a <- nt_popup(d, title = "GEOID", fields = c("Type" = "nt_conc"))
  b <- nt_popup(d, title = "GEOID", fields = c("Type" = "nt_conc"))
  expect_identical(a, b)
})

test_that("nt_popup appends custom html for rich popups", {
  d <- nt_test_sf()
  bars <- paste0("<div data-w='", round(d$pBlack * 100), "'></div>")
  p <- nt_popup(d, title = "GEOID", fields = c("Type" = "nt_conc"), html = bars)
  expect_match(p[1], "<div data-w='5'></div>", fixed = TRUE)
  # custom html follows the fields
  expect_true(regexpr("Type: Mostly White", p[1]) < regexpr("data-w", p[1]))
})

test_that("nt_popup errors helpfully", {
  d <- nt_test_sf()
  expect_error(nt_popup(d, fields = c("X" = "nope")), "not found")
  expect_error(nt_popup(d), "title.*fields.*html|Nothing to show")
})

test_that("nt_popup literal vs column title", {
  d <- nt_test_sf()
  lit <- nt_popup(d, title = "Bay Area", fields = c("Type" = "nt_conc"))
  expect_true(all(grepl("<b>Bay Area</b>", lit, fixed = TRUE)))
  col <- nt_popup(d, title = "GEOID", prefix_title = FALSE,
                  fields = c("Type" = "nt_conc"))
  expect_match(col[2], "<b>06001000002</b>")
})
