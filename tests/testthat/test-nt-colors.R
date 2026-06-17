# The neighborhood-typology color key is the single source of truth shared by
# nt_pal() (leaflet) and the MapLibre choropleth path. These tests guard against
# drift between the colors, the factor levels, and the two consumers.

test_that(".nt_colors has 19 entries aligned to the nt_conc factor levels", {
  lvls <- levels(us_nt_tracts2024$nt_conc)
  expect_length(.nt_colors, 19L)
  expect_identical(names(.nt_colors), lvls)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", .nt_colors)))
})

test_that("nt_pal() and the shared color key agree", {
  df <- head(us_nt_tracts2024, 200)
  pal <- nt_pal(df)
  # leaflet upper-cases hex; compare case-insensitively for present levels
  present <- intersect(names(.nt_colors), as.character(df$nt_conc))
  got <- toupper(vapply(present, pal, character(1)))
  want <- toupper(.nt_colors[present])
  expect_equal(unname(got), unname(want))
})

test_that("categorical color resolution falls back for unknown categories", {
  cc <- .nt_categorical_colors(factor(c("a", "b", "c")))
  expect_length(cc$colors, 3L)
  expect_true(all(grepl("^#", cc$colors)))
  # known typology levels use the canonical palette
  cc2 <- .nt_categorical_colors(factor("Diverse", levels = levels(us_nt_tracts2024$nt_conc)))
  expect_identical(cc2$colors, unname(.nt_colors["Diverse"]))
})
