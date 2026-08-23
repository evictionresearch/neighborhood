test_that("nt_bivariate_pal returns an n x n grid keyed x-y", {
  p <- nt_bivariate_pal()
  expect_length(p, 9L)
  expect_setequal(names(p), paste0(rep(1:3, times = 3), "-", rep(1:3, each = 3)))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", p)))
  expect_identical(attr(p, "n"), 3L)

  p4 <- nt_bivariate_pal(n = 4)
  expect_length(p4, 16L)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", p4)))
})

test_that("the published 3 x 3 grids are returned verbatim", {
  p <- nt_bivariate_pal(3, "bluered")
  expect_identical(unname(p[["1-1"]]), "#E8E8E8")
  expect_identical(unname(p[["3-3"]]), "#574249")
  expect_identical(unname(p[["3-1"]]), "#C85A5A")
  expect_identical(unname(p[["1-3"]]), "#64ACBE")
})

test_that("interpolated grids reproduce the four corners exactly", {
  for (pal in c("bluered", "purplegreen", "ern")) {
    cn <- neighborhood:::.nt_biv_corners[[pal]]
    for (n in c(2L, 4L, 5L)) {
      p <- nt_bivariate_pal(n, pal)
      expect_identical(toupper(unname(p[["1-1"]])), toupper(unname(cn[["base"]])))
      expect_identical(toupper(unname(p[[paste0(n, "-1")]])), toupper(unname(cn[["x"]])))
      expect_identical(toupper(unname(p[[paste0("1-", n)]])), toupper(unname(cn[["y"]])))
      expect_identical(toupper(unname(p[[paste0(n, "-", n)]])), toupper(unname(cn[["xy"]])))
    }
  }
})

test_that("nt_bivariate_pal rejects an out-of-range n", {
  expect_error(nt_bivariate_pal(n = 1), "between 2 and 6")
  expect_error(nt_bivariate_pal(n = 7), "between 2 and 6")
})

test_that("classes span 1..n and put ties on a break in the upper class", {
  cls <- neighborhood:::.nt_biv_class(c(0, 1, 2, 3, 4), c(1, 3))
  expect_identical(cls, c(1L, 2L, 2L, 3L, 3L))
  expect_true(is.na(neighborhood:::.nt_biv_class(NA_real_, c(1, 3))))
  # a value exactly on the TOP break must land in the top class, not below it
  expect_identical(neighborhood:::.nt_biv_class(3, c(1, 3)), 3L)
  expect_identical(neighborhood:::.nt_biv_class(1, c(1, 3)), 2L)
})

test_that("supplied breaks must have n - 1 thresholds", {
  expect_error(
    neighborhood:::.nt_biv_breaks(1:10, 3, c(0.5), "x_breaks"),
    "must have 2 interior break"
  )
  expect_identical(
    neighborhood:::.nt_biv_breaks(1:10, 3, c(7, 4), "x_breaks"), c(4, 7)
  )
})

test_that("nt_add_bivariate adds a layer and a grid legend bound to it", {
  skip_if_not_installed("mapgl")
  d <- nt_test_sf()
  d$pRent <- c(0.10, 0.40, 0.65, 0.85)

  m <- nt_maplibre(d) |>
    nt_add_bivariate(d, x = "pBlack", y = "pRent", id = "overlap")

  expect_s3_class(m, "htmlwidget")
  js <- paste(unlist(m$x$calls), collapse = " ")
  html <- paste(vapply(m$jsHooks$render, function(h) h$code %||% "", character(1)),
                collapse = " ")
  expect_match(html, "nt-bivariate-legend")
  expect_match(html, 'data-layer-id', fixed = TRUE)
  expect_match(html, "overlap", fixed = TRUE)
})

test_that("nt_add_bivariate rejects non-numeric and missing columns", {
  skip_if_not_installed("mapgl")
  d <- nt_test_sf()
  d$pRent <- c(0.10, 0.40, 0.65, 0.85)
  m <- nt_maplibre(d)
  expect_error(nt_add_bivariate(m, d, x = "nt_conc", y = "pRent"), "must be numeric")
  expect_error(nt_add_bivariate(m, d, x = "nope", y = "pRent"), "is not in `data`")
})

test_that("a feature missing either measure gets no grid cell", {
  d <- nt_test_sf()
  d$pRent <- c(0.10, NA, 0.65, 0.85)
  xi <- neighborhood:::.nt_biv_class(d$pBlack, c(0.2, 0.5))
  yi <- neighborhood:::.nt_biv_class(d$pRent,  c(0.3, 0.7))
  cls <- ifelse(is.na(xi) | is.na(yi), NA_character_, paste0(xi, "-", yi))
  expect_true(is.na(cls[2]))
  expect_false(any(is.na(cls[-2])))
})
