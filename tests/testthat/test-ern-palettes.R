# The palette registry, its WCAG guarantees, and the swatch sheet. These pin
# the *rules* (brand red slot 2, red-free neutral, 3:1 fills, monotone ramps)
# rather than every hex, so palette tuning that keeps the rules keeps passing.

test_that("ern_palette exposes the documented palettes with the brand rules", {
  q <- ern_palette("qualitative")
  expect_length(q, 8L)
  expect_identical(q[1], "#223754")            # blue navy leads
  expect_identical(q[2], "#F9322B")            # brand red, slot 2
  expect_false("#F9322B" %in% ern_palette("neutral"))   # neutral is red-free
  expect_identical(ern_palette("legacy"),               # pre-1.1.0, verbatim
                   c("#19222C", "#F9322B", "#223754", "#8BA3BE",
                     "#C95123", "#33a02c", "#9b66b0", "#1f78b4"))
  b <- ern_palette("brand")
  expect_identical(unname(b[["accent"]]),        "#F9322B")
  expect_identical(unname(b[["accent_deep"]]),   "#CC2118")  # canonical text red
  expect_identical(unname(b[["accent_deeper"]]), "#B01D16")
  expect_identical(unname(b[["muted"]]),         "#586573")  # not deprecated #6c7a89
})

test_that("qualitative fills meet WCAG 1.4.11 (>= 3:1 on white)", {
  for (p in c("qualitative", "neutral"))
    expect_true(all(neighborhood:::.ern_contrast(ern_palette(p)) >= 3),
                info = p)
})

test_that("text reds meet WCAG AA (>= 4.5:1) on white and on the tint", {
  b <- ern_palette("brand")
  for (r in c("accent_deep", "accent_deeper")) {
    expect_gte(neighborhood:::.ern_contrast(b[[r]], "#FFFFFF"), 4.5)
    expect_gte(neighborhood:::.ern_contrast(b[[r]], b[["tint"]]), 4.5)
  }
  # graphics red: passes the 3:1 non-text minimum, fails AA body text
  expect_gte(neighborhood:::.ern_contrast(b[["accent"]]), 3)
  expect_lt(neighborhood:::.ern_contrast(b[["accent"]]), 4.5)
})

test_that("sequential ramps darken monotonically; diverging ramps peak center", {
  for (p in c("blues", "reds", "greens", "ramp"))
    expect_true(all(diff(neighborhood:::.ern_lum(ern_palette(p))) < 0), info = p)
  for (p in c("div_brand", "div_gold")) {
    L <- neighborhood:::.ern_lum(ern_palette(p))
    mid <- which.max(L)
    expect_identical(mid, as.integer(ceiling(length(L) / 2)), info = p)
    expect_true(all(diff(L[1:mid]) > 0) && all(diff(L[mid:length(L)]) < 0),
                info = p)
  }
})

test_that("n truncates qualitative palettes and interpolates ramps", {
  expect_identical(ern_palette("qualitative", n = 3),
                   ern_palette("qualitative")[1:3])
  expect_length(ern_palette("blues", n = 5), 5L)
  expect_length(ern_palette("div_gold", n = 9), 9L)
  expect_identical(ern_palette("reds", reverse = TRUE),
                   rev(ern_palette("reds")))
})

test_that("discrete scales accept the new palettes; continuous default unchanged", {
  skip_if_not_installed("ggplot2")
  expect_s3_class(scale_fill_ern("neutral"), "ScaleDiscrete")
  expect_s3_class(scale_color_ern("legacy"), "ScaleDiscrete")
  # default continuous scale still the risk ramp (back-compat)
  s <- ggplot2::ggplot_build(
    ggplot2::ggplot(data.frame(x = 1:2, y = 1:2, z = c(0, 1)),
                    ggplot2::aes(x, y, color = z)) +
    ggplot2::geom_point() + scale_color_ern_c())
  expect_identical(tolower(s$data[[1]]$colour),
                   tolower(c("#fed976", "#54278f")))
})

test_that("diverging scales pin the neutral center at midpoint", {
  skip_if_not_installed("ggplot2")
  s <- scale_fill_ern_div("gold", midpoint = 0)
  expect_s3_class(s, "ScaleContinuous")
  # asymmetric data around 0: the midpoint value must map to the ramp center
  d <- ggplot2::ggplot_build(
    ggplot2::ggplot(data.frame(x = 1:3, y = 1, z = c(-10, 0, 30)),
                    ggplot2::aes(x, y, fill = z)) +
    ggplot2::geom_tile() + scale_fill_ern_div("gold", midpoint = 0))
  expect_identical(tolower(d$data[[1]]$fill[2]),
                   tolower(ern_palette("div_gold")[4]))
})

test_that("ern_swatch writes a titled sheet listing every palette by name", {
  f <- ern_swatch(path = file.path(tempdir(), "ern-swatch-test.html"),
                  open = FALSE)
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")
  for (id in c("qualitative", "neutral", "legacy", "typology", "blues", "reds",
               "greens", "ramp", "div_brand", "div_gold", "brand"))
    expect_match(html, sprintf('ern_palette\\("%s"\\)', id), info = id)
  expect_match(html, "F9322B", ignore.case = TRUE)
  expect_match(html, "The red rule")
  unlink(f)
})
