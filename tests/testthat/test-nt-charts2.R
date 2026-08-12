# Object-shape tests for nt_stacked_bar, nt_slope, nt_scatter, nt_waffle and the
# ggplot2 theme/scale kit. They inspect the option payload (or the returned
# object class), so they run on CI without a browser.

# ---- nt_stacked_bar -------------------------------------------------------

tenure <- function() data.frame(
  race   = rep(c("White", "Black", "Latine"), each = 2),
  tenure = rep(c("Own", "Rent"), 3),
  hh     = c(620, 380, 240, 760, 300, 700),
  stringsAsFactors = FALSE)

test_that("nt_stacked_bar proportional caps the value axis at 1 and uses percent", {
  skip_if_not_installed("echarts4r")
  e <- nt_stacked_bar(tenure(), "race", "hh", group = "tenure")  # horizontal default
  expect_length(e$x$opts$series, 2L)                # one series per group level
  expect_true(isTRUE(e$x$opts$legend$show))
  expect_equal(e$x$opts$xAxis[[1]]$max, 1)          # value axis (x after flip)
  expect_true(all(vapply(e$x$opts$series, function(s) !is.null(s$stack), logical(1))))
})

test_that("nt_stacked_bar non-proportional does not cap the axis", {
  skip_if_not_installed("echarts4r")
  e <- nt_stacked_bar(tenure(), "race", "hh", group = "tenure",
                      proportional = FALSE, value_fmt = "comma")
  expect_null(e$x$opts$xAxis[[1]]$max)
})

test_that("nt_stacked_bar default palette is the neutral (accent-free) ramp", {
  skip_if_not_installed("echarts4r")
  e <- nt_stacked_bar(tenure(), "race", "hh", group = "tenure")
  expect_false("#F9322B" %in% unlist(e$x$opts$color))   # no accent flooding a band
})

test_that("nt_stacked_bar warns and zeroes a category whose values sum to 0", {
  skip_if_not_installed("echarts4r")
  d <- data.frame(r = rep(c("A", "B"), each = 2), t = rep(c("Own", "Rent"), 2),
                  h = c(0, 0, 3, 4))
  expect_warning(nt_stacked_bar(d, "r", "h", group = "t"), "sum to 0")
})

test_that("nt_stacked_bar in-bar label formatter is orientation-aware", {
  skip_if_not_installed("echarts4r")
  ev <- nt_stacked_bar(tenure(), "race", "hh", group = "tenure", orientation = "vertical")
  expect_match(as.character(ev$x$opts$series[[1]]$label$formatter), "raw[1]", fixed = TRUE)
})

# Category order lives in the ROW order echarts4r serializes, not in the factor
# levels, so these read the axis data the way the browser does.
axis_cats <- function(e, orientation = "horizontal") {
  ax <- if (orientation == "horizontal") e$x$opts$yAxis else e$x$opts$xAxis
  as.character(unlist(ax[[1]]$data))
}

ranked <- function() data.frame(          # totals: White 1000, Black 250, Latine 700
  race   = rep(c("White", "Black", "Latine"), each = 2),
  tenure = rep(c("Own", "Rent"), 3),
  hh     = c(620, 380,  100, 150,  300, 400),
  stringsAsFactors = FALSE)

test_that("nt_stacked_bar sorts bars by raw total, leader on top when horizontal", {
  skip_if_not_installed("echarts4r")
  d <- ranked()
  e <- nt_stacked_bar(d, "race", "hh", group = "tenure", sort = "desc")
  # horizontal: first category renders at the BOTTOM, so descending order reverses
  expect_identical(axis_cats(e), c("Black", "Latine", "White"))

  ev <- nt_stacked_bar(d, "race", "hh", group = "tenure", sort = "desc",
                       orientation = "vertical")
  expect_identical(axis_cats(ev, "vertical"), c("White", "Latine", "Black"))

  ea <- nt_stacked_bar(d, "race", "hh", group = "tenure", sort = "asc")
  expect_identical(axis_cats(ea), c("White", "Latine", "Black"))
})

test_that("nt_stacked_bar sort ranks on raw totals even when proportional", {
  skip_if_not_installed("echarts4r")
  # every bar is 100% once normalized; only the raw totals can rank them
  d <- data.frame(race = rep(c("Small", "Big"), each = 2),
                  tenure = rep(c("Own", "Rent"), 2),
                  hh = c(1, 1, 500, 500))
  e <- nt_stacked_bar(d, "race", "hh", group = "tenure", sort = "desc")
  expect_identical(axis_cats(e), c("Small", "Big"))   # Big on top
})

test_that("nt_stacked_bar n keeps the leading categories and drops their rows", {
  skip_if_not_installed("echarts4r")
  d <- data.frame(race = rep(c("A", "B", "C"), each = 2),
                  tenure = rep(c("Own", "Rent"), 3),
                  hh = c(10, 10, 500, 500, 100, 100))
  e <- nt_stacked_bar(d, "race", "hh", group = "tenure", sort = "desc", n = 2)
  expect_identical(axis_cats(e), c("C", "B"))
  expect_length(e$x$opts$series[[1]]$data, 2L)   # rows for the dropped bar are gone
})

test_that("an unsorted chart keeps its data order (the new arguments change nothing)", {
  skip_if_not_installed("echarts4r")
  expect_identical(axis_cats(nt_stacked_bar(tenure(), "race", "hh", group = "tenure")),
                   c("White", "Black", "Latine"))   # as supplied, first at the bottom
})

test_that("nt_stacked_bar tooltip names the hovered segment", {
  skip_if_not_installed("echarts4r")
  f <- as.character(nt_stacked_bar(tenure(), "race", "hh", group = "tenure")$x$opts$tooltip$formatter)
  expect_match(f, "p.seriesName", fixed = TRUE)   # which segment, not just which bar
  expect_false(grepl(" of ", f, fixed = TRUE))    # no counts unless asked for
})

test_that("nt_stacked_bar tooltip_count adds the count and the bar total", {
  skip_if_not_installed("echarts4r")
  e <- nt_stacked_bar(tenure(), "race", "hh", group = "tenure",
                      tooltip_count = TRUE, tooltip_count_suffix = " households")
  f <- as.character(e$x$opts$tooltip$formatter)
  expect_match(f, "' of '", fixed = TRUE)
  expect_match(f, "households", fixed = TRUE)
  expect_match(f, "White__Own", fixed = TRUE)   # count lookup keyed category__series
  expect_match(f, '"White":1000', fixed = TRUE) # White = 620 + 380, the bar total

  # a named column counts something other than the stacked value
  d <- tenure(); d$people <- d$hh * 3
  f2 <- as.character(nt_stacked_bar(d, "race", "hh", group = "tenure",
                                    tooltip_count = "people")$x$opts$tooltip$formatter)
  expect_match(f2, '"White":3000', fixed = TRUE)
})

# ---- nt_slope -------------------------------------------------------------

slope_df <- function() data.frame(
  place = rep(c("Hennepin", "Ramsey", "Dakota"), each = 2),
  year  = rep(c(2019, 2024), 3),
  rent  = c(1180, 1490, 1090, 1380, 1320, 1610),
  stringsAsFactors = FALSE)

test_that("nt_slope draws a line per group with end + start labels", {
  skip_if_not_installed("echarts4r")
  e <- nt_slope(slope_df(), "year", "rent", group = "place", value_fmt = "currency")
  expect_length(e$x$opts$series, 3L)
  expect_true(all(vapply(e$x$opts$series,
                         function(s) isTRUE(s$endLabel$show), logical(1))))
  # the first point of each line carries a left-hand value label
  expect_true(is.list(e$x$opts$series[[1]]$data[[1]]))
  expect_true(isTRUE(e$x$opts$series[[1]]$data[[1]]$label$show))
})

test_that("nt_slope highlight accents chosen series and mutes the rest", {
  skip_if_not_installed("echarts4r")
  e <- nt_slope(slope_df(), "year", "rent", group = "place", highlight = "Ramsey")
  cols <- vapply(e$x$opts$series, function(s) s$lineStyle$color %||% NA_character_,
                 character(1))
  names(cols) <- vapply(e$x$opts$series, function(s) s$name, character(1))
  expect_identical(cols[["Ramsey"]], neighborhood:::.ern_brand$accent)
  expect_identical(cols[["Dakota"]], neighborhood:::.ern_brand$steel)
})

test_that("nt_slope warns when a named palette matches no group", {
  skip_if_not_installed("echarts4r")
  expect_warning(
    nt_slope(slope_df(), "year", "rent", group = "place", palette = c(Nope = "#111111")),
    "match no group")
})

# ---- nt_scatter -----------------------------------------------------------

test_that("nt_scatter has a value x-axis and adds a trend line when asked", {
  skip_if_not_installed("echarts4r")
  d <- data.frame(x = 1:20, y = (1:20) * 2 + rnorm(20))
  e0 <- nt_scatter(d, "x", "y")
  expect_identical(e0$x$opts$xAxis[[1]]$type, "value")
  expect_length(e0$x$opts$series, 1L)
  e1 <- nt_scatter(d, "x", "y", trend = TRUE)
  expect_length(e1$x$opts$series, 2L)               # points + lm line
})

# ---- nt_waffle ------------------------------------------------------------

test_that("nt_waffle apportions exactly n squares across one series per category", {
  skip_if_not_installed("echarts4r")
  e <- nt_waffle(c(White = 52, Black = 23, Latine = 15, Asian = 10))
  expect_length(e$x$opts$series, 4L)
  total <- sum(vapply(e$x$opts$series, function(s) length(s$data), integer(1)))
  expect_equal(total, 100L)
  expect_false(isTRUE(e$x$opts$xAxis[[1]]$show))     # axes hidden
})

test_that("nt_waffle accepts a data frame and rejects bad input", {
  skip_if_not_installed("echarts4r")
  df <- data.frame(grp = c("a", "b"), v = c(3, 1))
  e <- nt_waffle(df, category = "grp", value = "v", squares = 20)
  expect_equal(sum(vapply(e$x$opts$series, function(s) length(s$data), integer(1))), 20L)
  expect_error(nt_waffle(c(1, 2, 3)), "named")                   # no names
  expect_error(nt_waffle(c(a = 1, b = -1)), "non-negative")
  expect_error(nt_waffle(c(50, B = 50)), "named")                # partially named
  expect_error(nt_waffle(c(A = 0, B = 0)), "positive")              # zero total
  expect_error(nt_waffle(c(A = NA_real_, B = NA_real_)), "positive") # all-NA total
})

test_that("nt_waffle JSON-escapes category names containing a double quote", {
  skip_if_not_installed("echarts4r")
  e <- nt_waffle(c(`O"Brien` = 50, Other = 50))
  tip <- as.character(e$x$opts$tooltip$formatter)
  expect_true(grepl('O\\"Brien', tip, fixed = TRUE))   # quote backslash-escaped
})

# ---- ggplot2 theme + scales ----------------------------------------------

test_that("theme_ern returns a ggplot theme", {
  skip_if_not_installed("ggplot2")
  th <- theme_ern()
  expect_s3_class(th, "theme")
})

test_that("ern_palette returns the expected colors", {
  expect_identical(unname(ern_palette("brand")["navy"]), "#19222C")
  expect_length(ern_palette("qualitative", n = 3), 3L)
  expect_length(ern_palette("ramp"), 4L)
  expect_named(ern_palette("typology"))
})

test_that("ERN ggplot scales return Scale objects", {
  skip_if_not_installed("ggplot2")
  expect_s3_class(scale_color_ern(), "Scale")
  expect_s3_class(scale_fill_ern(palette = "typology"), "Scale")
  expect_s3_class(scale_fill_ern_c(), "Scale")
  expect_identical(scale_colour_ern, scale_color_ern)
})

# ---- legend placement -----------------------------------------------------
# ECharts 6 defaults a legend to bottom-center, on top of the axis labels, so
# every chart states its own position instead of inheriting that default.

test_that("chart legends are positioned explicitly, never left to the default", {
  skip_if_not_installed("echarts4r")
  d <- data.frame(x = rep(c("A", "B"), 2), g = rep(c("p", "q"), each = 2),
                  y = c(3, 5, 4, 2))
  legs <- list(
    bar     = nt_bar(d, "x", "y", group = "g"),
    stacked = nt_stacked_bar(d, "x", "y", group = "g"),
    waffle  = nt_waffle(data.frame(c = c("a", "b"), v = c(.4, .6)), "c", "v"),
    scatter = nt_scatter(data.frame(x = 1:4, y = c(2, 4, 3, 5),
                                    g = rep(c("p", "q"), 2)), "x", "y", group = "g"),
    chart   = nt_chart(d, "x", "y", group = "g", type = "bar")
  )
  for (nm in names(legs)) {
    lg <- legs[[nm]]$x$opts$legend
    expect_true(is.numeric(lg$top), label = paste(nm, "legend has a numeric top"))
    expect_null(lg$bottom)
  }
})

test_that("a shown legend takes its row out of the plot, and a hidden one does not", {
  skip_if_not_installed("echarts4r")
  d <- data.frame(x = rep(c("A", "B"), 2), g = rep(c("p", "q"), each = 2),
                  y = c(3, 5, 4, 2))
  with_leg <- nt_stacked_bar(d, "x", "y", group = "g")
  no_leg   <- nt_stacked_bar(d, "x", "y", group = "g", legend = FALSE)
  expect_equal(with_leg$x$opts$grid[[1]]$top, 30L + neighborhood:::.nt_legend_row)
  expect_equal(no_leg$x$opts$grid[[1]]$top, 30L)   # unchanged from before the fix
})

test_that("a legend clears the title instead of landing on it", {
  skip_if_not_installed("echarts4r")
  d <- data.frame(x = rep(c("A", "B"), 2), g = rep(c("p", "q"), each = 2),
                  y = c(3, 5, 4, 2))
  plain <- nt_stacked_bar(d, "x", "y", group = "g")
  titled <- nt_stacked_bar(d, "x", "y", group = "g", title = "One line")
  subbed <- nt_stacked_bar(d, "x", "y", group = "g", title = "One line",
                           subtext = "and a subtitle")
  expect_equal(plain$x$opts$legend$top, 30L)
  expect_equal(titled$x$opts$legend$top, neighborhood:::.nt_title_head("One line"))
  expect_gt(subbed$x$opts$legend$top, titled$x$opts$legend$top)
  # a titled chart with no legend keeps the headroom it always had
  expect_equal(nt_stacked_bar(d, "x", "y", group = "g", title = "One line",
                              legend = FALSE)$x$opts$grid[[1]]$top, 30L)
})

test_that("nt_chart legend_pos still puts the legend below the plot on request", {
  skip_if_not_installed("echarts4r")
  d <- data.frame(x = rep(c("A", "B"), 2), g = rep(c("p", "q"), each = 2),
                  y = c(3, 5, 4, 2))
  below <- nt_chart(d, "x", "y", group = "g", type = "bar", legend_pos = "bottom")
  expect_equal(below$x$opts$legend$bottom, 28)
  expect_null(below$x$opts$legend$top)
  expect_equal(below$x$opts$grid[[1]]$bottom, 96)   # room for labels, legend, source
})
