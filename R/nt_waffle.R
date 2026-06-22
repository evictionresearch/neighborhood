# Waffle / unit chart in the ERN house style: a grid of squares where each
# square is a share of the whole. The most W.E.B. Du Bois-authentic form in the
# kit -- a spare "data portrait" of composition. Built on echarts4r; assistance
# from Claude Opus 4.8 (Anthropic).

#' Waffle / unit chart (ERN / Du Bois data-portrait style)
#'
#' @description
#' A grid of `n` squares (10 × 10 = 100 by default) where each category fills a
#' number of squares proportional to its share — the unit-chart "data portrait"
#' in the spirit of W.E.B. Du Bois. A clean way to show one composition (e.g. the
#' racial makeup of renter households, or the share of tenants who are
#' rent-burdened) where part-of-whole is the message.
#'
#' @param data Either a named numeric vector (`names` = categories, values =
#'   counts/shares) **or** a data frame with a category and a value column.
#' @param category When `data` is a data frame, the name of the category column.
#' @param value When `data` is a data frame, the name of the numeric column.
#' @param squares Total number of squares (the grid resolution; default `100`).
#'   Category square counts are apportioned by largest remainder so they sum
#'   exactly to `squares`. (Note this is grid resolution, not the top-`n` filter
#'   used by [nt_bar()]/[nt_lollipop()]/[nt_dumbbell()]/[nt_range()].) `n` is a
#'   deprecated alias.
#' @param n Deprecated alias for `squares`.
#' @param ncol Number of columns in the grid (default `10`).
#' @param flow `"row"` (default; fill row by row from the bottom) or `"column"`
#'   (fill column by column from the left).
#' @param palette Square colors, one per category. Defaults to a neutral
#'   accent-free ramp; pass a named vector to emphasize a group — e.g. the race
#'   convention `c(Black = "#F9322B", White = "#aab4be", Latine = "#223754")`.
#' @param square_size Square size in pixels (default `18`).
#' @param legend Logical; show the category legend (default `TRUE`).
#' @param title,subtext,source,height Standard editorial extras (see [nt_bar()]).
#' @return An `echarts4r`/`htmlwidget` object.
#' @family ern_charts
#' @seealso [nt_stacked_bar()] for composition as a bar.
#' @examplesIf interactive() && requireNamespace("echarts4r", quietly = TRUE)
#' nt_waffle(c(White = 52, Black = 23, Latine = 15, Asian = 10),
#'           title = "Renter households by race")
#' @export
nt_waffle <- function(data, category = NULL, value = NULL,
                      squares = 100, ncol = 10, flow = c("row", "column"),
                      palette = NULL, square_size = 18, legend = TRUE,
                      title = NULL, subtext = NULL, source = NULL,
                      height = NULL, n = NULL) {
  flow <- match.arg(flow)
  if (!is.null(n)) {                          # soft-deprecated alias
    if (missing(squares)) squares <- n
    warning("`n` is deprecated in nt_waffle(); use `squares`.", call. = FALSE)
  }
  n <- squares

  if (is.data.frame(data)) {
    if (is.null(category) || is.null(value)) {
      stop("With a data frame, supply `category` and `value` column names.",
           call. = FALSE)
    }
    .nt_check_cols(data, c(category, value))
    cats <- as.character(data[[category]])
    vals <- as.numeric(data[[value]])
  } else if (is.numeric(data) && !is.null(names(data)) && all(nzchar(names(data)))) {
    cats <- names(data)
    vals <- as.numeric(data)
  } else {
    stop("`data` must be a fully named numeric vector or a data frame with ",
         "`category`/`value`.", call. = FALSE)
  }
  if (any(vals < 0, na.rm = TRUE)) stop("`value` must be non-negative.", call. = FALSE)
  vals[is.na(vals)] <- 0
  tot <- sum(vals)
  if (!is.finite(tot) || tot <= 0) {
    stop("`value` must sum to a positive number.", call. = FALSE)
  }

  shares <- vals / tot
  # largest-remainder apportionment so the squares sum to exactly n
  raw    <- shares * n
  counts <- floor(raw)
  short  <- n - sum(counts)
  if (short > 0) {
    bump <- order(raw - counts, decreasing = TRUE)[seq_len(short)]
    counts[bump] <- counts[bump] + 1L
  }

  sq_cat <- rep(cats, times = counts)        # one entry per square, in cat order
  nrow_g <- ceiling(n / ncol)
  idx <- seq_len(length(sq_cat)) - 1L
  if (flow == "row") {
    col <- idx %% ncol
    row <- idx %/% ncol
  } else {
    col <- idx %/% nrow_g
    row <- idx %% nrow_g
  }
  pos <- data.frame(col = col, row = row,
                    cat = factor(sq_cat, levels = cats),
                    stringsAsFactors = FALSE)

  # neutral default (accent reserved for an explicit focus); pass a named palette
  # to emphasize a group, e.g. the race convention c(Black = "#F9322B", ...).
  pal <- palette %nt||% stats::setNames(.nt_seq_ramp(length(cats)), cats)

  d <- dplyr::group_by(pos, .data[["cat"]])
  e <- echarts4r::e_charts_(d, "col", height = height)
  e <- echarts4r::e_scatter_(e, "row", symbol = "rect",
                             symbolSize = square_size, legend = TRUE)
  e <- echarts4r::e_color(e, unname(pal[cats]), background = "rgba(0,0,0,0)")

  # square grid: hide both axes, pad by half a cell so edge squares aren't clipped
  blank <- list(show = FALSE)
  e <- echarts4r::e_x_axis(e, show = FALSE, min = -0.5, max = ncol - 0.5,
                           splitLine = blank)
  e <- echarts4r::e_y_axis(e, show = FALSE, min = -0.5, max = nrow_g - 0.5,
                           splitLine = blank)

  pct <- scales::percent(shares, accuracy = 0.1)
  # JSON-encode the category -> share lookup so names containing quotes or
  # backslashes can't produce invalid JS (jsonlite ships with htmlwidgets).
  share_map <- jsonlite::toJSON(stats::setNames(as.list(pct), cats), auto_unbox = TRUE)
  tip <- htmlwidgets::JS(sprintf(paste0(
    "function(p){ var m=%s; var s=m[p.seriesName]||'';",
    " return '<b>'+(p.seriesName||'')+'</b>'+(s?' \\u00b7 '+s:''); }"), share_map))
  e <- echarts4r::e_tooltip(e, trigger = "item",
                            backgroundColor = .ern_brand$navy, borderWidth = 0,
                            textStyle = list(color = "#ffffff", fontSize = 12,
                                             fontFamily = .ern_font),
                            formatter = tip)
  e <- echarts4r::e_legend(e, show = isTRUE(legend),
                           textStyle = list(fontFamily = .ern_font))
  e <- echarts4r::e_grid(e, left = 12, right = 12, top = 36, bottom = 12)
  if (!is.null(source)) e <- .nt_add_source(e, source)
  if (!is.null(title)) {
    e <- echarts4r::e_title(e, text = title, subtext = subtext %nt||% "",
                            textStyle = list(fontFamily = .ern_font,
                                             color = .ern_brand$navy, fontWeight = 600))
  }
  e
}
