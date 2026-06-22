# Ranked point-estimate-with-error-bar chart in the ERN house style: a dot at
# each estimate with a whisker for its uncertainty (a symmetric ACS margin of
# error, or explicit low/high bounds). Honest disparity comparison on survey
# data needs the interval, not just the point. Built on echarts4r; assistance
# from Claude Opus 4.8 (Anthropic).

#' Ranked estimate + error-bar (confidence interval) chart (ERN house style)
#'
#' @description
#' A ranked dot-and-whisker chart: a dot at each category's estimate with a bar
#' spanning its uncertainty. Because the Eviction Research Network works almost
#' entirely on American Community Survey data — where every estimate ships with a
#' margin of error — showing the interval is the honest way to compare groups
#' (overlapping whiskers warn against over-reading a gap). Same minimal editorial
#' style as [nt_lollipop()]; the leader is accented and the scale can hide until
#' hover.
#'
#' Give the uncertainty either as a symmetric `error` (the ACS MOE, so the bar is
#' `value ± error`) or as explicit `low`/`high` bound columns.
#'
#' @param data A data frame: one row per category.
#' @param x Name of the category column.
#' @param value Name of the point-estimate column.
#' @param error Name of a symmetric +/- column (e.g. an ACS margin of error).
#'   Mutually exclusive with `low`/`high`.
#' @param low,high Names of explicit lower/upper bound columns (use instead of
#'   `error`).
#' @param sort `"desc"` (default), `"asc"`, or `"none"` — order by the estimate.
#' @param n Optional integer; keep only the top `n` categories after sorting.
#' @param orientation `"horizontal"` (default) or `"vertical"`.
#' @param highlight Which estimate to accent: `NULL` (default) accents the rank
#'   leader; `FALSE` none; a character vector by category; numeric row positions.
#' @param palette Dot colors (defaults to the ERN palette).
#' @param value_fmt Value formatting (see [nt_bar()]).
#' @param value_labels Logical; label each dot with its estimate (default `TRUE`).
#' @param yaxis Value-axis scale visibility (see [nt_bar()]).
#' @param dot_size Dot diameter in pixels (default `12`).
#' @param title,subtext,source,height Standard editorial extras (see [nt_bar()]).
#' @param ... Passed to [echarts4r::e_scatter()].
#' @return An `echarts4r`/`htmlwidget` object.
#' @family ern_charts
#' @seealso [nt_lollipop()] for a point-only ranking; [nt_dumbbell()] for a gap
#'   between two measures.
#' @examplesIf interactive() && requireNamespace("echarts4r", quietly = TRUE)
#' inc <- data.frame(
#'   race  = c("White", "Black", "Latine", "Asian"),
#'   med   = c(78000, 51000, 56000, 92000),
#'   moe   = c(1800, 3200, 2900, 4100)
#' )
#' nt_range(inc, "race", value = "med", error = "moe",
#'          value_fmt = "currency", highlight = "Black")
#' @export
nt_range <- function(data, x, value, error = NULL, low = NULL, high = NULL,
                     sort = c("desc", "asc", "none"), n = NULL,
                     orientation = c("horizontal", "vertical"),
                     highlight = NULL, palette = NULL,
                     value_fmt = c("comma", "percent", "currency", "multiple", "none"),
                     value_labels = TRUE,
                     yaxis = c("always", "hover", "none"),
                     dot_size = 12,
                     title = NULL, subtext = NULL, source = NULL,
                     height = NULL, ...) {
  sort        <- match.arg(sort)
  orientation <- match.arg(orientation)
  value_fmt   <- match.arg(value_fmt)
  yaxis       <- match.arg(yaxis)
  .nt_check_cols(data, c(x, value, error, low, high))
  if (is.null(error) && (is.null(low) || is.null(high))) {
    stop("Supply either `error` (symmetric) or both `low` and `high`.", call. = FALSE)
  }
  if (!is.null(error) && (!is.null(low) || !is.null(high))) {
    stop("Supply `error` OR `low`/`high`, not both.", call. = FALSE)
  }
  pal <- palette %nt||% .nt_chart_palette

  d <- .nt_rank_prep(data, x, value, sort, n, orientation)
  lo <- if (!is.null(error)) d[[value]] - d[[error]] else d[[low]]
  hi <- if (!is.null(error)) d[[value]] + d[[error]] else d[[high]]

  # per-dot colors (accent the rank leader by default)
  cols <- rep(.ern_brand$navy, nrow(d))
  hl <- .nt_resolve_highlight(highlight, d[[x]], d[[value]], sort)
  if (length(hl)) cols[hl] <- .ern_brand$accent

  # whisker segments, one per category, in the final axis coordinate order
  seg <- lapply(seq_len(nrow(d)), function(i) {
    cat_i <- as.character(d[[x]])[i]
    if (orientation == "horizontal") {
      list(list(coord = list(lo[i], cat_i)), list(coord = list(hi[i], cat_i)))
    } else {
      list(list(coord = list(cat_i, lo[i])), list(coord = list(cat_i, hi[i])))
    }
  })

  e <- echarts4r::e_charts_(d, x, height = height)
  e <- echarts4r::e_scatter_(e, value, symbol = "circle", symbolSize = dot_size,
                             legend = FALSE, ...)
  if (orientation == "horizontal") e <- echarts4r::e_flip_coords(e)
  # whiskers drawn under the dots as one markLine on the estimate series
  e$x$opts$series[[1]]$markLine <- list(
    data = seg, symbol = "none", silent = TRUE, animation = FALSE,
    lineStyle = list(color = .ern_brand$muted, width = 2, opacity = 0.7),
    label = list(show = FALSE))
  e <- .nt_paint_points(e, 1L, cols)

  if (isTRUE(value_labels)) {
    e$x$opts$series[[1]]$label <- list(
      show = TRUE, distance = 7,
      position = if (orientation == "horizontal") "right" else "top",
      formatter = .nt_value_label_js(value_fmt, orientation),
      color = .ern_brand$navy, fontFamily = .ern_font, fontSize = 11, fontWeight = 600)
  }

  e <- .nt_cat_chart_style(e, orientation, value_fmt, yaxis, pal, source = source)
  # The value axis auto-scales to the dot estimates only (markLine coords are
  # ignored), so extend it to contain the whiskers — the extremes that matter
  # most when judging whether two intervals overlap.
  rng <- range(c(lo, hi), na.rm = TRUE)
  if (all(is.finite(rng)) && diff(rng) > 0) {
    pad <- diff(rng) * 0.04
    ax  <- if (orientation == "horizontal") "xAxis" else "yAxis"
    e$x$opts[[ax]][[1]]$min <- rng[1] - pad
    e$x$opts[[ax]][[1]]$max <- rng[2] + pad
  }

  if (!is.null(title)) {
    e <- echarts4r::e_title(e, text = title, subtext = subtext %nt||% "",
                            textStyle = list(fontFamily = .ern_font,
                                             color = .ern_brand$navy, fontWeight = 600))
  }
  if (identical(yaxis, "hover")) {
    ax <- if (orientation == "horizontal") "xAxis" else "yAxis"
    e <- htmlwidgets::onRender(e, .nt_axis_hover_js(ax))
  }
  e
}
