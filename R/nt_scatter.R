# Scatter plot (with optional linear trend line) in the ERN house style, for
# relationships between two continuous measures (e.g. an EDR vs. EER score).
# Built on echarts4r; assistance from Claude Opus 4.8 (Anthropic).

#' Scatter plot (ERN house style)
#'
#' @description
#' A scatter plot of two continuous measures, with an optional linear trend
#' line, in the minimal editorial style: a thin x hairline, a clean y axis whose
#' scale can hide until hover, and a dark tooltip. Use it for relationships such
#' as one index against another, or rent against income.
#'
#' @param data A data frame.
#' @param x Name of the x column: numeric (a value axis) or `Date`/`POSIXct`
#'   (a time axis).
#' @param y Name of the numeric y column.
#' @param group Optional column; one colored series per group.
#' @param trend Logical; add a linear (OLS) trend line (default `FALSE`). With a
#'   `group`, a line is fit per group.
#' @param point_size Point diameter in pixels (default `10`).
#' @param palette Point colors (defaults to the ERN palette).
#' @param value_fmt Formatting for the y axis / tooltip (see [nt_bar()]).
#' @param x_fmt Formatting for the x axis and the tooltip x value (same options
#'   as `value_fmt`). Ignored when `x` is a Date (a time axis is used instead).
#' @param yaxis y-axis scale visibility (see [nt_bar()]).
#' @param legend Logical; show a legend when `group` is set (default `TRUE`).
#' @param title,subtext,source,height Standard editorial extras (see [nt_bar()]).
#' @param ... Passed to [echarts4r::e_scatter()].
#' @return An `echarts4r`/`htmlwidget` object.
#' @family ern_charts
#' @examplesIf interactive() && requireNamespace("echarts4r", quietly = TRUE)
#' set.seed(1)
#' d <- data.frame(edr = runif(60, 0, 1))
#' d$eer <- pmin(1, pmax(0, d$edr * 0.8 + rnorm(60, 0, 0.12)))
#' nt_scatter(d, "edr", "eer", trend = TRUE, value_fmt = "percent", x_fmt = "percent")
#' @export
nt_scatter <- function(data, x, y, group = NULL,
                       trend = FALSE, point_size = 10,
                       palette = NULL,
                       value_fmt = c("comma", "percent", "currency", "multiple", "none"),
                       x_fmt = c("comma", "percent", "currency", "multiple", "none"),
                       yaxis = c("always", "hover", "none"),
                       legend = TRUE,
                       title = NULL, subtext = NULL, source = NULL,
                       height = NULL, ...) {
  value_fmt <- match.arg(value_fmt)
  x_fmt     <- match.arg(x_fmt)
  yaxis     <- match.arg(yaxis)
  .nt_check_cols(data, c(x, y, group))
  pal <- palette %nt||% .nt_chart_palette

  d <- as.data.frame(data, stringsAsFactors = FALSE)
  # x may be continuous (a value axis) or a date (a time axis); anything else
  # (e.g. a category) would silently stringify to NaN, so reject it up front.
  x_is_time <- inherits(d[[x]], c("Date", "POSIXct", "POSIXt"))
  if (!is.numeric(d[[x]]) && !x_is_time) {
    stop("nt_scatter() `x` must be numeric or Date/POSIXct.", call. = FALSE)
  }
  if (!is.numeric(d[[y]])) stop("nt_scatter() `y` must be numeric.", call. = FALSE)

  grouped <- !is.null(group)
  if (grouped) d <- dplyr::group_by(d, .data[[group]])
  e <- echarts4r::e_charts_(d, x, height = height)
  e <- echarts4r::e_scatter_(e, y, symbol = "circle", symbolSize = point_size,
                             name = y, ...)
  if (isTRUE(trend)) {
    if (x_is_time) {
      warning("nt_scatter(): `trend` is skipped for a Date/time x.", call. = FALSE)
    } else {
      e <- echarts4r::e_lm(e, stats::as.formula(paste(y, "~", x)),
                           symbol = "none",
                           lineStyle = list(color = .ern_brand$accent,
                                            width = 1.5, type = "dashed"))
    }
  }

  axis_label <- list(color = .ern_brand$muted, fontFamily = .ern_font, fontSize = 11)
  e <- echarts4r::e_color(e, pal, background = "rgba(0,0,0,0)")

  # x axis: a value axis (continuous) or a time axis (Date x); thin hairline
  x_args <- list(e, type = if (x_is_time) "time" else "value", scale = TRUE,
                 axisLine = list(lineStyle = list(color = .ern_brand$muted, width = 0.5)),
                 axisTick = list(show = FALSE), axisLabel = axis_label,
                 splitLine = list(show = FALSE))
  if (!x_is_time && x_fmt != "none") {
    x_args$formatter <- if (x_fmt == "multiple")
      htmlwidgets::JS(sprintf("function(v){ return %s; }", .nt_js_number_expr("multiple","v")))
    else echarts4r::e_axis_formatter(.nt_fmt_style(x_fmt))
  }
  e <- do.call(echarts4r::e_x_axis, x_args)

  # y axis: no line, gridlines/labels hidden unless "always"
  y_args <- list(e, scale = TRUE,
                 splitLine = list(show = FALSE), axisLine = list(show = FALSE),
                 axisTick = list(show = FALSE),
                 axisLabel = c(axis_label, list(show = identical(yaxis, "always"))))
  if (value_fmt != "none") {
    y_args$formatter <- if (value_fmt == "multiple")
      htmlwidgets::JS(sprintf("function(v){ return %s; }", .nt_js_number_expr("multiple","v")))
    else echarts4r::e_axis_formatter(.nt_fmt_style(value_fmt))
  }
  e <- do.call(echarts4r::e_y_axis, y_args)

  # tooltip: dark bubble showing (x, y). A time x prints as a local date.
  x_expr <- if (x_is_time) "new Date(+a[0]).toLocaleDateString()" else .nt_js_number_expr(x_fmt, "a[0]")
  tip <- htmlwidgets::JS(sprintf(paste0(
    "function(p){ var a=p.value; if(!Array.isArray(a)) return '';",
    " var xs=(%s), ys=(%s);",
    " var dot='<span style=\"display:inline-block;width:8px;height:8px;border-radius:50%%;margin-right:6px;background:'+(p.color||'#19222C')+'\"></span>';",
    " return '<div>'+dot+(p.seriesName||'')+'</div>'+",
    "   '<div style=\"display:flex;justify-content:space-between;gap:18px\"><span>x</span><span style=\"font-weight:600\">'+xs+'</span></div>'+",
    "   '<div style=\"display:flex;justify-content:space-between;gap:18px\"><span>y</span><span style=\"font-weight:600\">'+ys+'</span></div>'; }"),
    x_expr, .nt_js_number_expr(value_fmt, "a[1]")))
  e <- echarts4r::e_tooltip(e, trigger = "item",
                            backgroundColor = .ern_brand$navy, borderWidth = 0,
                            textStyle = list(color = "#ffffff", fontSize = 12,
                                             fontFamily = .ern_font),
                            formatter = tip)
  e <- echarts4r::e_legend(e, show = isTRUE(legend) && grouped,
                           textStyle = list(fontFamily = .ern_font))
  e <- echarts4r::e_grid(e, left = 58, right = 28, top = 30, bottom = 40)
  if (!is.null(source)) e <- .nt_add_source(e, source)
  if (!is.null(title)) {
    e <- echarts4r::e_title(e, text = title, subtext = subtext %nt||% "",
                            textStyle = list(fontFamily = .ern_font,
                                             color = .ern_brand$navy, fontWeight = 600))
  }
  if (identical(yaxis, "hover")) e <- htmlwidgets::onRender(e, .nt_axis_hover_js("yAxis"))
  e
}
