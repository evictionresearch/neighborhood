# Slope chart in the ERN house style: one line per series connecting two (or a
# few) periods, with direct labels at both ends instead of a legend. The spare,
# "let the change speak" form for before/after comparisons (2019 -> 2024).
# Built on echarts4r; assistance from Claude Opus 4.8 (Anthropic).

# Value formatter for a line series (the numeric sits last in an [x, y] pair).
.nt_line_value_js <- function(value_fmt) {
  fmt <- .nt_js_number_expr(value_fmt, "v")
  htmlwidgets::JS(sprintf(paste0(
    "function(p){ var raw=p.value; var v=Array.isArray(raw)?raw[raw.length-1]:raw;",
    " if(v===null||v===undefined||isNaN(+v)) return ''; return (%s); }"), fmt))
}

#' Slope chart (ERN house style)
#'
#' @description
#' A slope (or "parallel coordinates") chart: each `group` is a line connecting
#' its value across the periods on `x` — typically two (a before and an after).
#' Direct labels at the line ends replace the legend, and a highlighted series
#' can be accented while the rest recede. This is the spare editorial form for
#' "what changed between 2019 and 2024" comparisons.
#'
#' @param data A data frame in long form: one row per `group` × `x`.
#' @param x Name of the period column (2+ ordered values, e.g. years).
#' @param y Name of the numeric value column.
#' @param group Name of the series column (one line each).
#' @param highlight Optional `group` values to accent (a character vector of
#'   names, or numeric positions); the rest are drawn in a muted steel. If `NULL`
#'   or `FALSE`, the full ERN palette is applied to every line. (Unlike the
#'   ranked charts, where `NULL` accents the leader, a slope chart has no single
#'   leader to accent.)
#' @param palette Optional series colors (named vector matched to group names
#'   takes precedence over `highlight`).
#' @param value_fmt Value formatting for the end labels and hover (see
#'   [nt_bar()]).
#' @param value_labels Logical; label each line's value at both ends (default
#'   `TRUE`).
#' @param yaxis Value-axis scale visibility; defaults to `"none"` (the values
#'   live in the direct labels, so the scale is usually redundant).
#' @param title,subtext,source,height Standard editorial extras (see [nt_bar()]).
#' @param ... Passed to [echarts4r::e_line()].
#' @return An `echarts4r`/`htmlwidget` object.
#' @family ern_charts
#' @seealso [nt_chart()] for many-period trends; [nt_dumbbell()] for gaps.
#' @examplesIf interactive() && requireNamespace("echarts4r", quietly = TRUE)
#' chg <- data.frame(
#'   place = rep(c("Hennepin", "Ramsey", "Dakota"), each = 2),
#'   year  = rep(c(2019, 2024), 3),
#'   rent  = c(1180, 1490, 1090, 1380, 1320, 1610)
#' )
#' nt_slope(chg, "year", "rent", group = "place",
#'          value_fmt = "currency", highlight = "Ramsey")
#' @export
nt_slope <- function(data, x, y, group,
                     highlight = NULL,
                     palette = NULL,
                     value_fmt = c("comma", "percent", "currency", "multiple", "none"),
                     value_labels = TRUE,
                     yaxis = c("none", "hover", "always"),
                     title = NULL, subtext = NULL, source = NULL,
                     height = NULL, ...) {
  value_fmt <- match.arg(value_fmt)
  yaxis     <- match.arg(yaxis)
  .nt_check_cols(data, c(x, y, group))

  d <- as.data.frame(data, stringsAsFactors = FALSE)
  # ordered period axis: dates/numbers keep their order; otherwise first-seen
  if (!inherits(d[[x]], c("Date", "POSIXct", "POSIXt"))) {
    lv <- if (is.numeric(d[[x]])) as.character(sort(unique(d[[x]])))
          else unique(as.character(d[[x]]))
    d[[x]] <- factor(as.character(d[[x]]), levels = lv)
  }

  groups <- unique(as.character(d[[group]]))
  # build the per-series palette: explicit palette wins; else highlight scheme;
  # else the default qualitative ramp (named so .nt_style_series colors by name).
  if (!is.null(palette)) {
    # a named palette that matches no group would silently fall back to ECharts'
    # default theme (off-brand); warn and use the house ramp instead.
    if (!is.null(names(palette)) && any(nzchar(names(palette))) &&
        !length(intersect(names(palette), groups))) {
      warning("`palette` names match no group; using the default ERN ramp.",
              call. = FALSE)
      pal <- stats::setNames(rep(.nt_chart_palette, length.out = length(groups)), groups)
    } else {
      pal <- palette
    }
  } else if (!is.null(highlight) && !isFALSE(highlight)) {
    hl <- if (is.numeric(highlight)) groups[highlight] else as.character(highlight)
    pal <- stats::setNames(
      ifelse(groups %in% hl, .ern_brand$accent, .ern_brand$steel), groups)
  } else {
    pal <- stats::setNames(rep(.nt_chart_palette, length.out = length(groups)), groups)
  }

  dd <- dplyr::group_by(d, .data[[group]])
  e <- echarts4r::e_charts_(dd, x, height = height)
  e <- echarts4r::e_line_(e, y, symbol = "circle", symbolSize = 7,
                          smooth = FALSE, ...)

  e <- .nt_ern_chart_style(e, pal, value_fmt, crosshair = FALSE, legend = FALSE,
                           x_title = NULL, y_title = NULL, has_marks = FALSE,
                           yaxis = yaxis, end_label = isTRUE(value_labels))
  # named-palette colors + "name · value" end labels (right end)
  e <- .nt_style_series(e, line_styles = NULL, palette = pal,
                        end_label = isTRUE(value_labels), value_fmt = value_fmt)

  # add the left-end value label on each line's first point
  if (isTRUE(value_labels)) {
    start_js <- .nt_line_value_js(value_fmt)
    for (i in seq_along(e$x$opts$series)) {
      s <- e$x$opts$series[[i]]
      if (!identical(s$type, "line") || !length(s$data)) next
      first <- s$data[[1]]
      v0 <- if (is.list(first) && !is.null(first$value)) first$value else first
      col <- s$lineStyle$color %nt||% .ern_brand$navy
      s$data[[1]] <- list(value = v0, label = list(
        show = TRUE, position = "left", formatter = start_js,
        color = col, fontFamily = .ern_font, fontSize = 11, fontWeight = 600))
      e$x$opts$series[[i]] <- s
    }
  }

  if (!is.null(title)) {
    e <- echarts4r::e_title(e, text = title, subtext = subtext %nt||% "",
                            textStyle = list(fontFamily = .ern_font,
                                             color = .ern_brand$navy, fontWeight = 600))
  }
  if (!is.null(source)) e <- .nt_add_source(e, source)
  if (identical(yaxis, "hover")) {
    e <- htmlwidgets::onRender(e, .nt_axis_hover_js("yAxis"))
  }
  e
}
