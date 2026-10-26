#' Interactive editorial chart (ERN house style)
#'
#' @description
#' Builds an interactive line, area, or bar chart in the Eviction Research
#' Network "newspaper graphic" style — the look of the Washington and Minnesota
#' state-profile charts. Hovering reads the data out: a line or area chart
#' gets a quiet vertical guide and a tooltip with each series' value at that
#' point, and a bar chart highlights the whole hovered bar and lists every
#' value in it. The axes are otherwise minimal (no chart-junk), so the data
#' leads.
#'
#' It is a thin, opinionated wrapper over the
#' [echarts4r](https://echarts4r.john-coene.com/) package (by John Coene), which
#' bundles Apache ECharts. As with the mapping functions, it returns the
#' underlying `echarts4r` widget, so you can keep piping `echarts4r::e_*`
#' functions to customize anything it doesn't expose. Charts are `htmlwidgets`:
#' they display in RStudio, Quarto/R Markdown, and Shiny, and save to standalone
#' HTML with [htmlwidgets::saveWidget()].
#'
#' @details
#' A full crosshair — drawn to both axes with the value printed on the y-axis
#' where you point — remains available as ECharts' `axisPointer` of
#' `type = "cross"` via `crosshair = TRUE`, but it is opt-in, not the default:
#' ordinary hover uses the guide-and-tooltip behavior above. Editorial touches
#' map to arguments:
#'
#' * `baseline` draws a dashed horizontal reference line (e.g. a pre-pandemic
#'   average, or a `1.0` parity line on a ratio chart), like the state-profile
#'   charts.
#' * `band` shades a vertical region between two x values (e.g. an eviction
#'   moratorium window).
#' * `highlight_last` marks the most recent point in the accent color (for a
#'   grouped chart, the leading series — the one with the largest latest value).
#' * `end_label` writes a direct label at the end of each line ("Black · 33.5"),
#'   the editorial alternative to a legend.
#' * `line_styles` varies the dash pattern per series for print/grayscale
#'   legibility, and `source` prints a small attribution in the lower-right —
#'   together these reproduce the look of the published profile plates.
#'
#' @param data A data frame (one row per x value, or per x × group).
#' @param x Name of the column for the x-axis (a date, year, or category).
#' @param y Name of the numeric column to plot.
#' @param group Optional column name; one series per group (e.g. race, county).
#' @param type One of `"line"`, `"area"`, or `"bar"`.
#' @param stack Optional group key to stack bars/areas (pass a string, commonly
#'   the same as `group` or `"total"`).
#' @param baseline Optional numeric y value for a dashed reference line.
#' @param baseline_label Optional label for the baseline.
#' @param band Optional length-2 vector `c(start, end)` (same type as `x`) to
#'   shade a vertical band.
#' @param band_label Optional label for the band.
#' @param palette Series colors; defaults to the ERN palette.
#' @param value_fmt How to format values on the y-axis, the hover crosshair, and
#'   the tooltip (all kept in agreement): one of `"comma"` (default), `"percent"`,
#'   `"currency"`, `"multiple"`, or `"none"`. **`"percent"` expects proportions in
#'   `[0, 1]`** (e.g. `0.25` renders as `25%`), matching [nt_popup()] and
#'   `scales::percent()`. `"multiple"` renders a ratio as e.g. `1.52×` (useful for
#'   a parity/disparity chart).
#' @param digits Decimal places for formatted values (tooltip, crosshair,
#'   axis ticks, bar labels). Default `NULL` uses per-format defaults:
#'   0 for `"comma"`/`"currency"`, 1 for `"percent"`, 2 for `"multiple"`.
#' @param tooltip_count Optional name of a column in `data` holding the
#'   count behind each plotted value. When set, the tooltip appends it in
#'   parentheses — e.g. a `value_fmt = "percent"` chart shows
#'   `43.1% (2,183 households)`. Pair with `tooltip_count_suffix`. On a
#'   stacked chart the tooltip also closes with a `Total` row, the counts
#'   summed across the bar's segments.
#' @param tooltip_count_suffix Text appended after the count inside the
#'   parentheses (e.g. `" households"`, `" people"`). Default `""`.
#' @param tooltip_trigger `"axis"` (default) shows one tooltip listing every
#'   series at the hovered x-position; `"item"` shows a tooltip only while
#'   hovering a line or bar itself — the right choice for line charts with
#'   many series.
#' @param isolate Logical; when `TRUE`, double-clicking a series — its line
#'   or its end-of-line label — isolates it and dims the rest. The isolated
#'   series pops to full opacity even if `series_opacity` mutes it by
#'   default; double-click it again (or empty space) to restore every series
#'   to its default opacity. Line charts with a `group`.
#' @param bar_labels Logical; print each bar segment's value inside the bar
#'   (`value_fmt`/`digits` formatting). A label that doesn't fit its segment
#'   is hidden — except on the stack's LAST series (the right end of a
#'   flipped bar, the top of a column), where it moves just past the end of
#'   the bar instead. White numerals with a dark halo stay legible inside
#'   dark bars, inside pale bars, and outside on white. Bars only.
#' @param flip Logical; draw bars horizontally (categories on the y-axis).
#'   Prefer this over piping [echarts4r::e_flip_coords()] afterwards — the
#'   engine then knows the orientation, so `bar_labels` overflow placement
#'   and the hover highlight land correctly.
#' @param smooth Logical; smooth lines/areas (default `TRUE` for line/area,
#'   `FALSE` for bars).
#' @param crosshair Logical; add the full hover crosshair, drawn to both axes
#'   with boxed on-axis values (default `FALSE`). Left off, hovering still
#'   reads the data out: line/area charts get a quiet vertical guide plus the
#'   tooltip, and bar charts highlight the whole hovered bar with every value
#'   listed in the tooltip.
#' @param yaxis When the y-axis scale is shown: `"always"` (default; labels
#'   always visible), `"hover"` (the "newspaper graphic" style used in the ERN
#'   state profiles — the y-axis labels and dashed gridlines are hidden until the
#'   reader hovers the chart, then appear), or `"none"` (never; rely on the
#'   hover tooltip alone).
#' @param y_max Optional fixed maximum for the value axis. Normalized (100%)
#'   stacked percent bars are capped at 1 automatically — ECharts would
#'   otherwise pick the next "nice" tick and render a phantom 120% ceiling — so
#'   this is only needed to override that (or to cap a non-percent chart).
#' @param y_min Optional fixed minimum for the value axis. Auto-capped percent
#'   stacks (see `y_max`) are pinned to 0 automatically so an all-high stacked
#'   bar (e.g. every value 80-98%) starts at 0 rather than floating up to the
#'   data floor; supply this to override that or to set a floor elsewhere.
#' @param highlight_last Logical; mark the most recent point in the accent color.
#'   With a `group`, the leading series (largest latest value) is accented.
#' @param end_label Logical; draw a direct label at the end of each line — the
#'   series name and its latest value (e.g. `"Black · 33.5"`) in the series color.
#'   This is the editorial alternative to a legend, so the legend is hidden by
#'   default when `end_label = TRUE`. Line/area charts only.
#' @param line_styles Optional character vector of dash styles recycled across
#'   series — any of `"solid"`, `"dashed"`, `"dotted"` — for print- and
#'   grayscale-friendly differentiation. If the vector is named, names are matched
#'   to series (group) names; otherwise styles are applied in series order.
#' @param series_opacity Optional named numeric vector (series name → 0–1)
#'   setting each named series' default line opacity; unnamed series stay at
#'   full ink. The editorial move for a one-versus-the-region line chart:
#'   mute the context series so the highlighted one carries the chart, and
#'   pair with `isolate = TRUE`, which pops a double-clicked series to full
#'   opacity and restores these defaults on release. Line charts only.
#' @param source Optional attribution shown small in the lower-right corner
#'   (e.g. a data-source credit line). A credit too long for one line is wrapped
#'   to the chart width when the page renders, and the plot area gives up the
#'   rows it needs, so a full method note stays on the canvas at any width.
#' @param legend Logical; show a legend (defaults to `TRUE` when `group` is set,
#'   and to `FALSE` when `end_label = TRUE`).
#' @param legend_pos Where the legend sits: `"top"` (default) centers it in the
#'   strip between the title and the plot, and `"bottom"` centers it beneath the
#'   plot and reserves bottom space so the x-axis labels, the legend, and the
#'   `source` credit each get their own row. Either way the plot gives up the
#'   room, rather than the legend being drawn over the chart. (Piping `e_grid()`
#'   after the fact does NOT work — echarts4r appends a second inert grid
#'   instead of editing this one.)
#' @param legend_nudge Pixel offsets to fine-tune the legend from wherever
#'   `legend_pos` put it: `c(x, y)` with `x > 0` shifting right and `y > 0`
#'   shifting down (a single number is taken as `y`). E.g.
#'   `legend_pos = "bottom", legend_nudge = -6` lifts the legend 6px.
#' @param title,subtext Optional chart title and subtitle.
#' @param x_title,y_title Optional axis titles.
#' @param height Optional CSS height (e.g. `"380px"`).
#' @param ... Passed to the underlying series function
#'   ([echarts4r::e_line()] / [echarts4r::e_bar()] / [echarts4r::e_area()]).
#' @return An `echarts4r`/`htmlwidget` object.
#' @family ern_charts
#' @seealso [nt_spark()] for inline sparklines;
#'   [echarts4r::e_charts()] and [echarts4r::e_tooltip()] underneath.
#' @examplesIf interactive() && requireNamespace("echarts4r", quietly = TRUE)
#' filings <- data.frame(
#'   month   = seq(as.Date("2019-01-01"), by = "month", length.out = 60),
#'   filings = round(1000 + 200 * sin(seq_len(60) / 6))
#' )
#'
#' # Monthly trend with a pre-pandemic baseline and a moratorium band
#' nt_chart(filings, "month", "filings", type = "line",
#'          baseline = mean(filings$filings[1:12]), baseline_label = "Pre-2020 avg",
#'          band = c(as.Date("2020-03-01"), as.Date("2021-06-01")),
#'          band_label = "Moratorium", highlight_last = TRUE)
#' @export
nt_chart <- function(data, x, y, group = NULL,
                     type = c("line", "area", "bar"),
                     stack = NULL,
                     baseline = NULL, baseline_label = NULL,
                     band = NULL, band_label = NULL,
                     palette = NULL,
                     value_fmt = c("comma", "percent", "currency", "multiple", "none"),
                     digits = NULL,
                     tooltip_count = NULL,
                     tooltip_count_suffix = "",
                     tooltip_trigger = c("axis", "item"),
                     isolate = FALSE,
                     bar_labels = FALSE,
                     flip = FALSE,
                     smooth = NULL,
                     crosshair = FALSE,
                     yaxis = c("always", "hover", "none"),
                     y_max = NULL,
                     y_min = NULL,
                     highlight_last = FALSE,
                     end_label = FALSE,
                     line_styles = NULL,
                     series_opacity = NULL,
                     source = NULL,
                     legend = !is.null(group),
                     legend_pos = c("top", "bottom"),
                     legend_nudge = c(0, 0),
                     title = NULL, subtext = NULL,
                     x_title = NULL, y_title = NULL,
                     height = NULL, ...) {
  type <- match.arg(type)
  value_fmt <- match.arg(value_fmt)
  yaxis <- match.arg(yaxis)
  legend_pos <- match.arg(legend_pos)
  tooltip_trigger <- match.arg(tooltip_trigger)
  # length-1 nudge = vertical; full form is c(x, y)
  if (length(legend_nudge) == 1L) legend_nudge <- c(0, legend_nudge)
  stopifnot("`legend_nudge` must be numeric c(x, y)" =
              is.numeric(legend_nudge) && length(legend_nudge) == 2L)
  # Direct end-labels replace the legend, so hide it unless the caller asked for
  # one explicitly.
  if (isTRUE(end_label) && missing(legend)) legend <- FALSE
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  for (col in c(x, y, group, tooltip_count)) {
    if (!is.null(col) && !col %in% names(data)) {
      stop(sprintf("Column \"%s\" not found in the data.", col), call. = FALSE)
    }
  }
  if (is.null(smooth)) smooth <- type != "bar"

  # Normalize the value axis for 100% stacked percent bars. Two fixes, same gate:
  #   * max -> 1: ECharts otherwise rounds up to the next "nice" tick, so a chart
  #     whose stacks sum to exactly 1 renders a phantom 120% ceiling.
  #   * min -> 0: the value axis carries scale = TRUE (so non-percent charts can
  #     zoom), which otherwise lets the min float up to the data floor -- an
  #     all-high stacked bar (e.g. every value 80-98%) would start at 78%, not 0.
  # Fires only for a stacked percent chart whose tallest stack is <= ~100%;
  # explicit y_max / y_min each win independently (so a caller can pin the max at
  # 1 and still get the auto 0 baseline), and stacks that legitimately exceed
  # 100% (max stack > 1) are left alone.
  if (value_fmt == "percent" && !is.null(stack) && (is.null(y_max) || is.null(y_min))) {
    yv <- suppressWarnings(as.numeric(data[[y]]))
    if (length(yv) && any(is.finite(yv)) && all(yv >= -1e-9, na.rm = TRUE)) {
      stack_sums <- tapply(yv, as.character(data[[x]]), sum, na.rm = TRUE)
      top <- suppressWarnings(max(stack_sums, na.rm = TRUE))
      if (is.finite(top) && top > 0 && top <= 1.02) {
        if (is.null(y_max)) y_max <- 1
        if (is.null(y_min)) y_min <- 0
      }
    }
  }

  pal <- palette %nt||% .nt_chart_palette

  # Axis type: dates -> time axis (continuous); everything else -> category.
  # Discrete numeric x (e.g. years) must be categorical, otherwise ECharts puts
  # it on a 0-based value axis and the points bunch up. Coerce to an ordered
  # factor so the category axis keeps numeric order.
  x_is_time <- inherits(data[[x]], c("Date", "POSIXct", "POSIXt"))
  if (!x_is_time && is.numeric(data[[x]])) {
    lvls <- as.character(sort(unique(data[[x]])))
    data[[x]] <- factor(as.character(data[[x]]), levels = lvls)
  }

  d <- data
  if (!is.null(group)) d <- dplyr::group_by(d, .data[[group]])
  e <- echarts4r::e_charts_(d, x, height = height)
  e <- switch(
    type,
    line = echarts4r::e_line_(e, y, smooth = smooth, symbol = "none", ...),
    area = if (!is.null(stack))
             echarts4r::e_area_(e, y, smooth = smooth, symbol = "none", stack = stack, ...)
           else echarts4r::e_area_(e, y, smooth = smooth, symbol = "none", ...),
    bar  = {
      # Optional in-bar value labels. Placement rules: keep inside when the
      # segment fits the text; if it doesn't fit, the LAST series' label
      # moves just past the bar end and every other series' label hides
      # (the tooltip still carries the value).
      lab <- if (isTRUE(bar_labels)) {
        n_series <- if (is.null(group)) 1L else length(unique(data[[group]]))
        outside <- if (isTRUE(flip)) {
          "return {x: r.x + r.width + 6, align: 'left'};"
        } else {
          "return {y: r.y - 4, verticalAlign: 'bottom'};"
        }
        list(
          label = list(
            show = TRUE, position = "inside", fontSize = 10,
            fontFamily = .ern_font, color = "#fff",
            textBorderColor = "rgba(25,34,44,0.9)", textBorderWidth = 2,
            formatter = htmlwidgets::JS(sprintf(
              "function(p){ var val = %s; return isNaN(+val) ? '' : (%s); }",
              .nt_js_value_pick(), .nt_js_number_expr(value_fmt, "val", digits)))
          ),
          labelLayout = htmlwidgets::JS(paste0(
            "function(params){",
            "  var r = params.rect || {}, lr = params.labelRect || {};",
            "  var fits = (lr.width + 8 <= (r.width || 0)) && (lr.height + 2 <= (r.height || 0));",
            "  if (fits) { return {hideOverlap: true}; }",
            "  if (params.seriesIndex === ", n_series - 1L, ") { ", outside, " }",
            "  return {fontSize: 0};",
            "}"))
        )
      } else list()
      do.call(echarts4r::e_bar_,
              c(list(e, y), if (!is.null(stack)) list(stack = stack),
                lab, list(...)))
    }
  )

  # Reserve grid headroom for the title so tall marks (e.g. stacked-100% bars)
  # never run into it.
  grid_top <- .nt_title_head(title, subtext)

  # Counts behind the plotted values, for the tooltip. Series name falls back to
  # the y column for ungrouped charts, matching echarts4r's naming. A stacked
  # chart also gets its bar total, which is what the segments add up to.
  count_map <- NULL
  total_map <- NULL
  if (!is.null(tooltip_count)) {
    count_map <- .nt_count_map(
      data[[x]], if (!is.null(group)) data[[group]] else y, data[[tooltip_count]])
    if (!is.null(stack)) total_map <- .nt_total_map(data[[x]], data[[tooltip_count]])
  }

  e <- .nt_ern_chart_style(e, pal, value_fmt, crosshair, legend,
                           x_title, y_title, !is.null(baseline) || !is.null(band),
                           chart_type = type, stacked = !is.null(stack),
                           yaxis = yaxis, y_max = y_max, y_min = y_min,
                           end_label = isTRUE(end_label),
                           legend_pos = legend_pos, legend_nudge = legend_nudge,
                           grid_top = grid_top, digits = digits,
                           tooltip_count = count_map,
                           tooltip_count_suffix = tooltip_count_suffix,
                           tooltip_total = total_map,
                           tooltip_trigger = tooltip_trigger)

  if (!is.null(baseline)) {
    e <- echarts4r::e_mark_line(
      e, data = list(yAxis = baseline),
      lineStyle = list(color = .ern_brand$muted, type = "dashed", width = 1),
      label = list(
        formatter = baseline_label %nt||% "",
        position = "insideStartTop", color = .ern_brand$muted, fontFamily = .ern_font
      ),
      symbol = "none", silent = TRUE
    )
  }
  if (!is.null(band)) {
    if (length(band) != 2L) stop("`band` must be length 2: c(start, end).", call. = FALSE)
    # On a category axis (everything except a Date/time x), the band edges must
    # match the category VALUES as strings; numeric years were coerced to a
    # character category above, so coerce the band the same way.
    if (!x_is_time) band <- as.character(band)
    e <- echarts4r::e_mark_area(
      e,
      data = list(
        list(xAxis = band[[1]], name = band_label %nt||% NULL),
        list(xAxis = band[[2]])
      ),
      itemStyle = list(color = "rgba(25,34,44,0.05)"),
      label = list(color = .ern_brand$muted, fontFamily = .ern_font, position = "top"),
      silent = TRUE
    )
  }
  if (isTRUE(highlight_last)) {
    if (is.null(group)) {
      last_row <- data[nrow(data), , drop = FALSE]
      e <- echarts4r::e_mark_point(
        e, data = list(coord = list(last_row[[x]], last_row[[y]])),
        itemStyle = list(color = .ern_brand$accent), symbol = "circle",
        symbolSize = 8, label = list(show = FALSE)
      )
    } else {
      # Accent the leading series: the group whose latest value is largest.
      last_by <- dplyr::ungroup(dplyr::slice_tail(
        dplyr::group_by(dplyr::arrange(data, .data[[x]]), .data[[group]]), n = 1))
      if (nrow(last_by)) {
        lead <- last_by[which.max(last_by[[y]]), , drop = FALSE]
        e <- echarts4r::e_mark_point(
          e, serie = as.character(lead[[group]]),
          data = list(coord = list(lead[[x]], lead[[y]])),
          itemStyle = list(color = .ern_brand$accent), symbol = "circle",
          symbolSize = 8, label = list(show = FALSE)
        )
      }
    }
  }
  if (!is.null(title)) {
    e <- echarts4r::e_title(e, text = title, subtext = subtext %nt||% "",
                            textStyle = list(fontFamily = .ern_font,
                                             color = .ern_brand$navy, fontWeight = 600))
  }
  if (!is.null(source)) e <- .nt_add_source(e, source)
  # Editorial per-series touches: varied dash patterns, named-palette colors,
  # and direct end-of-line labels. Applied last so the mark functions above
  # don't clobber them.
  if (type %in% c("line", "area") &&
      (!is.null(line_styles) || !is.null(series_opacity) || isTRUE(end_label) ||
       (!is.null(names(pal)) && any(nzchar(names(pal)))))) {
    e <- .nt_style_series(e, line_styles, pal, isTRUE(end_label), value_fmt,
                          series_opacity)
  }
  # "hover" y-axis: reveal the labels + dashed gridlines only while the cursor is
  # over the chart (the ERN newspaper-graphic style).
  if (identical(yaxis, "hover")) {
    e <- htmlwidgets::onRender(e, .nt_yaxis_hover_js())
  }
  # double-click a series (line or end label) to isolate it; repeat or
  # double-click empty space to restore
  if (isTRUE(isolate)) {
    e <- htmlwidgets::onRender(e, .nt_isolate_js())
  }
  if (isTRUE(flip)) {
    e <- echarts4r::e_flip_coords(e)
  }
  e
}

#' Inline sparkline (ERN house style)
#'
#' @description
#' A tiny, axis-free line or bar chart for embedding in stat cards or table
#' cells — the small "trend at a glance" graphic used beside headline numbers in
#' the state profiles.
#'
#' @param values A numeric vector (the series).
#' @param type `"line"` (default) or `"bar"`.
#' @param color Line/bar color (default ERN navy).
#' @param height CSS height (default `"40px"`).
#' @param width CSS width (default `"120px"`).
#' @return An `echarts4r`/`htmlwidget` object.
#' @family ern_charts
#' @seealso [nt_chart()] for full charts.
#' @examplesIf interactive() && requireNamespace("echarts4r", quietly = TRUE)
#' nt_spark(c(3, 5, 4, 6, 8, 7, 9, 12), type = "bar")
#' @export
nt_spark <- function(values, type = c("line", "bar"),
                     color = "#19222C", height = "40px", width = "120px") {
  type <- match.arg(type)
  df <- data.frame(i = seq_along(values), v = as.numeric(values))
  e <- echarts4r::e_charts_(df, "i", height = height, width = width)
  e <- if (type == "bar") {
    echarts4r::e_bar_(e, "v", legend = FALSE,
                      itemStyle = list(color = color))
  } else {
    echarts4r::e_line_(e, "v", legend = FALSE, symbol = "none", smooth = TRUE,
                       lineStyle = list(color = color, width = 1.5),
                       areaStyle = list(color = "rgba(25,34,44,0.06)"))
  }
  e <- echarts4r::e_x_axis(e, show = FALSE)
  e <- echarts4r::e_y_axis(e, show = FALSE, scale = TRUE)
  e <- echarts4r::e_legend(e, show = FALSE)
  e <- echarts4r::e_grid(e, left = 1, right = 1, top = 2, bottom = 2)
  e <- echarts4r::e_color(e, color, background = "rgba(0,0,0,0)")
  echarts4r::e_tooltip(e, trigger = "axis")
}

# ---------------------------------------------------------------------------
# Internal: apply the ERN editorial chart style (axes, tooltip, crosshair,
# colors, grid). Kept here so nt_chart() and future chart functions agree.
# ---------------------------------------------------------------------------
.nt_ern_chart_style <- function(e, pal, value_fmt, crosshair, legend,
                                x_title, y_title, has_marks,
                                chart_type = "line", yaxis = "always",
                                y_max = NULL, y_min = NULL,
                                end_label = FALSE, legend_pos = "top",
                                legend_nudge = c(0, 0), grid_top = 30,
                                digits = NULL, tooltip_count = NULL,
                                tooltip_count_suffix = "",
                                tooltip_total = NULL,
                                tooltip_trigger = "axis",
                                stacked = FALSE) {
  axis_label <- list(color = .ern_brand$muted, fontFamily = .ern_font, fontSize = 11)
  # y-axis labels start hidden for "hover" (revealed on hover via onRender) and
  # for "none"; always shown for "always".
  y_label <- c(axis_label, list(show = identical(yaxis, "always")))

  e <- echarts4r::e_color(e, pal, background = "rgba(0,0,0,0)")

  # x axis: thin hairline, no ticks
  e <- echarts4r::e_x_axis(
    e,
    axisLine  = list(lineStyle = list(color = .ern_brand$muted, width = 0.5)),
    axisTick  = list(show = FALSE),
    axisLabel = axis_label,
    name = x_title %nt||% "", nameLocation = "middle", nameGap = 28,
    nameTextStyle = list(color = .ern_brand$muted, fontFamily = .ern_font)
  )

  # y axis: no axis line, no persistent gridlines (revealed on hover via the
  # crosshair). Values formatted per value_fmt.
  y_args <- list(
    e,
    splitLine = list(show = FALSE),
    axisLine  = list(show = FALSE),
    axisTick  = list(show = FALSE),
    axisLabel = y_label,
    scale = TRUE,
    name = y_title %nt||% "", nameLocation = "end", nameGap = 12,
    nameTextStyle = list(color = .ern_brand$muted, fontFamily = .ern_font, align = "left")
  )
  if (value_fmt != "none") {
    # "multiple" has no Intl style, so format it with a small JS function;
    # everything else uses echarts4r's Intl-backed axis formatter.
    y_args$formatter <- if (value_fmt == "multiple") {
      htmlwidgets::JS(sprintf("function(v){ return %s; }",
                              .nt_js_number_expr("multiple", "v", digits)))
    } else if (is.null(digits)) {
      echarts4r::e_axis_formatter(.nt_fmt_style(value_fmt))
    } else {
      echarts4r::e_axis_formatter(.nt_fmt_style(value_fmt), digits = digits)
    }
    # format the boxed value the crosshair prints on the value axis — only the
    # opt-in crosshair draws to the value axis, so the label rides with it
    if (isTRUE(crosshair))
      y_args$axisPointer <- list(label = list(formatter = .nt_pointer_js(value_fmt, digits)))
  }
  if (!is.null(y_max)) y_args$max <- y_max
  if (!is.null(y_min)) y_args$min <- y_min
  e <- do.call(echarts4r::e_y_axis, y_args)

  # tooltip: dark navy bubble, crosshair axisPointer, ERN-formatted values.
  # "item" trigger fires only on the hovered series (no crosshair).
  tip_args <- list(
    e, trigger = tooltip_trigger,
    # confine keeps the tooltip inside the chart's own box: these widgets ship
    # as iframes, and a tooltip past an iframe edge is clipped mid-word (seen
    # 2026-09-06 on the two-panel overcrowding figure)
    confine = TRUE,
    backgroundColor = .ern_brand$navy, borderWidth = 0,
    textStyle = list(color = "#ffffff", fontSize = 12, fontFamily = .ern_font),
    formatter = if (identical(tooltip_trigger, "item")) {
      .nt_tooltip_item_js(value_fmt, digits)
    } else {
      # `stacked` is the argument, threaded from the caller's real stack
      # state — the old `!is.null(stack)` here resolved to base::stack
      # lexically (never NULL), which reversed EVERY axis tooltip as if
      # stacked (caught 2026-09-06, the rents-chart row-order report)
      .nt_tooltip_js(value_fmt, digits, tooltip_count, tooltip_count_suffix,
                     tooltip_total, stacked = stacked)
    }
  )
  if (identical(tooltip_trigger, "axis")) {
    tip_args$axisPointer <- if (isTRUE(crosshair)) {
      # the opt-in crosshair: dashed accent lines to both axes, boxed values
      list(
        type = "cross",
        lineStyle = list(color = .ern_brand$accent, width = 1, type = "dashed"),
        crossStyle = list(color = .ern_brand$accent, width = 1, type = "dashed"),
        label = list(backgroundColor = .ern_brand$navy)
      )
    } else if (identical(chart_type, "bar")) {
      # the whole hovered bar (or bar cluster) is shaded, and the axis tooltip
      # lists every value in it — works flipped or not, since ECharts hangs the
      # shadow on the category axis either way
      list(type = "shadow",
           shadowStyle = list(color = "rgba(25,34,44,0.07)"),
           label = list(show = FALSE))
    } else {
      # line/area: a quiet vertical guide marks the hovered x; the values live
      # in the tooltip, with no boxed on-axis readouts
      list(type = "line",
           lineStyle = list(color = .ern_brand$muted, width = 1, type = "dashed"),
           label = list(show = FALSE))
    }
  }
  e <- do.call(echarts4r::e_tooltip, tip_args)

  # Legend below the plot gets its own row between the x-axis labels and the
  # source credit; the grid reserves the extra bottom space for all three.
  legend_below <- isTRUE(legend) && identical(legend_pos, "bottom")
  legend_args <- list(e, show = isTRUE(legend),
                      textStyle = list(fontFamily = .ern_font))
  if (legend_below) {
    legend_args$bottom <- 28 - legend_nudge[2]   # y > 0 nudges down
    legend_args$left <- "center"
  } else {
    # A top legend is placed explicitly, never left to the echarts default:
    # ECharts 6 moved that default to bottom-center, where the legend lands on
    # the x-axis labels. It sits in the strip below the title (grid_top already
    # carries the title's headroom), and the plot gives up that row.
    legend_args$top <- grid_top + legend_nudge[2]
    legend_args$left <- "center"
    if (isTRUE(legend)) grid_top <- grid_top + .nt_legend_row
  }
  if (legend_nudge[1] != 0) {
    # A centered legend can't take an x offset directly; asymmetric padding
    # shifts its contents (echarts padding order: top, right, bottom, left).
    legend_args$padding <- c(5, 5 + 2 * max(-legend_nudge[1], 0),
                             5, 5 + 2 * max(legend_nudge[1], 0))
  }
  e <- do.call(echarts4r::e_legend, legend_args)
  # leave room on the right so baseline / end-of-line / series labels are not
  # clipped (end labels like "San Francisco · $2,999" need the most).
  right <- if (isTRUE(end_label)) 185 else if (has_marks) 84 else 28
  e <- echarts4r::e_grid(e, left = 58, right = right, top = grid_top,
                         bottom = if (legend_below) 96 else 40)
  e
}

# Map our value_fmt to echarts4r::e_axis_formatter style.
.nt_fmt_style <- function(value_fmt) {
  switch(value_fmt, comma = "decimal", percent = "percent", currency = "currency",
         "decimal")
}

# JS to format the boxed crosshair value on the VALUE axis. The formatter is
# attached only to the value-axis config, and e_flip_coords() carries it along
# when a chart is flipped horizontal — so no dimension check: format any
# numeric, pass anything else through raw.
.nt_pointer_js <- function(value_fmt, digits = NULL) {
  fmt <- .nt_js_number_expr(value_fmt, "p.value", digits)
  htmlwidgets::JS(sprintf(
    "function(p){ return isNaN(+p.value) ? p.value : (%s); }", fmt
  ))
}

# JS axis-trigger tooltip formatter: dark bubble, header = axis label, one
# "series  value" row per series, no color-dot dependence (matches the ERN
# hand-built tooltips and stays legible on the dark background).
#
# On a stacked chart the rows are reversed. echarts hands the formatter its
# params in series order, which is the order the stack is built from the
# axis outward, so an unreversed list reads bottom-to-top against the bar
# the reader is pointing at. Unstacked charts keep series order, which is
# also the legend's order.
.nt_tooltip_js <- function(value_fmt, digits = NULL, count_map = NULL,
                           count_suffix = "", total_map = NULL,
                           stacked = FALSE) {
  fmt <- .nt_js_number_expr(value_fmt, "val", digits)
  # Optional "(2,183 households)" appended after the formatted value. The
  # counts arrive as a lookup keyed "category__series" — echarts4r series
  # rows carry only (x, y), so extra columns can't ride along in p.value.
  # Built with paste0 and passed through sprintf as a VALUE (never as part
  # of the format string): category labels like "<30% AMI" carry literal
  # percent signs that would break sprintf parsing.
  count_js <- if (is.null(count_map)) "" else paste0(
    "    var __ck = (p.name || p.axisValueLabel || '') + '__' + (p.seriesName || '');",
    "    var __cv = (", count_map, ")[__ck];",
    "    if (__cv !== undefined && __cv !== null && !isNaN(+__cv)) {",
    "      fv += ' (' + (+__cv).toLocaleString(undefined, {maximumFractionDigits: 0}) + '",
    count_suffix, ")';",
    "    }"
  )
  # A stacked chart's whole point is the part-to-whole, so when the counts are
  # supplied the tooltip closes with the bar's total — the number every reader
  # of a 100% stack asks for next (issue #13).
  total_js <- if (is.null(total_map)) "''" else paste0(
    "(function(){",
    "  var __tv = (", total_map, ")[head];",
    "  if(__tv === undefined || __tv === null || isNaN(+__tv)) return '';",
    "  return '<div style=\"display:flex;justify-content:space-between;gap:18px;",
    "line-height:1.5;margin-top:3px;padding-top:3px;border-top:1px solid rgba(255,255,255,0.25)\">'+",
    "    '<span>Total</span>'+",
    "    '<span style=\"font-weight:600\">'+(+__tv).toLocaleString(undefined, {maximumFractionDigits: 0})+'",
    count_suffix, "</span></div>';",
    "})()"
  )
  htmlwidgets::JS(sprintf(
    paste0(
      "function(params){",
      "  if(!params || !params.length){ return ''; }",
      "  var head = params[0].axisValueLabel || params[0].name || '';",
      "  function __v(p){ var val = ", .nt_js_value_pick(), ";",
      "    return (val === null || val === undefined || isNaN(+val)) ? -Infinity : +val; }",
      # rows mirror the chart's vertical order at the hovered point: a stacked
      # bar reads top segment first (series order reversed), everything else
      # sorts by value so who's on top in the plot is on top in the tip
      "  var rows = ",
      if (stacked) "params.slice().reverse()"
      else "params.slice().sort(function(a, b){ return __v(b) - __v(a); })",
      ".map(function(p){",
      "    var val = ", .nt_js_value_pick(), ";",
      "    if(val === null || val === undefined){ val = NaN; }",
      "    var fv = isNaN(+val) ? '\\u2014' : (%s);",
      "%s",
      "    var dot = '<span style=\"display:inline-block;width:8px;height:8px;border-radius:50%%;margin-right:6px;background:'+p.color+'\"></span>';",
      "    return '<div style=\"display:flex;justify-content:space-between;gap:18px;line-height:1.5\">'+",
      "      '<span>'+dot+(p.seriesName||'')+'</span>'+",
      "      '<span style=\"font-weight:600\">'+fv+'</span></div>';",
      "  }).join('');",
      "  return '<div style=\"font-size:11px;opacity:0.85;margin-bottom:2px\">'+head+'</div>'+rows+(%s);",
      "}"
    ),
    fmt, count_js, total_js
  ))
}

# Counts behind the plotted values, keyed "category__series" for a tooltip
# lookup: echarts4r series rows carry only (x, y), so extra columns cannot ride
# along in p.value. Returns JSON (a JS object literal) or NULL for no counts.
.nt_count_map <- function(cats, series, counts) {
  keys <- paste0(as.character(cats), "__", as.character(series))
  jsonlite::toJSON(stats::setNames(as.list(round(as.numeric(counts))), keys),
                   auto_unbox = TRUE)
}

# Stack totals for the same lookup, keyed by category alone.
.nt_total_map <- function(cats, counts) {
  tot <- tapply(as.numeric(counts), as.character(cats), sum, na.rm = TRUE)
  jsonlite::toJSON(stats::setNames(as.list(round(unname(tot))), names(tot)),
                   auto_unbox = TRUE)
}

# Tooltip for trigger = "item": one series at a time, headed by the hovered
# point's x (dates render as "Mar 2021"; categories pass through).
.nt_tooltip_item_js <- function(value_fmt, digits = NULL) {
  fmt <- .nt_js_number_expr(value_fmt, "val", digits)
  htmlwidgets::JS(paste0(
    "function(p){",
    "  var val = ", .nt_js_value_pick(), ";",
    "  var x = Array.isArray(p.value) ? p.value[0] : (p.name || '');",
    "  var d = new Date(x);",
    "  var head = (x && !isNaN(d.getTime()) && String(x).length > 4) ?",
    "    d.toLocaleDateString(undefined, {month: 'short', year: 'numeric'}) : String(p.name || x || '');",
    "  var fv = (val === null || val === undefined || isNaN(+val)) ? '\\u2014' : (", fmt, ");",
    "  var dot = '<span style=\"display:inline-block;width:8px;height:8px;border-radius:50%;margin-right:6px;background:'+p.color+'\"></span>';",
    "  return '<div style=\"font-size:11px;opacity:0.85;margin-bottom:2px\">'+head+'</div>'+",
    "    '<div style=\"display:flex;justify-content:space-between;gap:18px;line-height:1.5\">'+",
    "    '<span>'+dot+(p.seriesName||'')+'</span>'+",
    "    '<span style=\"font-weight:600\">'+fv+'</span></div>';",
    "}"
  ))
}

# JS (htmlwidgets::onRender) for isolate = TRUE: double-click a series (its
# line or its end label) to pop it to full ink and dim every other series;
# double-click it again — or empty canvas — to restore. Original end-label
# colors AND line opacities are stashed on the first use, so a chart whose
# context series start muted (series_opacity) returns to exactly that state
# rather than everything snapping to full ink.
.nt_isolate_js <- function() {
  paste0(
    "function(el, x){\n",
    "  var inst = (window.echarts && echarts.getInstanceByDom) ? echarts.getInstanceByDom(el) : null;\n",
    "  if(!inst){ var c = el.querySelector('div'); if(c && window.echarts) inst = echarts.getInstanceByDom(c); }\n",
    "  if(!inst) return;\n",
    "  var isolated = null, elColors = null, baseOp = null;\n",
    "  function apply(){\n",
    "    var series = (inst.getOption().series || []);\n",
    "    if(!elColors) elColors = series.map(function(s){ return s.endLabel ? (s.endLabel.color || null) : null; });\n",
    "    if(!baseOp) baseOp = series.map(function(s){ return (s.lineStyle && s.lineStyle.opacity != null) ? s.lineStyle.opacity : 1; });\n",
    "    series.forEach(function(s, i){\n",
    "      var dim = isolated !== null && s.name !== isolated;\n",
    "      var op = isolated === null ? baseOp[i] : (dim ? 0.12 : 1);\n",
    "      s.lineStyle = s.lineStyle || {};\n",
    "      s.lineStyle.opacity = op;\n",
    "      s.itemStyle = s.itemStyle || {};\n",
    "      s.itemStyle.opacity = op;\n",
    "      if(s.endLabel){ s.endLabel.color = dim ? 'rgba(25,34,44,0.18)' : elColors[i]; }\n",
    "    });\n",
    "    inst.setOption({series: series});\n",
    "  }\n",
    "  inst.on('dblclick', function(p){\n",
    "    if(p.componentType !== 'series' || !p.seriesName) return;\n",
    "    isolated = (isolated === p.seriesName) ? null : p.seriesName;\n",
    "    apply();\n",
    "  });\n",
    "  inst.getZr().on('dblclick', function(ev){\n",
    "    if(!ev.target && isolated !== null){ isolated = null; apply(); }\n",
    "  });\n",
    "}"
  )
}

# JS (htmlwidgets::onRender) that reveals the y-axis labels + dashed gridlines
# only while the cursor is over the chart, then hides them again on leave.
.nt_yaxis_hover_js <- function() {
  paste0(
    "function(el, x){\n",
    "  var inst = (window.echarts && echarts.getInstanceByDom) ? echarts.getInstanceByDom(el) : null;\n",
    "  if(!inst){ var c = el.querySelector('div'); if(c && window.echarts) inst = echarts.getInstanceByDom(c); }\n",
    "  if(!inst) return;\n",
    "  var on = false;\n",
    "  function reveal(show){ if(show===on) return; on=show;\n",
    "    inst.setOption({ yAxis: { axisLabel: { show: show },\n",
    "      splitLine: { show: show, lineStyle: { color: '#19222C', type: 'dashed', opacity: 0.22 } } } });\n",
    "  }\n",
    "  el.addEventListener('mouseenter', function(){ reveal(true); });\n",
    "  el.addEventListener('mouseleave', function(){ reveal(false); });\n",
    "}"
  )
}

# A JS expression (string) picking the numeric value out of an echarts
# callback param `p`: dataset-bound series hand the whole data row to
# formatters as an array, and flipped (horizontal) charts move the value to
# the x encode. Falls back to the last numeric element.
.nt_js_value_pick <- function() {
  paste0(
    "(function(p){",
    "  var v = p.value;",
    "  if (Array.isArray(v)) {",
    "    var e = p.encode || {};",
    "    var a = (p.axisDim === 'y') ? (e.x || [])[0] : (e.y || [])[0];",
    "    if (a !== undefined && !isNaN(+v[a])) return v[a];",
    "    var b = (p.axisDim === 'y') ? (e.y || [])[0] : (e.x || [])[0];",
    "    if (b !== undefined && !isNaN(+v[b])) return v[b];",
    "    for (var i = v.length - 1; i >= 0; i--) { if (!isNaN(+v[i])) return v[i]; }",
    "    return NaN;",
    "  }",
    "  return v;",
    "})(p)"
  )
}

# A JS expression (string) formatting the numeric variable `var_name` per fmt.
# NOTE: "percent" expects a proportion in [0, 1] and multiplies by 100, to match
# echarts4r::e_axis_formatter("percent") (Intl percent style) used on the y-axis
# and the package-wide convention in nt_popup(). Keeping the axis ticks and the
# on-hover value in agreement is essential — they format the same data.
.nt_js_number_expr <- function(value_fmt, var_name, digits = NULL) {
  d <- digits %nt||% switch(value_fmt, percent = 1, multiple = 2, 0)
  loc <- sprintf("toLocaleString(undefined, {maximumFractionDigits: %d})", d)
  switch(
    value_fmt,
    comma    = sprintf("(+%s).%s", var_name, loc),
    currency = sprintf("((+%1$s)<0?'-':'')+'$'+Math.abs(+%1$s).%2$s", var_name, loc),
    percent  = sprintf("(+%s * 100).toFixed(%d) + '%%'", var_name, d),
    multiple = sprintf("(+%s).toFixed(%d) + '\\u00d7'", var_name, d),
    none     = var_name,
    sprintf("(+%s).%s", var_name, loc)
  )
}

# Add a small lower-right attribution. ECharts stores `title` as an array of
# title objects, so a source credit is just one more title positioned bottom-right.
# The id tags it for .nt_source_wrap_js(), which wraps long credits in the browser.
.nt_source_id <- "ern-source"

.nt_add_source <- function(e, source) {
  src <- list(
    id = .nt_source_id,
    text = source, right = 10, bottom = 8,
    textStyle = list(color = .ern_brand$muted, fontFamily = .ern_font,
                     fontSize = 10, fontWeight = "normal")
  )
  t <- e$x$opts$title
  if (is.null(t)) {
    e$x$opts$title <- list(src)
  } else {
    e$x$opts$title[[length(t) + 1L]] <- src
  }
  htmlwidgets::onRender(e, .nt_source_wrap_js())
}

# JS (htmlwidgets::onRender) that keeps the source credit inside the canvas.
# ECharts title text never wraps, so a credit wider than the chart runs off the
# left edge and is silently clipped — the front of the attribution just
# disappears. Only the browser knows the chart's pixel width, so the wrap
# happens here: measure the credit in its own font, break it greedily to the
# canvas width, then reserve the extra rows by pushing the grid (and a
# bottom-anchored legend) up by a line each. A credit that already fits is left
# exactly where it was, right-anchored; a wrapped one switches to left-anchored
# so the block is not ragged on its left. Re-runs whenever the chart resizes.
.nt_source_wrap_js <- function() {
  paste0(
    "function(el, x){\n",
    "  var inst = (window.echarts && echarts.getInstanceByDom) ? echarts.getInstanceByDom(el) : null;\n",
    "  if(!inst){ var c = el.querySelector('div'); if(c && window.echarts) inst = echarts.getInstanceByDom(c); }\n",
    "  if(!inst) return;\n",
    "  var ID = '", .nt_source_id, "', PAD = 10, LINE = 13, GAP = 8;\n",
    "  var opt = inst.getOption(), titles = opt.title || [], idx = -1;\n",
    "  for(var i = 0; i < titles.length; i++){ if(titles[i].id === ID) idx = i; }\n",
    "  if(idx < 0) return;\n",
    "  var ts = titles[idx].textStyle || {}, raw = titles[idx].text || '';\n",
    "  var size = ts.fontSize || 10;\n",
    "  var font = (ts.fontWeight || 'normal') + ' ' + size + 'px ' + (ts.fontFamily || 'sans-serif');\n",
    # The grid and a bottom-anchored legend are only nudged from where the chart
    # function put them, so stash those before the first wrap. ECharts 6 gives
    # every legend a numeric `bottom` by default, hidden ones included, so a
    # legend only counts as bottom-anchored if it is shown and has no `top`.
    "  var g = (opt.grid || [])[0] || {}, lg = (opt.legend || [])[0] || {};\n",
    "  var gridBase = (typeof g.bottom === 'number') ? g.bottom : null;\n",
    "  var legBase = (lg.show !== false && typeof lg.bottom === 'number' &&\n",
    "                 (lg.top === null || lg.top === undefined || lg.top === 'auto')) ? lg.bottom : null;\n",
    "  var ctx = document.createElement('canvas').getContext('2d');\n",
    "  function wrap(text, max){\n",
    "    ctx.font = font;\n",
    "    var out = [];\n",
    "    String(text).split('\\n').forEach(function(para){\n",
    "      var words = para.split(/\\s+/).filter(function(w){ return w.length; }), line = '';\n",
    "      for(var i = 0; i < words.length; i++){\n",
    "        var trial = line ? line + ' ' + words[i] : words[i];\n",
    "        if(!line || ctx.measureText(trial).width <= max){ line = trial; }\n",
    "        else { out.push(line); line = words[i]; }\n",
    # One token wider than the whole canvas (a long URL, say): break it by
    # character rather than let it overflow.
    "        while(line.length > 1 && ctx.measureText(line).width > max){\n",
    "          var cut = line.length;\n",
    "          while(cut > 1 && ctx.measureText(line.slice(0, cut)).width > max) cut--;\n",
    "          out.push(line.slice(0, cut)); line = line.slice(cut);\n",
    "        }\n",
    "      }\n",
    "      if(line.length) out.push(line);\n",
    "    });\n",
    "    return out.length ? out : [''];\n",
    "  }\n",
    "  var last = raw;\n",
    "  function layout(){\n",
    "    if(inst.isDisposed && inst.isDisposed()) return;\n",
    "    var w = inst.getWidth();\n",
    "    if(!w || w <= 2 * PAD) return;\n",   # hidden container: retry on resize
    "    var lines = wrap(raw, w - 2 * PAD), text = lines.join('\\n');\n",
    "    if(text === last) return;\n",
    "    last = text;\n",
    "    var t = {id: ID, text: text, bottom: PAD - 2,\n",
    "             left: lines.length > 1 ? PAD : null, right: lines.length > 1 ? null : PAD,\n",
    "             textAlign: 'left',\n",
    "             textStyle: {color: ts.color, fontFamily: ts.fontFamily, fontSize: size,\n",
    "                         fontWeight: ts.fontWeight, lineHeight: LINE}};\n",
    # A one-line credit already fits the space the chart reserved, so it moves
    # nothing. Each wrapped line costs a row, plus GAP once, because that
    # reserved space runs flush against the x-axis labels and a stacked block of
    # small type needs to read as separate from them.
    "    var patch = {title: t},\n",
    "        extra = lines.length > 1 ? (lines.length - 1) * LINE + GAP : 0;\n",
    "    if(gridBase !== null) patch.grid = {bottom: gridBase + extra};\n",
    "    if(legBase !== null) patch.legend = {bottom: legBase + extra};\n",
    "    inst.setOption(patch);\n",
    "  }\n",
    "  layout();\n",
    # Shiny re-renders into the same element, so drop the previous watcher
    # first — otherwise every re-render leaves one behind, holding the credit
    # it was built with, and they fight over the title on the next resize.
    "  if(el.ernSourceObs){ el.ernSourceObs.disconnect(); el.ernSourceObs = null; }\n",
    "  if(el.ernSourceWin){ window.removeEventListener('resize', el.ernSourceWin); }\n",
    "  if(window.ResizeObserver){\n",
    "    el.ernSourceObs = new ResizeObserver(layout); el.ernSourceObs.observe(el);\n",
    "  } else {\n",
    "    el.ernSourceWin = layout; window.addEventListener('resize', layout);\n",
    "  }\n",
    "}"
  )
}

# Editorial per-series styling, applied by post-processing the serialized series
# (echarts4r has no per-series API for grouped charts). For each line series:
#   * line_styles  -> the dash pattern (recycled, or matched by series name)
#   * a named palette -> the line/marker color for that group
#   * end_label    -> a direct "<name> · <value>" label at the line end
.nt_style_series <- function(e, line_styles, palette, end_label, value_fmt,
                             series_opacity = NULL) {
  series <- e$x$opts$series
  if (is.null(series) || !length(series)) return(e)
  styles    <- if (is.null(line_styles)) NULL else as.character(line_styles)
  st_names  <- names(line_styles)
  named_pal <- if (!is.null(names(palette)) && any(nzchar(names(palette)))) palette else NULL
  end_js <- htmlwidgets::JS(sprintf(
    paste0("function(p){ var v = Array.isArray(p.value) ? p.value[p.value.length-1] : p.value;",
           " if(v===null||v===undefined||isNaN(+v)) return p.seriesName||'';",
           " var fv = (%s); return (p.seriesName ? p.seriesName + ' \\u00b7 ' : '') + fv; }"),
    .nt_js_number_expr(value_fmt, "v")))

  li <- 0L
  for (i in seq_along(series)) {
    s <- series[[i]]
    if (!identical(s$type, "line")) next   # echarts4r area series are also type "line"
    li <- li + 1L
    nm  <- s$name %nt||% ""
    col <- if (!is.null(named_pal) && nzchar(nm) && nm %in% names(named_pal)) {
      unname(named_pal[[nm]])
    } else NULL
    ls <- s$lineStyle %nt||% list()
    if (!is.null(styles)) {
      # A NAMED line_styles vector styles only the series it names — everyone
      # else stays solid. Unnamed vectors recycle across series in order.
      key <- if (!is.null(st_names) && any(nzchar(st_names))) {
        if (nzchar(nm) && nm %in% st_names) styles[[which(st_names == nm)[1]]] else "solid"
      } else styles[[((li - 1L) %% length(styles)) + 1L]]
      ls$type <- key
    }
    if (!is.null(col)) ls$color <- col
    # default per-series muting (series_opacity); the isolate JS stashes and
    # restores these, so an isolate cycle can never strand a series at the
    # wrong ink
    op <- if (!is.null(series_opacity) && nzchar(nm) &&
              nm %in% names(series_opacity)) {
      as.numeric(series_opacity[[nm]])
    } else NULL
    if (!is.null(op)) ls$opacity <- op
    if (length(ls)) s$lineStyle <- ls
    if (!is.null(col) || !is.null(op)) {
      it <- s$itemStyle %nt||% list()
      if (!is.null(col)) it$color <- col
      if (!is.null(op))  it$opacity <- op
      s$itemStyle <- it
    }
    if (isTRUE(end_label)) {
      s$endLabel <- list(show = TRUE, formatter = end_js,
                         color = col %nt||% "inherit", fontFamily = .ern_font,
                         fontSize = 12, fontWeight = 600)
      s$labelLayout <- list(moveOverlap = "shiftY")  # nudge crowded end labels apart
    }
    series[[i]] <- s
  }
  e$x$opts$series <- series
  e
}
