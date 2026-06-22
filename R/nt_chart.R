#' Interactive editorial chart (ERN house style)
#'
#' @description
#' Builds an interactive line, area, or bar chart in the Eviction Research
#' Network "newspaper graphic" style — the look of the Washington and Minnesota
#' state-profile charts. Hovering reveals the scale: a crosshair tracks the
#' cursor and **the value is printed on the y-axis** where you point, alongside a
#' tooltip. The axes are otherwise minimal (no chart-junk), so the data leads.
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
#' The signature interaction — a crosshair that draws to the axes and shows the
#' value on the y-axis on hover — is ECharts' `axisPointer` of `type = "cross"`,
#' enabled by `crosshair = TRUE` (the default). Editorial touches map to
#' arguments:
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
#' @param smooth Logical; smooth lines/areas (default `TRUE` for line/area,
#'   `FALSE` for bars).
#' @param crosshair Logical; the hover crosshair + on-axis value (default
#'   `TRUE`).
#' @param yaxis When the y-axis scale is shown: `"always"` (default; labels
#'   always visible), `"hover"` (the "newspaper graphic" style used in the ERN
#'   state profiles — the y-axis labels and dashed gridlines are hidden until the
#'   reader hovers the chart, then appear), or `"none"` (never; rely on the
#'   hover crosshair value alone).
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
#' @param source Optional short attribution string shown small in the lower-right
#'   corner (e.g. a data-source credit line).
#' @param legend Logical; show a legend (defaults to `TRUE` when `group` is set,
#'   and to `FALSE` when `end_label = TRUE`).
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
                     smooth = NULL,
                     crosshair = TRUE,
                     yaxis = c("always", "hover", "none"),
                     highlight_last = FALSE,
                     end_label = FALSE,
                     line_styles = NULL,
                     source = NULL,
                     legend = !is.null(group),
                     title = NULL, subtext = NULL,
                     x_title = NULL, y_title = NULL,
                     height = NULL, ...) {
  type <- match.arg(type)
  value_fmt <- match.arg(value_fmt)
  yaxis <- match.arg(yaxis)
  # Direct end-labels replace the legend, so hide it unless the caller asked for
  # one explicitly.
  if (isTRUE(end_label) && missing(legend)) legend <- FALSE
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  for (col in c(x, y, group)) {
    if (!is.null(col) && !col %in% names(data)) {
      stop(sprintf("Column \"%s\" not found in the data.", col), call. = FALSE)
    }
  }
  if (is.null(smooth)) smooth <- type != "bar"
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
    area = echarts4r::e_area_(e, y, smooth = smooth, symbol = "none", ...),
    bar  = if (!is.null(stack)) echarts4r::e_bar_(e, y, stack = stack, ...)
           else echarts4r::e_bar_(e, y, ...)
  )

  e <- .nt_ern_chart_style(e, pal, value_fmt, crosshair, legend,
                           x_title, y_title, !is.null(baseline) || !is.null(band),
                           yaxis = yaxis, end_label = isTRUE(end_label))

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
      (!is.null(line_styles) || isTRUE(end_label) ||
       (!is.null(names(pal)) && any(nzchar(names(pal)))))) {
    e <- .nt_style_series(e, line_styles, pal, isTRUE(end_label), value_fmt)
  }
  # "hover" y-axis: reveal the labels + dashed gridlines only while the cursor is
  # over the chart (the ERN newspaper-graphic style).
  if (identical(yaxis, "hover")) {
    e <- htmlwidgets::onRender(e, .nt_yaxis_hover_js())
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
                                x_title, y_title, has_marks, yaxis = "always",
                                end_label = FALSE) {
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
                              .nt_js_number_expr("multiple", "v")))
    } else {
      echarts4r::e_axis_formatter(.nt_fmt_style(value_fmt))
    }
    # format the boxed value the crosshair prints on the y-axis
    y_args$axisPointer <- list(label = list(formatter = .nt_pointer_js(value_fmt)))
  }
  e <- do.call(echarts4r::e_y_axis, y_args)

  # tooltip: dark navy bubble, crosshair axisPointer, ERN-formatted values
  tip_args <- list(
    e, trigger = "axis",
    backgroundColor = .ern_brand$navy, borderWidth = 0,
    textStyle = list(color = "#ffffff", fontSize = 12, fontFamily = .ern_font),
    formatter = .nt_tooltip_js(value_fmt)
  )
  if (isTRUE(crosshair)) {
    tip_args$axisPointer <- list(
      type = "cross",
      lineStyle = list(color = .ern_brand$accent, width = 1, type = "dashed"),
      crossStyle = list(color = .ern_brand$accent, width = 1, type = "dashed"),
      label = list(backgroundColor = .ern_brand$navy)
    )
  }
  e <- do.call(echarts4r::e_tooltip, tip_args)

  e <- echarts4r::e_legend(e, show = isTRUE(legend), textStyle = list(fontFamily = .ern_font))
  # leave room on the right so baseline / end-of-line / series labels are not
  # clipped (end labels like "Black · 33.5" need the most).
  right <- if (isTRUE(end_label)) 124 else if (has_marks) 84 else 28
  e <- echarts4r::e_grid(e, left = 58, right = right, top = 30, bottom = 40)
  e
}

# Map our value_fmt to echarts4r::e_axis_formatter style.
.nt_fmt_style <- function(value_fmt) {
  switch(value_fmt, comma = "decimal", percent = "percent", currency = "currency",
         "decimal")
}

# JS to format the boxed crosshair value on the y-axis only (leave x alone).
.nt_pointer_js <- function(value_fmt) {
  fmt <- .nt_js_number_expr(value_fmt, "p.value")
  htmlwidgets::JS(sprintf(
    "function(p){ return p.axisDimension === 'y' ? (%s) : p.value; }", fmt
  ))
}

# JS axis-trigger tooltip formatter: dark bubble, header = axis label, one
# "series  value" row per series, no color-dot dependence (matches the ERN
# hand-built tooltips and stays legible on the dark background).
.nt_tooltip_js <- function(value_fmt) {
  fmt <- .nt_js_number_expr(value_fmt, "val")
  htmlwidgets::JS(sprintf(
    paste0(
      "function(params){",
      "  if(!params || !params.length){ return ''; }",
      "  var head = params[0].axisValueLabel || params[0].name || '';",
      "  var rows = params.map(function(p){",
      "    var val = Array.isArray(p.value) ? p.value[p.value.length-1] : p.value;",
      "    if(val === null || val === undefined){ val = NaN; }",
      "    var fv = isNaN(+val) ? '\\u2014' : (%s);",
      "    var dot = '<span style=\"display:inline-block;width:8px;height:8px;border-radius:50%%;margin-right:6px;background:'+p.color+'\"></span>';",
      "    return '<div style=\"display:flex;justify-content:space-between;gap:18px;line-height:1.5\">'+",
      "      '<span>'+dot+(p.seriesName||'')+'</span>'+",
      "      '<span style=\"font-weight:600\">'+fv+'</span></div>';",
      "  }).join('');",
      "  return '<div style=\"font-size:11px;opacity:0.85;margin-bottom:2px\">'+head+'</div>'+rows;",
      "}"
    ),
    fmt
  ))
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

# A JS expression (string) formatting the numeric variable `var_name` per fmt.
# NOTE: "percent" expects a proportion in [0, 1] and multiplies by 100, to match
# echarts4r::e_axis_formatter("percent") (Intl percent style) used on the y-axis
# and the package-wide convention in nt_popup(). Keeping the axis ticks and the
# on-hover value in agreement is essential — they format the same data.
.nt_js_number_expr <- function(value_fmt, var_name) {
  switch(
    value_fmt,
    comma    = sprintf("(+%s).toLocaleString()", var_name),
    currency = sprintf("'$' + (+%s).toLocaleString()", var_name),
    percent  = sprintf("(+%s * 100).toFixed(1) + '%%'", var_name),
    multiple = sprintf("(+%s).toFixed(2) + '\\u00d7'", var_name),
    none     = var_name,
    sprintf("(+%s).toLocaleString()", var_name)
  )
}

# Add a small lower-right attribution. ECharts stores `title` as an array of
# title objects, so a source credit is just one more title positioned bottom-right.
.nt_add_source <- function(e, source) {
  src <- list(
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
  e
}

# Editorial per-series styling, applied by post-processing the serialized series
# (echarts4r has no per-series API for grouped charts). For each line series:
#   * line_styles  -> the dash pattern (recycled, or matched by series name)
#   * a named palette -> the line/marker color for that group
#   * end_label    -> a direct "<name> · <value>" label at the line end
.nt_style_series <- function(e, line_styles, palette, end_label, value_fmt) {
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
      key <- if (!is.null(st_names) && nzchar(nm) && nm %in% st_names) {
        styles[[which(st_names == nm)[1]]]
      } else styles[[((li - 1L) %% length(styles)) + 1L]]
      ls$type <- key
    }
    if (!is.null(col)) ls$color <- col
    if (length(ls)) s$lineStyle <- ls
    if (!is.null(col)) {
      it <- s$itemStyle %nt||% list()
      it$color <- col
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
