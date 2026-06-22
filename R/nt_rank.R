# Ranked categorical charts in the ERN "newspaper graphic" style: ranked /
# horizontal bars (nt_bar), lollipop / dot plots (nt_lollipop), and disparity
# dumbbells (nt_dumbbell). These share a value-vs-category axis styler that
# mirrors the look of nt_chart() but works in either orientation (the value axis
# can sit on x or y) and uses an item-trigger tooltip.
#
# Built with assistance from Claude Opus 4.8 (Anthropic). They are thin,
# deterministic wrappers over John Coene's echarts4r and require no AI to run.

# ---------------------------------------------------------------------------
# Shared internals
# ---------------------------------------------------------------------------

# Hover-reveal hook for an arbitrary axis ("xAxis" or "yAxis"): show that axis's
# labels + dashed gridlines only while the cursor is over the chart. Generalizes
# nt_chart()'s y-axis reveal so a *horizontal* chart can reveal its (x) value axis.
.nt_axis_hover_js <- function(axis = c("yAxis", "xAxis")) {
  axis <- match.arg(axis)
  paste0(
    "function(el, x){\n",
    "  var inst = (window.echarts && echarts.getInstanceByDom) ? echarts.getInstanceByDom(el) : null;\n",
    "  if(!inst){ var c = el.querySelector('div'); if(c && window.echarts) inst = echarts.getInstanceByDom(c); }\n",
    "  if(!inst) return;\n",
    "  var on=false;\n",
    "  function reveal(show){ if(show===on) return; on=show;\n",
    "    var o={}; o['", axis, "']={ axisLabel:{ show:show },\n",
    "      splitLine:{ show:show, lineStyle:{ color:'#19222C', type:'dashed', opacity:0.22 } } };\n",
    "    inst.setOption(o);\n",
    "  }\n",
    "  el.addEventListener('mouseenter', function(){ reveal(true); });\n",
    "  el.addEventListener('mouseleave', function(){ reveal(false); });\n",
    "}"
  )
}

# Crosshair boxed-value formatter for the value axis on dimension "x" or "y".
.nt_pointer_axis_js <- function(value_fmt, dim = "y") {
  fmt <- .nt_js_number_expr(value_fmt, "p.value")
  htmlwidgets::JS(sprintf(
    "function(p){ return p.axisDimension === '%s' ? (%s) : p.value; }", dim, fmt))
}

# Which element of a serialized echarts datum holds the numeric value. echarts4r
# encodes a single-series categorical point as [category, value] in the natural
# (vertical) layout, and as [value, category] after e_flip_coords() (horizontal).
# So the value sits at index 0 horizontally, index 1 vertically.
.nt_value_index <- function(orientation) if (orientation == "horizontal") 0L else 1L

# Item-trigger tooltip (dark navy bubble): one category name, one formatted value.
# Reads the value from the right index for the orientation (see .nt_value_index).
.nt_item_tooltip_js <- function(value_fmt, orientation = "horizontal") {
  idx <- .nt_value_index(orientation)
  fmt <- .nt_js_number_expr(value_fmt, "v")
  htmlwidgets::JS(sprintf(paste0(
    "function(p){",
    "  var raw = p.value; var v = Array.isArray(raw) ? raw[%d] : raw;",
    "  var fv = (v===null||v===undefined||isNaN(+v)) ? '\\u2014' : (%s);",
    "  var dot = '<span style=\"display:inline-block;width:8px;height:8px;border-radius:50%%;margin-right:6px;background:'+(p.color||'#19222C')+'\"></span>';",
    "  return '<div style=\"display:flex;justify-content:space-between;gap:18px;line-height:1.5\">'+",
    "    '<span>'+dot+(p.name||'')+'</span>'+",
    "    '<span style=\"font-weight:600\">'+fv+'</span></div>';",
    "}"), idx, fmt))
}

# Direct value-label formatter at the end of a bar / lollipop head.
.nt_value_label_js <- function(value_fmt, orientation = "horizontal") {
  idx <- .nt_value_index(orientation)
  fmt <- .nt_js_number_expr(value_fmt, "v")
  htmlwidgets::JS(sprintf(paste0(
    "function(p){ var raw=p.value; var v=Array.isArray(raw)?raw[%d]:raw;",
    " if(v===null||v===undefined||isNaN(+v)) return ''; return (%s); }"), idx, fmt))
}

# Apply the ERN editorial style to a categorical (value-vs-category) chart in
# either orientation. Sets the color, the two axes (thin category hairline; a
# clean value axis with the value_fmt formatter and on-hover crosshair), an
# item-trigger tooltip, a hidden legend, the grid, and an optional source.
.nt_cat_chart_style <- function(e, orientation, value_fmt, yaxis, pal,
                                tooltip = TRUE, legend = FALSE, source = NULL,
                                zero_base = FALSE,
                                grid_left = NULL, grid_right = NULL) {
  axis_label <- list(color = .ern_brand$muted, fontFamily = .ern_font, fontSize = 11)
  val_show   <- identical(yaxis, "always")

  e <- echarts4r::e_color(e, pal, background = "rgba(0,0,0,0)")

  cat_axis <- list(
    axisLine  = list(lineStyle = list(color = .ern_brand$muted, width = 0.5)),
    axisTick  = list(show = FALSE),
    axisLabel = axis_label
  )
  val_axis <- list(
    # faint dashed value gridlines accompany the labels when shown ("always");
    # hidden for "none", and toggled by the hover hook for "hover" (starts off).
    splitLine = list(show = val_show, lineStyle = list(color = .ern_brand$muted,
                                                       type = "dashed", opacity = 0.22)),
    axisLine  = list(show = FALSE),
    axisTick  = list(show = FALSE),
    axisLabel = c(axis_label, list(show = val_show)),
    # Filled bars MUST start at zero (honest area); dot/whisker charts may scale
    # to the data so differences are legible. `zero_base` distinguishes them.
    scale = !isTRUE(zero_base)
  )
  if (isTRUE(zero_base)) val_axis$min <- 0
  val_dim <- if (orientation == "horizontal") "x" else "y"
  if (value_fmt != "none") {
    val_axis$formatter <- if (value_fmt == "multiple") {
      htmlwidgets::JS(sprintf("function(v){ return %s; }",
                              .nt_js_number_expr("multiple", "v")))
    } else {
      echarts4r::e_axis_formatter(.nt_fmt_style(value_fmt))
    }
    val_axis$axisPointer <- list(label = list(
      formatter = .nt_pointer_axis_js(value_fmt, val_dim)))
  }

  if (orientation == "vertical") {
    e <- do.call(echarts4r::e_x_axis, c(list(e), cat_axis))
    e <- do.call(echarts4r::e_y_axis, c(list(e), val_axis))
  } else {
    e <- do.call(echarts4r::e_y_axis, c(list(e), cat_axis))
    e <- do.call(echarts4r::e_x_axis, c(list(e), val_axis))
  }

  if (isTRUE(tooltip)) {
    e <- echarts4r::e_tooltip(
      e, trigger = "item",
      backgroundColor = .ern_brand$navy, borderWidth = 0,
      textStyle = list(color = "#ffffff", fontSize = 12, fontFamily = .ern_font),
      formatter = .nt_item_tooltip_js(value_fmt, orientation))
  }
  e <- echarts4r::e_legend(e, show = isTRUE(legend),
                           textStyle = list(fontFamily = .ern_font))

  left  <- grid_left  %nt||% (if (orientation == "horizontal") 120 else 58)
  right <- grid_right %nt||% (if (orientation == "horizontal") 64 else 28)
  e <- echarts4r::e_grid(e, left = left, right = right, top = 30, bottom = 40)
  if (!is.null(source)) e <- .nt_add_source(e, source)
  e
}

# Repaint a series' points/bars one color per element. Reads the existing data
# (scalar in a vertical chart, a {value:[val,cat]} object after a flip) and
# wraps each as {value=<orig>, itemStyle=list(color=...)} so the i-th element
# (which carries its own category) is colored regardless of plotting order.
.nt_paint_points <- function(e, series_idx, colors) {
  s   <- e$x$opts$series[[series_idx]]
  dat <- s$data
  out <- vector("list", length(dat))
  for (i in seq_along(dat)) {
    di  <- dat[[i]]
    val <- if (is.list(di) && !is.null(di$value)) di$value else di
    out[[i]] <- list(value = val, itemStyle = list(color = colors[[i]]))
  }
  s$data <- out
  e$x$opts$series[[series_idx]] <- s
  e
}

# Which rows to accent. NULL -> the rank leader (max, or min for ascending sort;
# none when unsorted); FALSE -> none; character -> rows whose category matches;
# numeric -> those row indices.
.nt_resolve_highlight <- function(highlight, cats, vals, sort) {
  if (isFALSE(highlight)) return(integer(0))
  if (is.null(highlight)) {
    if (sort == "none") return(integer(0))
    return(if (sort == "asc") which.min(vals) else which.max(vals))
  }
  if (is.character(highlight)) return(which(as.character(cats) %in% highlight))
  if (is.numeric(highlight))   return(as.integer(highlight))
  integer(0)
}

# Sort, top-N, and set the category as an ordered factor so the bars/dots plot
# in rank order. Horizontal charts reverse the levels because a category axis
# places its first level at the bottom (we want the leader on top).
.nt_rank_prep <- function(data, x, y, sort, n, orientation) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (anyDuplicated(as.character(data[[x]]))) {
    stop(sprintf(paste0("Column \"%s\" has duplicate categories; aggregate to one ",
                        "row per category first (or pass `group=` for dodged bars)."), x),
         call. = FALSE)
  }
  if (sort != "none") {
    data <- data[order(data[[y]], decreasing = (sort == "desc")), , drop = FALSE]
  }
  if (!is.null(n)) data <- utils::head(data, n)
  cats <- as.character(data[[x]])
  lvls <- if (orientation == "horizontal") rev(cats) else cats
  data[[x]] <- factor(cats, levels = lvls)
  data
}

.nt_check_cols <- function(data, cols) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  for (col in cols) {
    if (!is.null(col) && !col %in% names(data)) {
      stop(sprintf("Column \"%s\" not found in the data.", col), call. = FALSE)
    }
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# nt_bar
# ---------------------------------------------------------------------------

#' Ranked / horizontal bar chart (ERN house style)
#'
#' @description
#' A categorical bar chart in the Eviction Research Network "newspaper graphic"
#' style — the look of the "top counties by filing rate" and "rate by race"
#' plates. Bars are ranked and (by default) horizontal so long category names
#' read cleanly; the leading bar is accented, each bar is labelled with its
#' value, and the value-axis scale can hide until the reader hovers
#' (`yaxis = "hover"`).
#'
#' Like [nt_chart()], it returns the underlying `echarts4r` widget, so you can
#' keep piping `echarts4r::e_*` functions to customize anything it doesn't
#' expose.
#'
#' @details
#' Three common forms fall out of the arguments:
#'
#' * **Ranked bars** (the default): `sort = "desc"`, optionally `n` to keep the
#'   top N, with the leader accented. Set `highlight` to accent specific
#'   categories instead (e.g. `highlight = "Black"` on a rate-by-race chart).
#' * **Diverging bars**: `diverging = TRUE` colors each bar by whether it sits
#'   above or below `baseline` (e.g. above/below the statewide average), and
#'   draws a dashed reference line there.
#' * **Grouped bars**: pass `group` for side-by-side (dodged) bars — e.g. rate
#'   by race *within* each county.
#'
#' @param data A data frame: one row per category (or per category × group).
#' @param x Name of the category column.
#' @param y Name of the numeric value column.
#' @param group Optional column; one (dodged) series per group.
#' @param orientation `"horizontal"` (default) or `"vertical"`.
#' @param sort `"desc"` (default), `"asc"`, or `"none"` — order categories by
#'   value. Ignored when `group` is set.
#' @param n Optional integer; keep only the top `n` categories after sorting.
#' @param highlight Which bars to accent. `NULL` (default) accents the rank
#'   leader; `FALSE` accents none; a character vector accents categories by
#'   name; a numeric vector accents those row positions. Ignored when `group`
#'   or `diverging` is set.
#' @param diverging Logical; color bars by their sign relative to `baseline`
#'   (above = accent, below = navy) and draw a dashed reference line. Good for
#'   "above/below average" charts.
#' @param baseline Optional numeric reference value (the divider for
#'   `diverging`, or just a dashed reference line otherwise).
#' @param baseline_label Optional label for the reference line.
#' @param palette Bar colors (defaults to the ERN palette). For grouped bars,
#'   one color per group (a named vector matches by group name).
#' @param value_fmt How to format values on the axis, hover crosshair, tooltip,
#'   and bar labels: `"comma"` (default), `"percent"` (expects `[0,1]`),
#'   `"currency"`, `"multiple"` (`1.52×`), or `"none"`.
#' @param value_labels Logical; print each bar's value at its end (default
#'   `TRUE`; suppressed for grouped bars).
#' @param legend Logical; show the series legend (only applies to grouped bars;
#'   default `TRUE`).
#' @param yaxis When the value-axis scale shows: `"always"` (default), `"hover"`
#'   (the newspaper style — hidden until the reader hovers), or `"none"`. The
#'   value axis always starts at zero (filled bars are honest about area).
#' @param bar_width Optional bar width (e.g. `"62%"` or a pixel number).
#' @param title,subtext Optional chart title and subtitle.
#' @param source Optional attribution shown small in the lower-right.
#' @param height Optional CSS height (e.g. `"420px"`).
#' @param ... Passed to [echarts4r::e_bar()].
#' @return An `echarts4r`/`htmlwidget` object.
#' @family ern_charts
#' @seealso [nt_lollipop()] for the dot-plot variant; [nt_chart()] for trends.
#' @examplesIf interactive() && requireNamespace("echarts4r", quietly = TRUE)
#' counties <- data.frame(
#'   county = c("Hennepin", "Ramsey", "Anoka", "Dakota", "Olmsted"),
#'   rate   = c(42.3, 38.1, 19.4, 22.7, 15.2)
#' )
#' nt_bar(counties, "county", "rate", value_fmt = "comma")
#' @export
nt_bar <- function(data, x, y, group = NULL,
                   orientation = c("horizontal", "vertical"),
                   sort = c("desc", "asc", "none"),
                   n = NULL,
                   highlight = NULL,
                   diverging = FALSE,
                   baseline = NULL, baseline_label = NULL,
                   palette = NULL,
                   value_fmt = c("comma", "percent", "currency", "multiple", "none"),
                   value_labels = TRUE, legend = TRUE,
                   yaxis = c("always", "hover", "none"),
                   bar_width = NULL,
                   title = NULL, subtext = NULL, source = NULL,
                   height = NULL, ...) {
  orientation <- match.arg(orientation)
  sort        <- match.arg(sort)
  value_fmt   <- match.arg(value_fmt)
  yaxis       <- match.arg(yaxis)
  .nt_check_cols(data, c(x, y, group))
  pal <- palette %nt||% .nt_chart_palette

  grouped <- !is.null(group)
  if (grouped) {
    d <- as.data.frame(data, stringsAsFactors = FALSE)
    # keep category order stable; reverse for horizontal so first reads on top
    cats <- unique(as.character(d[[x]]))
    d[[x]] <- factor(as.character(d[[x]]),
                     levels = if (orientation == "horizontal") rev(cats) else cats)
    d <- dplyr::group_by(d, .data[[group]])
    e <- echarts4r::e_charts_(d, x, height = height)
    e <- echarts4r::e_bar_(e, y, ...)
    if (orientation == "horizontal") e <- echarts4r::e_flip_coords(e)
    value_labels <- FALSE
  } else {
    d <- .nt_rank_prep(data, x, y, sort, n, orientation)
    e <- echarts4r::e_charts_(d, x, height = height)
    e <- echarts4r::e_bar_(e, y, barWidth = bar_width %nt||% "62%", ...)
    if (orientation == "horizontal") e <- echarts4r::e_flip_coords(e)

    vals <- d[[y]]
    if (isTRUE(diverging)) {
      ref  <- baseline %nt||% 0
      cols <- ifelse(vals >= ref, .ern_brand$accent, .ern_brand$navy_soft)
    } else {
      cols <- rep(.ern_brand$navy, length(vals))
      hl <- .nt_resolve_highlight(highlight, d[[x]], vals, sort)
      if (length(hl)) cols[hl] <- .ern_brand$accent
    }
    e <- .nt_paint_points(e, 1L, cols)

    if (isTRUE(value_labels)) {
      e$x$opts$series[[1]]$label <- list(
        show = TRUE,
        position = if (orientation == "horizontal") "right" else "top",
        formatter = .nt_value_label_js(value_fmt, orientation),
        color = .ern_brand$navy, fontFamily = .ern_font,
        fontSize = 11, fontWeight = 600)
    }
  }

  e <- .nt_cat_chart_style(e, orientation, value_fmt, yaxis, pal,
                           legend = grouped && isTRUE(legend), source = source,
                           zero_base = TRUE)

  if (isTRUE(diverging) || !is.null(baseline)) {
    ref <- baseline %nt||% 0
    coord <- if (orientation == "horizontal") list(xAxis = ref) else list(yAxis = ref)
    e <- echarts4r::e_mark_line(
      e, data = coord,
      lineStyle = list(color = .ern_brand$muted, type = "dashed", width = 1),
      label = list(formatter = baseline_label %nt||% "",
                   color = .ern_brand$muted, fontFamily = .ern_font),
      symbol = "none", silent = TRUE)
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

# ---------------------------------------------------------------------------
# nt_lollipop
# ---------------------------------------------------------------------------

#' Ranked lollipop / dot plot (ERN house style)
#'
#' @description
#' A leaner alternative to [nt_bar()] in the same editorial style: a dot at each
#' value, optionally on a thin stem from the axis (a lollipop). Dot plots carry
#' the same ranking as bars with far less ink — the W.E.B. Du Bois "let the data
#' stand" instinct — and read especially well for "indexed to the white rate"
#' disparity rankings.
#'
#' @inheritParams nt_bar
#' @param stem Logical; draw the thin stem from the axis to the dot (default
#'   `TRUE`). `FALSE` gives a pure dot plot.
#' @param dot_size Dot diameter in pixels (default `13`).
#' @param ... Passed to [echarts4r::e_scatter()] (the dots).
#' @return An `echarts4r`/`htmlwidget` object.
#' @family ern_charts
#' @seealso [nt_bar()] for solid bars; [nt_dumbbell()] for two-point gaps.
#' @examplesIf interactive() && requireNamespace("echarts4r", quietly = TRUE)
#' disp <- data.frame(
#'   race  = c("Black", "Latine", "Other", "White"),
#'   index = c(3.1, 1.8, 1.4, 1.0)
#' )
#' nt_lollipop(disp, "race", "index", value_fmt = "multiple",
#'             highlight = "Black", baseline = 1)
#' @export
nt_lollipop <- function(data, x, y,
                        orientation = c("horizontal", "vertical"),
                        sort = c("desc", "asc", "none"),
                        n = NULL,
                        highlight = NULL,
                        baseline = NULL, baseline_label = NULL,
                        palette = NULL,
                        value_fmt = c("comma", "percent", "currency", "multiple", "none"),
                        value_labels = TRUE,
                        yaxis = c("always", "hover", "none"),
                        stem = TRUE, dot_size = 13,
                        title = NULL, subtext = NULL, source = NULL,
                        height = NULL, ...) {
  orientation <- match.arg(orientation)
  sort        <- match.arg(sort)
  value_fmt   <- match.arg(value_fmt)
  yaxis       <- match.arg(yaxis)
  .nt_check_cols(data, c(x, y))
  pal <- palette %nt||% .nt_chart_palette

  d <- .nt_rank_prep(data, x, y, sort, n, orientation)
  vals <- d[[y]]
  cols <- rep(.ern_brand$navy, length(vals))
  hl <- .nt_resolve_highlight(highlight, d[[x]], vals, sort)
  if (length(hl)) cols[hl] <- .ern_brand$accent

  e <- echarts4r::e_charts_(d, x, height = height)
  # stems: a very thin bar from the axis to the value, drawn under the dots
  if (isTRUE(stem)) {
    e <- echarts4r::e_bar_(e, y, barWidth = 2, name = "stem",
                           itemStyle = list(color = .ern_brand$steel),
                           legend = FALSE)
  }
  e <- echarts4r::e_scatter_(e, y, symbol = "circle", symbolSize = dot_size,
                             name = "dot", legend = FALSE, ...)
  if (orientation == "horizontal") e <- echarts4r::e_flip_coords(e)

  dot_idx <- length(e$x$opts$series)   # scatter is the last series added
  e <- .nt_paint_points(e, dot_idx, cols)
  if (isTRUE(value_labels)) {
    e$x$opts$series[[dot_idx]]$label <- list(
      show = TRUE,
      position = if (orientation == "horizontal") "right" else "top",
      distance = 8,
      formatter = .nt_value_label_js(value_fmt, orientation),
      color = .ern_brand$navy, fontFamily = .ern_font,
      fontSize = 11, fontWeight = 600)
  }

  e <- .nt_cat_chart_style(e, orientation, value_fmt, yaxis, pal, source = source)

  if (!is.null(baseline)) {
    coord <- if (orientation == "horizontal") list(xAxis = baseline) else list(yAxis = baseline)
    e <- echarts4r::e_mark_line(
      e, data = coord,
      lineStyle = list(color = .ern_brand$muted, type = "dashed", width = 1),
      label = list(formatter = baseline_label %nt||% "",
                   color = .ern_brand$muted, fontFamily = .ern_font),
      symbol = "none", silent = TRUE)
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

# ---------------------------------------------------------------------------
# nt_dumbbell
# ---------------------------------------------------------------------------

#' Disparity dumbbell chart (ERN house style)
#'
#' @description
#' Two dots per category joined by a connecting bar — the cleanest way to show a
#' *gap* between two measures, which is the Eviction Research Network's core
#' visual idiom for racial disparities (e.g. the Black vs. white filing rate by
#' county) and for change between two periods (2019 vs. 2024). Categories are
#' sorted by the size of the gap so the widest disparities rise to the top.
#'
#' @param data A data frame: one row per category.
#' @param x Name of the category column.
#' @param low,high Names of the two value columns (the two ends of each
#'   dumbbell). `high` is the focus/right end (accented by default), `low` the
#'   comparison/left end.
#' @param low_label,high_label Series names for the two ends, shown in the
#'   legend, e.g. `"White"` and `"Black"`. (The tooltip shows the category and
#'   the value.)
#' @param sort `"gap"` (default; by `high - low`), `"high"`, `"low"`, or
#'   `"none"`.
#' @param n Optional integer; keep only the top `n` categories after sorting.
#' @param orientation `"horizontal"` (default) or `"vertical"`.
#' @param palette Length-2 colors for the `low` and `high` ends (defaults to a
#'   muted steel and the ERN accent). (`colors` is a deprecated alias.)
#' @param legend Logical; show the two-end legend (default `TRUE`).
#' @param colors Deprecated alias for `palette`.
#' @param value_fmt Value formatting (see [nt_bar()]).
#' @param value_labels Logical; label each end with its value (default `TRUE`).
#' @param yaxis Value-axis scale visibility (see [nt_bar()]).
#' @param dot_size Dot diameter in pixels (default `12`).
#' @param title,subtext,source,height Standard editorial extras (see [nt_bar()]).
#' @param ... Passed to [echarts4r::e_scatter()].
#' @return An `echarts4r`/`htmlwidget` object.
#' @family ern_charts
#' @seealso [nt_lollipop()] for a single-value ranked dot plot.
#' @examplesIf interactive() && requireNamespace("echarts4r", quietly = TRUE)
#' gap <- data.frame(
#'   county = c("Hennepin", "Ramsey", "Dakota", "Anoka"),
#'   white  = c(12, 14, 9, 8),
#'   black  = c(41, 38, 22, 19)
#' )
#' nt_dumbbell(gap, "county", low = "white", high = "black",
#'             low_label = "White", high_label = "Black")
#' @export
nt_dumbbell <- function(data, x, low, high,
                        low_label = NULL, high_label = NULL,
                        sort = c("gap", "high", "low", "none"),
                        n = NULL,
                        orientation = c("horizontal", "vertical"),
                        palette = NULL,
                        value_fmt = c("comma", "percent", "currency", "multiple", "none"),
                        value_labels = TRUE, legend = TRUE,
                        yaxis = c("always", "hover", "none"),
                        dot_size = 12,
                        title = NULL, subtext = NULL, source = NULL,
                        height = NULL, colors = NULL, ...) {
  sort        <- match.arg(sort)
  orientation <- match.arg(orientation)
  value_fmt   <- match.arg(value_fmt)
  yaxis       <- match.arg(yaxis)
  .nt_check_cols(data, c(x, low, high))

  if (!is.null(colors)) {                      # soft-deprecated alias
    if (is.null(palette)) palette <- colors
    warning("`colors` is deprecated in nt_dumbbell(); use `palette`.", call. = FALSE)
  }
  lo_lab <- low_label  %nt||% low
  hi_lab <- high_label %nt||% high
  cols <- palette %nt||% c(.ern_brand$steel, .ern_brand$accent)
  if (length(cols) < 2L) stop("`palette` must be length 2 (low, high).", call. = FALSE)

  d <- as.data.frame(data, stringsAsFactors = FALSE)
  key <- switch(sort,
                gap  = d[[high]] - d[[low]],
                high = d[[high]],
                low  = d[[low]],
                none = NULL)
  if (!is.null(key)) d <- d[order(key, decreasing = TRUE), , drop = FALSE]
  if (!is.null(n)) d <- utils::head(d, n)

  cats <- as.character(d[[x]])
  if (anyDuplicated(cats)) {
    stop(sprintf("Column \"%s\" has duplicate categories; aggregate to one row per category first.", x),
         call. = FALSE)
  }
  lvls <- if (orientation == "horizontal") rev(cats) else cats
  d[[x]] <- factor(cats, levels = lvls)

  # connector segments: one two-point line per category. The coord order must
  # match the FINAL axis layout — [value, category] after a horizontal flip,
  # [category, value] otherwise.
  seg <- lapply(seq_len(nrow(d)), function(i) {
    cat_i <- as.character(d[[x]])[i]
    if (orientation == "horizontal") {
      list(list(coord = list(d[[low]][i],  cat_i)),
           list(coord = list(d[[high]][i], cat_i)))
    } else {
      list(list(coord = list(cat_i, d[[low]][i])),
           list(coord = list(cat_i, d[[high]][i])))
    }
  })

  e <- echarts4r::e_charts_(d, x, height = height)
  e <- echarts4r::e_scatter_(e, low, symbol = "circle",
                             symbolSize = dot_size, legend = isTRUE(legend), ...)
  e <- echarts4r::e_scatter_(e, high, symbol = "circle",
                             symbolSize = dot_size, legend = isTRUE(legend))
  # name the two ends (echarts4r seeds the series name from the column)
  e$x$opts$series[[1]]$name <- lo_lab
  e$x$opts$series[[2]]$name <- hi_lab
  if (orientation == "horizontal") e <- echarts4r::e_flip_coords(e)
  # draw all connectors as one markLine on the first series (e_mark_line() would
  # otherwise spread them one-per-series); set it directly for full control.
  e$x$opts$series[[1]]$markLine <- list(
    data = seg, symbol = "none", silent = TRUE, animation = FALSE,
    lineStyle = list(color = .ern_brand$muted, width = 2, opacity = 0.6),
    label = list(show = FALSE))

  # color the two dot series; optionally label each end
  e$x$opts$series[[1]]$itemStyle <- list(color = cols[[1]])
  e$x$opts$series[[2]]$itemStyle <- list(color = cols[[2]])
  if (isTRUE(value_labels)) {
    lab <- function(pos) list(show = TRUE, position = pos, distance = 7,
                              formatter = .nt_value_label_js(value_fmt, orientation),
                              color = .ern_brand$navy, fontFamily = .ern_font,
                              fontSize = 10, fontWeight = 600)
    if (orientation == "horizontal") {
      e$x$opts$series[[1]]$label <- lab("left")
      e$x$opts$series[[2]]$label <- lab("right")
    } else {
      e$x$opts$series[[1]]$label <- lab("bottom")
      e$x$opts$series[[2]]$label <- lab("top")
    }
  }

  e <- .nt_cat_chart_style(e, orientation, value_fmt, yaxis,
                           pal = cols, tooltip = TRUE, legend = isTRUE(legend),
                           source = source)
  if (isTRUE(legend)) e$x$opts$legend$data <- list(lo_lab, hi_lab)

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
