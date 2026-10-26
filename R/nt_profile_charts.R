# ==============================================================================
# nt_profile_charts.R — ERN state-profile inline SVG charts
#
# Raw-SVG string builders for the ERN state-profile pages
# (evictionresearch.net/<state>/). Unlike the echarts4r/mapgl widgets in the
# rest of the package, these emit the profile pages' hand-tuned inline-SVG
# chart markup: the page's own CSS + a small vanilla-JS layer (hover tooltips,
# the --sx constant-px text watcher, phone windowing) attach to the classes
# and ids these functions write, so the markup contract matters — it is kept
# byte-compatible with the shipped Washington profile (the reference
# implementation, ported from that repo's render.R).
#
# All functions return a character scalar of HTML/SVG. In a Quarto document,
# emit them inside a raw-html block via nt_raw_html() (see
# nt_profile_widgets.R) so pandoc passes the markup through untouched.
# ==============================================================================

# Integer with thousands separator ("23,913") — the profiles' number format.
.nt_fmt_int <- function(x) format(round(x), big.mark = ",", trim = TRUE)

# Horizontal gridline group (line + right-aligned value label) for an SVG
# chart. Generated from the same value->y map the data uses so the labels can
# never drift from the plotted geometry.
.nt_grid_lines_svg <- function(vals, x0, x1, label_x, y_of, fmt_lab) {
  yy <- y_of(vals)
  paste(
    sprintf(paste0('          <line class="grid-line" x1="%d" y1="%.1f" x2="%d" y2="%.1f"/>\n',
                   '          <text class="grid-label" x="%d" y="%.1f" text-anchor="end">%s</text>'),
            x0, yy, x1, yy, label_x, yy + 4, vapply(vals, fmt_lab, character(1))),
    collapse = "\n"
  )
}

#' Monthly filings trend chart (state-profile Figure 1) as inline SVG
#'
#' Builds the profile pages' monthly trend line — moratoria band, data-driven
#' gridlines, pre-pandemic baseline, the filings path, a focal dot + callout on
#' the latest month, and every-other-year axis ticks — as a single inline
#' `<svg>` string. The page JS attaches hover tooltips to the emitted ids and
#' re-windows the path on phones, so ids and classes follow the profile-page
#' contract (`.data`, `.focal`, `.moratoria-band`, `.hover-grid`,
#' `.hover-indicator`).
#'
#' @param dates Date vector, one per month, ascending (typically the profile
#'   window, e.g. 2019-01 onward).
#' @param values Integer filings per month, same length as `dates`.
#' @param baseline Pre-pandemic monthly mean drawn as the dashed reference
#'   line.
#' @param band_start,band_end Dates bounding the shaded moratorium band
#'   (first of month). `NULL` omits the band.
#' @param baseline_range_label Label for the baseline period used in the
#'   provenance comment (default `"Jan 2017 – Feb 2020"`).
#' @param y_step Gridline interval; the y ceiling rounds the series max up to
#'   this step (default 500).
#' @param svg_id,indicator_id Element ids the page JS looks up.
#' @param source_note Provenance comment for the data path (names the source
#'   table).
#' @param projection Emit the projection layer: styles plus two empty paths
#'   (`.proj-band`, `.proj-line`) that the page JS fills when a reader picks a
#'   horizon from [nt_profile_projection_toggle()], using the forecast that
#'   [nt_profile_monthly_js()] carries. `FALSE` (default) leaves the output
#'   byte-identical for pages without the control.
#' @return Character scalar: the complete `<svg>...</svg>` block.
#' @family state profile
#' @export
nt_profile_trend_svg <- function(dates, values, baseline,
                                 band_start = NULL, band_end = NULL,
                                 baseline_range_label = "Jan 2017 \u2013 Feb 2020",
                                 y_step = 500,
                                 svg_id = "chart-monthly-svg",
                                 indicator_id = "chart-monthly-indicator",
                                 source_note = "d3_state_agg",
                                 projection = FALSE) {
  stopifnot(length(dates) == length(values), length(dates) >= 2)
  mN <- length(values)
  mX0 <- 40; mX1 <- 970; mY0 <- 78; mY1 <- 298
  m_x <- mX0 + (seq_len(mN) - 1) * (mX1 - mX0) / (mN - 1)
  y_max <- max(y_step, ceiling(max(c(values, baseline), na.rm = TRUE) / y_step) * y_step)
  y_of  <- function(v) mY1 - v * (mY1 - mY0) / y_max
  m_y <- y_of(values)

  grid_svg <- .nt_grid_lines_svg(seq(y_step, y_max, by = y_step), mX0, mX1, 36,
                                 y_of, function(v) .nt_fmt_int(v))
  path_d <- paste(
    "M", sprintf("%.1f", m_x[1]), sprintf("%.1f", m_y[1]),
    paste("L", sprintf("%.1f", m_x[-1]), sprintf("%.1f", m_y[-1]), collapse = " ")
  )

  focal_x <- m_x[mN]; focal_y <- m_y[mN]
  annot_label <- sprintf("%s \u00b7 %s", .nt_fmt_int(values[mN]),
                         format(dates[mN], "%B %Y"))

  # Moratoria band — mapped through the same date->x scale as the data line.
  band_svg <- ""
  if (!is.null(band_start) && !is.null(band_end)) {
    x_of_date <- function(d) {
      mX0 + (12 * (as.integer(format(d, "%Y")) - as.integer(format(dates[1], "%Y"))) +
               (as.integer(format(d, "%m")) - as.integer(format(dates[1], "%m")))) *
        (mX1 - mX0) / (mN - 1)
    }
    band_svg <- sprintf(paste0(
      '        <!-- Pandemic moratoria band. X and width come from content.md\'s\n',
      '             moratorium block; the band shades the start\u2192end span of the\n',
      '             state\'s eviction moratorium in chart coordinate space. -->\n',
      '        <rect class="moratoria-band" x="%.1f" y="20" width="%.1f" height="280"/>\n'),
      x_of_date(band_start), x_of_date(band_end) - x_of_date(band_start))
  }

  # X-axis ticks: every other year, plus the latest year pinned to the right edge.
  years_in_data <- sort(unique(as.integer(format(dates, "%Y"))))
  latest_year <- max(years_in_data)
  tick_years <- unique(c(years_in_data[seq(1, length(years_in_data), by = 2)], latest_year))
  tick_jan <- as.Date(paste0(tick_years, "-01-01"))
  tick_x <- mX0 + (match(tick_jan, dates) - 1) * (mX1 - mX0) / (mN - 1)
  tick_x[tick_years == max(tick_years)] <- mX1
  axis_anchor <- ifelse(tick_years == min(tick_years), "",
                 ifelse(tick_years == max(tick_years), ' text-anchor="end"',
                        ' text-anchor="middle"'))
  axis_svg <- paste(
    sprintf('        <text class="ax" x="%.0f" y="316"%s>%d</text>',
            tick_x, axis_anchor, tick_years),
    collapse = "\n"
  )

  paste0(
    '      <svg id="', svg_id, '" viewBox="0 0 1000 320" xmlns="http://www.w3.org/2000/svg">\n',
    '        <style>\n',
    '          .ax    { font-family: \'Inter\', sans-serif; font-size: calc(11px / var(--sx, 1)); fill: #586573; letter-spacing: 0.02em; }\n',
    '          /* The end-value label sits over the series, so it carries a\n',
    '             white halo: paint-order draws the stroke first, then the fill,\n',
    '             which keeps the text readable wherever the line happens to run.\n',
    '             The halo is counter-scaled with the text so it stays even. */\n',
    '          .annot { font-family: \'Inter\', sans-serif; font-style: italic; font-size: calc(13px / var(--sx, 1)); fill: #19222C;\n',
    '                   paint-order: stroke; stroke: #ffffff; stroke-width: calc(4px / var(--sx, 1)); stroke-linejoin: round; }\n',
    '          .annot-line { stroke: #19222C; stroke-width: 0.5; stroke-dasharray: 2 3; }\n',
    '          .baseline { stroke: #6c7a89; stroke-width: 0.5; stroke-dasharray: 1 4; }\n',
    '          .data { fill: none; stroke: #19222C; stroke-width: 1.5; stroke-linejoin: round; }\n',
    '          .focal { fill: #F9322B; }\n',
    '          .moratoria-band { fill: #19222C; opacity: 0.05; }\n',
    if (projection) paste0(
      '          .proj-band { fill: rgba(249, 50, 43, 0.08); stroke: none; }\n',
      '          .proj-line { fill: none; stroke: #F9322B; stroke-width: 1.5; stroke-dasharray: 4 3; }\n'),
    '        </style>\n',
    band_svg,
    sprintf(paste0(
      '        <!-- Y scale: 0 \u2192 y=%d (floor), %d \u2192 y=%d (data-driven).\n',
      '             Gridlines + labels are generated in render.R (grid_lines_svg) so\n',
      '             they always match the plotted line\'s scale. -->\n'),
      mY1, y_max, mY0),
    '        <g class="hover-grid">\n',
    grid_svg, '\n',
    '        </g>\n',
    sprintf('        <!-- Pre-pandemic baseline (%s monthly mean = %s). -->\n',
            baseline_range_label, .nt_fmt_int(baseline)),
    sprintf('        <line class="baseline" x1="%d" y1="%.1f" x2="%d" y2="%.1f"/>\n',
            mX0, y_of(baseline), mX1, y_of(baseline)),
    sprintf('        <!-- Real monthly filings (%d months from %s). -->\n', mN, source_note),
    '        <path class="data" d="', path_d, '" />\n',
    if (projection) paste0(
      '        <!-- Projection layer: empty until the page JS draws a horizon. -->\n',
      '        <path class="proj-band" d=""/>\n',
      '        <path class="proj-line" d=""/>\n'),
    sprintf('        <circle class="focal" cx="%.1f" cy="%.1f" r="3.5"/>\n', focal_x, focal_y),
    sprintf('        <text class="annot" x="%.1f" y="%.1f" text-anchor="end">%s</text>\n',
            focal_x - 5, focal_y - 26, annot_label),
    sprintf('        <line class="annot-line" x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f"/>\n',
            focal_x - 28, focal_y - 20, focal_x - 4, focal_y - 4),
    '        <!-- X-axis ticks (every-other-year + right edge). -->\n',
    axis_svg, '\n',
    '        <g class="hover-indicator" id="', indicator_id, '">\n',
    '          <line class="hover-vguide" x1="0" y1="20" x2="0" y2="305"/>\n',
    '          <circle class="hover-dot" cx="0" cy="0" r="5"/>\n',
    '        </g>\n',
    '      </svg>'
  )
}

#' Yearly filings bar chart (state-profile Figure 2) as inline SVG
#'
#' Builds the profile pages' yearly totals bars — data-driven gridlines, the
#' pre-pandemic baseline, one labeled bar per year with the partial latest
#' year accented, and an optional SARIMA year-end projection overlay (80% PI
#' band + dashed extension + point-estimate tick) — as an inline `<svg>`
#' string. Bars carry the `data-key` / `data-label` / `data-value` (and, for
#' the projected year, `data-proj-*`) attributes the page's tooltip JS reads.
#'
#' @param years Integer years, ascending.
#' @param values Filing totals per year (the latest year may be partial).
#' @param baseline Pre-pandemic 12-month-equivalent reference value.
#' @param latest_month Last data month (1–12) of the final year in `years`;
#'   values < 12 mark that year partial (accent + "(Jan–Mon)" label).
#' @param projection Optional list with `total`, `lo`, `hi` (e.g. from
#'   [nt_profile_project_year()]) drawn as the projection overlay on the
#'   partial year. `lo` and `hi` may be omitted for a point projection (a
#'   county's share of the state's, say): the dashed extension and tick draw,
#'   the range band and the tooltip's range line do not.
#' @param y_ceiling_step Step the y ceiling rounds up to (default 2500);
#'   gridlines draw every `y_grid_step` (default 5000).
#' @param y_grid_step Gridline interval (default 5000, labeled "5k", "10k", …).
#' @param svg_id Element id the page JS looks up.
#' @param focal_last Colour the latest year's bar and value label with the focal
#'   (red) treatment even when the year is complete. For annual-feed states,
#'   where no year is ever partial but the current year should still read as
#'   current. Does not imply partiality: the axis label and projection are
#'   still governed by `latest_month`/`projection`.
#' @param year_labels Optional character vector, one per `years`, replacing the
#'   printed axis and hover text. For fiscal-year states, where the bar keyed
#'   2016 covers FY 2015-16. `NULL` (default) prints the numeric years.
#' @param fit_width Shrink the viewBox to the bars instead of padding it out to
#'   the default 1000 units. Use for a series short enough to leave dead canvas
#'   on the right (Florida opens in 2019 and its eight bars filled two-thirds of
#'   the figure). Text keeps its pixel size because the page divides every
#'   font-size by `--sx`, the rendered-to-viewBox scale. `FALSE` (default)
#'   leaves Washington and Texas byte-identical.
#' @return Character scalar: the complete `<svg>...</svg>` block.
#' @family state profile
#' @export
nt_profile_yearly_svg <- function(years, values, baseline,
                                  latest_month = 12,
                                  projection = NULL,
                                  y_ceiling_step = 2500,
                                  y_grid_step = 5000,
                                  svg_id = "chart-yearly-svg",
                                  year_labels = NULL,
                                  fit_width = FALSE,
                                  focal_last = FALSE) {
  stopifnot(length(years) == length(values))
  yN <- length(years)
  yX0 <- 70; ybW <- 55; ybGap <- 20
  y_x <- yX0 + (seq_len(yN) - 1) * (ybW + ybGap)
  y_floor <- 280
  latest_year <- max(years)
  months_short <- c("Jan","Feb","Mar","Apr","May","Jun",
                    "Jul","Aug","Sep","Oct","Nov","Dec")

  has_pi  <- !is.null(projection) && !is.null(projection$lo) && !is.null(projection$hi)
  proj_hi <- if (has_pi) projection$hi else if (!is.null(projection)) projection$total else 0
  y_max  <- max(y_grid_step,
                ceiling(max(c(values, baseline, proj_hi), na.rm = TRUE) / y_ceiling_step) * y_ceiling_step)
  y_unit <- 220 / y_max
  y_of <- function(v) y_floor - v * y_unit
  # The bar row grows with the number of years. Anything past 11 or so runs
  # off the fixed 1000-unit viewBox (Texas opens in 2012, Washington in
  # 2016), so the right edge and the canvas follow the bars. The max() floors
  # keep the short-series geometry byte-identical to what it was.
  #
  # fit_width drops those floors for a series short enough to leave dead
  # canvas. Florida opens in 2019, so its eight bars end at x=660 while the
  # axis ran on to 970, a third of the figure empty. Shrinking the viewBox
  # is safe rather than clever: the page measures rendered width over viewBox
  # width into --sx and every font-size in here divides by it, so the text
  # holds its pixel size and only the bars render wider.
  x_right  <- if (fit_width) max(y_x) + ybW + 10 else max(970, max(y_x) + ybW + 10)
  vb_width <- if (fit_width) x_right + 30        else max(1000, x_right + 30)
  # Thousands read as "5k"; a step under 1,000 (a small county) prints whole.
  grid_lab <- if (y_grid_step >= 1000) function(v) paste0(v / 1000, "k") else .nt_fmt_int
  grid_svg <- .nt_grid_lines_svg(seq(y_grid_step, y_max, by = y_grid_step), 40, x_right, 36,
                                 y_of, grid_lab)
  y_height <- values * y_unit
  y_y <- y_floor - y_height

  is_partial <- years == latest_year & latest_month < 12
  # `years` stays numeric because it drives bar geometry and the latest-year
  # test, but states on a fiscal year need the axis to say what the bar
  # actually covers: California's 2016 bar is FY 2015-16, and printing "2016"
  # would quietly misdate the series by a year. year_labels overrides the
  # printed text only; NULL keeps calendar-year output byte-identical.
  stopifnot("year_labels must be one per year" =
              is.null(year_labels) || length(year_labels) == length(years))
  axis_labels <- if (is.null(year_labels)) as.character(years) else as.character(year_labels)
  year_data_label <- ifelse(is_partial,
                            sprintf("%s (Jan\u2013%s)", axis_labels, months_short[latest_month]),
                            axis_labels)
  # The focal (red) bar normally marks a partial latest year, because that is
  # the bar a reader must not compare like-for-like. An annual-feed state has
  # no partial year but still wants the current year to read as current, so
  # focal_last colours the last bar without claiming it is incomplete: the
  # "(Jan-Mon)" data label and the projection stay tied to is_partial alone.
  is_focal  <- is_partial | (focal_last & years == latest_year)
  bar_class <- ifelse(is_focal, "ybar focal", "ybar")
  val_class <- ifelse(is_focal, "yval focal", "yval")
  has_proj <- is_partial & !is.null(projection)
  proj_attr <- ifelse(has_proj,
    if (has_pi)
      sprintf(' data-proj-total="%s" data-proj-lo="%s" data-proj-hi="%s"',
              .nt_fmt_int(round(projection$total)),
              .nt_fmt_int(round(projection$lo)),
              .nt_fmt_int(round(projection$hi)))
    else if (!is.null(projection))
      sprintf(' data-proj-total="%s"', .nt_fmt_int(round(projection$total)))
    else "",
    "")

  bars_svg <- paste(
    sprintf(
      paste0('        <rect class="%s" data-key="%d" data-label="%s" data-value="%s"%s ',
             'x="%.0f" y="%.1f" width="%d" height="%.1f"/>\n',
             '        <text class="%s" x="%.0f" y="%.1f" text-anchor="middle">%s</text>'),
      bar_class, years, year_data_label, .nt_fmt_int(values), proj_attr,
      y_x, y_y, ybW, y_height,
      val_class, y_x + ybW/2, y_y - 7, .nt_fmt_int(values)),
    collapse = "\n"
  )

  proj_svg <- ""
  if (!is.null(projection) && latest_month < 12) {
    .pi <- which(years == latest_year)
    if (length(.pi) == 1) {
      .px <- y_x[.pi]
      .actual_y  <- y_y[.pi]
      .proj_y    <- y_floor - projection$total * y_unit
      .proj_y_lo <- y_floor - (if (has_pi) projection$lo else projection$total) * y_unit
      .proj_y_hi <- y_floor - (if (has_pi) projection$hi else projection$total) * y_unit
      proj_svg <- paste0(
        '        <!-- Projection for partial latest year (ARIMA on monthly 2023+). -->\n',
        if (has_pi) sprintf(paste0(
          '        <!-- 80%% PI band: pale fill from PI-low to PI-high -->\n',
          '        <rect class="yproj-pi" x="%.0f" y="%.1f" width="%d" height="%.1f"/>\n'),
          .px, .proj_y_hi, ybW, .proj_y_lo - .proj_y_hi),
        sprintf(paste0(
          '        <!-- Dashed outline from top-of-actual up to projected mean -->\n',
          '        <rect class="yproj-bar" x="%.0f" y="%.1f" width="%d" height="%.1f"/>\n',
          '        <!-- Point-estimate tick + label -->\n',
          '        <line class="yproj-tick" x1="%.0f" y1="%.1f" x2="%.0f" y2="%.1f"/>\n',
          '        <text class="yproj-label" x="%.0f" y="%.1f" text-anchor="middle">~%s projected</text>\n'),
          .px, .proj_y, ybW, .actual_y - .proj_y,
          .px - 4, .proj_y, .px + ybW + 4, .proj_y,
          .px + ybW/2, .proj_y - 8, .nt_fmt_int(round(projection$total))))
    }
  }

  axis_svg <- paste(
    sprintf('        <text class="yax" x="%.0f" y="%d" text-anchor="middle"%s>%s</text>',
            y_x + ybW/2, 304,
            ifelse(years == latest_year,
                   ' style="font-weight: 600; fill: #19222C;"', ""),
            axis_labels),
    collapse = "\n"
  )

  paste0(
    '      <svg id="', svg_id, '" viewBox="0 0 ', vb_width, ' 320" xmlns="http://www.w3.org/2000/svg">\n',
    '        <style>\n',
    '          .yax    { font-family: \'Inter\', sans-serif; font-size: calc(11px / var(--sx, 1)); fill: #586573; letter-spacing: 0.02em; }\n',
    '          .yval   { font-family: \'Inter\', sans-serif; font-size: calc(13px / var(--sx, 1)); fill: #19222C; font-weight: 600; font-variant-numeric: tabular-nums; }\n',
    '          .yval.focal { fill: #F9322B; }\n',
    '          .ybar   { fill: #19222C; }\n',
    '          .ybar.focal { fill: #F9322B; }\n',
    '          .ybaseline { stroke: #6c7a89; stroke-width: 0.5; stroke-dasharray: 1 4; }\n',
    '          .yfloor { stroke: #19222C; stroke-width: 1; }\n',
    '          /* Projection styles \u2014 pale band = 80% PI; dashed outline = point estimate */\n',
    '          .yproj-pi    { fill: rgba(249, 50, 43, 0.07); stroke: none; }\n',
    '          .yproj-bar   { fill: none; stroke: #F9322B; stroke-width: 1; stroke-dasharray: 3 3; opacity: 0.85; }\n',
    '          .yproj-tick  { stroke: #F9322B; stroke-width: 1.5; }\n',
    '          .yproj-label { font-family: \'Inter\', sans-serif; font-size: calc(11px / var(--sx, 1)); font-style: italic; fill: #CC2118; font-weight: 600; font-variant-numeric: tabular-nums; }\n',
    '        </style>\n',
    sprintf(paste0(
      '        <!-- Y scale: 0 \u2192 y=%d (floor); ceiling is data-driven in render.R.\n',
      '             Gridlines + labels generated there (grid_lines_svg) so they always\n',
      '             match the bar scale (previously hardcoded 5k/10k/15k drifted when\n',
      '             the projection pushed the ceiling past 18k). -->\n'), y_floor),
    '        <g class="hover-grid">\n',
    grid_svg, '\n',
    '        </g>\n',
    '        <!-- Pre-pandemic baseline line; label lives in the legend strip below the SVG. -->\n',
    sprintf('        <line class="ybaseline" x1="40" y1="%.1f" x2="%d" y2="%.1f"/>\n',
            y_of(baseline), x_right, y_of(baseline)),
    sprintf('        <line class="yfloor" x1="40" y1="%d" x2="%d" y2="%d"/>\n', y_floor, x_right, y_floor),
    '        <!-- Bars + value labels (one rect+text per year). -->\n',
    bars_svg, '\n',
    proj_svg,
    '        <!-- x-axis labels (one per year). -->\n',
    axis_svg, '\n',
    '      </svg>'
  )
}

#' Month-by-month filings projection (SARIMA)
#'
#' The monthly companion to [nt_profile_project_year()]: the same seasonal
#' ARIMA, fit on the same window with the same settings, forecast `h` months
#' past the last observed month. Because the two fit an identical model, the
#' first months here plus the year-to-date actual sum to the year-end total
#' [nt_profile_project_year()] reports, so a Figure 1 projection and the
#' Figure 2 dashed bar cannot disagree. Feeds [nt_profile_monthly_js()].
#'
#' @inheritParams nt_profile_project_year
#' @param h Months to forecast past the last observed month (default 12).
#' @return `NULL` when the window is too short or the fit fails; otherwise a
#'   data frame with `date` (first of each forecast month), `mean`, `lo`, and
#'   `hi`, carrying the fitted model in attribute `"order"`.
#' @family state profile
#' @export
nt_profile_project_months <- function(dates, values, h = 12L,
                                      fit_start = "2023-01-01",
                                      level = 80, min_months = 24) {
  if (!requireNamespace("forecast", quietly = TRUE)) {
    stop("nt_profile_project_months() needs the 'forecast' package: install.packages(\"forecast\")")
  }
  stopifnot(length(dates) == length(values), h >= 1)
  ord <- order(dates)
  dates <- dates[ord]; values <- values[ord]

  keep <- dates >= as.Date(fit_start)
  if (sum(keep) < min_months) return(NULL)
  d_fit <- dates[keep]; v_fit <- values[keep]
  ts_fit <- stats::ts(v_fit,
                      start = c(as.integer(format(d_fit[1], "%Y")),
                                as.integer(format(d_fit[1], "%m"))),
                      frequency = 12)
  tryCatch({
    fit <- forecast::auto.arima(ts_fit, seasonal = TRUE,
                                stepwise = TRUE, approximation = FALSE)
    fc  <- forecast::forecast(fit, h = h, level = level)
    first <- as.Date(format(dates[length(dates)], "%Y-%m-01"))
    out <- data.frame(date = seq(first, by = "month", length.out = h + 1L)[-1],
                      mean = as.numeric(fc$mean),
                      lo   = as.numeric(fc$lower),
                      hi   = as.numeric(fc$upper))
    .arma <- fit$arma
    attr(out, "order") <- sprintf("SARIMA(%d,%d,%d)(%d,%d,%d)[%d]",
                                  .arma[1], .arma[6], .arma[2],
                                  .arma[3], .arma[7], .arma[4], .arma[5])
    out
  }, error = function(e) NULL)
}

#' Per-county yearly bar charts behind Figure 2's county picker
#'
#' Draws one [nt_profile_yearly_svg()] per county, hidden, for the page JS to
#' swap in when a reader picks a county from [nt_profile_yearly_picker()].
#' Each chart is the statewide one at county scale: the county's own
#' pre-pandemic 12-month-equivalent baseline, a y-scale stepped to its size,
#' and, for a partial latest year, its projected year-end from
#' [nt_profile_project_counties()] drawn as a point projection (no range,
#' since an allocated share has no interval of its own).
#'
#' @param county_agg Data frame of county-month filings: `county_code`,
#'   `county`, `year`, `month`, `filings`.
#' @param projection Optional output of [nt_profile_project_counties()].
#' @param baseline_start,baseline_end First and last month of the
#'   pre-pandemic baseline (default Jan 2017 to Feb 2020, as statewide).
#' @param svg_prefix Each chart's id is this plus the county code.
#' @return Character scalar: the hidden `<svg>` blocks, one per county, each
#'   carrying `data-county` and `data-baseline` for the page JS.
#' @family state profile
#' @export
nt_profile_yearly_counties <- function(county_agg, projection = NULL,
                                       baseline_start = "2017-01-01",
                                       baseline_end = "2020-02-01",
                                       svg_prefix = "chart-yearly-svg-") {
  stopifnot(all(c("county_code", "county", "year", "month", "filings") %in% names(county_agg)))
  d <- as.Date(sprintf("%d-%02d-01", county_agg$year, county_agg$month))
  latest_year  <- max(county_agg$year)
  latest_month <- max(county_agg$month[county_agg$year == latest_year])
  years <- sort(unique(county_agg$year))
  in_base <- d >= as.Date(baseline_start) & d <= as.Date(baseline_end)
  codes <- unique(county_agg$county_code[order(county_agg$county)])

  vapply(codes, function(code) {
    k <- county_agg$county_code == code
    vals <- vapply(years, function(y) sum(county_agg$filings[k & county_agg$year == y], na.rm = TRUE),
                   numeric(1))
    base <- 12 * mean(county_agg$filings[k & in_base], na.rm = TRUE)
    proj <- NULL
    if (!is.null(projection) && latest_month < 12) {
      proj <- list(total = projection$total[match(code, projection$county_code)])
    }
    top  <- max(c(vals, base, if (!is.null(proj)) proj$total), na.rm = TRUE)
    grid <- max(1, diff(pretty(c(0, top), n = 4))[1])
    svg <- nt_profile_yearly_svg(years, vals, baseline = base,
                                 latest_month = latest_month, projection = proj,
                                 y_ceiling_step = max(1, grid / 2), y_grid_step = grid,
                                 svg_id = paste0(svg_prefix, code))
    sub('<svg id="', sprintf('<svg style="display:none" data-county="%s" data-baseline="%s" id="',
                             county_agg$county[k][1], .nt_fmt_int(base)),
        svg, fixed = TRUE)
  }, character(1), USE.NAMES = FALSE) |> paste(collapse = "\n")
}

#' Year-end filings projection for the partial latest year (SARIMA)
#'
#' Fits `forecast::auto.arima` (seasonal) to the monthly filings series from
#' `fit_start` onward, forecasts the months remaining in the latest year, and
#' returns the year-end estimate: year-to-date actual plus the forecast sum,
#' with the prediction-interval bounds. This is the number behind the dashed
#' projection overlay in [nt_profile_yearly_svg()] and the projection prose on
#' the profile pages.
#'
#' @param dates Date vector, one per month, ascending (full series).
#' @param values Integer filings per month.
#' @param fit_start First month of the training window (default
#'   `"2023-01-01"`, the post-recovery plateau on the ERN profiles).
#' @param level Prediction-interval level in percent (default 80).
#' @param min_months Minimum training months required (default 24); fewer
#'   returns `NULL`.
#' @return `NULL` when the latest year is complete, the window is too short,
#'   or the fit fails; otherwise a list: `total`, `lo`, `hi` (year-end
#'   estimate + PI bounds), `ytd` (actual to date), `h` (months forecast),
#'   `order` (e.g. `"SARIMA(3,1,0)(1,0,0)[12]"`).
#' @family state profile
#' @export
nt_profile_project_year <- function(dates, values, fit_start = "2023-01-01",
                                    level = 80, min_months = 24) {
  if (!requireNamespace("forecast", quietly = TRUE)) {
    stop("nt_profile_project_year() needs the 'forecast' package: install.packages(\"forecast\")")
  }
  stopifnot(length(dates) == length(values))
  ord <- order(dates)
  dates <- dates[ord]; values <- values[ord]
  latest <- dates[length(dates)]
  latest_year <- as.integer(format(latest, "%Y"))
  latest_mo   <- as.integer(format(latest, "%m"))
  if (latest_mo >= 12) return(NULL)

  keep <- dates >= as.Date(fit_start)
  if (sum(keep) < min_months) return(NULL)
  d_fit <- dates[keep]; v_fit <- values[keep]
  ts_fit <- stats::ts(v_fit,
                      start = c(as.integer(format(d_fit[1], "%Y")),
                                as.integer(format(d_fit[1], "%m"))),
                      frequency = 12)
  tryCatch({
    fit <- forecast::auto.arima(ts_fit, seasonal = TRUE,
                                stepwise = TRUE, approximation = FALSE)
    h  <- 12L - latest_mo
    fc <- forecast::forecast(fit, h = h, level = level)
    yr <- as.integer(format(dates, "%Y"))
    mo <- as.integer(format(dates, "%m"))
    ytd <- sum(values[yr == latest_year & mo <= latest_mo], na.rm = TRUE)
    .arma <- fit$arma
    list(total = ytd + sum(as.numeric(fc$mean)),
         lo    = ytd + sum(as.numeric(fc$lower)),
         hi    = ytd + sum(as.numeric(fc$upper)),
         ytd   = ytd,
         h     = h,
         order = sprintf("SARIMA(%d,%d,%d)(%d,%d,%d)[%d]",
                         .arma[1], .arma[6], .arma[2],
                         .arma[3], .arma[7], .arma[4], .arma[5]))
  }, error = function(e) NULL)
}

#' "Four P's" policy-lever diagram (state-profile Figure 5) as inline SVG
#'
#' Builds the action section's data-driven diagram: the eviction-to-
#' displacement pathway, four policy bands (Prevent / Protect / Preserve /
#' Produce) filled with tier-badged lever chips, the "strongest together"
#' connector between the two Tier-1 anchors, and a three-line footer. Text is
#' real (accessible) SVG text sized with the profile pages' constant-px rule:
#' `font-size: calc(<px> / var(--sx, 8.2))`, where the page's scale-watcher JS
#' sets `--sx` per SVG (the 8.2 fallback = the 820px CSS min-width over the
#' 100-unit viewBox — do not reset it to 1).
#'
#' Defaults reproduce the Washington profile's graphic; every content slot is
#' an argument so other states can adjust the levers or copy.
#'
#' @param bands Data frame with one row per band: `P` (band name), `num`
#'   (badge glyph), `tag` (right-aligned italic descriptor), `fill` (band
#'   color).
#' @param levers Named list (names matching `bands$P`); each element is a list
#'   of `c(label, tier)` character pairs, in reading order.
#' @param ncols Named integer vector: chip columns per band.
#' @param pair Character vector of two lever labels to accent in red and join
#'   with the "strongest together" connector.
#' @param pair_label Connector label text.
#' @param pathway_labels,pathway_x Stage labels (use `\n` for two-line chips)
#'   and their x positions in the 0–100 design space.
#' @param tier_key `list(title=, t1=, t2=, t3=)` strings for the tier legend.
#' @param pathway_title Header line above the pathway.
#' @param footer_lines Character vector of up to three footer lines (bold
#'   lead, plain second, muted third; `\n` in the third wraps it).
#' @param aria_label Accessible description of the graphic.
#' @param svg_id,svg_class Element id/class (the page JS scale-watcher looks
#'   the id up).
#' @param wrap Wrap the SVG in `<div class="plate6-svg-wrap">` (the page CSS
#'   gives the wrap a min-width + horizontal pan on phones).
#' @param crop_top ViewBox top-crop in design units (default 4 — the shipped
#'   pages crop the empty title zone; the claim heading above the figure
#'   carries the headline instead).
#' @return Character scalar: the `<div class="plate6-svg-wrap">…</div>` block
#'   (or bare `<svg>` when `wrap = FALSE`).
#' @family state profile
#' @export
nt_profile_fourps_svg <- function(
    bands = data.frame(
      P    = c("PREVENT", "PROTECT", "PRESERVE", "PRODUCE"),
      num  = c("\u2460", "\u2461", "\u2462", "\u2463"),
      tag  = c("stop the eviction before it reaches court",
               "defend tenants & limit evictions",
               "keep today's low-cost homes",
               "add low-cost homes for the long run"),
      fill = c("#19222C", "#3C4C5C", "#6E7C8A", "#B4BFC9"),
      stringsAsFactors = FALSE),
    levers = list(
      PREVENT  = list(c("Rental assistance", 1), c("Pre-filing mediation", 1), c("Right to cure", 2)),
      PROTECT  = list(c("Right to counsel", 1), c("Just cause", 2), c("Source-of-income rule", 2), c("Rent stabilization", 2)),
      PRESERVE = list(c("Curb serial filers", 2), c("Limit algorithmic pricing", 2), c("Preserve low-cost rentals", 3), c("Community land trusts", 3)),
      PRODUCE  = list(c("Social housing", 3), c("Sub-50% AMI homes", 3), c("Voucher restoration", 3))),
    ncols = c(PREVENT = 3, PROTECT = 4, PRESERVE = 4, PRODUCE = 3),
    pair = c("Rental assistance", "Right to counsel"),
    pair_label = "STRONGEST TOGETHER",
    pathway_labels = c("Rent\nburden", "Missed\nrent", "Eviction\nnotice",
                       "Court\nfiling", "Judgment\n& writ", "Displacement"),
    pathway_x = c(8, 24, 40, 56, 72, 89),
    tier_key = list(title = "DOCUMENTED EFFECT:",
                    t1 = "T1 largest, fastest",
                    t2 = "T2 moderate / partial",
                    t3 = "T3 structural, slow"),
    pathway_title = "FROM EVICTION TO DISPLACEMENT \u2192",
    footer_lines = c(
      "Prevent and Protect interrupt the eviction; Preserve and Produce ease the rent pressure that starts it.",
      "Right to counsel needs both: substantial, sustained funding to meet today's caseload, and better ways to connect tenants to the lawyers.",
      "Tiers and reinforcement reflect the eviction-prevention literature (HUD-ERA; Cassidy & Currie; Desmond; Colburn & Aldern). This profile's data\ndescribe filing trends, not causal policy effects.  \u00b7  Eviction Research Network, UC Berkeley"),
    aria_label = paste0(
      "The four P's of eviction prevention \u2014 Prevent, Protect, Preserve, Produce \u2014 ",
      "with documented-effect tiers (T1 largest, T3 structural), ",
      "along the path from a missed rent payment to displacement."),
    svg_id = "plate6-svg", svg_class = "plate6-svg",
    wrap = TRUE, crop_top = 4) {

  navy <- "#19222C"; red <- "#F9322B"; muted <- "#6B7785"
  rule <- "#D7DCE1"; grey3 <- "#8A95A0"; white <- "#FFFFFF"
  tcol <- c("1" = red, "2" = navy, "3" = grey3)
  band_txt <- rep(navy, nrow(bands))  # band label always navy, above the band

  AR  <- 8.6 / 12
  sx  <- function(x) sprintf("%.2f", x)
  sy  <- function(y) sprintf("%.2f", (100 - y) * AR)
  fs  <- function(px) sprintf("calc(%gpx / var(--sx, 8.2))", px)
  esc <- function(t) { t <- gsub("&", "&amp;", t, fixed = TRUE)
                       t <- gsub("<", "&lt;",  t, fixed = TRUE)
                       gsub(">", "&gt;", t, fixed = TRUE) }

  el <- character(0)
  add <- function(s) el[[length(el) + 1]] <<- s

  text <- function(x, y, label, px, fill, anchor = "start", baseline = "auto",
                   weight = "400", style = "normal", lh = 1.05) {
    lines <- strsplit(label, "\n", fixed = TRUE)[[1]]
    n <- length(lines)
    fdy <- if (baseline %in% c("central", "middle")) -(n - 1) * lh / 2 else 0
    inner <- if (n <= 1) esc(label) else
      paste0(vapply(seq_len(n), function(i)
        sprintf('<tspan x="%s" dy="%sem">%s</tspan>', sx(x),
                sprintf("%.3f", if (i == 1) fdy else lh), esc(lines[i])),
        character(1)), collapse = "")
    sprintf(paste0('<text x="%s" y="%s" style="font-size:%s" fill="%s"',
                   ' text-anchor="%s" dominant-baseline="%s" font-weight="%s"',
                   ' font-style="%s">%s</text>'),
            sx(x), sy(y), fs(px), fill, anchor, baseline, weight, style, inner)
  }
  rect <- function(x0, y0, x1, y1, fill, stroke = "none", sw = 0) {
    sprintf('<rect x="%s" y="%s" width="%s" height="%s" fill="%s" stroke="%s" stroke-width="%s" rx="0.4"/>',
            sx(x0), sy(y1), sprintf("%.2f", x1 - x0), sprintf("%.2f", (y1 - y0) * AR),
            fill, stroke, sprintf("%.2f", sw))
  }
  seg <- function(x0, y0, x1, y1, stroke, sw) {
    sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="%s" stroke-width="%s"/>',
            sx(x0), sy(y0), sx(x1), sy(y1), stroke, sprintf("%.2f", sw))
  }

  path_col <- c(rep(navy, length(pathway_x) - 1), red)

  # geometry rows (design space, y up): fixed to the shipped profiles' layout
  band_geo <- data.frame(y0 = c(62.5, 48.5, 34.5, 20.5),
                         y1 = c(72.5, 58.5, 44.5, 30.5))
  x0 <- 5; x1 <- 97
  pad <- 1.4; gap <- 1.3
  red_chip <- list()

  # tier key
  add(text(2,  90.5, tier_key$title, 11, muted, weight = "700"))
  add(text(21, 90.5, tier_key$t1,    11, red,   weight = "700"))
  add(text(41, 90.5, tier_key$t2,    11, navy,  weight = "700"))
  add(text(63, 90.5, tier_key$t3,    11, grey3, weight = "700"))

  # pathway
  add(text(2, 86.5, pathway_title, 11, navy, weight = "700"))
  py <- 79.5
  for (i in seq_len(length(pathway_x) - 1)) {
    mx <- (pathway_x[i] + pathway_x[i + 1]) / 2
    add(text(mx, py, "\u2192", 32, navy, anchor = "middle", baseline = "central", weight = "800"))
  }
  for (i in seq_along(pathway_x)) {
    w <- 11; h <- 3.6
    add(rect(pathway_x[i] - w/2, py - h/2/AR, pathway_x[i] + w/2, py + h/2/AR, white))
    add(text(pathway_x[i], py, pathway_labels[i], 11, path_col[i], anchor = "middle",
             baseline = "central", weight = "700", lh = 1.0))
  }

  # bands + chips
  for (b in seq_len(nrow(bands))) {
    by0 <- band_geo$y0[b]; by1 <- band_geo$y1[b]; P <- bands$P[b]
    add(rect(x0, by0, x1, by1, bands$fill[b]))
    add(text(x0, by1 + 1.0, paste0(bands$num[b], "  ", P), 16, band_txt[b],
             baseline = "auto", weight = "700"))
    add(text(x1, by1 + 1.0, bands$tag[b], 11, muted, anchor = "end",
             baseline = "auto", style = "italic"))

    items <- levers[[P]]; n <- length(items); nc <- ncols[[P]]; nr <- ceiling(n / nc)
    ix0 <- x0 + pad; ix1 <- x1 - pad; iy0 <- by0 + pad; iy1 <- by1 - pad
    cw <- (ix1 - ix0 - gap * (nc - 1)) / nc
    ch <- (iy1 - iy0 - gap * (nr - 1)) / nr
    for (i in seq_len(n)) {
      nm <- items[[i]][1]; tier <- items[[i]][2]
      r <- (i - 1) %/% nc; cc <- (i - 1) %% nc
      cxmin <- ix0 + cc * (cw + gap); cxmax <- cxmin + cw
      cymax <- iy1 - r * (ch + gap); cymin <- cymax - ch
      ispair <- nm %in% pair
      add(rect(cxmin, cymin, cxmax, cymax, white,
               stroke = if (ispair) red else rule, sw = if (ispair) 0.55 else 0.18))
      add(text(cxmin + 1.0, cymax - 0.6, paste0("T", tier), 11, tcol[[tier]],
               baseline = "hanging", weight = "700"))
      add(text((cxmin + cxmax)/2, (cymin + cymax)/2, stringr::str_wrap(nm, 13), 13, navy,
               anchor = "middle", baseline = "central", weight = "700", lh = 1.05))
      if (ispair) red_chip[[nm]] <- c(xmin = cxmin, xmax = cxmax, ymin = cymin, ymax = cymax)
    }
  }

  # strongest-together connector between the two paired chips
  if (length(red_chip) == 2) {
    ra <- red_chip[[pair[1]]]; rc <- red_chip[[pair[2]]]
    lx <- (as.numeric(rc["xmin"]) + as.numeric(rc["xmax"])) / 2
    add(seg(lx, as.numeric(rc["ymax"]), lx, as.numeric(ra["ymin"]), red, 0.9))
    add(text(lx + 1.5, (as.numeric(ra["ymin"]) + as.numeric(rc["ymax"]))/2,
             pair_label, 13, red, baseline = "central", weight = "700"))
  }

  # footer
  add(seg(2, 16, 98, 16, rule, 0.4))
  if (length(footer_lines) >= 1)
    add(text(2, 13.4, footer_lines[1], 13, navy, baseline = "hanging", weight = "700"))
  if (length(footer_lines) >= 2)
    add(text(2, 9.8, footer_lines[2], 11, navy, baseline = "hanging"))
  if (length(footer_lines) >= 3)
    add(text(2, 5.0, footer_lines[3], 11, muted, baseline = "hanging", lh = 1.35))

  svg <- paste0(
    '<svg id="', svg_id, '" class="', svg_class, '" viewBox="0 ',
    sprintf("%g", crop_top), ' 100 ', sprintf("%.2f", 100 * AR + 2 - crop_top), '" ',
    'preserveAspectRatio="xMidYMid meet" role="img" ',
    'aria-label="', esc(aria_label), '">',
    paste0(unlist(el), collapse = ""), '</svg>')
  if (wrap) paste0('<div class="plate6-svg-wrap">', svg, '</div>') else svg
}

# ==============================================================================
# Race charts (state-profile Figures 5 and 6)
#
# Ported from the Minnesota profile's hand-authored inline SVG, which was the
# reference implementation and lived only in that repo's index.html with its
# geometry precomputed by code/o1b_inline_svg_geometry.r. Two states needed it,
# so it moves here. The markup contract (ids, classes, the multi-row tooltip)
# matches Minnesota's, and the page template's attachMultiLineChart binds to
# the emitted ids.
# ==============================================================================

# The four-way ERN categorization, in draw order (back to front) with the
# visual weight Minnesota settled on. Line weight and dash carry the ranking,
# so the chart survives greyscale and reads for anyone who cannot separate the
# hues; colour is a second channel, never the only one.
.NT_RACE_SPEC <- list(
  other  = list(css = "rother",  stroke = "#8BA3BE", width = 0.75, dash = NULL,
                swatch = "muted",  dot_r = 3,    dot_fill = "#8BA3BE",
                dot_stroke = "#fff", dot_sw = 1.5),
  white  = list(css = "rwhite",  stroke = "#19222C", width = 1,    dash = "4 4",
                swatch = "dashed", dot_r = 3.5,  dot_fill = "#fff",
                dot_stroke = "#19222C", dot_sw = 1.5),
  latine = list(css = "rlatine", stroke = "#223754", width = 1.5,  dash = "1 3",
                swatch = "",       dot_r = 3.25, dot_fill = "#223754",
                dot_stroke = "#fff", dot_sw = 1.25),
  black  = list(css = "rblack",  stroke = "#19222C", width = 2,    dash = NULL,
                swatch = "focal",  dot_r = 4,    dot_fill = "#F9322B",
                dot_stroke = "#fff", dot_sw = 1.25)
)

.nt_race_svg <- function(dates, series, labels, chart_id, y_max, grid_step,
                         y_fmt, tip_fmt, label_fmt, focal, parity, aria,
                         x0, x1, y_floor, y_top, y_min) {
  stopifnot("series must be a named list" = !is.null(names(series)),
            "every series must match the dates" =
              all(vapply(series, length, integer(1)) == length(dates)),
            "unknown series name" = all(names(series) %in% names(.NT_RACE_SPEC)))

  n <- length(dates)
  xs <- x0 + (seq_len(n) - 1) * (x1 - x0) / (n - 1)
  yv <- function(v) y_floor - (v - y_min) * (y_floor - y_top) / (y_max - y_min)

  grid_vals <- seq(y_min + grid_step, y_max, by = grid_step)
  grid <- paste(vapply(grid_vals, function(g) paste0(
    sprintf('<line class="grid-line" x1="%.0f" y1="%.1f" x2="%.0f" y2="%.1f"/>',
            x0, yv(g), x1, yv(g)),
    sprintf('<text class="grid-label" x="%.0f" y="%.1f" text-anchor="end">%s</text>',
            x0 - 4, yv(g) + 4, y_fmt(g))), character(1)), collapse = "")

  # A parity reference (the ratio chart's 1.0 line) sits under the data and
  # says in words what the level means, so the reader is not left to infer it.
  parity_svg <- if (!is.null(parity)) paste0(
    sprintf('<line class="rparity" x1="%.0f" y1="%.1f" x2="%.0f" y2="%.1f"/>',
            x0, yv(parity$at), x1, yv(parity$at)),
    sprintf('<text class="rparity-label" x="%.0f" y="%.1f">%s</text>',
            x0 + 6, yv(parity$at) - 5, parity$label)) else ""

  draw <- names(.NT_RACE_SPEC)[names(.NT_RACE_SPEC) %in% names(series)]
  paths <- paste(vapply(draw, function(g) {
    s <- .NT_RACE_SPEC[[g]]
    sprintf('<path class="%s" fill="none" stroke="%s" stroke-width="%s"%s d="M %s"/>',
            s$css, s$stroke, s$width,
            if (is.null(s$dash)) "" else sprintf(' stroke-dasharray="%s"', s$dash),
            paste(sprintf("%.1f %.1f", xs, yv(series[[g]])), collapse = " L "))
  }, character(1)), collapse = "")

  # End labels, ordered by final value and nudged apart where two series
  # finish within 11px. Without the nudge the middle labels overlap.
  ends <- data.frame(g = draw,
                     v = vapply(draw, function(g) series[[g]][n], numeric(1)),
                     stringsAsFactors = FALSE)
  ends <- ends[order(-ends$v), ]
  ends$y <- yv(ends$v)
  for (i in seq_len(nrow(ends))[-1]) {
    if (ends$y[i] - ends$y[i - 1] < 11) ends$y[i] <- ends$y[i - 1] + 11
  }
  end_labels <- paste(vapply(seq_len(nrow(ends)), function(i) {
    g <- ends$g[i]
    # The reference group is set back: it is the thing being compared against,
    # not one of the comparisons.
    style <- if (identical(g, "white"))
      ' style="font-style: italic; font-size: calc(12px / var(--sx, 1)); fill: #586573;"' else ""
    sprintf('<text class="rlabel" x="%.0f" y="%.1f"%s>%s &middot; %s</text>',
            x1 + 8, ends$y[i] + 4, style, labels[[g]], label_fmt(ends$v[i]))
  }, character(1)), collapse = "")

  focal_svg <- if (!is.null(focal) && focal %in% draw)
    sprintf('<circle class="rfocal" cx="%.0f" cy="%.1f" r="3"/>',
            x1, yv(series[[focal]][n])) else ""

  # Axis: first year, a middle year, and the last, anchored so neither end
  # label runs off the plot.
  yrs <- as.integer(format(dates, "%Y"))
  ticks <- unique(c(1, which(yrs == yrs[1] + ceiling((max(yrs) - yrs[1]) / 2))[1], n))
  ticks <- ticks[!is.na(ticks)]
  anchors <- ifelse(ticks == 1, "", ifelse(ticks == n, ' text-anchor="end"',
                                           ' text-anchor="middle"'))
  axis <- paste(sprintf('<text class="rax" x="%.1f" y="%.0f"%s>%d</text>',
                        xs[ticks], y_floor - 3, anchors, yrs[ticks]), collapse = "")

  dots <- paste(vapply(draw, function(g) {
    s <- .NT_RACE_SPEC[[g]]
    sprintf(paste0('<circle class="hover-dot rt-%s" cx="0" cy="0" r="%s" ',
                   'style="fill: %s; stroke: %s; stroke-width: %s;"/>'),
            g, s$dot_r, s$dot_fill, s$dot_stroke, s$dot_sw)
  }, character(1)), collapse = "")

  paste0(
    sprintf('<svg id="%s-svg" viewBox="0 0 480 240" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="%s">',
            chart_id, aria),
    '<style>',
    '.rax{font-family:Inter,sans-serif;font-size:calc(11px / var(--sx,1));fill:#586573}',
    '.rlabel{font-family:Inter,sans-serif;font-style:italic;',
    'font-size:calc(13px / var(--sx,1));fill:#19222C;font-weight:500}',
    '.rfocal{fill:#F9322B}',
    '.rparity{stroke:#8BA3BE;stroke-width:.75;stroke-dasharray:2 3}',
    '.rparity-label{font-family:Inter,sans-serif;font-style:italic;',
    'font-size:calc(10px / var(--sx,1));fill:#8A93A0}',
    '</style>',
    '<g class="hover-grid">', grid, '</g>',
    parity_svg, paths, focal_svg, end_labels, axis,
    sprintf('<g class="hover-indicator" id="%s-indicator">', chart_id),
    sprintf('<line class="hover-vguide" x1="0" y1="%.0f" x2="0" y2="%.0f"/>',
            y_top - 20, y_floor - 11),
    dots, '</g></svg>',
    sprintf('<div class="hover-tooltip" id="%s-tooltip"><div class="tip-date"></div><div class="tip-rows"></div></div>',
            chart_id))
}

# Series payload the template's attachMultiLineChart reads. Kept beside the
# SVG rather than re-derived from the path data, so the hover reports the
# values the chart was drawn from.
.nt_race_payload <- function(dates, series, labels, chart_id, tip_digits, tip_suffix,
                             x0, x1, y_floor, y_top, y_min, y_max) {
  draw <- names(.NT_RACE_SPEC)[names(.NT_RACE_SPEC) %in% names(series)]
  arr <- function(v) paste0("[", paste(sprintf("%.4f", v), collapse = ","), "]")
  cfg <- paste0(
    '{ x0: ', x0, ', x1: ', x1, ', yFloor: ', y_floor, ', yTop: ', y_top,
    ', yMin: ', y_min, ', yMax: ', y_max,
    ', digits: ', tip_digits, ', suffix: ', jsonlite::toJSON(tip_suffix, auto_unbox = TRUE),
    ', months: [', paste(sprintf('"%s"', format(dates, "%b %Y")), collapse = ","), ']',
    ', series: [',
    paste(vapply(draw, function(g) sprintf(
      '{ key: "%s", label: %s, swatch: "%s", values: %s }',
      g, jsonlite::toJSON(labels[[g]], auto_unbox = TRUE),
      .NT_RACE_SPEC[[g]]$swatch, arr(series[[g]])), character(1)), collapse = ","),
    '] }')
  sprintf('<script>window.__ernRaceCharts = window.__ernRaceCharts || {}; window.__ernRaceCharts[%s] = %s;</script>',
          jsonlite::toJSON(chart_id, auto_unbox = TRUE), cfg)
}

#' Filing rate by race over time (state-profile Figure 5) as inline SVG
#'
#' The Minnesota profile's race trend, generalized. Draws one line per
#' race-ethnicity group over a monthly series, with end-of-line labels, a
#' focal marker on the lead group, and the hover scaffolding the page
#' template's `attachMultiLineChart` binds to: a shared vertical guide, one
#' dot per series, and a multi-row tooltip reading every group at the hovered
#' month.
#'
#' Line weight and dash carry the ranking so the chart reads in greyscale and
#' for a reader who cannot separate the hues; colour is a second channel.
#'
#' @param dates Date vector, one per month, ascending.
#' @param series Named list of numeric rate vectors, each the length of
#'   `dates`. Names must be drawn from `black`, `latine`, `other`, `white`.
#'   Pass only the groups whose denominator you are willing to publish.
#' @param labels Named character vector of display labels for `series`.
#' @param y_max Y ceiling; `NULL` rounds the series max up to `grid_step`.
#' @param grid_step Gridline interval (default 10).
#' @param chart_id Element id root. The SVG, indicator, and tooltip take
#'   `<id>-svg`, `<id>-indicator`, `<id>-tooltip`.
#' @param focal Series whose end point takes the red focal dot (default
#'   `"black"`); `NULL` draws none.
#' @param aria Accessible description of the chart.
#' @return Character scalar: `<svg>`, tooltip `<div>`, and the `<script>`
#'   payload the hover reads.
#' @family state profile
#' @export
nt_profile_race_trend_svg <- function(dates, series,
                                      labels = c(black = "Black", latine = "Latine",
                                                 other = "Other", white = "White"),
                                      y_max = NULL, grid_step = 10,
                                      chart_id = "chart-race-trend",
                                      focal = "black",
                                      aria = paste("Twelve-month rolling eviction filing rate",
                                                   "per 1,000 renter households by race")) {
  if (is.null(y_max)) {
    y_max <- ceiling(max(unlist(series), na.rm = TRUE) / grid_step) * grid_step
  }
  paste0(
    .nt_race_svg(dates, series, labels, chart_id,
                 y_max = y_max, grid_step = grid_step, y_min = 0,
                 y_fmt = function(v) .nt_fmt_int(v),
                 tip_fmt = function(v) sprintf("%.1f", v),
                 label_fmt = function(v) sprintf("%.1f", v),
                 focal = focal, parity = NULL, aria = aria,
                 x0 = 40, x1 = 400, y_floor = 231, y_top = 71),
    .nt_race_payload(dates, series, labels, chart_id, tip_digits = 1,
                     tip_suffix = "", x0 = 40, x1 = 400,
                     y_floor = 231, y_top = 71, y_min = 0, y_max = y_max))
}

#' Disparity gap: filing rate as a ratio of the reference group (Figure 6)
#'
#' The companion to [nt_profile_race_trend_svg()], and the Minnesota
#' profile's Plate VI. Each line is a group's filing rate divided by the
#' reference group's, so 1.0 is parity and the vertical distance from that
#' line *is* the disparity. A rate chart shows four lines rising together
#' during a wave; this one shows whether the gap between them moved, which is
#' the question the rate chart cannot answer.
#'
#' @param dates Date vector, one per month, ascending.
#' @param series Named list of numeric rate vectors (the same input as the
#'   trend chart, not pre-divided). The reference group must be present.
#' @param reference Series the others are divided by (default `"white"`).
#' @param labels Named character vector of display labels.
#' @param y_min,y_max Y bounds as ratios; `NULL` pads the observed range.
#' @param grid_step Gridline interval in ratio units (default 0.25).
#' @param chart_id Element id root.
#' @param focal Series taking the red focal dot (default `"black"`).
#' @param parity_label Text on the reference line.
#' @param aria Accessible description of the chart.
#' @return Character scalar: `<svg>`, tooltip `<div>`, and `<script>` payload.
#' @family state profile
#' @export
nt_profile_race_ratio_svg <- function(dates, series, reference = "white",
                                      labels = c(black = "Black", latine = "Latine",
                                                 other = "Other", white = "White"),
                                      y_min = NULL, y_max = NULL, grid_step = 0.25,
                                      chart_id = "chart-race-ratio",
                                      focal = "black",
                                      parity_label = "parity with white renters",
                                      aria = paste("Eviction filing rate of each group as a",
                                                   "ratio of the white filing rate")) {
  stopifnot("reference series missing" = reference %in% names(series))
  ref <- series[[reference]]
  stopifnot("reference rate hits zero, so a ratio is undefined" = all(ref > 0))

  ratios <- lapply(series[setdiff(names(series), reference)], function(v) v / ref)
  # The reference is drawn as a flat line at 1.0 rather than dropped, so the
  # reader can see what the others are measured against.
  ratios[[reference]] <- rep(1, length(ref))

  rng <- range(unlist(ratios), na.rm = TRUE)
  if (is.null(y_min)) y_min <- floor((rng[1] - 0.05) / grid_step) * grid_step
  if (is.null(y_max)) y_max <- ceiling((rng[2] + 0.05) / grid_step) * grid_step

  fmt_ratio <- function(v) sub("\\.?0+$", "", sprintf("%.2f", v))
  paste0(
    .nt_race_svg(dates, ratios, labels, chart_id,
                 y_max = y_max, grid_step = grid_step, y_min = y_min,
                 y_fmt = function(v) paste0(fmt_ratio(v), "×"),
                 tip_fmt = function(v) sprintf("%.2f", v),
                 label_fmt = function(v) paste0(sprintf("%.2f", v), "×"),
                 focal = focal,
                 parity = list(at = 1, label = parity_label),
                 aria = aria,
                 x0 = 40, x1 = 400, y_floor = 231, y_top = 71),
    .nt_race_payload(dates, ratios, labels, chart_id, tip_digits = 2,
                     tip_suffix = "×", x0 = 40, x1 = 400,
                     y_floor = 231, y_top = 71, y_min = y_min, y_max = y_max))
}

#' Filing rate by race, one bar per group (state-profile Figure 6)
#'
#' The Minnesota profile's race bars, generalized. The trend and ratio charts
#' answer how the gap moved; this one answers how big it is right now, which
#' is the number a reader repeats. Geometry is Minnesota's, so the chart looks
#' the same on any state that draws it.
#'
#' Bars carry `data-key`/`data-label`/`data-value`, which is what the page
#' template's `attachBarChart` hover reads, so the tooltip needs no payload.
#'
#' @param values Named numeric vector of rates, drawn in the order given.
#'   Sort before calling if a particular order is wanted.
#' @param labels Named character vector of display labels. Defaults to the
#'   four standard groups; names not present here fall back to the key.
#' @param focal Series taking the red bar and the bold axis label
#'   (default `"black"`).
#' @param grid_step Gridline interval in rate units (default 10).
#' @param y_max Top of the scale; `NULL` rounds the largest bar up to the
#'   next `grid_step`.
#' @param chart_id Element id root. The SVG is `<chart_id>-svg` and the
#'   tooltip `<chart_id>-tooltip`.
#' @param bar_w,x_start,x_step Bar width and horizontal placement, in viewBox
#'   units. The defaults are Minnesota's hand-tuned values for four groups.
#' @param aria Accessible description of the chart.
#' @return Character scalar: the `<svg>` and its tooltip `<div>`.
#' @family state profile
#' @export
nt_profile_race_bars_svg <- function(values,
                                     labels = c(black = "Black", latine = "Latine",
                                                other = "Other", white = "White"),
                                     focal = "black",
                                     grid_step = 10,
                                     y_max = NULL,
                                     chart_id = "chart-race-bars",
                                     bar_w = 40, x_start = 80, x_step = 90,
                                     aria = paste("Eviction filing rate per 1,000 renter",
                                                  "households by race, most recent 12 months")) {
  stopifnot("values must be named" = !is.null(names(values)),
            "values must be finite" = all(is.finite(values)))

  # Plot area, from the Minnesota original. y = 220 is zero, y = 50 the ceiling.
  X0 <- 60; X1 <- 430; Y0 <- 220; Y1 <- 50
  if (is.null(y_max)) y_max <- ceiling(max(values) / grid_step) * grid_step
  y_of <- function(v) Y0 - v * (Y0 - Y1) / y_max

  # Half-values round away from zero, as they were placed by hand on the
  # Minnesota page; sprintf("%.0f") rounds half to even and shifts a gridline.
  py <- function(v) floor(y_of(v) + 0.5)
  grid <- paste0(vapply(rev(seq(grid_step, y_max, by = grid_step)), function(v) {
    paste0(sprintf('<line class="grid-line" x1="%d" y1="%d" x2="%d" y2="%d"/>', X0, py(v), X1, py(v)),
           sprintf('<text class="grid-label" x="%d" y="%d" text-anchor="end">%s</text>',
                   X0 - 4, py(v) + 4, format(v, trim = TRUE)))
  }, character(1)), collapse = "")

  keys <- names(values)
  bars <- paste0(vapply(seq_along(keys), function(i) {
    k <- keys[i]
    lab <- if (!is.na(labels[k])) unname(labels[k]) else k
    x <- x_start + (i - 1) * x_step
    cx <- x + bar_w / 2
    top <- y_of(values[[k]])
    # Value labels sit 8px clear of the bar, as they do on the Minnesota page.
    paste0(
      sprintf('<rect class="rbar%s" data-key="%s" data-label="%s" data-value="%s" x="%.0f" y="%.1f" width="%.0f" height="%.1f"/>',
              if (identical(k, focal)) " focal" else "", k, lab,
              format(round(values[[k]], 1), trim = TRUE), x, top, bar_w, Y0 - top),
      sprintf('<text class="rbval" x="%.0f" y="%.1f" text-anchor="middle">%s</text>',
              cx, top - 8, format(round(values[[k]], 1), trim = TRUE)),
      sprintf('<text class="rbax" x="%.0f" y="%d" text-anchor="middle"%s>%s</text>',
              cx, Y0 + 14,
              if (identical(k, focal)) ' style="font-weight: 600; fill: #19222C;"' else "", lab))
  }, character(1)), collapse = "")

  paste0(
    sprintf('<svg id="%s-svg" viewBox="0 0 480 240" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="%s">',
            chart_id, aria),
    '<style>',
    '.rbax{font-family:Inter,sans-serif;font-size:calc(11px / var(--sx,1));fill:#586573}',
    '.rbval{font-family:Inter,sans-serif;font-size:calc(13px / var(--sx,1));fill:#19222C;',
    'font-weight:600;font-variant-numeric:tabular-nums}',
    '.rbar{fill:#19222C}',
    '.rbar.focal{fill:#F9322B}',
    '.rbase{stroke:#19222C;stroke-width:1}',
    '</style>',
    '<g class="hover-grid">', grid, '</g>',
    sprintf('<line class="rbase" x1="%d" y1="%d" x2="%d" y2="%d"/>', X0, Y0, X1, Y0),
    bars, '</svg>',
    sprintf('<div class="hover-tooltip" id="%s-tooltip"><div class="tip-date"></div><div class="tip-value"></div></div>',
            chart_id))
}

# ==========================================================================
# nt_profile_rent_dumbbell_svg() — rent then vs. now, one row per place
# ==========================================================================

#' Two-date rent comparison (state-profile rents figure) as inline SVG
#'
#' Builds the profile pages' rent plate: one row per geography, an open dot at
#' the earlier rent, a filled dot at the later one, the connector between
#' them, the current level printed at the dot and the percent change at the
#' right edge. Rows sort by percent change, so the markets that moved most sit
#' on top. The chart is static by design and carries no hover JS: every value
#' it encodes is already printed on it, which is what makes a two-date
#' comparison readable without interaction.
#'
#' Pairs with [nt_apartmentlist_rents()], but takes plain vectors, so any
#' two-date rent or price series can drive it.
#'
#' @param labels Character vector of place names, one per row.
#' @param from,to Numeric rents at the earlier and later date, same length as
#'   `labels`.
#' @param from_label,to_label Date labels for the key beneath the chart (e.g.
#'   `"February 2020"`).
#' @param accent Optional character vector of `labels` to draw in the focal
#'   colour and bold, typically the statewide row.
#' @param x_min,x_max Rent-axis bounds; default to the data rounded out to
#'   `x_step`. Set them to hold one scale across states or releases.
#' @param x_step Gridline interval in dollars (default 500).
#' @param row_height Vertical pitch per row in design units (default 34).
#' @param svg_id Element id, kept unique per page.
#' @return Character scalar: the complete `<svg>...</svg>` block.
#' @family state profile
#' @export
nt_profile_rent_dumbbell_svg <- function(labels, from, to,
                                         from_label, to_label,
                                         accent = NULL,
                                         x_min = NULL, x_max = NULL,
                                         x_step = 500, row_height = 34,
                                         svg_id = "chart-rents-svg") {
  stopifnot(length(labels) == length(from), length(from) == length(to),
            length(labels) >= 1, all(from > 0))
  pct <- 100 * (to - from) / from
  ord <- order(pct, decreasing = TRUE)
  labels <- labels[ord]; from <- from[ord]; to <- to[ord]; pct <- pct[ord]
  n <- length(labels)

  # The label gutter is fixed so the dot field starts in the same place
  # regardless of the longest place name.
  X0 <- 196; X1 <- 838
  TOP <- 58
  H <- TOP + n * row_height + 34
  lo <- if (is.null(x_min)) floor(min(from, to) / x_step) * x_step else x_min
  hi <- if (is.null(x_max)) ceiling(max(from, to) / x_step) * x_step else x_max
  x_of <- function(v) X0 + (v - lo) * (X1 - X0) / (hi - lo)
  y_of <- function(i) TOP + (i - 0.5) * row_height

  dollars <- function(v) paste0("$", formatC(round(v), big.mark = ",", format = "d"))

  ticks <- seq(lo, hi, by = x_step)
  grid <- paste(sprintf(paste0(
    '        <line class="rg" x1="%.1f" y1="%d" x2="%.1f" y2="%.1f"/>\n',
    '        <text class="rax" x="%.1f" y="%d" text-anchor="middle">%s</text>'),
    x_of(ticks), TOP - 24, x_of(ticks), TOP + n * row_height - 6,
    x_of(ticks), TOP - 32, dollars(ticks)), collapse = "\n")

  rows <- vapply(seq_len(n), function(i) {
    cls <- if (!is.null(accent) && labels[i] %in% accent) " acc" else ""
    y <- y_of(i)
    paste0(
      sprintf('        <g class="rrow%s">\n', cls),
      sprintf('          <text class="rlab" x="%d" y="%.1f" text-anchor="end">%s</text>\n',
              X0 - 22, y + 4, labels[i]),
      sprintf('          <line class="rconn" x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f"/>\n',
              x_of(from[i]), y, x_of(to[i]), y),
      sprintf('          <circle class="rfrom" cx="%.1f" cy="%.1f" r="4.5"/>\n',
              x_of(from[i]), y),
      sprintf('          <circle class="rto" cx="%.1f" cy="%.1f" r="4.5"/>\n',
              x_of(to[i]), y),
      sprintf('          <text class="rval" x="%.1f" y="%.1f">%s</text>\n',
              x_of(to[i]) + 11, y + 4, dollars(to[i])),
      sprintf('          <text class="rpct" x="%d" y="%.1f" text-anchor="end">%+.0f%%</text>\n',
              990, y + 4, pct[i]),
      '        </g>')
  }, character(1))

  paste0(
    '      <svg id="', svg_id, '" viewBox="0 0 1000 ', H,
    '" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="',
    sprintf('Typical rent by place, %s compared with %s', from_label, to_label),
    '">\n',
    '        <style>\n',
    '          .rax  { font-family: \'Inter\', sans-serif; font-size: calc(11px / var(--sx, 1)); fill: #586573; letter-spacing: 0.02em; }\n',
    '          .rlab { font-family: \'Inter\', sans-serif; font-size: calc(13px / var(--sx, 1)); fill: #19222C; }\n',
    '          .rval { font-family: \'Inter\', sans-serif; font-size: calc(12px / var(--sx, 1)); font-weight: 500; fill: #19222C; }\n',
    '          .rpct { font-family: \'Inter\', sans-serif; font-size: calc(12px / var(--sx, 1)); font-weight: 500; fill: #586573; }\n',
    '          .rg   { stroke: #19222C; stroke-width: 0.5; opacity: 0.12; }\n',
    '          .rconn{ stroke: #19222C; stroke-width: 1.5; opacity: 0.45; }\n',
    '          .rfrom{ fill: #ffffff; stroke: #6c7a89; stroke-width: 1.5; }\n',
    '          .rto  { fill: #19222C; }\n',
    '          .rrow.acc .rlab { font-weight: 700; }\n',
    '          .rrow.acc .rto  { fill: #F9322B; }\n',
    '          .rrow.acc .rval,\n',
    '          .rrow.acc .rpct { fill: #F9322B; font-weight: 700; }\n',
    '          .rkey { font-family: \'Inter\', sans-serif; font-size: calc(11.5px / var(--sx, 1)); fill: #586573; }\n',
    '        </style>\n',
    sprintf('        <!-- Rent scale %s to %s, gridlines every %s. -->\n',
            dollars(lo), dollars(hi), dollars(x_step)),
    grid, '\n',
    paste(rows, collapse = "\n"), '\n',
    '        <!-- Key: which dot is which date. -->\n',
    sprintf('        <circle class="rfrom" cx="%d" cy="%d" r="4.5"/>\n', X0, H - 12),
    sprintf('        <text class="rkey" x="%d" y="%d">%s</text>\n', X0 + 11, H - 8, from_label),
    sprintf('        <circle class="rto" cx="%d" cy="%d" r="4.5"/>\n', X0 + 132, H - 12),
    sprintf('        <text class="rkey" x="%d" y="%d">%s</text>\n', X0 + 143, H - 8, to_label),
    '      </svg>'
  )
}

# ==========================================================================
# nt_profile_rent_trend_svg() — multi-series rent lines, focal + colored
# ==========================================================================

#' Rent-level line chart (state-profile rents figure) as interactive inline SVG
#'
#' Draws one line per geography with the focal series in the profile ink and
#' the comparison series in a colorblind-aware categorical palette, end labels
#' named, valued, and tinted to their lines, an optional dated reference line,
#' and the same hover scaffolding as [nt_profile_race_trend_svg()]: a shared
#' vertical guide and, in this chart's `nearest` hover mode, a single dot and
#' a one-row tooltip on the line nearest the pointer (a ten-row column tooltip
#' blocked the upper lines), bound by the page template's
#' `attachMultiLineChart`. Series that start late (leading `NA`s) simply begin
#' where their data does; the hover skips them in the months they lack.
#'
#' Palette colors are assigned to the non-focal series in descending order of
#' their final value, so lines that end near each other take contrasting hues.
#' The default palette is the house red-free qualitative set (see
#' `ern_swatch()`), red-free so the focal series' red end dot stays the only
#' red on the plate, plus the legacy burnt orange for a ninth series. End
#' labels keep readable ink and carry a small dot in the line's color instead
#' of tinted text (gold and steel fail contrast as small text).
#'
#' Pairs with [nt_apartmentlist_rents()], but takes plain vectors, so any
#' monthly level series can drive it.
#'
#' @param dates Date vector, one per month, ascending, shared by all series.
#' @param series Named list of numeric vectors, one per geography, each the
#'   length of `dates`. `NA` runs are not drawn.
#' @param focal Name of the series drawn in the profile ink with the red end
#'   and hover dots; every other series takes the palette.
#' @param palette Character vector of line colors for the non-focal series,
#'   recycled if shorter. The default is a colorblind-aware categorical set.
#' @param y_min,y_max Axis bounds; default to the data rounded out to
#'   `y_step`.
#' @param y_step Gridline interval in dollars (default 500).
#' @param mark Optional `list(date =, label =)` — a dashed vertical reference
#'   at that month (e.g. the last pre-pandemic month).
#' @param chart_id Element id root; the SVG, indicator, and tooltip take
#'   `<id>-svg`, `<id>-indicator`, `<id>-tooltip`, and the enclosing figure
#'   div must carry `id="<chart_id>"` for the hover to bind.
#' @param aria Accessible description of the figure.
#' @return Character scalar: `<svg>`, tooltip `<div>`, and the `<script>`
#'   payload the template's hover binder reads.
#' @family state profile
#' @export
nt_profile_rent_trend_svg <- function(dates, series, focal,
                                      palette = c("#2A8A86", "#C8860D", "#5A3A88",
                                                  "#C95123", "#223754", "#7B96B5",
                                                  "#2C6B3A", "#7A4A1E", "#586573"),
                                      y_min = NULL, y_max = NULL, y_step = 500,
                                      mark = NULL,
                                      chart_id = "chart-rents",
                                      aria = "Estimated median rent on new leases by place and month") {
  stopifnot("series must be a named list" = !is.null(names(series)),
            "every series must match the dates" =
              all(vapply(series, length, integer(1)) == length(dates)),
            "focal must name a series" = focal %in% names(series))
  n <- length(dates)
  X0 <- 52; X1 <- 830; Y_TOP <- 40; Y_FLOOR <- 310; VB_W <- 1000; VB_H <- 344
  vals <- unlist(series)
  lo <- if (is.null(y_min)) floor(min(vals, na.rm = TRUE) / y_step) * y_step else y_min
  hi <- if (is.null(y_max)) ceiling(max(vals, na.rm = TRUE) / y_step) * y_step else y_max
  x_of <- function(i) X0 + (i - 1) * (X1 - X0) / (n - 1)
  y_of <- function(v) Y_FLOOR - (v - lo) * (Y_FLOOR - Y_TOP) / (hi - lo)
  dollars <- function(v) paste0("$", formatC(round(v), big.mark = ",", format = "d"))
  slug <- function(x) gsub("[^a-z0-9]+", "-", tolower(x))
  last_val <- function(v) v[max(which(!is.na(v)))]

  # Per-series keys, used as swatch classes, data-series attributes on the
  # drawn elements, and the isolation dropdown's option values.
  keys <- stats::setNames(paste0(slug(chart_id), "-", slug(names(series))), names(series))

  # Colors: focal keeps the ink; the rest take the palette in descending
  # end-value order, so near-neighbors at the right edge contrast.
  others <- setdiff(names(series), focal)
  others <- others[order(-vapply(series[others], last_val, numeric(1)))]
  col <- stats::setNames(rep_len(palette, length(others)), others)
  col[focal] <- "#19222C"

  grid_vals <- seq(lo + y_step, hi, by = y_step)
  grid <- paste(sprintf(paste0(
    '        <line class="rtg" x1="%d" y1="%.1f" x2="%d" y2="%.1f"/>\n',
    '        <text class="rtax" x="%d" y="%.1f" text-anchor="end">%s</text>'),
    X0, y_of(grid_vals), X1, y_of(grid_vals),
    X0 - 6, y_of(grid_vals) + 4, dollars(grid_vals)), collapse = "\n")

  # One path per series; NA months break into separate segments rather than
  # being interpolated across.
  path_d <- function(v) {
    ok <- !is.na(v)
    runs <- split(seq_len(n)[ok], cumsum(!ok)[ok])
    paste(vapply(runs, function(idx) paste0(
      "M ", paste(sprintf("%.1f %.1f", x_of(idx), y_of(v[idx])), collapse = " L ")),
      character(1)), collapse = " ")
  }
  paths <- paste(vapply(others, function(g) sprintf(
    '        <path class="rt-line" data-series="%s" stroke="%s" d="%s"/>',
    keys[[g]], col[[g]], path_d(series[[g]])), character(1)), collapse = "\n")
  focal_v <- series[[focal]]
  focal_path <- sprintf('        <path class="rt-focal" data-series="%s" d="%s"/>',
                        keys[[focal]], path_d(focal_v))

  # End labels: "Name · $X,XXX", ordered by final value, tinted to the line,
  # nudged apart where two series finish within 11 design-px.
  ends <- data.frame(g = names(series),
                     v = vapply(series, last_val, numeric(1)))
  ends <- ends[order(-ends$v), ]
  ends$y <- y_of(ends$v)
  for (i in seq_len(nrow(ends))[-1]) {
    if (ends$y[i] - ends$y[i - 1] < 11) ends$y[i] <- ends$y[i - 1] + 11
  }
  end_labels <- paste(vapply(seq_len(nrow(ends)), function(i) {
    g <- ends$g[i]
    if (g == focal) sprintf(
      '        <text class="rtlab focal" data-series="%s" x="%d" y="%.1f">%s &#183; %s</text>',
      keys[[g]], X1 + 8, ends$y[i] + 4, g, dollars(ends$v[i]))
    else paste0(
      sprintf('        <circle class="rtlab-dot" data-series="%s" cx="%d" cy="%.1f" r="3" fill="%s"/>\n',
              keys[[g]], X1 + 11, ends$y[i], col[[g]]),
      sprintf('        <text class="rtlab" data-series="%s" x="%d" y="%.1f">%s &#183; %s</text>',
              keys[[g]], X1 + 18, ends$y[i] + 4, g, dollars(ends$v[i])))
  }, character(1)), collapse = "\n")
  focal_dot <- sprintf('        <circle class="rt-dot" cx="%.1f" cy="%.1f" r="3.5"/>',
                       x_of(max(which(!is.na(focal_v)))), y_of(last_val(focal_v)))

  mark_svg <- ""
  if (!is.null(mark)) {
    mi <- which(dates == as.Date(mark$date))
    if (length(mi) == 1) mark_svg <- sprintf(paste0(
      '        <line class="rt-mark" x1="%.1f" y1="%d" x2="%.1f" y2="%d"/>\n',
      '        <text class="rt-mark-label" x="%.1f" y="%d" text-anchor="middle">%s</text>\n'),
      x_of(mi), Y_TOP - 8, x_of(mi), Y_FLOOR, x_of(mi), Y_TOP - 14, mark$label)
  }

  years <- sort(unique(as.integer(format(dates, "%Y"))))
  tick_years <- unique(c(years[seq(1, length(years), by = 2)], max(years)))
  tick_i <- match(as.Date(paste0(tick_years, "-01-01")), dates)
  tick_x <- x_of(tick_i); tick_x[tick_years == max(tick_years)] <- X1
  anchor <- ifelse(tick_years == min(tick_years), "",
            ifelse(tick_years == max(tick_years), ' text-anchor="end"',
                   ' text-anchor="middle"'))
  axis_svg <- paste(sprintf(
    '        <text class="rtax" x="%.0f" y="%d"%s>%d</text>',
    tick_x, Y_FLOOR + 20, anchor, tick_years), collapse = "\n")

  # Tooltip swatch colors. A <style> inside an inline SVG is document CSS, so
  # these classes reach the HTML tooltip; the chart_id prefix keeps them from
  # colliding with anything else on the page.
  swatch_css <- paste(vapply(names(series), function(g) sprintf(
    '          .tip-row .swatch.%s { background: %s; border-color: %s; }',
    keys[[g]], col[[g]], col[[g]]), character(1)), collapse = "\n")

  # Hover scaffolding: the template's attachMultiLineChart moves one dot per
  # series, keyed rt-<key>, inside the indicator group.
  hover_dots <- paste(vapply(names(series), function(g) sprintf(
    '          <circle class="hover-dot rt-%s" cx="0" cy="0" r="%s" fill="%s" stroke="#fff" stroke-width="1.25"/>',
    keys[[g]], if (g == focal) "4" else "3",
    if (g == focal) "#F9322B" else col[[g]]), character(1)), collapse = "\n")

  # Payload: counties in end-value order, the focal series LAST so the
  # template treats it as the lead (tooltip anchor + first row). NAs become
  # JS nulls; the binder hides that series' dot and row for those months.
  arr <- function(v) paste0("[", paste(ifelse(is.na(v), "null",
                                              sprintf("%.0f", v)), collapse = ","), "]")
  ordered_for_payload <- c(rev(others), focal)   # reversed again by the binder
  payload <- paste0(
    '{ x0: ', X0, ', x1: ', X1, ', yFloor: ', Y_FLOOR, ', yTop: ', Y_TOP,
    ', yMin: ', lo, ', yMax: ', hi, ', vbW: ', VB_W, ', vbH: ', VB_H,
    ', digits: 0, prefix: "$", suffix: "", mode: "nearest"',
    ', months: [', paste(sprintf('"%s"', format(dates, "%b %Y")), collapse = ","), ']',
    ', series: [',
    paste(vapply(ordered_for_payload, function(g) sprintf(
      '{ key: "%s", label: %s, swatch: "%s", values: %s }',
      keys[[g]], jsonlite::toJSON(g, auto_unbox = TRUE), keys[[g]],
      arr(series[[g]])), character(1)), collapse = ","),
    '] }')

  # Isolation chooser, the Figure 3 control pattern: pick one place and every
  # other line dims to a ghost field. The template binder wires it by id;
  # options run focal first, then end-value order to match the labels.
  sel_order <- c(focal, as.character(ends$g[ends$g != focal]))
  select_html <- paste0(
    '      <div class="ct-controls">\n',
    '        <label class="ct-select-wrap">\n',
    '          <span class="ct-select-label">Show:</span>\n',
    '          <select id="', chart_id, '-select" class="ct-select">\n',
    '            <option value="">&#8212; all places &#8212;</option>\n',
    paste(sprintf('            <option value="%s">%s</option>',
                  keys[sel_order], sel_order), collapse = "\n"), '\n',
    '          </select>\n',
    '        </label>\n',
    '      </div>\n')

  paste0(
    select_html,
    '      <svg id="', chart_id, '-svg" viewBox="0 0 ', VB_W, ' ', VB_H,
    '" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="', aria, '">\n',
    '        <style>\n',
    '          .rtax  { font-family: \'Inter\', sans-serif; font-size: calc(11px / var(--sx, 1)); fill: #586573; letter-spacing: 0.02em; }\n',
    '          .rtg   { stroke: #19222C; stroke-width: 0.5; opacity: 0.12; }\n',
    '          .rt-line { fill: none; stroke-width: 1.4; stroke-linejoin: round; opacity: 0.9; }\n',
    '          .rt-focal { fill: none; stroke: #19222C; stroke-width: 2.25; stroke-linejoin: round; }\n',
    '          .rt-dot { fill: #F9322B; }\n',
    '          .rtlab { font-family: \'Inter\', sans-serif; font-size: calc(11.5px / var(--sx, 1)); fill: #586573; }\n',
    '          .rtlab.focal { font-size: calc(12.5px / var(--sx, 1)); font-weight: 700; fill: #19222C; }\n',
    '          .rt-mark { stroke: #19222C; stroke-width: 0.75; stroke-dasharray: 3 4; opacity: 0.55; }\n',
    '          .rt-mark-label { font-family: \'Inter\', sans-serif; font-style: italic; font-size: calc(11px / var(--sx, 1)); fill: #586573; }\n',
    '          .rt-dim { opacity: 0.12; }\n',
    swatch_css, '\n',
    '        </style>\n',
    sprintf('        <!-- Rent scale %s to %s, gridlines every %s. -->\n',
            dollars(lo), dollars(hi), dollars(y_step)),
    grid, '\n', mark_svg, paths, '\n', focal_path, '\n', focal_dot, '\n',
    end_labels, '\n', axis_svg, '\n',
    '        <g class="hover-indicator" id="', chart_id, '-indicator">\n',
    sprintf('          <line class="hover-vguide" x1="0" y1="%d" x2="0" y2="%d"/>\n',
            Y_TOP - 8, Y_FLOOR),
    hover_dots, '\n',
    '        </g>\n',
    '      </svg>\n',
    sprintf('      <div class="hover-tooltip" id="%s-tooltip"><div class="tip-date"></div><div class="tip-rows"></div></div>\n',
            chart_id),
    sprintf('<script>window.__ernRaceCharts = window.__ernRaceCharts || {}; window.__ernRaceCharts[%s] = %s;</script>',
            jsonlite::toJSON(chart_id, auto_unbox = TRUE), payload)
  )
}

# ------------------------------------------------------------------------------
# Two-measure scatter (state-profile comparison plate)
# ------------------------------------------------------------------------------

#' Two-measure county scatter as inline SVG
#'
#' Plots one county-level rate against another so a reader can see whether the
#' two move together. Built for the question "does the county that files the
#' most evictions have the most homelessness?", where the answer turns out to
#' be no and the chart has to make that legible rather than argue it.
#'
#' Design choices that follow from that job:
#' \itemize{
#'   \item No trend line. Drawing one over an uncorrelated cloud invites the
#'     reader to see a slope that the correlation says is not there. The
#'     correlation belongs in the caption, in words.
#'   \item Point area is proportional to `weight` (renter households), so a
#'     county of 400 renters cannot read as loud as King.
#'   \item Only `label` counties are named. Labelling all 39 turns the plate
#'     into a word cloud; naming the extremes and the big five is what carries
#'     the argument.
#' }
#'
#' Static by design and carries no hover JS, like [nt_profile_rent_dumbbell_svg()]:
#' the point of the plate is the shape of the cloud, not any single value.
#'
#' @param x,y Numeric vectors of the two rates, same length.
#' @param labels Character vector of place names, same length as `x`.
#' @param weight Optional numeric sizing vector (e.g. renter households).
#'   Areas, not radii, scale with it. Defaults to equal sizes.
#' @param label_which Character vector of `labels` to print on the plate.
#'   Defaults to none.
#' @param accent Character vector of `labels` to draw in the focal colour.
#' @param x_lab,y_lab Axis titles.
#' @param x_step,y_step Gridline intervals in data units.
#' @param x_max,y_max Axis ceilings; default to the data rounded out to a step.
#' @param svg_id Element id, kept unique per page.
#' @return Character scalar: the complete `<svg>...</svg>` block.
#' @family state profile
#' @export
nt_profile_scatter_svg <- function(x, y, labels,
                                   weight = NULL,
                                   label_which = character(0),
                                   accent = character(0),
                                   x_lab = "", y_lab = "",
                                   x_step = 10, y_step = 20,
                                   x_max = NULL, y_max = NULL,
                                   svg_id = "chart-scatter-svg") {
  stopifnot(length(x) == length(y), length(x) == length(labels))
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]; labels <- labels[ok]
  weight <- if (is.null(weight)) rep(1, length(x)) else weight[ok]

  x_max <- if (is.null(x_max)) ceiling(max(x, na.rm = TRUE) / x_step) * x_step else x_max
  y_max <- if (is.null(y_max)) ceiling(max(y, na.rm = TRUE) / y_step) * y_step else y_max

  # plot frame inside the 1000 x 360 viewBox
  px0 <- 78; px1 <- 962; py0 <- 30; py1 <- 296
  x_of <- function(v) px0 + (v / x_max) * (px1 - px0)
  y_of <- function(v) py1 - (v / y_max) * (py1 - py0)

  grid_y <- .nt_grid_lines_svg(seq(y_step, y_max, by = y_step), px0, px1, px0 - 10,
                               y_of, function(v) format(v, trim = TRUE))
  xt <- seq(0, x_max, by = x_step)
  grid_x <- paste(sprintf(
    paste0('          <line class="grid-line" x1="%.1f" y1="%d" x2="%.1f" y2="%d"/>\n',
           '          <text class="grid-label" x="%.1f" y="%d" text-anchor="middle">%s</text>'),
    x_of(xt), py0, x_of(xt), py1, x_of(xt), py1 + 20, format(xt, trim = TRUE)),
    collapse = "\n")

  # area, not radius, tracks the weight
  w <- weight / max(weight, na.rm = TRUE)
  r <- 4 + 13 * sqrt(w)

  is_acc <- labels %in% accent
  ord <- order(r, decreasing = TRUE)          # big points first, small on top
  pts <- paste(sprintf(
    '          <circle class="%s" cx="%.1f" cy="%.1f" r="%.1f"/>',
    ifelse(is_acc[ord], "sc-dot sc-dot-accent", "sc-dot"),
    x_of(x[ord]), y_of(y[ord]), r[ord]), collapse = "\n")

  lab_i <- which(labels %in% label_which)
  labs <- if (length(lab_i)) paste(sprintf(
    '          <text class="sc-label%s" x="%.1f" y="%.1f" text-anchor="middle">%s</text>',
    ifelse(is_acc[lab_i], " sc-label-accent", ""),
    x_of(x[lab_i]), y_of(y[lab_i]) - r[lab_i] - 6, labels[lab_i]), collapse = "\n") else ""

  paste0(
    '      <svg id="', svg_id, '" viewBox="0 0 1000 360" xmlns="http://www.w3.org/2000/svg">\n',
    '        <g class="grid">\n', grid_x, '\n', grid_y, '\n        </g>\n',
    '        <g class="points">\n', pts, '\n        </g>\n',
    if (nzchar(labs)) paste0('        <g class="labels">\n', labs, '\n        </g>\n') else "",
    sprintf('        <text class="sc-axis-title" x="%.1f" y="%d" text-anchor="middle">%s</text>\n',
            (px0 + px1) / 2, py1 + 48, x_lab),
    sprintf('        <text class="sc-axis-title" transform="rotate(-90 %d %.1f)" x="%d" y="%.1f" text-anchor="middle">%s</text>\n',
            22, (py0 + py1) / 2, 22, (py0 + py1) / 2, y_lab),
    '      </svg>'
  )
}
