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
#' @return Character scalar: the complete `<svg>...</svg>` block.
#' @family state profile
#' @export
nt_profile_trend_svg <- function(dates, values, baseline,
                                 band_start = NULL, band_end = NULL,
                                 baseline_range_label = "Jan 2017 – Feb 2020",
                                 y_step = 500,
                                 svg_id = "chart-monthly-svg",
                                 indicator_id = "chart-monthly-indicator",
                                 source_note = "d3_state_agg") {
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
  annot_label <- sprintf("%s · %s", .nt_fmt_int(values[mN]),
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
      '             moratorium block; the band shades the start→end span of the\n',
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
    '          .annot { font-family: \'Inter\', sans-serif; font-style: italic; font-size: calc(13px / var(--sx, 1)); fill: #19222C; }\n',
    '          .annot-line { stroke: #19222C; stroke-width: 0.5; stroke-dasharray: 2 3; }\n',
    '          .baseline { stroke: #6c7a89; stroke-width: 0.5; stroke-dasharray: 1 4; }\n',
    '          .data { fill: none; stroke: #19222C; stroke-width: 1.5; stroke-linejoin: round; }\n',
    '          .focal { fill: #F9322B; }\n',
    '          .moratoria-band { fill: #19222C; opacity: 0.05; }\n',
    '        </style>\n',
    band_svg,
    sprintf(paste0(
      '        <!-- Y scale: 0 → y=%d (floor), %d → y=%d (data-driven).\n',
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
#'   partial year.
#' @param y_ceiling_step Step the y ceiling rounds up to (default 2500);
#'   gridlines draw every `y_grid_step` (default 5000).
#' @param y_grid_step Gridline interval (default 5000, labeled "5k", "10k", …).
#' @param svg_id Element id the page JS looks up.
#' @return Character scalar: the complete `<svg>...</svg>` block.
#' @family state profile
#' @export
nt_profile_yearly_svg <- function(years, values, baseline,
                                  latest_month = 12,
                                  projection = NULL,
                                  y_ceiling_step = 2500,
                                  y_grid_step = 5000,
                                  svg_id = "chart-yearly-svg") {
  stopifnot(length(years) == length(values))
  yN <- length(years)
  yX0 <- 70; ybW <- 55; ybGap <- 20
  y_x <- yX0 + (seq_len(yN) - 1) * (ybW + ybGap)
  y_floor <- 280
  latest_year <- max(years)
  months_short <- c("Jan","Feb","Mar","Apr","May","Jun",
                    "Jul","Aug","Sep","Oct","Nov","Dec")

  proj_hi <- if (!is.null(projection)) projection$hi else 0
  y_max  <- max(y_grid_step,
                ceiling(max(c(values, baseline, proj_hi), na.rm = TRUE) / y_ceiling_step) * y_ceiling_step)
  y_unit <- 220 / y_max
  y_of <- function(v) y_floor - v * y_unit
  grid_svg <- .nt_grid_lines_svg(seq(y_grid_step, y_max, by = y_grid_step), 40, 970, 36,
                                 y_of, function(v) paste0(v / 1000, "k"))
  y_height <- values * y_unit
  y_y <- y_floor - y_height

  is_partial <- years == latest_year & latest_month < 12
  year_data_label <- ifelse(is_partial,
                            sprintf("%d (Jan–%s)", years, months_short[latest_month]),
                            as.character(years))
  bar_class <- ifelse(is_partial, "ybar focal", "ybar")
  val_class <- ifelse(is_partial, "yval focal", "yval")
  has_proj <- is_partial & !is.null(projection)
  proj_attr <- ifelse(has_proj,
    sprintf(' data-proj-total="%s" data-proj-lo="%s" data-proj-hi="%s"',
            .nt_fmt_int(round(if (!is.null(projection)) projection$total else 0)),
            .nt_fmt_int(round(if (!is.null(projection)) projection$lo    else 0)),
            .nt_fmt_int(round(if (!is.null(projection)) projection$hi    else 0))),
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
      .proj_y_lo <- y_floor - projection$lo * y_unit
      .proj_y_hi <- y_floor - projection$hi * y_unit
      proj_svg <- paste0(
        '        <!-- Projection for partial latest year (ARIMA on monthly 2023+). -->\n',
        sprintf(paste0(
          '        <!-- 80%% PI band: pale fill from PI-low to PI-high -->\n',
          '        <rect class="yproj-pi" x="%.0f" y="%.1f" width="%d" height="%.1f"/>\n',
          '        <!-- Dashed outline from top-of-actual up to projected mean -->\n',
          '        <rect class="yproj-bar" x="%.0f" y="%.1f" width="%d" height="%.1f"/>\n',
          '        <!-- Point-estimate tick + label -->\n',
          '        <line class="yproj-tick" x1="%.0f" y1="%.1f" x2="%.0f" y2="%.1f"/>\n',
          '        <text class="yproj-label" x="%.0f" y="%.1f" text-anchor="middle">~%s projected</text>\n'),
          .px, .proj_y_hi, ybW, .proj_y_lo - .proj_y_hi,
          .px, .proj_y, ybW, .actual_y - .proj_y,
          .px - 4, .proj_y, .px + ybW + 4, .proj_y,
          .px + ybW/2, .proj_y - 8, .nt_fmt_int(round(projection$total))))
    }
  }

  axis_svg <- paste(
    sprintf('        <text class="yax" x="%.0f" y="%d" text-anchor="middle"%s>%d</text>',
            y_x + ybW/2, 304,
            ifelse(years == latest_year,
                   ' style="font-weight: 600; fill: #19222C;"', ""),
            years),
    collapse = "\n"
  )

  paste0(
    '      <svg id="', svg_id, '" viewBox="0 0 1000 320" xmlns="http://www.w3.org/2000/svg">\n',
    '        <style>\n',
    '          .yax    { font-family: \'Inter\', sans-serif; font-size: calc(11px / var(--sx, 1)); fill: #586573; letter-spacing: 0.02em; }\n',
    '          .yval   { font-family: \'Inter\', sans-serif; font-size: calc(13px / var(--sx, 1)); fill: #19222C; font-weight: 600; font-variant-numeric: tabular-nums; }\n',
    '          .yval.focal { fill: #F9322B; }\n',
    '          .ybar   { fill: #19222C; }\n',
    '          .ybar.focal { fill: #F9322B; }\n',
    '          .ybaseline { stroke: #6c7a89; stroke-width: 0.5; stroke-dasharray: 1 4; }\n',
    '          .yfloor { stroke: #19222C; stroke-width: 1; }\n',
    '          /* Projection styles — pale band = 80% PI; dashed outline = point estimate */\n',
    '          .yproj-pi    { fill: rgba(249, 50, 43, 0.07); stroke: none; }\n',
    '          .yproj-bar   { fill: none; stroke: #F9322B; stroke-width: 1; stroke-dasharray: 3 3; opacity: 0.85; }\n',
    '          .yproj-tick  { stroke: #F9322B; stroke-width: 1.5; }\n',
    '          .yproj-label { font-family: \'Inter\', sans-serif; font-size: calc(11px / var(--sx, 1)); font-style: italic; fill: #CC2118; font-weight: 600; font-variant-numeric: tabular-nums; }\n',
    '        </style>\n',
    sprintf(paste0(
      '        <!-- Y scale: 0 → y=%d (floor); ceiling is data-driven in render.R.\n',
      '             Gridlines + labels generated there (grid_lines_svg) so they always\n',
      '             match the bar scale (previously hardcoded 5k/10k/15k drifted when\n',
      '             the projection pushed the ceiling past 18k). -->\n'), y_floor),
    '        <g class="hover-grid">\n',
    grid_svg, '\n',
    '        </g>\n',
    '        <!-- Pre-pandemic baseline line; label lives in the legend strip below the SVG. -->\n',
    sprintf('        <line class="ybaseline" x1="40" y1="%.1f" x2="970" y2="%.1f"/>\n',
            y_of(baseline), y_of(baseline)),
    sprintf('        <line class="yfloor" x1="40" y1="%d" x2="970" y2="%d"/>\n', y_floor, y_floor),
    '        <!-- Bars + value labels (one rect+text per year). -->\n',
    bars_svg, '\n',
    proj_svg,
    '        <!-- x-axis labels (one per year). -->\n',
    axis_svg, '\n',
    '      </svg>'
  )
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
      num  = c("①", "②", "③", "④"),
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
    pathway_title = "FROM EVICTION TO DISPLACEMENT →",
    footer_lines = c(
      "Prevent and Protect interrupt the eviction; Preserve and Produce ease the rent pressure that starts it.",
      "Right to counsel needs both: substantial, sustained funding to meet today's caseload, and better ways to connect tenants to the lawyers.",
      "Tiers and reinforcement reflect the eviction-prevention literature (HUD-ERA; Cassidy & Currie; Desmond; Colburn & Aldern). This profile's data\ndescribe filing trends, not causal policy effects.  ·  Eviction Research Network, UC Berkeley"),
    aria_label = paste0(
      "The four P's of eviction prevention — Prevent, Protect, Preserve, Produce — ",
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
    add(text(mx, py, "→", 32, navy, anchor = "middle", baseline = "central", weight = "800"))
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
