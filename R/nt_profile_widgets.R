# ==============================================================================
# nt_profile_widgets.R — ERN state-profile interactive shells + data payloads
#
# The state-profile pages' interactive pieces (the county-trend chart, the
# MapLibre county explorer, the searchable county table) are drawn by the
# page's own vanilla JS from data embedded in the document. These functions
# emit the two halves that the page JS binds together:
#   * the HTML shells (ids/classes are the JS contract — keep them), and
#   * <script> data payloads assigned to window.* globals.
# The interaction JS itself ships with the page template (the Quarto format
# extension), not with this package.
# ==============================================================================

#' Emit raw HTML from a Quarto/knitr chunk
#'
#' Wraps a string in a pandoc `{=html}` raw block and `cat()`s it, so markup
#' emitted from a chunk with `results: asis` passes through pandoc untouched
#' (indented HTML would otherwise be re-parsed as markdown and can turn into
#' code blocks).
#'
#' @param ... Character pieces, pasted together in order.
#' @return Invisibly, the assembled string (after printing it).
#' @family state profile
#' @export
nt_raw_html <- function(...) {
  s <- paste0(..., collapse = "")
  cat("```{=html}\n", s, "\n```\n", sep = "")
  invisible(s)
}

#' County-trend chart shell (state-profile Figure 3)
#'
#' Emits the interactive county-trend chart's HTML shell: the yearly/monthly
#' toggle, the county-highlight dropdown, the empty `<svg>` the page JS draws
#' into (d3 with Catmull-Rom smoothing), the tooltip node, and the caption.
#' Pair with [nt_profile_county_trend_data()], which embeds the data payload
#' the JS reads.
#'
#' @param label Eyebrow label (e.g. `"Figure 3 · Counties on the rise"`).
#' @param caption_num Caption figure number (e.g. `"Figure 3"`).
#' @param caption_desc Caption description HTML. The page JS rewrites it when
#'   the granularity toggles, so seed it with the monthly variant.
#' @param caption_src Caption source line (e.g. `"source · wa ocla"`).
#' @param id Shell element id.
#' @param projection_toggle Add the yearly view's latest-year control:
#'   "Projected year-end" (the default state) or the count to date, labelled
#'   `through_label`. The page JS shows it only in the yearly view and only
#'   when [nt_profile_county_trend_data()] carries county projections. `FALSE`
#'   (default) leaves the output byte-identical.
#' @param through_label Label of the to-date button (e.g. `"Through
#'   September"`).
#' @return Character scalar: the `<div class="plate-rising-chart">…</div>`
#'   block.
#' @family state profile
#' @export
nt_profile_county_trend <- function(label,
                                    caption_num,
                                    caption_desc,
                                    caption_src,
                                    id = "plate-counties-rising",
                                    projection_toggle = FALSE,
                                    through_label = "To date") {
  # Its own button class: the granularity handler binds every .ct-toggle-btn.
  proj_toggle <- if (projection_toggle) paste0(
    '          <div class="ct-toggle ct-proj-toggle" role="group" aria-label="Latest year" hidden>\n',
    '            <button type="button" class="ct-proj-btn ct-active"\n',
    '                    data-proj="on"\n',
    '                    aria-pressed="true">Projected year-end</button>\n',
    '            <button type="button" class="ct-proj-btn"\n',
    '                    data-proj="off"\n',
    '                    aria-pressed="false">', through_label, '</button>\n',
    '          </div>\n') else ""
  paste0(
    '      <div class="plate-rising-chart" id="', id, '">\n',
    '        <span class="plate-num">', label, '</span>\n',
    '        <div class="ct-controls">\n',
    '          <div class="ct-toggle" role="group" aria-label="Time granularity">\n',
    '            <button type="button" class="ct-toggle-btn"\n',
    '                    data-granularity="yearly"\n',
    '                    aria-pressed="false">Yearly</button>\n',
    '            <button type="button" class="ct-toggle-btn ct-active"\n',
    '                    data-granularity="monthly"\n',
    '                    aria-pressed="true">Monthly</button>\n',
    '          </div>\n',
    proj_toggle,
    '          <label class="ct-select-wrap">\n',
    '            <span class="ct-select-label">Highlight:</span>\n',
    '            <select id="ct-county-select" class="ct-select">\n',
    '              <option value="">\u2014 pick a county \u2014</option>\n',
    '            </select>\n',
    '          </label>\n',
    '        </div>\n',
    '        <svg id="chart-county-trend" viewBox="0 0 880 360" preserveAspectRatio="xMidYMid meet"></svg>\n',
    '        <div class="ct-tooltip" id="ct-tooltip" role="status" aria-live="polite"></div>\n',
    '        <div class="caption">\n',
    '          <span class="num">', caption_num, '</span>\n',
    '          <span class="desc" id="ct-caption-desc">', caption_desc, '</span>\n',
    '          <span class="src">', caption_src, '</span>\n',
    '        </div>\n',
    '      </div>'
  )
}

#' County-trend data payload (state-profile Figure 3)
#'
#' Builds the compact yearly + monthly per-county payload the county-trend
#' chart JS consumes, and returns it as a `<script>` tag assigning a
#' `window.*` global. Counties are ordered by their record (all-time full-
#' year) filing count, descending; each carries `rec = TRUE` when that record
#' fell in the last `record_window` complete years — the chart colors those
#' lines red.
#'
#' @param county_agg Data frame of county-month filings: `county_code`,
#'   `county`, `year`, `month`, `filings` (one row per county-month).
#' @param record_window A county is a "record county" when its best full year
#'   is within this many years of the latest year (default 2, i.e. the last
#'   two complete years for a partial current year).
#' @param global Name of the `window.` global to assign (default
#'   `"__waCountyTrendData"`, the name the shipped profile JS reads).
#' @param projection Optional county year-end projections for a partial latest
#'   year, from [nt_profile_project_counties()]. Each county then carries
#'   `proj`, and the payload `projYear` and `projThrough` (the last observed
#'   month's name), which the yearly view draws as a dashed final segment.
#'   `NULL` (default) leaves the output unchanged.
#' @return Character scalar: a `<script>…</script>` tag.
#' @family state profile
#' @export
nt_profile_county_trend_data <- function(county_agg, record_window = 2,
                                         global = "__waCountyTrendData",
                                         projection = NULL) {
  stopifnot(all(c("county_code", "county", "year", "month", "filings") %in% names(county_agg)))

  latest_year <- max(county_agg$year)
  latest_mo   <- max(county_agg$month[county_agg$year == latest_year])

  ct_yearly <- county_agg |>
    dplyr::group_by(.data$county_code, .data$county, .data$year) |>
    dplyr::summarise(filings = sum(.data$filings, na.rm = TRUE), .groups = "drop")

  ct_full_years <- ct_yearly |>
    dplyr::filter(.data$year < latest_year | (.data$year == latest_year & latest_mo == 12))
  ct_records <- ct_full_years |>
    dplyr::group_by(.data$county_code) |>
    dplyr::summarise(record_year    = .data$year[which.max(.data$filings)],
                     record_filings = max(.data$filings),
                     .groups = "drop") |>
    dplyr::mutate(is_record = .data$record_year >= (latest_year - as.integer(record_window)))

  ct_years  <- min(ct_yearly$year):max(ct_yearly$year)
  ct_yearly_wide <- ct_yearly |>
    dplyr::select("county_code", "county", "year", "filings") |>
    dplyr::arrange(.data$county_code, .data$year)

  ct_monthly <- county_agg |>
    dplyr::arrange(.data$county_code, .data$year, .data$month) |>
    dplyr::mutate(ym = sprintf("%d-%02d", .data$year, .data$month))
  ct_months <- unique(ct_monthly$ym)

  name_of <- ct_yearly |> dplyr::distinct(.data$county_code, .data$county)
  ct_county_list <- ct_records |>
    dplyr::arrange(dplyr::desc(.data$record_filings)) |>
    dplyr::transmute(code = .data$county_code,
                     name = name_of$county[match(.data$county_code, name_of$county_code)],
                     rec  = .data$is_record)

  to_yearly_vec <- function(code) {
    d <- ct_yearly_wide[ct_yearly_wide$county_code == code, ]
    out <- rep(0L, length(ct_years))
    out[match(d$year, ct_years)] <- as.integer(d$filings)
    out
  }
  to_monthly_vec <- function(code) {
    d <- ct_monthly[ct_monthly$county_code == code, ]
    out <- rep(0L, length(ct_months))
    out[match(d$ym, ct_months)] <- as.integer(d$filings)
    out
  }

  if (!is.null(projection)) {
    stopifnot(all(c("county_code", "total") %in% names(projection)),
              all(ct_county_list$code %in% projection$county_code))
  }
  counties_json <- jsonlite::toJSON(
    lapply(seq_len(nrow(ct_county_list)), function(i) {
      out <- list(code    = jsonlite::unbox(ct_county_list$code[i]),
                  name    = jsonlite::unbox(ct_county_list$name[i]),
                  rec     = jsonlite::unbox(ct_county_list$rec[i]),
                  yearly  = to_yearly_vec(ct_county_list$code[i]),
                  monthly = to_monthly_vec(ct_county_list$code[i]))
      if (!is.null(projection)) {
        out$proj <- jsonlite::unbox(as.integer(round(
          projection$total[match(ct_county_list$code[i], projection$county_code)])))
      }
      out
    }),
    auto_unbox = FALSE
  )

  proj <- if (is.null(projection)) "" else
    sprintf(', projYear: %d, projThrough: "%s"', as.integer(latest_year), month.name[latest_mo])
  sprintf('    <script>window.%s = { years: %s, months: %s, counties: %s%s };</script>',
          global,
          jsonlite::toJSON(ct_years),
          jsonlite::toJSON(ct_months),
          counties_json, proj)
}

#' County year-end projections that sum to the state's (Figure 3)
#'
#' Splits the statewide forecast for the rest of a partial year (from
#' [nt_profile_project_year()]) across counties by each county's share of
#' statewide filings over the last `window` months, and adds each county's
#' count to date. The county totals therefore add up exactly to the state's
#' year-end projection, and a small county never gets a forecast of its own
#' from a handful of filings. It assumes each county keeps its recent share
#' through year-end; it is an allocation, not a county-level model.
#'
#' @param county_agg Data frame of county-month filings: `county_code`,
#'   `year`, `month`, `filings`.
#' @param projection The state's year-end projection, a list with `total` and
#'   `ytd` as [nt_profile_project_year()] returns. `NULL` returns `NULL`.
#' @param window Months of recent filings that set each county's share
#'   (default 12).
#' @return `NULL`, or a data frame with `county_code`, `ytd`, `share`, and
#'   `total` (the projected year-end count).
#' @family state profile
#' @export
nt_profile_project_counties <- function(county_agg, projection, window = 12L) {
  if (is.null(projection)) return(NULL)
  stopifnot(all(c("county_code", "year", "month", "filings") %in% names(county_agg)))
  d <- as.Date(sprintf("%d-%02d-01", county_agg$year, county_agg$month))
  latest <- max(d)
  latest_year <- as.integer(format(latest, "%Y"))
  recent <- d > seq(latest, by = "-1 month", length.out = window + 1L)[window + 1L]
  in_year <- county_agg$year == latest_year

  codes <- sort(unique(county_agg$county_code))
  sum_by <- function(keep) {
    v <- tapply(county_agg$filings[keep], county_agg$county_code[keep], sum, na.rm = TRUE)
    out <- as.numeric(v[codes]); out[is.na(out)] <- 0; out
  }
  recent_n <- sum_by(recent)
  ytd      <- sum_by(in_year)
  share    <- recent_n / sum(recent_n)
  data.frame(county_code = codes, ytd = ytd, share = share,
             total = ytd + share * (projection$total - projection$ytd))
}

#' Monthly-series data payload (state-profile Figure 1)
#'
#' Embeds the monthly filings series plus the scale constants the Figure 1
#' interaction JS needs (hover tooltips + the phone re-window), as a
#' `<script>` tag assigning a `window.*` global.
#'
#' @param values Integer filings per month, ascending by month.
#' @param year_base,month_base Calendar anchor of `values[1]`: its year, and
#'   its zero-based month (January = 0).
#' @param y_div The chart's y-axis ceiling — must equal the `y_max` that
#'   [nt_profile_trend_svg()] derived (series max rounded up to `y_step`).
#' @param global Name of the `window.` global to assign.
#' @param projection Optional forecast from [nt_profile_project_months()]. When
#'   given, the payload also carries the rounded `mean`, `lo`, and `hi` series
#'   and `y_step`, which the page JS needs to draw a projection horizon and
#'   re-ceiling the y-axis to fit its band. `NULL` (default) leaves the output
#'   unchanged.
#' @param y_step Gridline interval of the chart, as passed to
#'   [nt_profile_trend_svg()] (default 500). Emitted only with `projection`.
#' @return Character scalar: a `<script>…</script>` tag.
#' @family state profile
#' @export
nt_profile_monthly_js <- function(values, year_base, month_base = 0,
                                  y_div, global = "__waMonthly",
                                  projection = NULL, y_step = 500) {
  proj <- ""
  if (!is.null(projection)) {
    stopifnot(all(c("mean", "lo", "hi") %in% names(projection)))
    arr <- function(x) paste(round(x), collapse = ", ")
    proj <- sprintf(", yStep: %d, proj: { mean: [%s], lo: [%s], hi: [%s] }",
                    as.integer(y_step), arr(projection$mean),
                    arr(pmax(projection$lo, 0)), arr(projection$hi))
  }
  sprintf('    <script>window.%s = { filings: [%s], yearBase: %d, monthBase: %d, yDiv: %d%s };</script>',
          global, paste(values, collapse = ", "),
          as.integer(year_base), as.integer(month_base), as.integer(y_div), proj)
}

#' County picker for the yearly bars (Figure 2)
#'
#' A dropdown ("Show: Washington (statewide)" plus every county, alphabetical)
#' that the page JS binds to, swapping the statewide chart for the county
#' chart [nt_profile_yearly_counties()] drew.
#'
#' @param county_agg Data frame with `county_code` and `county`.
#' @param state_name Name of the statewide option and of the state in the
#'   caption, which the JS rewrites to the picked county.
#' @return Character scalar of HTML.
#' @family state profile
#' @export
nt_profile_yearly_picker <- function(county_agg, state_name) {
  co <- unique(county_agg[c("county_code", "county")])
  co <- co[order(co$county), ]
  paste0(
    '      <div class="ct-controls yearly-controls">\n',
    '        <label class="ct-select-wrap">\n',
    '          <span class="ct-select-label">Show:</span>\n',
    '          <select id="yearly-county-select" class="ct-select" data-state="', state_name, '">\n',
    '            <option value="">', state_name, ' (statewide)</option>\n',
    paste(sprintf('            <option value="%s">%s County</option>', co$county_code, co$county),
          collapse = "\n"), '\n',
    '          </select>\n',
    '        </label>\n',
    '      </div>'
  )
}

#' Projection-horizon buttons for the monthly trend chart (Figure 1)
#'
#' A segmented control the page JS binds to: "Off" plus one button per
#' horizon. Picking a horizon extends Figure 1 by that many forecast months
#' (dashed expected count and the prediction band), using the forecast carried
#' by [nt_profile_monthly_js()]. Pair with `projection = TRUE` in
#' [nt_profile_trend_svg()]. The buttons take their own class,
#' `proj-toggle-btn`, so the county-trend toggle handler never binds to them.
#'
#' @param horizons Integer months, one button each (default 3, 6, 12).
#' @param label Visible label before the buttons.
#' @param selected Horizon the chart opens on: 0 (default) for "Off", or one
#'   of `horizons`. The page JS draws that horizon on load.
#' @return Character scalar of HTML.
#' @family state profile
#' @export
nt_profile_projection_toggle <- function(horizons = c(3L, 6L, 12L),
                                         label = "Projection",
                                         selected = 0L) {
  stopifnot(selected %in% c(0L, horizons))
  btn <- function(h, text, on) {
    sprintf('            <button type="button" class="proj-toggle-btn%s" data-h="%d" aria-pressed="%s">%s</button>',
            if (on) " ct-active" else "", as.integer(h), if (on) "true" else "false", text)
  }
  paste0(
    '      <div class="ct-controls proj-controls">\n',
    '          <span class="ct-select-label">', label, ':</span>\n',
    '          <div class="ct-toggle" role="group" aria-label="', label, ' horizon">\n',
    btn(0L, "Off", selected == 0), '\n',
    paste(vapply(horizons, function(h) btn(h, sprintf("%d months", as.integer(h)), h == selected),
                 character(1)), collapse = "\n"), '\n',
    '          </div>\n',
    '      </div>'
  )
}

#' Searchable county table (state-profile Table 1)
#'
#' Emits the all-counties table: the data payload (a `window.*` global the
#' page JS renders rows from, re-summing when the map explorer's time window
#' changes), the search/sort shell, and the caption. Row markup — inline rate
#' bars, the † small-renter flag, the red leader highlight — is drawn by the
#' page JS.
#'
#' @param counties Data frame with `county`, `renters`, `filings`, `rate`
#'   (rate per 1,000 renter households over the display window). Rows are
#'   emitted in the order given — sort by rate descending for the shipped
#'   look. The top row by rate is flagged `hi: true` (red highlight).
#' @param caption_num Caption label (e.g. `"Table 1"`).
#' @param caption_desc Caption description.
#' @param caption_src Caption source line.
#' @param filings_th Initial "Filings" column header; the page JS rewrites it
#'   when the explorer window changes (default `"Filings (12 mo.)"`).
#' @param search_placeholder Placeholder for the county filter box.
#' @param global Name of the `window.` global holding the rows.
#' @param sub_labels Optional named character vector of second-line column
#'   labels, keyed `county`, `renters`, `filings`, `rate`, `chg`. Renders as
#'   `<span class="th-sub">` under the header, the way California's annual
#'   table names each column's period and vintage. `NULL` (default) emits the
#'   single-line headers unchanged.
#' @param change_th Header for a change column comparing the displayed window
#'   against each county's pre-pandemic rate. Requires a `base_rate` column on
#'   `counties`, the same rate measured over the baseline period. `NULL`
#'   (default) omits the column entirely, so states that have no pre-pandemic
#'   baseline in their series are unaffected.
#' @return Character scalar: `<script>` payload + `<div class="table-wrap">`
#'   shell + caption.
#' @family state profile
#' @export
nt_profile_county_table <- function(counties,
                                    caption_num = "Table 1",
                                    caption_desc,
                                    caption_src,
                                    filings_th = "Filings (12 mo.)",
                                    search_placeholder = "Search county\u2026",
                                    global = "__waCountyTable",
                                    sub_labels = NULL,
                                    change_th = NULL) {
  stopifnot(all(c("county", "renters", "filings", "rate", "chg") %in% names(counties)))

  # Second-line header labels. A missing key renders nothing, so a caller can
  # label two columns and leave the rest alone.
  sub <- function(key) {
    if (is.null(sub_labels) || is.na(sub_labels[key]) || !nzchar(sub_labels[key])) return("")
    sprintf('<span class="th-sub">%s</span>', sub_labels[[key]])
  }

  # which.max ignores NA, so a table carrying no-data rows still highlights the
  # real leader rather than nothing.
  hi_row <- which.max(counties$rate)
  # `chg` is the percent change in the annualized rate between the two ends of
  # the window, comparing blocks of up to 12 months at each end rather than
  # single months: most counties file single digits a month, and one unusual
  # month at either end would otherwise swing the column and flip signs. The
  # value here is the static one for the default window; the page recomputes it
  # from the monthly series whenever the slider moves, by the same rule.
  chg_js <- ifelse(is.na(counties$chg), ", chg: null",
                   sprintf(", chg: %.1f", counties$chg))
  # A row may carry no data at all: a county the source does not cover still
  # belongs in the table, so the reader can see it is absent rather than assume
  # it was overlooked. R's sprintf would render those NAs as the literal `NA`,
  # which is not valid JavaScript and would take the whole table down, so every
  # numeric field is emitted as `null` instead. The page already renders a null
  # rate as "not reported" (template.html, hasRate).
  jnum <- function(x, fmt) ifelse(is.na(x), "null", sprintf(fmt, x))
  rows_js <- paste0(
    "[\n",
    paste0(
      "      ",
      sprintf('{ county: %s, renters: %s, filings: %s, rate: %s%s%s }',
              vapply(counties$county,
                     function(s) as.character(jsonlite::toJSON(s, auto_unbox = TRUE)),
                     character(1)),
              jnum(round(counties$renters), "%d"),
              jnum(round(counties$filings), "%d"),
              jnum(counties$rate, "%.1f"),
              chg_js,
              ifelse(seq_len(nrow(counties)) == hi_row, ", hi: true", "")),
      collapse = ",\n"
    ),
    "\n    ]"
  )

  if (is.null(change_th)) change_th <- "Change in rate"
  chg_th_html <- paste0(
    '                <th data-sort="chg" class="num-col col-chg" id="th-chg">',
    change_th, sub("chg"), '</th>\n')

  paste0(
    '      <script>window.', global, ' = ', rows_js, ';</script>\n',
    '      <div class="table-wrap">\n',
    '        <div class="table-search">\n',
    '          <input id="county-search" type="search" placeholder="', search_placeholder,
    '" aria-label="Search counties">\n',
    '          <span class="count" id="county-count">\u2014 counties</span>\n',
    '          <button type="button"\n',
    '                  id="county-bars-toggle"\n',
    '                  class="bars-toggle"\n',
    '                  aria-pressed="true"\n',
    '                  aria-label="Toggle inline rate bars"\n',
    '                  title="Show or hide the inline bars in each column">Hide bars</button>\n',
    '        </div>\n',
    '        <div class="table-scroll">\n',
    '          <table class="county-table" id="county-table">\n',
    '            <thead>\n',
    '              <tr>\n',
    '                <th data-sort="county">County', sub("county"), '</th>\n',
    '                <th data-sort="filings" class="num-col col-filings" id="th-filings">',
    '<span class="th-main">', filings_th, '</span>', sub("filings"), '</th>\n',
    '                <th data-sort="renters" class="num-col col-renters">Renter HH', sub("renters"), '</th>\n',
    '                <th data-sort="rate" class="num-col sort-desc col-rate">Rate / 1,000', sub("rate"), '</th>\n',
    chg_th_html,
    '              </tr>\n',
    '            </thead>\n',
    '            <tbody id="county-tbody">\n',
    '              <!-- Rows injected by JS -->\n',
    '            </tbody>\n',
    '          </table>\n',
    '        </div>\n',
    '      </div>\n',
    '      <div class="caption">\n',
    '        <span class="num">', caption_num, '</span>\n',
    '        <span class="desc">', caption_desc, '</span>\n',
    '        <span class="src">', caption_src, '</span>\n',
    '      </div>'
  )
}

#' MapLibre county explorer (state-profile Figure 4)
#'
#' Emits the profile pages' full map-explorer section: the MapLibre map node
#' with its PMTiles + choropleth configuration in `data-*` attributes, the
#' address-search toolbar, help panel/onboarding/loading affordances, the
#' boundary/name toggles, legend node, usage disclosure, caption, the
#' time-window slider stack, and the live readout card (stat tiles, monthly
#' sparkline, and the ACS county-characteristics block). All behavior is
#' bound by the page template's explorer JS via the emitted ids.
#'
#' @param county_json,acs_json Relative URLs (with cache-bust query) of the
#'   county-monthly and county-ACS JSON sidecars.
#' @param pmtiles Relative URL (with cache-bust query) of the county PMTiles.
#' @param center Numeric `c(lon, lat)` initial map center.
#' @param zoom Initial zoom (default 6).
#' @param bbox Numeric `c(w, s, e, n)` bounds for fit + search biasing.
#' @param breaks,colors,labels Initial choropleth bins: lower bounds, fills,
#'   and tier names (the JS recomputes numeric breaks per window via ckmeans;
#'   labels persist).
#' @param choropleth_var Variable the choropleth colors (default
#'   `"rate_12mo"`).
#' @param state_name,state_abbr Used across the search/help/readout copy.
#' @param n_counties County count for the rank tile label.
#' @param window_full_label The full selectable window, for the disclosure
#'   copy (e.g. `"January 2016 to April 2026"`).
#' @param county_scale_note Parenthetical explaining why the map stays at
#'   county scale (data-feed constraint), used in the disclosure.
#' @param caption_num,caption_desc,caption_src Figure caption fields.
#' @param ec_note_html The `<p class="ec-note">…</p>` inner HTML: the ACS
#'   vintage + model-driver note shown under the county characteristics.
#' @param tract_note Clause completing the map-help "Zoom in" line, for a
#'   state whose tract layer covers a different window than the slider.
#' @param tract_json,tract_pmtiles Optional tract-level equivalents (empty
#'   strings — the default — keep the explorer county-only).
#' @return Character scalar: the `<section class="map-explorer">…</section>`
#'   block.
#' @family state profile
#' @export
nt_profile_map_explorer <- function(county_json, acs_json,
                                    pmtiles,
                                    center, zoom = 6, bbox,
                                    breaks = c(0, 15, 25, 35),
                                    colors = c("#fed976", "#fd8d3c", "#F9322B", "#54278f"),
                                    labels = c("Lower", "Moderate", "High", "Extreme"),
                                    choropleth_var = "rate_12mo",
                                    state_name = "Washington", state_abbr = "WA",
                                    n_counties = 39,
                                    window_full_label = "January 2016 to April 2026",
                                    county_scale_note = "Washington's OCLA feed publishes county-level filings only",
                                    caption_num = "Figure 4",
                                    caption_desc = "Filing rate per 1,000 renter households by county, Washington.",
                                    caption_src = "source \u00b7 ocla \u00b7 acs \u00b7 ern \u00b7 2026",
                                    ec_note_html = paste0(
                                      'ACS 5-year, 2020&ndash;2024. The <span class="ec-driver-tag">&#9650;</span> marks <strong>severe rent burden</strong>, ',
                                      'one of the two factors the statistical model identified as most strongly tied to higher eviction-filing rates. ',
                                      'The other is a county\'s Black population share, a structural signal of where disinvestment and segregation concentrate, ',
                                      'not a statement about individuals. Affordability is shown as rent burden, not AMI.'),
                                    tract_note = paste("the legend keeps its county-rate bins"),
                                    tract_json = "", tract_pmtiles = "") {
  json_vec <- function(x) as.character(jsonlite::toJSON(x))
  paste0(
    '    <section class="map-explorer"\n',
    '             data-county-json="', county_json, '"\n',
    '             data-tract-json="', tract_json, '"\n',
    '             data-county-acs-json="', acs_json, '">\n',
    '      <div class="counties-map-grid">\n',
    '      <div class="counties-map-col">\n',
    '        <div class="map-section">\n',
    '        <div id="state-map"\n',
    '             data-county-pmtiles="', pmtiles, '"\n',
    '             data-tract-pmtiles="', tract_pmtiles, '"\n',
    sprintf('             data-center-lon="%.6f"\n', center[1]),
    sprintf('             data-center-lat="%.6f"\n', center[2]),
    '             data-zoom="', zoom, '"\n',
    '             data-choropleth-var="', choropleth_var, '"\n',
    "             data-breaks='", json_vec(breaks), "'\n",
    "             data-colors='", json_vec(colors), "'\n",
    "             data-labels='", json_vec(labels), "'\n",
    "             data-bbox='", json_vec(bbox), "'></div>\n",
    '        <div class="map-toolbar" role="search">\n',
    '          <input id="map-search" type="search" autocomplete="off"\n',
    '                 placeholder="Search address or place in ', state_name, '\u2026"\n',
    '                 aria-label="Search the map"\n',
    '                 title="Type any ', state_abbr, ' address or place, then press Enter">\n',
    '          <button id="map-search-btn" type="button"\n',
    '                  title="Run search and drop a pin">Search</button>\n',
    '        </div>\n',
    '        <button class="map-help-btn" id="map-help-toggle"\n',
    '                type="button"\n',
    '                aria-label="Map help"\n',
    '                title="Show map usage tips">?</button>\n',
    '        <div class="map-help-panel" id="map-help-panel" hidden>\n',
    '          <h4>How to use this map</h4>\n',
    '          <ul>\n',
    '            <li><strong>Drag the slider</strong> below the map to pick a time window; bins recompute live.</li>\n',
    '            <li><strong>Zoom in</strong> on any region for closer reading; ', tract_note, '.</li>\n',
    '            <li><strong>Hover</strong> any polygon for its window rate.</li>\n',
    '            <li><strong>Click</strong> any polygon to load its full profile in the card.</li>\n',
    '            <li><strong>Click a legend tier</strong> to isolate; <em>show all</em> resets.</li>\n',
    '            <li><strong>Search box</strong> (top-left): jump to any ', state_abbr, ' address or place.</li>\n',
    '            <li><strong>My-location button</strong> (bottom-right): zoom to your position.</li>\n',
    '            <li><strong>Fullscreen icon</strong>: take over the screen.</li>\n',
    '          </ul>\n',
    '        </div>\n',
    '        <div class="map-onboarding" id="map-onboarding">\n',
    '          Drag the slider below for any time window \u00b7 click a legend tier to isolate \u00b7 click a county for its profile.\n',
    '          <span class="close" id="map-onboarding-close" aria-label="Dismiss">\u00d7</span>\n',
    '        </div>\n',
    '        <!-- Loading indicator: visible whenever the choropleth fill-opacity\n',
    '             is 0 (initial load, or any time the map is in transition).\n',
    '             JS hides this the moment the first paintMapState lands the\n',
    '             real ckmeans paint. -->\n',
    '        <div class="map-loading" id="map-loading" role="status" aria-live="polite">\n',
    '          <span class="map-loading-spinner" aria-hidden="true"></span>\n',
    '          <span class="map-loading-label">Loading map data\u2026</span>\n',
    '        </div>\n',
    '      </div>\n',
    '      <div class="map-help-inline" id="map-zoom-hint">HOVER OR CLICK ANY COUNTY FOR ITS PROFILE  \u00b7  PINCH OR SCROLL TO ZOOM</div>\n',
    '      <div class="map-toggle-row">\n',
    '        <button type="button" id="map-counties-toggle" class="map-toggle-btn"\n',
    '                aria-pressed="true"\n',
    '                aria-label="Toggle county boundary lines"\n',
    '                title="Show or hide county boundary lines on the map">Hide county boundaries</button>\n',
    '        <button type="button" id="map-county-names-toggle" class="map-toggle-btn"\n',
    '                aria-pressed="true"\n',
    '                aria-label="Toggle county name labels"\n',
    '                title="Show or hide county name labels on the map">Hide county names</button>\n',
    '      </div>\n',
    '      <p class="map-refresh-hint">If the map appears blank or stuck, <a href="#" onclick="window.location.reload();return false;">refresh the page</a>; it resets the slider, the legend, and the map state.</p>\n',
    '      <div class="map-legend" id="map-legend"></div>\n',
    '      <details class="map-help-disclosure">\n',
    '        <summary>How to use this map</summary>\n',
    '        <ul>\n',
    '          <li><strong>Drag the date-range slider</strong> below the map to pick any window from ', window_full_label,
    ', or type exact dates in the Start / End pickers. The choropleth, the legend bins, and the stats card all update with the window.</li>\n',
    '          <li><strong>Zoom in</strong> on any region for closer reading. The map stays at the county scale at every zoom (', county_scale_note, ').</li>\n',
    '          <li><strong>Hover</strong> any polygon for a quick read (name + rate in the active window).</li>\n',
    '          <li><strong>Click</strong> any polygon to load its full profile in the stats card below: annualized rate, filings, rank, a monthly mini-chart, and county demographics.</li>\n',
    '          <li><strong>Click a legend tier</strong> (Lower / Moderate / High / Extreme) to isolate it; click another tier to add it on, or click <em>show all</em> to reset. The selection persists through slider drags and zoom changes; only polygons currently in your chosen tier(s) stay visible.</li>\n',
    '          <li><strong>Search box</strong> (top-left): jump to any ', state_abbr, ' address or place.</li>\n',
    '          <li><strong>My-location button</strong> (bottom-right): zoom to your position.</li>\n',
    '          <li><strong>Fullscreen icon</strong>: take over the screen for closer reading.</li>\n',
    '        </ul>\n',
    '      </details>\n',
    '      <div class="caption">\n',
    '        <span class="num">', caption_num, '</span>\n',
    '        <span class="desc">', caption_desc, '</span>\n',
    '        <span class="src">', caption_src, '</span>\n',
    '      </div>\n',
    '\n',
    '      <!-- Timeline slider, placed under the Figure 4 caption to fill the space\n',
    '           beside the taller readout card. Drives the map + readout window. -->\n',
    '      <div class="explorer-controls">\n',
    '        <div class="trend-stack">\n',
    '          <svg id="explorer-histogram" viewBox="0 0 600 70" preserveAspectRatio="none"></svg>\n',
    '          <div id="explorer-slider"></div>\n',
    '          <div class="trend-axis">\n',
    '            <span id="explorer-min-date">\u2014</span>\n',
    '            <span id="explorer-max-date">\u2014</span>\n',
    '          </div>\n',
    '        </div>\n',
    '        <div class="date-pickers-row">\n',
    '          <p class="hint">Drag the handles to resize the window, or grab the bar to slide it through time. Or type exact dates. The map and stats card follow.</p>\n',
    '          <div class="date-pickers">\n',
    '            <label>\n',
    '              <span>Start</span>\n',
    '              <input type="date" id="explorer-date-start" />\n',
    '            </label>\n',
    '            <label>\n',
    '              <span>End</span>\n',
    '              <input type="date" id="explorer-date-end" />\n',
    '            </label>\n',
    '            <button type="button" id="explorer-date-reset" class="ghost-btn">Reset</button>\n',
    '          </div>\n',
    '        </div>\n',
    '      </div>\n',
    '      </div><!-- /.counties-map-col -->\n',
    '\n',
    '      <!-- Live readout \u2014 the LEFT grid column (placed via CSS); the map sits to\n',
    '           its right. Updates on map click. Empty state = ', state_name, ' statewide. -->\n',
    '      <div class="explorer-card" id="explorer-card">\n',
    '        <div class="explorer-card-head">\n',
    '          <div class="explorer-place">\n',
    '            <span class="explorer-kicker" id="explorer-kicker">', state_name, ', statewide</span>\n',
    '            <span class="explorer-sub"    id="explorer-sub">click any county for its profile</span>\n',
    '          </div>\n',
    '          <div class="explorer-window" id="explorer-window">window \u2014</div>\n',
    '        </div>\n',
    '\n',
    '        <div class="explorer-tiles">\n',
    '          <div class="explorer-tile">\n',
    '            <div class="explorer-tile-num"   id="tile-rate">\u2014</div>\n',
    '            <div class="explorer-tile-label">rate per 1,000 renter HH<br><span class="explorer-tile-sublabel">annualized over window</span></div>\n',
    '          </div>\n',
    '          <div class="explorer-tile">\n',
    '            <div class="explorer-tile-num"   id="tile-filings">\u2014</div>\n',
    '            <div class="explorer-tile-label">filings in window</div>\n',
    '          </div>\n',
    '          <div class="explorer-tile">\n',
    '            <div class="explorer-tile-num"   id="tile-rank">\u2014</div>\n',
    '            <div class="explorer-tile-label" id="tile-rank-label">rank by rate (of ', n_counties, ' counties)</div>\n',
    '          </div>\n',
    '          <div class="explorer-tile explorer-tile-spark">\n',
    '            <div class="explorer-tile-label" id="spark-title" style="margin-bottom:6px;">state monthly filings in window</div>\n',
    '            <div class="spark-wrap">\n',
    '              <svg id="tile-spark" viewBox="0 0 220 64" preserveAspectRatio="none"></svg>\n',
    '              <!-- The dot lives in HTML so it stays a perfect circle\n',
    '                   regardless of SVG horizontal stretching. -->\n',
    '              <span class="spark-dot" id="spark-dot" hidden></span>\n',
    '            </div>\n',
    '            <!-- One-line caption below the chart: "max X \u00b7 MMM YYYY \u00b7 N filings".\n',
    '                 Each piece is HTML (no SVG stretching to fight). -->\n',
    '            <div class="spark-caption">\n',
    '              <span class="spark-caption-max" id="spark-axis-max">max \u2014</span>\n',
    '              <span class="spark-caption-sep" id="spark-caption-sep" hidden> \u00b7 </span>\n',
    '              <span class="spark-caption-tip" id="spark-tip" hidden>\n',
    '                <span class="spark-tip-date">\u2014</span> \u00b7 <span class="spark-tip-value">\u2014</span>\n',
    '              </span>\n',
    '            </div>\n',
    '          </div>\n',
    '        </div>\n',
    '\n',
    '        <!-- County characteristics (ACS 5-yr, from ', sub("\\?.*$", "", acs_json), ').\n',
    '             Rows carrying data-driver are tagged \u25b2 when they\'re among the\n',
    '             eviction model\'s most robust correlates (meta.drivers in the JSON;\n',
    '             restricted to NH-Black share + severe rent burden \u2014 see d7_county_acs.r). -->\n',
    '        <div class="explorer-chars" id="explorer-chars" hidden>\n',
    '          <div class="ec-group">\n',
    '            <div class="ec-group-title">Housing</div>\n',
    '            <div class="ec-row" data-driver="renter_share"><span class="ec-label">Renter households</span><span class="ec-val" id="ec-renters">\u2014</span></div>\n',
    '            <div class="ec-row" data-driver="med_rent"><span class="ec-label">Median rent</span><span class="ec-val" id="ec-medrent">\u2014</span></div>\n',
    '            <div class="ec-row" data-driver="vacancy_rate"><span class="ec-label">Rental vacancy</span><span class="ec-val" id="ec-vacancy">\u2014</span></div>\n',
    '          </div>\n',
    '          <div class="ec-group">\n',
    '            <div class="ec-group-title">Income &amp; affordability</div>\n',
    '            <div class="ec-row"><span class="ec-label">Median income (all households)</span><span class="ec-val" id="ec-incall">\u2014</span></div>\n',
    '            <div class="ec-row"><span class="ec-label">Median income (renters)</span><span class="ec-val" id="ec-incrent">\u2014</span></div>\n',
    '            <div class="ec-row"><span class="ec-label">Income needed to afford rent</span><span class="ec-val" id="ec-needed">\u2014</span></div>\n',
    '            <div class="ec-row" data-driver="rentburden_50plus"><span class="ec-label">Severely rent-burdened (&ge;50%)</span><span class="ec-val" id="ec-burden50">\u2014</span></div>\n',
    '            <div class="ec-row"><span class="ec-label">Cost-burdened (&ge;30%)</span><span class="ec-val" id="ec-burden30">\u2014</span></div>\n',
    '            <div class="ec-row"><span class="ec-label">Poverty rate</span><span class="ec-val" id="ec-pov">\u2014</span></div>\n',
    '            <div class="ec-row" data-driver="unemp_rate"><span class="ec-label">Unemployment</span><span class="ec-val" id="ec-unemp">\u2014</span></div>\n',
    '          </div>\n',
    '          <div class="ec-group">\n',
    '            <div class="ec-group-title">Who rents here</div>\n',
    '            <div class="ec-bars" id="ec-renter-race"></div>\n',
    '          </div>\n',
    '          <p class="ec-note">', ec_note_html, '</p>\n',
    '        </div>\n',
    '      </div><!-- /.explorer-card (readout) -->\n',
    '      </div><!-- /.counties-map-grid -->\n',
    '    </section>'
  )
}

# "August 19, 2026" without a platform-specific format string.
.nt_long_date <- function(x) {
  d <- as.Date(x)
  paste0(format(d, "%B"), " ", as.integer(format(d, "%d")), ", ", format(d, "%Y"))
}

#' Recent eviction-policy movements (state-profile News section)
#'
#' Renders `data/policy_news.json` as a dated list. The file's contract, and
#' the verification rules the review follows, are in
#' `library/standards/policy-news-schema.md`. Nothing here invents content:
#' the builder fails rather than render an item missing a source.
#'
#' A filing count without its policy context invites the wrong reading, since
#' filings move with the law in both directions. This section is what lets a
#' reader connect a step in the trend to the protection that arrived or lapsed.
#'
#' The status chip encodes whether the instrument is **in force**, not whether
#' it is good news. A preemption statute and a right-to-counsel statute are
#' both `enacted`.
#'
#' @param path Path to the state's `policy_news.json`.
#' @param form `"ledger"` (default) draws the compact date-rail ledger that
#'   replaced the cards on Washington's page. `"cards"` draws the superseded
#'   credits grid and `"list"` the older dated list; both are kept for a page
#'   that has not migrated.
#' @param max_items Most recent N to draw (default 8). The count of any
#'   remainder is stated rather than silently dropped.
#' @param state_name Full state name, for the empty-list sentence.
#' @param label,caption_num,caption_desc,caption_src Figure furniture. The
#'   review date is appended to `caption_desc` automatically.
#' @return Character scalar of HTML.
#' @family state profile
#' @export
nt_profile_news <- function(path,
                            form = c("ledger", "cards", "list"),
                            max_items = 8,
                            state_name = NULL,
                            label = "News · Eviction policy",
                            caption_num = "News",
                            caption_desc = "Recent movements in eviction policy.",
                            caption_src = "ern · policy review") {
  form <- match.arg(form)
  stopifnot("policy_news.json not found" = file.exists(path))
  doc <- jsonlite::fromJSON(path, simplifyVector = FALSE)

  # stopifnot() needs a literal name for its message, so dynamic field checks
  # go through a helper instead.
  need <- function(cond, msg) if (!isTRUE(cond)) stop(msg, call. = FALSE)
  for (k in c("state", "reviewed_on", "window_months", "items", "quarantine")) {
    need(!is.null(doc[[k]]), sprintf("policy_news.json is missing '%s'", k))
  }
  if (is.null(state_name)) state_name <- doc$state

  esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  # Chips group by whether the instrument is in force, never by valence.
  chip_class <- function(s) {
    if (s %in% c("enacted", "effective")) "in-force"
    else if (s %in% c("repealed", "struck down", "expired")) "lapsed"
    else "not-law"
  }
  # Month and year is the honest grain. A bill's "date" is the day it was
  # signed or took effect, and a reader does not need the day.
  mon <- function(d) format(as.Date(d), "%b %Y")

  items <- doc$items
  n_total <- length(items)
  shown <- utils::head(items, max_items)

  # Ledger row. Washington's form after Tim rejected the cards on 2026-08-20
  # for taking a screen and a half: one hairline row per instrument, an
  # effective-date rail on the left, mechanism and effect on the right, one
  # sources line under the whole ledger instead of one per card. The rail keys
  # on effective_date where the item has one, since when a rule starts binding
  # is what a reader is placing against the trend, not when it was signed.
  pl_row <- function(it) {
    when <- if (!is.null(it$effective_date) && nzchar(it$effective_date))
      it$effective_date else it$date
    d <- as.Date(when)
    paste0(
      '<div class="pl-row">',
      '<div class="pl-when">',
      '<span class="pl-date">', format(d, "%b"), " ", as.integer(format(d, "%d")),
      ", ", format(d, "%Y"), '</span>',
      # Washington's ledger carries only instruments in force, so the date rail
      # alone was unambiguous there. These files also hold bills that failed
      # and rulings since vacated, and a bare date beside those reads as though
      # they took effect. The status rides on the instrument line rather than
      # depending on whoever worded the title to have mentioned it.
      # The instrument line links its enacting document, so the citation sits
      # where the reader is already looking rather than only in the sources
      # line beneath. No class: the sheet styles .pl-inst a to be quiet in the
      # rail and wake to the text red on hover.
      if (!is.null(it$instrument) && nzchar(it$instrument))
        paste0('<span class="pl-inst"><a href="', it$source_url,
               '" target="_blank" rel="noopener noreferrer">', esc(it$instrument), '</a>',
               if (!it$status %in% c("enacted", "effective"))
                 paste0(' &middot; ', esc(it$status)) else '',
               '</span>') else '',
      if (!is.null(it$scope) && nzchar(it$scope))
        paste0('<span class="pl-scope">', esc(it$scope), '</span>') else '',
      '</div>',
      '<div class="pl-what">',
      '<h4>', esc(it$title), '</h4>',
      '<p>', esc(it$summary), '</p>',
      if (!is.null(it$effect) && nzchar(it$effect))
        paste0('<p class="pl-effect"><strong>On the counts:</strong> ',
               esc(it$effect), '</p>') else '',
      '</div></div>')
  }

  # The .credit-org line names the enacting document, which is the first of the
  # section's three rules. Scope and instrument are optional, so a file written
  # before they existed still renders.
  org_line <- function(it) {
    parts <- c(if (!is.null(it$scope) && nzchar(it$scope)) esc(it$scope),
               if (!is.null(it$instrument) && nzchar(it$instrument)) esc(it$instrument),
               paste0(esc(it$status), " ", mon(it$date)))
    paste(parts, collapse = " &middot; ")
  }
  card <- function(it) {
    paste0(
      '<div class="credit-card">',
      '<p class="credit-org">', org_line(it), '</p>',
      '<h4>', esc(it$title), '</h4>',
      '<p>', esc(it$summary), '</p>',
      if (!is.null(it$effect) && nzchar(it$effect))
        paste0('<p><strong>Effect on the counts:</strong> ', esc(it$effect), '</p>') else '',
      '<p class="credit-meta">Source: <a href="', it$source_url,
      '" class="unstyled" target="_blank" rel="noopener noreferrer">',
      esc(it$source_name), '</a>. Verified ', .nt_long_date(it$verified_on), '.</p>',
      '</div>')
  }

  body <- if (n_total == 0) {
    paste0('<p class="news-empty">No change in ', esc(state_name),
           '&rsquo;s eviction law was recorded in the ', doc$window_months,
           ' months to ', mon(doc$reviewed_on),
           '. The review ran and found nothing to report.</p>')
  } else {
    rows <- vapply(shown, function(it) {
      for (f in c("date", "title", "status", "summary", "source_url",
                  "source_name", "verification", "verified_on")) {
        need(!is.null(it[[f]]) && nzchar(it[[f]]),
             sprintf("news item is missing '%s'", f))
      }
      need(it$verification %in% c("fetched full text", "fetched abstract/landing page"),
           paste0("verification must be a known tag, not ", sQuote(it$verification),
                  ". An unfetched lead belongs in quarantine; free-text detail ",
                  "belongs in verification_note."))
      paste0(
        '<li class="news-item">',
        '<div class="news-meta">',
        '<span class="news-date">', mon(it$date), '</span>',
        '<span class="news-chip news-chip-', chip_class(it$status), '">',
        esc(it$status), '</span>',
        '</div>',
        '<div class="news-body">',
        '<h4 class="news-title">', esc(it$title), '</h4>',
        '<p class="news-summary">', esc(it$summary), '</p>',
        if (!is.null(it$effect) && nzchar(it$effect))
          paste0('<p class="news-effect"><strong>What it does to the count.</strong> ',
                 esc(it$effect), '</p>') else '',
        '<p class="news-src"><a href="', it$source_url,
        '" class="unstyled" target="_blank" rel="noopener noreferrer">',
        esc(it$source_name), '</a></p>',
        '</div></li>')
    }, character(1))
    if (identical(form, "ledger")) {
      # Latest first, Tim's order (2026-08-20): the rail reads as news, and the
      # most recent movement is the one a reader came for.
      #
      # Sorted on the rail's OWN date rather than taken in storage order. The
      # file is ordered by `date`, when the thing happened, while the rail
      # shows `effective_date` where there is one. Those diverge: Texas SB 38
      # was signed in June 2025 and binds in January 2026. Storage order
      # happens to agree today, but an item signed before another and binding
      # after it would print the rail out of sequence, which is the kind of
      # fault nobody notices until the year a state has two of them.
      rail_on <- function(it) as.Date(
        if (!is.null(it$effective_date) && nzchar(it$effective_date))
          it$effective_date else it$date)
      asc <- shown[order(vapply(shown, function(it) as.numeric(rail_on(it)), numeric(1)),
                        decreasing = TRUE)]
      srcs <- paste(vapply(asc, function(it)
        paste0('<a href="', it$source_url, '" class="unstyled" target="_blank" ',
               'rel="noopener noreferrer">', esc(it$source_name), '</a>'),
        character(1)), collapse = " &middot; ")
      return_ledger <- paste0(
        '<div class="policy-ledger">',
        paste(vapply(asc, pl_row, character(1)), collapse = ""),
        '<p class="pl-sources">Sources, each opened in the review: ', srcs, '.</p>',
        '</div>')
    }
    if (identical(form, "cards")) {
      return_cards <- paste0('<div class="credit-grid">',
                             paste(vapply(shown, card, character(1)), collapse = ""),
                             '</div>')
    }
    more <- if (n_total > length(shown))
      paste0('<p class="news-more">', n_total - length(shown),
             ' earlier item', if (n_total - length(shown) > 1) 's' else '',
             ' in the review window are in the page&rsquo;s data file.</p>') else ""
    if (identical(form, "ledger")) paste0(return_ledger, more)
    else if (identical(form, "cards")) paste0(return_cards, more)
    else paste0('<ul class="news-list">', paste(rows, collapse = ""), '</ul>', more)
  }

  paste0(
    '    <div class="figure" style="margin-top: 32px;">\n',
    '      <span class="plate-num">', label, '</span>\n',
    body, '\n',
    '      <div class="caption">',
    '<span class="num">', caption_num, '</span>',
    '<span class="desc">', caption_desc,
    # %-d is glibc-only and prints literally on macOS, so the day is built
    # from an integer instead.
    ' Policy review current to ', .nt_long_date(doc$reviewed_on), '.</span>',
    '<span class="src">', caption_src, '</span>',
    '</div>\n    </div>')
}
