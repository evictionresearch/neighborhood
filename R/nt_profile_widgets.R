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
#' @return Character scalar: the `<div class="plate-rising-chart">…</div>`
#'   block.
#' @family state profile
#' @export
nt_profile_county_trend <- function(label,
                                    caption_num,
                                    caption_desc,
                                    caption_src,
                                    id = "plate-counties-rising") {
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
    '          <label class="ct-select-wrap">\n',
    '            <span class="ct-select-label">Highlight:</span>\n',
    '            <select id="ct-county-select" class="ct-select">\n',
    '              <option value="">— pick a county —</option>\n',
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
#' @return Character scalar: a `<script>…</script>` tag.
#' @family state profile
#' @export
nt_profile_county_trend_data <- function(county_agg, record_window = 2,
                                         global = "__waCountyTrendData") {
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

  counties_json <- jsonlite::toJSON(
    lapply(seq_len(nrow(ct_county_list)), function(i) {
      list(code    = jsonlite::unbox(ct_county_list$code[i]),
           name    = jsonlite::unbox(ct_county_list$name[i]),
           rec     = jsonlite::unbox(ct_county_list$rec[i]),
           yearly  = to_yearly_vec(ct_county_list$code[i]),
           monthly = to_monthly_vec(ct_county_list$code[i]))
    }),
    auto_unbox = FALSE
  )

  sprintf('    <script>window.%s = { years: %s, months: %s, counties: %s };</script>',
          global,
          jsonlite::toJSON(ct_years),
          jsonlite::toJSON(ct_months),
          counties_json)
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
#' @return Character scalar: a `<script>…</script>` tag.
#' @family state profile
#' @export
nt_profile_monthly_js <- function(values, year_base, month_base = 0,
                                  y_div, global = "__waMonthly") {
  sprintf('    <script>window.%s = { filings: [%s], yearBase: %d, monthBase: %d, yDiv: %d };</script>',
          global, paste(values, collapse = ", "),
          as.integer(year_base), as.integer(month_base), as.integer(y_div))
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
#' @return Character scalar: `<script>` payload + `<div class="table-wrap">`
#'   shell + caption.
#' @family state profile
#' @export
nt_profile_county_table <- function(counties,
                                    caption_num = "Table 1",
                                    caption_desc,
                                    caption_src,
                                    filings_th = "Filings (12 mo.)",
                                    search_placeholder = "Search county…",
                                    global = "__waCountyTable") {
  stopifnot(all(c("county", "renters", "filings", "rate") %in% names(counties)))
  hi_row <- which.max(counties$rate)
  rows_js <- paste0(
    "[\n",
    paste0(
      "      ",
      sprintf('{ county: %s, renters: %d, filings: %d, rate: %.1f%s }',
              vapply(counties$county,
                     function(s) as.character(jsonlite::toJSON(s, auto_unbox = TRUE)),
                     character(1)),
              as.integer(round(counties$renters)),
              as.integer(counties$filings),
              counties$rate,
              ifelse(seq_len(nrow(counties)) == hi_row, ", hi: true", "")),
      collapse = ",\n"
    ),
    "\n    ]"
  )

  paste0(
    '      <script>window.', global, ' = ', rows_js, ';</script>\n',
    '      <div class="table-wrap">\n',
    '        <div class="table-search">\n',
    '          <input id="county-search" type="search" placeholder="', search_placeholder,
    '" aria-label="Search counties">\n',
    '          <span class="count" id="county-count">— counties</span>\n',
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
    '                <th data-sort="county">County</th>\n',
    '                <th data-sort="renters" class="num-col col-renters">Renter HH</th>\n',
    '                <th data-sort="filings" class="num-col col-filings" id="th-filings">', filings_th, '</th>\n',
    '                <th data-sort="rate" class="num-col sort-desc col-rate">Rate / 1,000</th>\n',
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
                                    caption_src = "source · ocla · acs · ern · 2026",
                                    ec_note_html = paste0(
                                      'ACS 5-year, 2020&ndash;2024. The <span class="ec-driver-tag">&#9650;</span> marks <strong>severe rent burden</strong>, ',
                                      'one of the two factors the statistical model identified as most strongly tied to higher eviction-filing rates. ',
                                      'The other is a county\'s Black population share, a structural signal of where disinvestment and segregation concentrate, ',
                                      'not a statement about individuals. Affordability is shown as rent burden, not AMI.'),
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
    '                 placeholder="Search address or place in ', state_name, '…"\n',
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
    '            <li><strong>Zoom in</strong> on any region for closer reading; the legend keeps its county-rate bins.</li>\n',
    '            <li><strong>Hover</strong> any polygon for its window rate.</li>\n',
    '            <li><strong>Click</strong> any polygon to load its full profile in the card.</li>\n',
    '            <li><strong>Click a legend tier</strong> to isolate; <em>show all</em> resets.</li>\n',
    '            <li><strong>Search box</strong> (top-left): jump to any ', state_abbr, ' address or place.</li>\n',
    '            <li><strong>My-location button</strong> (bottom-right): zoom to your position.</li>\n',
    '            <li><strong>Fullscreen icon</strong>: take over the screen.</li>\n',
    '          </ul>\n',
    '        </div>\n',
    '        <div class="map-onboarding" id="map-onboarding">\n',
    '          Drag the slider below for any time window · click a legend tier to isolate · click a county for its profile.\n',
    '          <span class="close" id="map-onboarding-close" aria-label="Dismiss">×</span>\n',
    '        </div>\n',
    '        <!-- Loading indicator: visible whenever the choropleth fill-opacity\n',
    '             is 0 (initial load, or any time the map is in transition).\n',
    '             JS hides this the moment the first paintMapState lands the\n',
    '             real ckmeans paint. -->\n',
    '        <div class="map-loading" id="map-loading" role="status" aria-live="polite">\n',
    '          <span class="map-loading-spinner" aria-hidden="true"></span>\n',
    '          <span class="map-loading-label">Loading map data…</span>\n',
    '        </div>\n',
    '      </div>\n',
    '      <div class="map-help-inline" id="map-zoom-hint">HOVER OR CLICK ANY COUNTY FOR ITS PROFILE  ·  PINCH OR SCROLL TO ZOOM</div>\n',
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
    '            <span id="explorer-min-date">—</span>\n',
    '            <span id="explorer-max-date">—</span>\n',
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
    '      <!-- Live readout — the LEFT grid column (placed via CSS); the map sits to\n',
    '           its right. Updates on map click. Empty state = ', state_name, ' statewide. -->\n',
    '      <div class="explorer-card" id="explorer-card">\n',
    '        <div class="explorer-card-head">\n',
    '          <div class="explorer-place">\n',
    '            <span class="explorer-kicker" id="explorer-kicker">', state_name, ', statewide</span>\n',
    '            <span class="explorer-sub"    id="explorer-sub">click any county for its profile</span>\n',
    '          </div>\n',
    '          <div class="explorer-window" id="explorer-window">window —</div>\n',
    '        </div>\n',
    '\n',
    '        <div class="explorer-tiles">\n',
    '          <div class="explorer-tile">\n',
    '            <div class="explorer-tile-num"   id="tile-rate">—</div>\n',
    '            <div class="explorer-tile-label">rate per 1,000 renter HH<br><span class="explorer-tile-sublabel">annualized over window</span></div>\n',
    '          </div>\n',
    '          <div class="explorer-tile">\n',
    '            <div class="explorer-tile-num"   id="tile-filings">—</div>\n',
    '            <div class="explorer-tile-label">filings in window</div>\n',
    '          </div>\n',
    '          <div class="explorer-tile">\n',
    '            <div class="explorer-tile-num"   id="tile-rank">—</div>\n',
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
    '            <!-- One-line caption below the chart: "max X · MMM YYYY · N filings".\n',
    '                 Each piece is HTML (no SVG stretching to fight). -->\n',
    '            <div class="spark-caption">\n',
    '              <span class="spark-caption-max" id="spark-axis-max">max —</span>\n',
    '              <span class="spark-caption-sep" id="spark-caption-sep" hidden> · </span>\n',
    '              <span class="spark-caption-tip" id="spark-tip" hidden>\n',
    '                <span class="spark-tip-date">—</span> · <span class="spark-tip-value">—</span>\n',
    '              </span>\n',
    '            </div>\n',
    '          </div>\n',
    '        </div>\n',
    '\n',
    '        <!-- County characteristics (ACS 5-yr, from ', sub("\\?.*$", "", acs_json), ').\n',
    '             Rows carrying data-driver are tagged ▲ when they\'re among the\n',
    '             eviction model\'s most robust correlates (meta.drivers in the JSON;\n',
    '             restricted to NH-Black share + severe rent burden — see d7_county_acs.r). -->\n',
    '        <div class="explorer-chars" id="explorer-chars" hidden>\n',
    '          <div class="ec-group">\n',
    '            <div class="ec-group-title">Housing</div>\n',
    '            <div class="ec-row" data-driver="renter_share"><span class="ec-label">Renter households</span><span class="ec-val" id="ec-renters">—</span></div>\n',
    '            <div class="ec-row" data-driver="med_rent"><span class="ec-label">Median rent</span><span class="ec-val" id="ec-medrent">—</span></div>\n',
    '            <div class="ec-row" data-driver="vacancy_rate"><span class="ec-label">Rental vacancy</span><span class="ec-val" id="ec-vacancy">—</span></div>\n',
    '          </div>\n',
    '          <div class="ec-group">\n',
    '            <div class="ec-group-title">Income &amp; affordability</div>\n',
    '            <div class="ec-row"><span class="ec-label">Median income (all households)</span><span class="ec-val" id="ec-incall">—</span></div>\n',
    '            <div class="ec-row"><span class="ec-label">Median income (renters)</span><span class="ec-val" id="ec-incrent">—</span></div>\n',
    '            <div class="ec-row" data-driver="rentburden_50plus"><span class="ec-label">Severely rent-burdened (&ge;50%)</span><span class="ec-val" id="ec-burden50">—</span></div>\n',
    '            <div class="ec-row"><span class="ec-label">Cost-burdened (&ge;30%)</span><span class="ec-val" id="ec-burden30">—</span></div>\n',
    '            <div class="ec-row"><span class="ec-label">Poverty rate</span><span class="ec-val" id="ec-pov">—</span></div>\n',
    '            <div class="ec-row" data-driver="unemp_rate"><span class="ec-label">Unemployment</span><span class="ec-val" id="ec-unemp">—</span></div>\n',
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
