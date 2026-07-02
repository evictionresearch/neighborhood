# The ERN/CiDR palette swatch sheet: a self-contained HTML page generated from
# the live palette vectors in nt_map_internals.R, so the sheet can never drift
# from what the package actually ships. Every palette is titled, labeled with
# the exact ern_palette() string, and annotated with WCAG contrast receipts
# computed here at build time.
#
# Built with assistance from Claude (Anthropic). Deterministic; no AI to run.

# WCAG 2.1 relative luminance (vectorized over hex colors)
.ern_lum <- function(hex) {
  rgb <- grDevices::col2rgb(hex) / 255
  lin <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  as.numeric(0.2126 * lin[1, ] + 0.7152 * lin[2, ] + 0.0722 * lin[3, ])
}

# WCAG 2.1 contrast ratio between two colors (vectorized over `a`)
.ern_contrast <- function(a, b = "#FFFFFF") {
  la <- .ern_lum(a); lb <- .ern_lum(b)
  (pmax(la, lb) + 0.05) / (pmin(la, lb) + 0.05)
}

# one swatch cell: hex (+ optional name) with a contrast badge vs white
.ern_swatch_cell <- function(hex, label = NULL, badge = TRUE, h = 80) {
  ratio <- .ern_contrast(hex)
  txt <- if (.ern_contrast("#FFFFFF", hex) >= 3) "#ffffff" else "#19222C"
  b <- if (!badge) "" else {
    tag <- if (ratio >= 4.5) "AA text" else if (ratio >= 3) "3:1 fill" else "tint"
    sprintf('<span class="badge">%s &middot; %.1f</span>', tag, ratio)
  }
  nm <- if (is.null(label) || !nzchar(label)) "" else
    sprintf('<span class="name">%s</span>', label)
  sprintf(
    '<div class="sw" style="background:%s;color:%s;height:%dpx">%s%s<span class="hex">%s</span></div>',
    hex, txt, h, b, nm, toupper(hex))
}

# one palette card: kicker + title + ern_palette() chip + usage note + swatches
.ern_swatch_card <- function(id, title, use, extra = "", h = 80, badge = TRUE) {
  cols <- ern_palette(id)
  labels <- if (is.null(names(cols))) rep("", length(cols)) else names(cols)
  cells <- paste(mapply(.ern_swatch_cell, cols, labels,
                        MoreArgs = list(badge = badge, h = h)), collapse = "")
  sprintf(paste0(
    '<div class="card"><div class="cardhead"><h3>%s</h3>',
    '<code class="chip">ern_palette("%s")</code></div>',
    '<p class="note"><strong>Use for:</strong> %s%s</p>',
    '<div class="row">%s</div></div>'),
    title, id, use, extra, cells)
}

#' Open the ERN palette swatch sheet
#'
#' @description
#' Writes (and opens) a self-contained HTML sheet of every palette the package
#' ships — qualitative, sequential, diverging, typology, and the brand tokens —
#' generated from the live palette definitions so it always matches the
#' installed version. Each palette is titled, labeled with the exact string to
#' pass to [ern_palette()] / the `scale_*_ern*()` family, and annotated with a
#' one-line "use for" rule plus WCAG 2.1 contrast receipts per swatch
#' (`AA text` \eqn{\ge} 4.5:1 on white, `3:1 fill` \eqn{\ge} 3:1 — the 1.4.11
#' non-text minimum, `tint` below 3:1 — backgrounds and light ramp ends only).
#'
#' Use it to choose a scheme by name: find the card that matches the data's
#' meaning (categories / magnitude / two directions), then pass its label to
#' the matching scale.
#'
#' @param path Where to write the HTML. Default `NULL` writes
#'   `ern-palettes.html` to [tempdir()].
#' @param open Logical; open the sheet in the default browser (default: only
#'   in interactive sessions).
#' @return The path to the written file, invisibly.
#' @family ern_charts
#' @seealso [ern_palette()] for the vectors, [scale_color_ern()] and friends
#'   for the ggplot2 scales.
#' @examples
#' \dontrun{
#' ern_swatch()  # open the sheet and pick a palette by name
#' }
#' @export
ern_swatch <- function(path = NULL, open = interactive()) {
  if (is.null(path)) path <- file.path(tempdir(), "ern-palettes.html")
  b <- .ern_brand

  hero <- paste0(
    '<div class="hero">',
    .ern_swatch_cell(b$navy,      "Dark navy", badge = FALSE, h = 110),
    .ern_swatch_cell(b$navy_soft, "Blue navy", badge = FALSE, h = 110),
    .ern_swatch_cell(b$accent,    "Brand red", badge = FALSE, h = 110),
    '</div>',
    '<p class="note">The 2022 brand triad. Charts lead with it: navy carries the ',
    'editorial line, red carries the highlight. The red is WCAG-compliant for its ',
    'chart role &mdash; see the red rule below.</p>')

  red_rule <- paste0(
    '<div class="card"><div class="cardhead"><h3>The red rule</h3>',
    '<code class="chip">ern_palette("brand")</code></div>',
    '<p class="note">Verified against WCAG 2.1: the brand red <strong>passes</strong> ',
    'as a graphic (1.4.11 needs 3:1 &mdash; fills, lines, markers, map washes, large text) ',
    'on white and on the light-blue tint, but <strong>fails</strong> as body text (needs 4.5:1). ',
    'So: <strong>#F9322B for shapes, #CC2118 for words, #B01D16 for hover/emphasis.</strong> ',
    'Never set red text on navy (2.9:1) &mdash; use white or steel there.</p><div class="row">',
    .ern_swatch_cell(b$accent,        "Graphics red &mdash; fills/lines/markers"),
    .ern_swatch_cell(b$accent_deep,   "Text red &mdash; labels/links"),
    .ern_swatch_cell(b$accent_deeper, "Deep red &mdash; hover/active"),
    '</div></div>')

  tokens <- .ern_swatch_card("brand", "Brand tokens",
    paste0("bespoke work needing raw brand values &mdash; titles in <code>navy</code>, ",
           "body in <code>navy_soft</code>, captions in <code>muted</code> (AA-safe), ",
           "<code>steel</code> on dark surfaces only (<code>steel_chart</code> is its ",
           "3:1 chart-fill twin), section washes in <code>tint</code>."), h = 64)

  qual <- paste0(
    '<h2>1 &middot; Qualitative &mdash; mutually exclusive categories</h2>',
    .ern_swatch_card("qualitative", "House default (brand-forward)",
      paste0("most multi-series charts: trend lines, dodged bars, scatter groups. ",
             "Brand triad first; the first four colors are the most colorblind-",
             "separable subset (gold arrives fifth &mdash; it blurs with red under ",
             "deuteranopia, so lean on the house style&rsquo;s direct labels past four ",
             "series). All fills clear 3:1 on white.")),
    .ern_swatch_card("neutral", "Neutral (red-free)",
      paste0("categories where red would moralize: tenure, program type, ",
             "race/ethnicity compositions. Same family, no alarm color.")),
    .ern_swatch_card("legacy", "Legacy (pre-1.1.0)",
      "reproducing figures published before the 1.1.0 palette; not for new work.",
      h = 56, badge = FALSE),
    .ern_swatch_card("typology", "Neighborhood typology (canonical key)",
      paste0("the 19 segregation-typology classes, always by this fixed key ",
             "(<code>scale_fill_ern(\"typology\")</code> or automatic in ",
             "<code>nt_add_choropleth()</code>)."), h = 56, badge = FALSE))

  seqs <- paste0(
    '<h2>2 &middot; Sequential &mdash; ordered magnitude, light &rarr; dark</h2>',
    .ern_swatch_card("blues", "Brand blue (neutral magnitude)",
      paste0("the default &ldquo;more vs. less&rdquo; ramp when neither end is good or ",
             "bad: density, counts, shares. <code>scale_fill_ern_c(\"blues\")</code>."),
      h = 56),
    .ern_swatch_card("reds", "Brand red (red-themed intensity)",
      paste0("quantities the org tells in red: eviction filings, displacement ",
             "counts. Light end is the semantic red tint; passes through brand red ",
             "to the deep text red."), h = 56),
    .ern_swatch_card("greens", "Positive (more = good)",
      "affordability, stability, opportunity &mdash; the high end is desirable.",
      h = 56),
    .ern_swatch_card("ramp", "Risk (HPRM / MN / WA house ramp)",
      paste0("genuinely bad quantities only &mdash; displacement risk, cost burden. ",
             "The published 4-class convention; don&rsquo;t reuse it for neutral ",
             "magnitudes."), h = 56))

  divs <- paste0(
    '<h2>3 &middot; Diverging &mdash; two directions around a neutral center</h2>',
    .ern_swatch_card("div_brand", "Navy &harr; red (valenced)",
      paste0("axes that truly run good &harr; bad &mdash; red sits on the bad pole, ",
             "exactly what brand red is for. ",
             "<code>scale_fill_ern_div(\"brand\", midpoint = 0)</code>."), h = 56),
    .ern_swatch_card("div_gold", "Blue &harr; gold (neutral)",
      paste0("two-direction axes where neither side is a warning: gain vs. loss, ",
             "above vs. below average. The most colorblind-safe diverging pair. ",
             "<code>scale_fill_ern_div(\"gold\", midpoint = 0)</code>."), h = 56))

  css <- paste0(
    ':root{--navy:#19222C;--bluenavy:#223754;--muted:#586573;--tint:#e8eef4}',
    '*{box-sizing:border-box}',
    'body{font-family:"Inter",system-ui,sans-serif;font-feature-settings:"ss01","cv11";',
    'color:var(--bluenavy);margin:0;padding:48px 40px 80px;max-width:1080px;background:#fff}',
    'h1{font-weight:200;font-size:40px;color:var(--navy);margin:0 0 4px}',
    '.sub{color:var(--muted);font-size:15px;margin:0 0 36px}',
    'h2{font-weight:600;font-size:13px;letter-spacing:.08em;text-transform:uppercase;',
    'color:var(--muted);margin:44px 0 10px;border-top:1px solid #eef1f5;padding-top:28px}',
    'h3{font-weight:600;font-size:19px;color:var(--navy);margin:0}',
    '.cardhead{display:flex;align-items:baseline;gap:12px;flex-wrap:wrap;margin:18px 0 2px}',
    '.chip{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:12px;',
    'background:var(--tint);color:var(--bluenavy);padding:2px 8px;border-radius:6px}',
    '.note{color:var(--muted);font-size:13.5px;margin:2px 0 10px;max-width:820px;line-height:1.55}',
    '.note code{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:12px}',
    '.row,.hero{display:flex;flex-wrap:wrap;border-radius:10px;overflow:hidden;',
    'box-shadow:0 1px 0 rgba(0,0,0,.06);margin-bottom:6px}',
    '.hero{margin-top:8px}',
    '.sw{flex:1 1 0;min-width:74px;display:flex;flex-direction:column;',
    'justify-content:flex-end;padding:8px 9px;gap:1px}',
    '.sw .name{font-size:11px;font-weight:600;line-height:1.2}',
    '.sw .hex{font-size:10.5px;font-family:ui-monospace,SFMono-Regular,Menlo,monospace;opacity:.85}',
    '.sw .badge{font-size:9.5px;font-weight:600;letter-spacing:.02em;opacity:.75;',
    'text-transform:uppercase}',
    '.foot{color:var(--muted);font-size:13.5px;line-height:1.6;margin-top:36px;',
    'border-top:1px solid #eef1f5;padding-top:24px;max-width:820px}')

  html <- paste0(
    '<!doctype html><html lang="en"><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    '<title>ERN / CiDR chart palettes</title>',
    '<link href="https://fonts.googleapis.com/css2?family=Inter:wght@200;400;500;600;700&display=swap" rel="stylesheet">',
    '<style>', css, '</style></head><body>',
    '<h1>ERN / CiDR chart palettes</h1>',
    '<p class="sub">Generated from the <code>neighborhood</code> package &mdash; ',
    'every card is labeled with the exact <code>ern_palette()</code> name. ',
    'Badges are WCAG 2.1 contrast on white: <strong>AA text</strong> &ge; 4.5:1, ',
    '<strong>3:1 fill</strong> &ge; 3:1 (the non-text minimum), ',
    '<strong>tint</strong> below 3:1 (backgrounds and light ramp ends only).</p>',
    hero, red_rule, qual, seqs, divs,
    '<h2>4 &middot; Brand tokens</h2>', tokens,
    '<p class="foot"><strong>Choosing:</strong> categories &rarr; qualitative ',
    '(red-free <em>neutral</em> if red would read as bad); one magnitude &rarr; ',
    'sequential (<em>blues</em> neutral, <em>reds</em> red-themed, <em>greens</em> ',
    'positive, <em>ramp</em> risk); two directions &rarr; diverging ',
    '(<em>div_brand</em> when red should mark the bad pole, <em>div_gold</em> when ',
    'neither pole is bad). ggplot2: <code>scale_color_ern()</code> / ',
    '<code>scale_fill_ern_c(palette=)</code> / <code>scale_fill_ern_div(palette=, ',
    'midpoint=)</code>; interactive charts take the same vectors via ',
    '<code>palette = ern_palette("...")</code>.</p>',
    '</body></html>')

  writeLines(html, path)
  if (isTRUE(open)) utils::browseURL(path)
  invisible(path)
}
