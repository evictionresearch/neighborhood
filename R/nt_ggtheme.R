# A ggplot2 theme + scales in the Eviction Research Network "newspaper graphic"
# look, for reproducing the STATIC artifacts (papers, HPRM reports, print
# figures) in the same design language as the interactive nt_* charts. Spare
# axes, faint horizontal guides only, navy titles, muted captions.
#
# Built with assistance from Claude Opus 4.8 (Anthropic). These are deterministic
# ggplot2 wrappers and require no AI to run.

#' Eviction Research Network color palettes
#'
#' @description
#' Exposes the ERN house colors so you can use them outside the bundled chart
#' functions (in bespoke `ggplot2`, base graphics, or tables). Call
#' [ern_swatch()] to open the titled swatch sheet of every palette below, with
#' WCAG contrast receipts, and choose by name.
#'
#' **Qualitative** — mutually exclusive categories:
#' * `"qualitative"` — the house default, brand triad first: blue navy, brand
#'   red, chart steel, teal, then gold / purple / green / slate. Every fill
#'   clears WCAG 1.4.11 (3:1 on white); the first four are the most
#'   colorblind-separable subset.
#' * `"neutral"` — red-free variant for categories where red would read as
#'   "bad" (tenure, program, demographic compositions).
#' * `"legacy"` — the pre-1.1.0 chart palette, verbatim, for reproducing
#'   already-published figures.
#' * `"typology"` — the 19 neighborhood-typology colors, named.
#'
#' **Sequential** — ordered magnitude, light to dark:
#' * `"blues"` — neutral magnitude, no valence (the default "more vs. less").
#' * `"reds"` — brand-red intensity for red-themed quantities (eviction
#'   filings, displacement counts).
#' * `"greens"` — positive quantities where more = good (affordability,
#'   stability).
#' * `"ramp"` — the 4-class yellow-orange-red-purple **risk** ramp used across
#'   the HPRM / Minnesota / Washington maps; reserve it for genuinely bad
#'   quantities.
#'
#' **Diverging** — two directions around a neutral center:
#' * `"div_brand"` — navy vs. brand red; valenced, red is the *bad* pole.
#' * `"div_gold"` — blue vs. gold; neutral, neither pole a warning (and the
#'   most colorblind-safe diverging pairing).
#'
#' `"brand"` returns the named editorial tokens (`navy`, `accent`,
#' `accent_deep`, `muted`, …). The red rule, WCAG-verified: `accent` `#F9322B`
#' is the *graphics* red (3.8:1 on white — passes 1.4.11 for fills, lines,
#' markers; fails AA body text), `accent_deep` `#CC2118` is the *text* red
#' (5.5:1, AA-safe), `accent_deeper` `#B01D16` is hover / deep emphasis.
#'
#' @param name Which palette (see above).
#' @param n Optional number of colors. Qualitative palettes truncate to `n`
#'   (interpolating only past their length); sequential / diverging ramps
#'   interpolate to exactly `n` (e.g. `n = 5` classes for a binned map).
#' @param reverse Logical; reverse the palette.
#' @return A character vector of hex colors (named for `"typology"`/`"brand"`).
#' @family ern_charts
#' @examples
#' ern_palette("brand")
#' ern_palette("qualitative", n = 3)
#' ern_palette("blues", n = 5)     # 5-class sequential bins
#' ern_palette("div_gold", n = 9)  # 9-class diverging bins
#' @export
ern_palette <- function(name = c("qualitative", "neutral", "legacy", "typology",
                                 "ramp", "blues", "reds", "greens",
                                 "div_brand", "div_gold", "brand"),
                        n = NULL, reverse = FALSE) {
  name <- match.arg(name)
  cols <- switch(name,
    qualitative = unname(.nt_chart_palette),
    neutral     = unname(.ern_qual_neutral),
    legacy      = unname(.nt_chart_palette_legacy),
    typology    = .nt_colors,
    ramp        = .ern_ramp,
    blues       = .ern_seq_blues,
    reds        = .ern_seq_reds,
    greens      = .ern_seq_greens,
    div_brand   = .ern_div_brand,
    div_gold    = .ern_div_gold,
    brand       = unlist(.ern_brand))
  qual <- name %in% c("qualitative", "neutral", "legacy")
  if (!is.null(n) && !name %in% c("typology", "brand")) {
    cols <- if (qual && n <= length(cols)) cols[seq_len(n)]
            else grDevices::colorRampPalette(cols)(n)
  }
  if (isTRUE(reverse)) cols <- rev(cols)
  cols
}

# discrete qualitative palette function for ggplot2::discrete_scale
.ern_qual_pal <- function(reverse = FALSE, palette = "qualitative") {
  function(n) {
    cols <- ern_palette(palette, n = n)
    if (isTRUE(reverse)) rev(cols) else cols
  }
}

#' ggplot2 theme in the ERN "newspaper graphic" style
#'
#' @description
#' A spare `ggplot2` theme matching the look of the interactive ERN charts: no
#' chart junk, faint horizontal guide lines only, a thin x baseline, navy bold
#' titles, and muted captions. Pair it with [scale_color_ern()] /
#' [scale_fill_ern()]. This is how staff reproduce the *static* (print / PDF)
#' versions of the profiles and report figures.
#'
#' @param base_size Base font size in points (default `12`).
#' @param base_family Base font family. Defaults to `""` (the device default) so
#'   it works without installing fonts; set to e.g. `"Inter"` if available.
#' @param grid Which guide lines to keep: `"y"` (default, faint horizontal),
#'   `"x"`, `"both"`, or `"none"`.
#' @param legend Legend placement: `"bottom"` (default), `"right"`, `"top"`,
#'   `"left"`, or `"none"` (the direct-label house style often wants none).
#' @return A `ggplot2` theme object to add to a plot.
#' @family ern_charts
#' @seealso [scale_color_ern()], [scale_fill_ern()], [ern_palette()].
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point(color = ern_palette("brand")["navy"]) +
#'   theme_ern()
#' @export
theme_ern <- function(base_size = 12, base_family = "",
                      grid = c("y", "x", "both", "none"),
                      legend = c("bottom", "right", "top", "left", "none")) {
  grid   <- match.arg(grid)
  legend <- match.arg(legend)
  navy  <- .ern_brand$navy
  muted <- .ern_brand$muted
  gline <- ggplot2::element_line(color = navy, linewidth = 0.25, linetype = "dashed")

  th <- ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(color = navy, face = "bold",
                                                size = base_size * 1.25,
                                                margin = ggplot2::margin(b = 4)),
      plot.subtitle    = ggplot2::element_text(color = muted,
                                                size = base_size * 0.95,
                                                margin = ggplot2::margin(b = 8)),
      plot.caption     = ggplot2::element_text(color = muted, size = base_size * 0.75,
                                               hjust = 1),
      axis.title       = ggplot2::element_text(color = muted, size = base_size * 0.85),
      axis.text        = ggplot2::element_text(color = muted, size = base_size * 0.8),
      axis.line.x      = ggplot2::element_line(color = muted, linewidth = 0.3),
      axis.ticks       = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      legend.title     = ggplot2::element_text(color = muted, size = base_size * 0.85),
      legend.text      = ggplot2::element_text(color = muted, size = base_size * 0.8),
      legend.position  = legend,
      plot.title.position = "plot",
      plot.caption.position = "plot"
    )

  if (grid %in% c("y", "both")) th <- th + ggplot2::theme(panel.grid.major.y = gline)
  if (grid %in% c("x", "both")) th <- th + ggplot2::theme(panel.grid.major.x = gline)
  th
}

#' ERN color and fill scales for ggplot2
#'
#' @description
#' Discrete, continuous, and diverging `ggplot2` scales in the ERN palettes, to
#' match [theme_ern()]. See [ern_palette()] for what each palette name means
#' and [ern_swatch()] for the visual sheet.
#'
#' * **Discrete** (`scale_color_ern()` / `scale_fill_ern()`) default to the
#'   `"qualitative"` chart palette; other choices are `"neutral"` (red-free),
#'   `"legacy"` (pre-1.1.0), and `"typology"` (the 19 neighborhood typologies
#'   by their canonical key).
#' * **Continuous** (`scale_color_ern_c()` / `scale_fill_ern_c()`) default to
#'   the `"ramp"` risk ramp (unchanged from earlier releases); pass
#'   `palette = "blues"`, `"reds"`, or `"greens"` for the sequential ramps.
#' * **Diverging** (`scale_color_ern_div()` / `scale_fill_ern_div()`) use
#'   `"brand"` (navy vs. red, valenced — red is the bad pole) or `"gold"`
#'   (blue vs. gold, neutral). Set `midpoint` to pin the neutral center (e.g.
#'   `midpoint = 0` for change scores) rather than letting it float at the
#'   middle of the data range.
#'
#' @param palette For discrete scales: `"qualitative"` (default), `"neutral"`,
#'   `"legacy"`, or `"typology"`. For continuous scales: `"ramp"` (default),
#'   `"blues"`, `"reds"`, or `"greens"`. For diverging scales: `"brand"`
#'   (default) or `"gold"`.
#' @param reverse Logical; reverse the palette order.
#' @param midpoint Diverging scales only: the data value placed at the neutral
#'   center. Default `NULL` centers on the middle of the data range.
#' @param ... Passed to the underlying `ggplot2` scale.
#' @return A `ggplot2` scale to add to a plot.
#' @name scale_ern
#' @family ern_charts
#' @seealso [theme_ern()], [ern_palette()], [ern_swatch()].
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' library(ggplot2)
#' ggplot(mpg, aes(displ, hwy, color = drv)) +
#'   geom_point() + scale_color_ern() + theme_ern()
#'
#' # diverging fill centered on zero change
#' df <- data.frame(x = LETTERS[1:7], chg = c(-30, -12, -4, 0, 8, 19, 42))
#' ggplot(df, aes(x, chg, fill = chg)) +
#'   geom_col() + scale_fill_ern_div("gold", midpoint = 0) + theme_ern()
NULL

#' @rdname scale_ern
#' @export
scale_color_ern <- function(palette = c("qualitative", "neutral", "legacy",
                                        "typology"),
                            reverse = FALSE, ...) {
  palette <- match.arg(palette)
  if (palette == "typology") {
    vals <- if (reverse) rev(.nt_colors) else .nt_colors
    ggplot2::scale_color_manual(values = vals, na.value = .nt_na_color, ...)
  } else {
    ggplot2::discrete_scale("colour", palette = .ern_qual_pal(reverse, palette),
                            na.value = .nt_na_color, ...)
  }
}

#' @rdname scale_ern
#' @export
scale_colour_ern <- scale_color_ern

#' @rdname scale_ern
#' @export
scale_fill_ern <- function(palette = c("qualitative", "neutral", "legacy",
                                       "typology"),
                           reverse = FALSE, ...) {
  palette <- match.arg(palette)
  if (palette == "typology") {
    vals <- if (reverse) rev(.nt_colors) else .nt_colors
    ggplot2::scale_fill_manual(values = vals, na.value = .nt_na_color, ...)
  } else {
    ggplot2::discrete_scale("fill", palette = .ern_qual_pal(reverse, palette),
                            na.value = .nt_na_color, ...)
  }
}

#' @rdname scale_ern
#' @export
scale_color_ern_c <- function(palette = c("ramp", "blues", "reds", "greens"),
                              reverse = FALSE, ...) {
  cols <- ern_palette(match.arg(palette), reverse = reverse)
  ggplot2::scale_color_gradientn(colors = cols, na.value = .nt_na_color, ...)
}

#' @rdname scale_ern
#' @export
scale_colour_ern_c <- scale_color_ern_c

#' @rdname scale_ern
#' @export
scale_fill_ern_c <- function(palette = c("ramp", "blues", "reds", "greens"),
                             reverse = FALSE, ...) {
  cols <- ern_palette(match.arg(palette), reverse = reverse)
  ggplot2::scale_fill_gradientn(colors = cols, na.value = .nt_na_color, ...)
}

# rescaler pinning a diverging ramp's neutral center to a data value
.ern_mid_rescaler <- function(midpoint) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE))
    scales::rescale_mid(x, to, from, mid = midpoint)
}

#' @rdname scale_ern
#' @export
scale_color_ern_div <- function(palette = c("brand", "gold"), midpoint = NULL,
                                reverse = FALSE, ...) {
  cols <- ern_palette(paste0("div_", match.arg(palette)), reverse = reverse)
  args <- list(colors = cols, na.value = .nt_na_color, ...)
  if (!is.null(midpoint)) args$rescaler <- .ern_mid_rescaler(midpoint)
  do.call(ggplot2::scale_color_gradientn, args)
}

#' @rdname scale_ern
#' @export
scale_colour_ern_div <- scale_color_ern_div

#' @rdname scale_ern
#' @export
scale_fill_ern_div <- function(palette = c("brand", "gold"), midpoint = NULL,
                               reverse = FALSE, ...) {
  cols <- ern_palette(paste0("div_", match.arg(palette)), reverse = reverse)
  args <- list(colors = cols, na.value = .nt_na_color, ...)
  if (!is.null(midpoint)) args$rescaler <- .ern_mid_rescaler(midpoint)
  do.call(ggplot2::scale_fill_gradientn, args)
}
