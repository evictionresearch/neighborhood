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
#' functions (in bespoke `ggplot2`, base graphics, or tables).
#'
#' @param name Which palette: `"qualitative"` (the multi-series chart ramp),
#'   `"typology"` (the 19 neighborhood-typology colors, named), `"ramp"` (the
#'   4-class sequential yellow→orange→red→purple risk ramp), or `"brand"` (the
#'   named editorial tokens: `navy`, `accent`, `muted`, …).
#' @param n Optional number of colors; for `"qualitative"`/`"ramp"` the palette
#'   is recycled or interpolated to length `n`.
#' @param reverse Logical; reverse the palette.
#' @return A character vector of hex colors (named for `"typology"`/`"brand"`).
#' @family ern_charts
#' @examples
#' ern_palette("brand")
#' ern_palette("qualitative", n = 3)
#' @export
ern_palette <- function(name = c("qualitative", "typology", "ramp", "brand"),
                        n = NULL, reverse = FALSE) {
  name <- match.arg(name)
  cols <- switch(name,
    qualitative = unname(.nt_chart_palette),
    typology    = .nt_colors,
    ramp        = .ern_ramp,
    brand       = unlist(.ern_brand))
  if (!is.null(n) && name %in% c("qualitative", "ramp")) {
    cols <- if (n <= length(cols)) cols[seq_len(n)]
            else grDevices::colorRampPalette(cols)(n)
  }
  if (isTRUE(reverse)) cols <- rev(cols)
  cols
}

# discrete qualitative palette function for ggplot2::discrete_scale
.ern_qual_pal <- function(reverse = FALSE) {
  function(n) {
    cols <- unname(.nt_chart_palette)
    out <- if (n <= length(cols)) cols[seq_len(n)] else grDevices::colorRampPalette(cols)(n)
    if (isTRUE(reverse)) rev(out) else out
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
#' Discrete and continuous `ggplot2` scales in the ERN palette, to match
#' [theme_ern()]. Discrete scales default to the qualitative chart palette; pass
#' `palette = "typology"` to color the 19 neighborhood typologies by their
#' canonical key. Continuous scales use the 4-class sequential risk ramp.
#'
#' @param palette `"qualitative"` (default) or `"typology"` (the named
#'   neighborhood-typology colors).
#' @param reverse Logical; reverse the palette order.
#' @param ... Passed to the underlying `ggplot2` scale.
#' @return A `ggplot2` scale to add to a plot.
#' @name scale_ern
#' @family ern_charts
#' @seealso [theme_ern()], [ern_palette()].
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' library(ggplot2)
#' ggplot(mpg, aes(displ, hwy, color = drv)) +
#'   geom_point() + scale_color_ern() + theme_ern()
NULL

#' @rdname scale_ern
#' @export
scale_color_ern <- function(palette = c("qualitative", "typology"),
                            reverse = FALSE, ...) {
  palette <- match.arg(palette)
  if (palette == "typology") {
    vals <- if (reverse) rev(.nt_colors) else .nt_colors
    ggplot2::scale_color_manual(values = vals, na.value = .nt_na_color, ...)
  } else {
    ggplot2::discrete_scale("colour", palette = .ern_qual_pal(reverse),
                            na.value = .nt_na_color, ...)
  }
}

#' @rdname scale_ern
#' @export
scale_colour_ern <- scale_color_ern

#' @rdname scale_ern
#' @export
scale_fill_ern <- function(palette = c("qualitative", "typology"),
                           reverse = FALSE, ...) {
  palette <- match.arg(palette)
  if (palette == "typology") {
    vals <- if (reverse) rev(.nt_colors) else .nt_colors
    ggplot2::scale_fill_manual(values = vals, na.value = .nt_na_color, ...)
  } else {
    ggplot2::discrete_scale("fill", palette = .ern_qual_pal(reverse),
                            na.value = .nt_na_color, ...)
  }
}

#' @rdname scale_ern
#' @export
scale_color_ern_c <- function(reverse = FALSE, ...) {
  cols <- if (reverse) rev(.ern_ramp) else .ern_ramp
  ggplot2::scale_color_gradientn(colors = cols, na.value = .nt_na_color, ...)
}

#' @rdname scale_ern
#' @export
scale_colour_ern_c <- scale_color_ern_c

#' @rdname scale_ern
#' @export
scale_fill_ern_c <- function(reverse = FALSE, ...) {
  cols <- if (reverse) rev(.ern_ramp) else .ern_ramp
  ggplot2::scale_fill_gradientn(colors = cols, na.value = .nt_na_color, ...)
}
