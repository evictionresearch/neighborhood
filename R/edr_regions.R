# EDR region lookup + event-line annotation helper for ERN trend graphics.
# Built with assistance from Claude Fable 5 (Anthropic). Deterministic; no AI
# required to run.

#' HPRM EDR regions, states to regions lookup
#'
#' @description
#' Returns the eight population-balanced U.S. sub-regions used to train the
#' Housing Precarity Risk Model's Estimated Displacement Risk (EDR) component,
#' as a state-level lookup table. The scheme groups whole states into regions
#' of roughly 33-53M residents each, balancing statistical power with
#' socio-political-demographic homogeneity (see the HPRM v5 codebook,
#' `hprm/data/hprm_v5_2022_codebook.md`, and `hprm/README.md`, "State &
#' regional revisions 2", June 27, 2023). Alaska and Hawaii are not assigned
#' (HPRM excludes them from national maps); they return `NA` region.
#'
#' Use it to attach the EDR region to any state-keyed data (sampling frames,
#' facet grids, region fixed effects) outside the HPRM pipeline itself.
#'
#' @param abb Optional character vector of two-letter state abbreviations (plus
#'   `"DC"`). If supplied, returns the matching region for each element (with
#'   `NA` for unassigned); if `NULL` (default), returns the full lookup table.
#' @return If `abb` is `NULL`, a `data.frame` with columns `state_abb` and
#'   `edr_region`; otherwise a character vector of regions aligned to `abb`.
#' @family ern_charts
#' @examples
#' head(edr_regions())
#' edr_regions(c("WA", "GA", "TX", "AK"))
#' @export
edr_regions <- function(abb = NULL) {
  lk <- list(
    midwestgreatlakes   = c("IL", "OH", "MI", "IN", "WI", "MN"),
    northeastnewengland = c("NY", "MA", "CT", "RI", "NH", "ME", "VT"),
    northeastpennwash   = c("PA", "NJ", "MD", "DE", "DC", "VA"),
    southatlantic       = c("FL", "GA", "NC", "SC"),
    southcentral        = c("TX", "OK"),
    southdeep           = c("TN", "AL", "LA", "KY", "MS", "WV", "AR", "MO"),
    westmountainmidwest = c("IA", "KS", "NE", "SD", "ND", "CO", "UT", "ID",
                            "MT", "WY", "AZ", "NV", "NM"),
    westpacific         = c("CA", "OR", "WA")
  )
  tab <- data.frame(
    state_abb  = unlist(lk, use.names = FALSE),
    edr_region = rep(names(lk), lengths(lk)),
    stringsAsFactors = FALSE
  )
  if (is.null(abb)) return(tab[order(tab$edr_region, tab$state_abb), ])
  tab$edr_region[match(toupper(abb), tab$state_abb)]
}

#' Annotate event or artifact years on a trend chart
#'
#' @description
#' Adds vertical reference lines (and optional top-of-panel labels) for dated
#' events to a `ggplot2` time chart, in the muted ERN style: dashed lines that
#' sit behind the data, small angled labels that do not fight the series for
#' attention. Built for trend figures that must distinguish market movement
#' from known shocks, for example HUD FMR methodology changes (the FY2012 ACS
#' rebenchmark, the FY2023 private-index jump), policy dates, or local events
#' (a headquarters move, a disaster).
#'
#' @param years Numeric vector of event years (x positions), or a `data.frame`
#'   with columns `year` and `label`.
#' @param labels Optional character labels, recycled against `years` (ignored
#'   when `years` is a `data.frame`).
#' @param color Line/label color (default the ERN muted gray-blue).
#' @param linetype Line type (default `"dotted"`).
#' @param label_y Where labels sit vertically: `"top"` (default) or a numeric
#'   y value.
#' @param label_size Label text size in mm (default `2.6`).
#' @param angle Label angle (default `90`, reading up the line).
#' @param alpha Line alpha (default `0.8`).
#' @return A list of `ggplot2` layers to add to a plot.
#' @family ern_charts
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' library(ggplot2)
#' df <- data.frame(fy = 2000:2026, y = cumsum(rnorm(27)))
#' ggplot(df, aes(fy, y)) +
#'   geom_line() +
#'   nt_event_lines(c(2012, 2023), c("ACS rebenchmark", "private-index era")) +
#'   theme_ern()
#' @export
nt_event_lines <- function(years, labels = NULL,
                           color = "#586573", linetype = "dotted",
                           label_y = "top", label_size = 2.6,
                           angle = 90, alpha = 0.8) {
  if (is.data.frame(years)) {
    labels <- years$label
    years  <- years$year
  }
  layers <- list(
    ggplot2::geom_vline(xintercept = years, color = color,
                        linetype = linetype, linewidth = 0.35, alpha = alpha)
  )
  if (!is.null(labels)) {
    yy <- if (identical(label_y, "top")) Inf else label_y
    layers <- c(layers, list(
      ggplot2::annotate("text", x = years, y = yy,
                        label = rep_len(labels, length(years)),
                        color = color, size = label_size,
                        angle = angle, hjust = 1.05, vjust = -0.4)
    ))
  }
  layers
}
