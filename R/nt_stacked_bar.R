# Composition / 100%-stacked bar chart in the ERN house style. The workhorse for
# "share of X by group": tenure by race, rent burden by AMI tier, racial
# composition. Built on echarts4r; assistance from Claude Opus 4.8 (Anthropic).

#' Composition (stacked / 100%) bar chart (ERN house style)
#'
#' @description
#' A stacked bar chart for showing *composition* — what share each `group` makes
#' up within each category `x`. By default it normalizes each bar to 100%
#' (`proportional = TRUE`), the form used across the CiDR Lab jurisdiction
#' reports for tenure-by-race, rent-burden-by-AMI, and racial composition. Set
#' `proportional = FALSE` for raw stacked counts. Segment shares are labelled
#' inside the bars, and the chart is horizontal by default.
#'
#' @param data A data frame in long form: one row per `x` × `group`.
#' @param x Name of the category column (one stacked bar per value).
#' @param y Name of the numeric value column (counts or amounts).
#' @param group Name of the column whose levels stack within each bar.
#' @param proportional Logical; normalize each bar to 100% (default `TRUE`).
#' @param orientation `"horizontal"` (default) or `"vertical"`.
#' @param palette Segment colors, one per `group` level. Defaults to a neutral
#'   accent-free ramp (interpolated to the number of groups; the brand accent is
#'   reserved for a single highlight, not a whole stacked band). A named vector
#'   is matched to group names — recommended for race compositions (e.g.
#'   `c(Black = "#F9322B", White = "#aab4be", ...)`).
#' @param value_labels Logical; print each segment's share/value inside it
#'   (default `TRUE`).
#' @param label_min Minimum segment share (of its bar) to label, so slivers stay
#'   uncluttered (default `0.05`). Applies when `proportional = TRUE`.
#' @param value_fmt One of `"comma"` (default), `"percent"`, `"currency"`,
#'   `"multiple"`, or `"none"`; applies when `proportional = FALSE` (ignored when
#'   proportional — segments are always shown as percentages).
#' @param yaxis Value-axis scale visibility (see [nt_bar()]).
#' @param legend Logical; show the group legend (default `TRUE`).
#' @param title,subtext,source,height Standard editorial extras (see [nt_bar()]).
#' @param ... Passed to [echarts4r::e_bar()].
#' @return An `echarts4r`/`htmlwidget` object.
#' @family ern_charts
#' @seealso [nt_bar()] for single-value bars.
#' @examplesIf interactive() && requireNamespace("echarts4r", quietly = TRUE)
#' tenure <- data.frame(
#'   race   = rep(c("White", "Black", "Latine", "Asian"), each = 2),
#'   tenure = rep(c("Own", "Rent"), 4),
#'   hh     = c(620, 380, 240, 760, 300, 700, 540, 460)
#' )
#' nt_stacked_bar(tenure, "race", "hh", group = "tenure")
#' @export
nt_stacked_bar <- function(data, x, y, group,
                           proportional = TRUE,
                           orientation = c("horizontal", "vertical"),
                           palette = NULL,
                           value_labels = TRUE, label_min = 0.05,
                           value_fmt = c("comma", "percent", "currency", "multiple", "none"),
                           yaxis = c("always", "hover", "none"),
                           legend = TRUE,
                           title = NULL, subtext = NULL, source = NULL,
                           height = NULL, ...) {
  orientation <- match.arg(orientation)
  value_fmt   <- match.arg(value_fmt)
  yaxis       <- match.arg(yaxis)
  .nt_check_cols(data, c(x, y, group))
  n_groups <- length(unique(as.character(data[[group]])))
  pal <- palette %nt||% .nt_seq_ramp(n_groups)

  d <- as.data.frame(data, stringsAsFactors = FALSE)
  if (isTRUE(proportional)) {
    tot <- tapply(d[[y]], as.character(d[[x]]), sum, na.rm = TRUE)
    d$.value <- d[[y]] / tot[as.character(d[[x]])]
    # a category whose values sum to 0 gives 0/0 = NaN -> a silently blank bar;
    # surface it and show an (intentional) empty segment instead.
    bad <- !is.finite(d$.value)
    if (any(bad)) {
      warning("nt_stacked_bar(): category(ies) whose values sum to 0 have no ",
              "composition; their segments are shown as 0.", call. = FALSE)
      d$.value[bad] <- 0
    }
    valcol <- ".value"
    vfmt   <- "percent"
  } else {
    d$.value <- d[[y]]
    valcol <- ".value"
    vfmt   <- value_fmt
  }

  cats <- unique(as.character(d[[x]]))
  d[[x]] <- factor(as.character(d[[x]]),
                   levels = if (orientation == "horizontal") rev(cats) else cats)
  # stable group order for consistent stacking + colors
  if (!is.null(names(pal)) && any(nzchar(names(pal)))) {
    glev <- intersect(names(pal), unique(as.character(d[[group]])))
    if (length(glev)) d[[group]] <- factor(as.character(d[[group]]), levels = glev)
  }

  d <- dplyr::group_by(d, .data[[group]])
  e <- echarts4r::e_charts_(d, x, height = height)
  e <- echarts4r::e_bar_(e, valcol, stack = "stk", barWidth = "62%", ...)
  if (orientation == "horizontal") e <- echarts4r::e_flip_coords(e)

  if (isTRUE(value_labels)) {
    min_show <- if (isTRUE(proportional)) label_min else 0
    idx <- .nt_value_index(orientation)   # value is [cat,val] vertical, [val,cat] flipped
    lab_fmt <- htmlwidgets::JS(sprintf(paste0(
      "function(p){ var raw=p.value; var v=Array.isArray(raw)?raw[%d]:raw;",
      " if(v===null||v===undefined||isNaN(+v)||(+v)<%s) return ''; return (%s); }"),
      idx, format(min_show, scientific = FALSE),
      .nt_js_number_expr(vfmt, "v")))
    for (i in seq_along(e$x$opts$series)) {
      e$x$opts$series[[i]]$label <- list(
        show = TRUE, position = "inside", formatter = lab_fmt,
        color = "#ffffff", fontFamily = .ern_font, fontSize = 10, fontWeight = 600)
    }
  }

  e <- .nt_cat_chart_style(e, orientation, vfmt, yaxis, pal,
                           tooltip = TRUE, legend = legend, source = source,
                           zero_base = TRUE)
  # 100% stacks read as true part-to-whole pinned to [0, 1].
  if (isTRUE(proportional)) {
    ax <- if (orientation == "horizontal") "xAxis" else "yAxis"
    e$x$opts[[ax]][[1]]$min <- 0
    e$x$opts[[ax]][[1]]$max <- 1
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
