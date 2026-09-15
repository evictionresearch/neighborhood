# Partial-dependence (PD) plots for the HPRM BART models, in the ERN house
# style: the posterior-mean partial effect with a 95% credible ribbon on top,
# and a frequency histogram of the predictor underneath — so the curve is always
# read against where the data actually are. This is the "PD curve with a
# frequency table below" idiom from the eviction/displacement BART workflow.
#
# Lineage (see the @references on nt_pd_bart()):
#   * bartMachine::pd_plot() — the native base-graphics partial-dependence plot
#     (Kapelner & Bleich 2016), and the original quick-look used in the UDP HPRM
#     phase-2 work by Alex Ramiller & Tim Thomas.
#   * CHEST-Lab's pdPlotGG.R (Scarpone, Brinkmann et al. 2020) reimplemented that
#     computation as a customizable ggplot; Sebastian Brinkmann's geobrinkmann.com
#     BART tutorial is the workflow the HPRM modeling descends from.
#   * nt_pd_plot() adds the frequency panel + ERN theming and decouples the
#     drawing from the (Java/bartMachine) computation, so the plotter is testable
#     and reusable without a fitted model. nt_pd_bart() is the bartMachine bridge.
#
# Built with assistance from Claude Opus 4.8 (Anthropic). nt_pd_plot() is
# deterministic ggplot2 + patchwork and requires no AI — and no Java — to run.

# Validate the tidy PD frame and return it as a plain data.frame.
.nt_pd_check <- function(pd) {
  need <- c("x", "mean", "lower", "upper")
  if (!is.data.frame(pd) || !all(need %in% names(pd))) {
    stop("`pd` must be a data frame with columns: ",
         paste(need, collapse = ", "), call. = FALSE)
  }
  pd <- as.data.frame(pd, stringsAsFactors = FALSE)
  if (nrow(pd) < 2) stop("`pd` needs at least two evaluation points.", call. = FALSE)
  pd[order(pd$x), need]
}

#' Partial-dependence plot with a frequency panel (ERN house style)
#'
#' @description
#' Draws a partial-dependence (PD) curve — the posterior-mean partial effect of
#' one predictor with a credible ribbon — over a histogram of that predictor's
#' observed values. The histogram (the "frequency table underneath") anchors the
#' curve to the data density, so readers discount the wiggly tails where the
#' model has little support. This is the figure form used for the HPRM EDR/EER
#' driver plots.
#'
#' The function is model-agnostic: it takes a tidy table of PD points plus the
#' raw predictor values, so it can plot partial dependence from any model. For
#' the HPRM `bartMachine` models, [nt_pd_bart()] computes that table for you.
#'
#' @param pd A data frame of the PD curve with columns `x` (the predictor value
#'   at each evaluation point), `mean` (posterior-mean partial effect), and
#'   `lower`/`upper` (credible-interval bounds).
#' @param observed Numeric vector of the predictor's observed (training) values,
#'   used to draw the frequency histogram beneath the curve. If `NULL`, only the
#'   PD curve is returned (no frequency panel).
#' @param var_label X-axis label (the human-readable predictor name). Defaults to
#'   `"x"`.
#' @param y_label Y-axis label for the PD curve. Defaults to `"Partial effect"`.
#' @param title,subtitle,caption Optional editorial text.
#' @param bins Number of histogram bins for the frequency panel (default `30`).
#' @param rel_heights Relative heights of the PD panel vs. the frequency panel
#'   (default `c(4, 1)`).
#' @param points Logical; draw a point at each PD evaluation quantile (default
#'   `TRUE`), echoing the CHEST-Lab/Brinkmann style.
#' @param effect Either `"response"` (default y-label "Partial effect") or
#'   `"probit"` (classification models; y-label "Partial effect (probit)"). Only
#'   affects the default y-label.
#' @return A `patchwork` object (PD panel stacked over the frequency panel) when
#'   `observed` is supplied, otherwise a single `ggplot2` object. Either is
#'   printable and `ggplot2::ggsave()`-able.
#' @family ern_charts
#' @seealso [nt_pd_bart()] to compute `pd`/`observed` from a fitted `bartMachine`
#'   model; [theme_ern()] for the shared print theme.
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("patchwork", quietly = TRUE)
#' set.seed(1)
#' x  <- sort(c(rnorm(400, 2, 0.6), rnorm(120, 4.5, 0.4)))     # observed values
#' xg <- quantile(x, c(.05, seq(.1, .9, .1), .95))             # evaluation grid
#' pd <- data.frame(x = xg, mean = 0.3 * xg,
#'                  lower = 0.3 * xg - 0.15, upper = 0.3 * xg + 0.15)
#' nt_pd_plot(pd, observed = x, var_label = "Median gross rent",
#'            title = "Partial dependence: median rent")
#' @export
nt_pd_plot <- function(pd, observed = NULL,
                       var_label = NULL, y_label = NULL,
                       title = NULL, subtitle = NULL, caption = NULL,
                       bins = 30, rel_heights = c(4, 1), points = TRUE,
                       effect = c("response", "probit")) {
  effect    <- match.arg(effect)
  pd        <- .nt_pd_check(pd)
  var_label <- var_label %nt||% "x"
  y_label   <- y_label   %nt||% (if (effect == "probit") "Partial effect (probit)" else "Partial effect")

  navy   <- .ern_brand$navy
  steel  <- .ern_brand$steel
  ribbon <- grDevices::adjustcolor(steel, alpha.f = 0.40)

  # shared x-range so the curve and the histogram line up exactly
  xr <- range(c(pd$x, observed), na.rm = TRUE)

  p_top <- ggplot2::ggplot(pd, ggplot2::aes(x = .data$x)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
                         fill = ribbon) +
    ggplot2::geom_line(ggplot2::aes(y = .data$lower), colour = navy,
                       linetype = "dashed", linewidth = 0.3) +
    ggplot2::geom_line(ggplot2::aes(y = .data$upper), colour = navy,
                       linetype = "dashed", linewidth = 0.3) +
    ggplot2::geom_line(ggplot2::aes(y = .data$mean), colour = navy, linewidth = 0.7) +
    ggplot2::coord_cartesian(xlim = xr) +
    theme_ern(grid = "y", legend = "none") +
    ggplot2::labs(x = NULL, y = y_label, title = title,
                  subtitle = subtitle, caption = caption)
  if (isTRUE(points)) {
    p_top <- p_top +
      ggplot2::geom_point(ggplot2::aes(y = .data$mean), colour = navy,
                          size = 1.6, shape = 21, fill = .ern_brand$paper, stroke = 0.7)
  }

  if (is.null(observed)) {
    return(p_top + ggplot2::labs(x = var_label))
  }

  # frequency panel: the predictor's observed distribution under the curve
  p_hist <- ggplot2::ggplot(data.frame(obs = as.numeric(observed)),
                            ggplot2::aes(x = .data$obs)) +
    ggplot2::geom_histogram(bins = bins, fill = steel, colour = .ern_brand$paper,
                            linewidth = 0.15) +
    ggplot2::coord_cartesian(xlim = xr) +
    theme_ern(grid = "none", legend = "none") +
    ggplot2::labs(x = var_label, y = "Count") +
    ggplot2::theme(plot.title = ggplot2::element_blank())

  # strip the top panel's x axis; the histogram carries the shared x label
  p_top <- p_top + ggplot2::theme(
    axis.text.x  = ggplot2::element_blank(),
    axis.line.x  = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank())

  patchwork::wrap_plots(p_top, p_hist, ncol = 1, heights = rel_heights)
}

#' Partial dependence from a fitted bartMachine model
#'
#' @description
#' Computes the partial dependence of a fitted `bartMachine` model on one feature
#' and (by default) draws it with [nt_pd_plot()]. The computation reproduces
#' bartMachine's `pd_plot()` / CHEST-Lab's `pdPlotGG.R`: for each quantile of the
#' feature `j`, a sample of training rows is fixed at that value, the BART
#' posterior is drawn, predictions are averaged across rows within each Gibbs
#' draw, and the posterior mean and credible interval are taken across draws.
#'
#' This is the HPRM bridge to [nt_pd_plot()]. It requires the **bartMachine**
#' package, a Java runtime (rJava), and the fitted model object — so it runs on
#' the modeling server, not on a laptop without Java. `nt_pd_plot()` itself has
#' none of those requirements.
#'
#' @param bart_machine A fitted `bartMachine` model.
#' @param j Feature to profile: a column name (character) or index (numeric)
#'   among the model's training features.
#' @param levs Quantiles of `j` at which to evaluate the partial effect
#'   (default `c(0.05, seq(0.1, 0.9, 0.1), 0.95)`).
#' @param lower_ci,upper_ci Credible-interval bounds (defaults `0.025` / `0.975`).
#' @param prop_data Fraction of training rows used to marginalize over the other
#'   predictors (`1` = all rows; lower is faster, noisier).
#' @param var_label,title Passed through to [nt_pd_plot()].
#' @param return_data If `TRUE`, return `list(pd, observed)` (the tidy PD table
#'   and the feature's observed values) instead of a plot — useful for combining
#'   panels or saving the underlying numbers. Default `FALSE`.
#' @param ... Further arguments to [nt_pd_plot()] (e.g. `bins`, `rel_heights`).
#' @return A `patchwork`/`ggplot2` plot (default), or a list of `pd` and
#'   `observed` when `return_data = TRUE`.
#' @family ern_charts
#' @seealso [nt_pd_plot()] for the model-agnostic plotter.
#' @references
#' Chipman, H. A., George, E. I., & McCulloch, R. E. (2010). BART: Bayesian
#'   Additive Regression Trees. *Annals of Applied Statistics*, 4(1), 266–298.
#'
#' Kapelner, A., & Bleich, J. (2016). bartMachine: Machine Learning with Bayesian
#'   Additive Regression Trees. *Journal of Statistical Software*, 70(4), 1–40.
#'   \doi{10.18637/jss.v070.i04}
#'
#' Scarpone, C., Brinkmann, S. T., Große, T., Sonnenwald, D., Fuchs, M., &
#'   Walker, B. B. (2020). A multimethod approach for county-scale geospatial
#'   analysis of emerging infectious diseases: a cross-sectional case study of
#'   COVID-19 incidence in Germany. *International Journal of Health Geographics*,
#'   19, 32. \doi{10.1186/s12942-020-00225-1}. Code:
#'   <https://github.com/CHEST-Lab/BART_Covid-19> (the `pdPlotGG.R` ggplot PD plot
#'   this function descends from).
#'
#' Brinkmann, S. T. (2021). BART: A Bayesian Machine Learning Workflow for
#'   Complex Spatial Data.
#'   <https://geobrinkmann.com/post/bart-a-bayesian-machine-learning-workflow-for-complex-spatial-data/>
#'
#' Original UDP/HPRM partial-dependence usage: Alex Ramiller & Tim Thomas.
#' @export
nt_pd_bart <- function(bart_machine, j,
                       levs = c(0.05, seq(from = 0.1, to = 0.9, by = 0.1), 0.95),
                       lower_ci = 0.025, upper_ci = 0.975, prop_data = 1,
                       var_label = NULL, title = NULL, return_data = FALSE, ...) {
  if (!requireNamespace("bartMachine", quietly = TRUE)) {
    stop("nt_pd_bart() needs the 'bartMachine' package (and a Java runtime). ",
         "Run it on the modeling server, or use nt_pd_plot() with a precomputed ",
         "PD table.", call. = FALSE)
  }
  if (!inherits(bart_machine, "bartMachine")) {
    stop("`bart_machine` must be a fitted bartMachine model.", call. = FALSE)
  }

  feats <- bart_machine$training_data_features
  if (is.numeric(j)) {
    if (j < 1 || j > bart_machine$p) stop(sprintf("`j` must be between 1 and p = %d.", bart_machine$p), call. = FALSE)
    j_name <- feats[j]
  } else if (is.character(j)) {
    if (!(j %in% feats)) stop("`j` must be one of the model's training features.", call. = FALSE)
    j_name <- j
  } else {
    stop("`j` must be a column name or index.", call. = FALSE)
  }

  x_j <- bart_machine$model_matrix_training_data[, j]
  if (length(unique(stats::na.omit(x_j))) <= 1)
    stop("Feature has no variation; PD plot not generated.", call. = FALSE)

  x_quants <- unique(stats::quantile(x_j, levs, na.rm = TRUE))
  if (length(x_quants) <= 1)
    stop("The selected quantiles collapse to one value; widen `levs`.", call. = FALSE)

  n_pd   <- round(bart_machine$n * prop_data)
  n_iter <- bart_machine$num_iterations_after_burn_in
  # predictions[q, , ] = posterior draws for n_pd rows with feature j fixed at quantile q
  preds  <- array(NA_real_, c(length(x_quants), n_pd, n_iter))
  for (q in seq_along(x_quants)) {
    idx <- sample(seq_len(bart_machine$n), n_pd)
    test <- bart_machine$X[idx, ]
    test[, j_name] <- x_quants[q]
    preds[q, , ] <- bartMachine::bart_machine_get_posterior(bart_machine, test)$y_hat_posterior_samples
  }
  is_class <- identical(bart_machine$pred_type, "classification")
  if (is_class) preds <- stats::qnorm(preds)

  # average over rows within each Gibbs draw, then summarize across draws
  pd <- data.frame(
    x     = as.numeric(x_quants),
    mean  = apply(preds, 1, function(a) mean(apply(a, 2, mean))),
    lower = apply(preds, 1, function(a) stats::quantile(apply(a, 2, mean), probs = lower_ci)),
    upper = apply(preds, 1, function(a) stats::quantile(apply(a, 2, mean), probs = upper_ci))
  )

  if (isTRUE(return_data)) return(list(pd = pd, observed = as.numeric(x_j)))

  nt_pd_plot(pd, observed = as.numeric(x_j),
             var_label = var_label %nt||% j_name, title = title,
             effect = if (is_class) "probit" else "response", ...)
}

#' Compact partial-dependence sparkline (Atlas cell style)
#'
#' @description
#' Draws a partial-dependence curve in the compact single-panel form used by
#' the HPRM Variable Direction Atlas (2026): posterior-mean line over a
#' credible ribbon, with the predictor's observed frequency distribution drawn
#' as a small bar strip along the bottom of the same panel. Where
#' [nt_pd_plot()] is the full-size, two-panel presentation figure, this is the
#' small-multiple cell: axis-free by default, sized to sit inside a table row
#' or a grid of dozens of predictors, while still showing the two things a PD
#' curve must never be read without, its uncertainty and where the data are.
#'
#' The function is model-agnostic and needs no Java: it takes the same tidy PD
#' table as [nt_pd_plot()], so [nt_pd_bart()] (with `return_data = TRUE`) can
#' feed it from a fitted `bartMachine` model.
#'
#' @param pd A data frame of the PD curve with columns `x`, `mean`, `lower`,
#'   `upper` (as for [nt_pd_plot()]).
#' @param observed Optional numeric vector of the predictor's observed values,
#'   drawn as the frequency strip. If `NULL`, no strip is drawn.
#' @param var_label Label used when `axes = TRUE` (x-axis title). Defaults to
#'   `"x"`.
#' @param axes If `FALSE` (default), a frameless sparkline for small-multiple
#'   use. If `TRUE`, adds axes and labels via [theme_ern()] so the same cell
#'   can stand alone.
#' @param bins Number of bins for the frequency strip (default `20`).
#' @param freq_height Height of the frequency strip as a fraction of the PD
#'   panel's vertical range (default `0.18`).
#' @param line_width Width of the mean line (default `0.7`).
#' @param points Draw a marker at each PD evaluation quantile (default
#'   `FALSE`; the full-size plot defaults to `TRUE`).
#' @return A `ggplot2` object.
#' @family ern_charts
#' @seealso [nt_pd_plot()] for the full-size two-panel figure; [nt_pd_bart()]
#'   to compute the PD table from a fitted model.
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' set.seed(1)
#' x  <- sort(c(rnorm(400, 2, 0.6), rnorm(120, 4.5, 0.4)))
#' xg <- quantile(x, c(.05, seq(.1, .9, .1), .95))
#' pd <- data.frame(x = xg, mean = 0.3 * xg,
#'                  lower = 0.3 * xg - 0.15, upper = 0.3 * xg + 0.15)
#' nt_pd_spark(pd, observed = x)                      # sparkline cell
#' nt_pd_spark(pd, observed = x, axes = TRUE,
#'             var_label = "Median gross rent")       # standalone version
#' @export
nt_pd_spark <- function(pd, observed = NULL, var_label = NULL, axes = FALSE,
                        bins = 20, freq_height = 0.18, line_width = 0.7,
                        points = FALSE) {
  pd <- .nt_pd_check(pd)
  var_label <- var_label %nt||% "x"

  navy   <- .ern_brand$navy
  steel  <- .ern_brand$steel
  ribbon <- grDevices::adjustcolor(steel, alpha.f = 0.40)

  p <- ggplot2::ggplot(pd, ggplot2::aes(x = .data$x)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
                         fill = ribbon) +
    ggplot2::geom_line(ggplot2::aes(y = .data$mean), colour = navy,
                       linewidth = line_width)
  if (isTRUE(points)) {
    p <- p + ggplot2::geom_point(ggplot2::aes(y = .data$mean), colour = navy,
                                 size = 1.1, shape = 21,
                                 fill = .ern_brand$paper, stroke = 0.5)
  }

  # frequency strip: observed distribution as bars along the panel floor,
  # kept below the ribbon so the curve is never overplotted
  if (!is.null(observed)) {
    obs <- as.numeric(stats::na.omit(observed))
    h   <- graphics::hist(obs, breaks = bins, plot = FALSE)
    yr  <- range(c(pd$lower, pd$upper))
    gap <- 0.06 * diff(yr)
    base <- yr[1] - gap
    bars <- data.frame(
      xmin = utils::head(h$breaks, -1), xmax = utils::tail(h$breaks, -1),
      ymin = base - freq_height * diff(yr) * (h$counts / max(h$counts, 1)),
      ymax = base)
    p <- p + ggplot2::geom_rect(
      data = bars, inherit.aes = FALSE,
      ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                   ymin = .data$ymin, ymax = .data$ymax),
      fill = steel, colour = .ern_brand$paper, linewidth = 0.1)
  }

  if (isTRUE(axes)) {
    p + theme_ern(grid = "y", legend = "none") +
      ggplot2::labs(x = var_label, y = "Partial effect")
  } else {
    p + ggplot2::theme_void()
  }
}
