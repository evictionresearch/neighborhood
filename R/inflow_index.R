# =============================================================================
# Revealed-destination layer: where low-income households are actually MOVING TO.
#
# The mirror of afford_index(). afford_index() asks where a group *could* live
# (price + availability -- the POTENTIAL set). inflow_index() asks where it
# *does* land, from HPRM's BART net-migration predictions: net in-migration is
# a revealed destination. Codebook: positive net migration = "confident
# in-migration ... low-income growth."
#
# Two bases, by need (per design): the `dis_value` composite for the OVERALL
# low-income picture, the raw per-tier `nmr_pred_*` when asking about a SPECIFIC
# group ("where are VLI households moving?").
#
# Descriptive, not normative. Revealed inflow may reflect households channeled
# into the only tracts they can afford (place stratification; Krysan & Crowder's
# cycles of segregation), not free choice. The signal is the CONTRAST with
# afford_index() (potential) and afford_stability() (holdable): the gap between
# where a group can afford to go and where it actually goes is the
# economic-segregation signal. See dev/good-place-design.md.
# =============================================================================

# ---- internal: path/data.frame -> data.frame (shared parquet/rds reader)
.afi_read_tabular <- function(hprm) {
  if (is.data.frame(hprm)) return(hprm)
  if (is.character(hprm) && length(hprm) == 1L) {
    if (grepl("\\.parquet$", hprm, ignore.case = TRUE)) {
      if (!requireNamespace("arrow", quietly = TRUE))
        stop("Reading a .parquet path needs the 'arrow' package ",
             "(install.packages('arrow')) -- or pass a data.frame.", call. = FALSE)
      return(as.data.frame(arrow::read_parquet(hprm)))
    }
    if (grepl("\\.rds$", hprm, ignore.case = TRUE)) return(as.data.frame(readRDS(hprm)))
    stop("`hprm` path must be a .parquet or .rds file.", call. = FALSE)
  }
  stop("`hprm` must be a data.frame or a path to a .parquet/.rds file.", call. = FALSE)
}

# ---- internal: pull GEOID + net-migration fields (missing fields -> NA columns)
.afi_hprm_inflow <- function(hprm) {
  d <- .afi_read_tabular(hprm)
  nm <- names(d)
  gcol <- if ("GEOID" %in% nm) "GEOID" else if ("geoid" %in% nm) "geoid" else
    stop("`hprm` needs a GEOID/geoid column.", call. = FALSE)
  get <- function(f) if (f %in% nm) as.numeric(d[[f]]) else rep(NA_real_, nrow(d))
  data.frame(GEOID = as.character(d[[gcol]]),
             dis_value = get("dis_value"), nmr_pred_el = get("nmr_pred_el"),
             nmr_pred_vl = get("nmr_pred_vl"), nmr_pred_l = get("nmr_pred_l"),
             stringsAsFactors = FALSE)
}

#' @title Revealed-destination layer: where low-income households are moving to
#' @description
#' The mirror of [afford_index()]. Where a group *could* live is affordability;
#' where it *does* land is revealed by HPRM's BART net-migration predictions.
#' `inflow_index()` reads net migration of low-income households and flags
#' **growth** tracts -- confident net in-migration, i.e. revealed destinations.
#' Compare it against [afford_index()] (potential) and [afford_stability()]
#' (holdable): the gap between where a group can afford to go and where it
#' actually goes is the economic-segregation signal.
#' @details
#' `tier` selects the basis (per design):
#' \itemize{
#'   \item `"overall"` uses **`dis_value`** -- the low-income composite (positive
#'     tail = "low-income growth", per the HPRM codebook). Best for the overall
#'     picture across ELI/VLI/LI together.
#'   \item `"ELI"`/`"VLI"`/`"LI"` use the **raw `nmr_pred_el`/`nmr_pred_vl`/
#'     `nmr_pred_l`** -- tier-specific net migration ("Negative = net outflow"),
#'     matched to the [afford_index()] tier. Best when asking about one group.
#' }
#' A tract is `growth = inflow > growth_cut` (default `+50`, HPRM's model-noise
#' buffer -- below it, net migration can't be told from zero). `inflow_cat` bins
#' this into `growth` / `break-even` (`0` to `growth_cut`) / `outflow` (`< 0`).
#'
#' **Descriptive, not normative**: high inflow can mean a group is *channeled*
#' into the only tracts it can afford, not freely choosing -- read it against
#' affordability and stability, not alone.
#'
#' **These are predictions, not observed moves.** `dis_value`/`nmr_pred_*` are
#' BART predictions (Data Axle panel, INLA-smoothed) -- a *modeled expectation*
#' of net migration, and the model's validated target is *displacement* (net
#' outflow), so the positive/growth tail is comparatively less validated and is
#' shrunk toward regional means. Treat inflow as a modeled signal to
#' **triangulate** against observed migration (ACS geographic-mobility-by-income
#' B07010; IRS SOI county flows by AGI), not as ground truth. To use an observed
#' source instead, pass a data.frame with `GEOID` and a net-migration column of
#' the expected name (`dis_value` or `nmr_pred_*`).
#'
#' **Known face-validity failure (positive tail) -- experimental; not a destination
#' signal.** Mapped for the Bay Area, positive `dis_value` reads exclusionary and
#' receiving suburbs as major low-income destinations (Marin median +338, Sonoma
#' +448, Contra Costa +400) while the expensive urban cores go negative -- because
#' EDR is a *displacement* model and its positive tail is unvalidated extrapolation
#' dominated by spatial smoothing where the low-income base is tiny. **Do not use
#' positive `dis_value` as a standalone growth/destination signal.** For
#' destinations use observed migration (ACS B07010 / IRS SOI) gated by affordable
#' capacity; positive `dis_value`'s only defensible use is *absence of displacement*
#' feeding [afford_stability()] (which clamps the magnitude away). A purpose-built
#' growth model is specified in `hprm/HPRM_3.0_growth_model.md`. This function is
#' retained as experimental scaffolding and for the displacement (negative) read.
#' @param hprm HPRM data: a data.frame with `GEOID`/`geoid` and the relevant
#'   net-migration fields (`dis_value` for `"overall"`; `nmr_pred_el/vl/l` for a
#'   tier), or a path to a `.parquet` (needs `arrow`) or `.rds`.
#' @param tier `"overall"` (default, `dis_value`) or `"ELI"`/`"VLI"`/`"LI"` (the
#'   matched raw `nmr_pred_*`).
#' @param x Optional [afford_index()] result to join the inflow onto (by
#'   `GEOID`) for a potential-vs-revealed comparison. For a tier-specific
#'   comparison, filter `x` to the same `ami_tier` first (inflow is per-tract and
#'   is broadcast across a tract's rows).
#' @param growth_cut Net-migration threshold above which a tract is a confident
#'   in-migration destination (default `+50`).
#' @return If `x` is `NULL`, a data.frame of `GEOID`, `tier`, `inflow` (the
#'   net-migration value), `growth` (logical), and `inflow_cat`. If `x` is
#'   supplied, `x` with those columns joined on.
#' @seealso [afford_index()], [afford_stability()]
#' @examples \dontrun{
#' hprm <- "~/data/.../hprm_v5_full_2022.parquet"
#' inflow_index(hprm)                       # overall low-income destinations (dis_value)
#' inflow_index(hprm, tier = "VLI")         # where VLI households move (nmr_pred_vl)
#' # potential vs revealed for VLI renters in King County:
#' idx <- afford_index("53", "033", 2024, tenure = "rent")
#' cmp <- inflow_index(hprm, tier = "VLI", x = idx[idx$ami_tier == "VLI", ])
#' }
#' @export
inflow_index <- function(hprm, tier = c("overall", "ELI", "VLI", "LI"),
                         x = NULL, growth_cut = 50) {
  tier <- match.arg(tier)
  h <- .afi_hprm_inflow(hprm)
  col <- switch(tier, overall = "dis_value", ELI = "nmr_pred_el",
                VLI = "nmr_pred_vl", LI = "nmr_pred_l")
  if (all(is.na(h[[col]])))
    stop("`hprm` has no usable `", col, "` values for tier = \"", tier, "\"",
         if (tier != "overall") " -- tier-specific inflow needs the raw nmr_pred_* fields."
         else ".", call. = FALSE)

  h$tier <- tier
  h$inflow <- h[[col]]
  h$growth <- ifelse(is.na(h$inflow), NA, h$inflow > growth_cut)
  h$inflow_cat <- factor(
    ifelse(is.na(h$inflow), NA_character_,
    ifelse(h$inflow > growth_cut, "growth",
    ifelse(h$inflow >= 0, "break-even", "outflow"))),
    levels = c("growth", "break-even", "outflow"))
  out <- h[, c("GEOID", "tier", "inflow", "growth", "inflow_cat")]

  if (!is.null(x)) {
    if (!is.data.frame(x) || !"GEOID" %in% names(x))
      stop("`x` must be an afford_index() result (with a GEOID column).", call. = FALSE)
    out <- dplyr::left_join(x, out, by = "GEOID")
  }
  out
}
