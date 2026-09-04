# =============================================================================
# SAFMR cross-check -- an independent second opinion on the entry-price signal.
#
# afford_entry()'s premium rests on one source (Zillow ZORI). HUD's Small Area
# Fair Market Rents are an independent, ACS-built benchmark of ZIP rent levels
# (40th percentile gross rent, 2BR, floors/caps applied), so where the two
# disagree hard, the tract's entry pricing deserves a flag rather than trust.
#
# The comparison is deliberately centered: an all-bedroom 40th percentile sits
# structurally BELOW a two-bedroom benchmark (studios and 1BRs pull it down),
# so the regional median ratio lands under 1 by construction. The flag marks
# tracts whose ratio strays from that regional offset, not from 1 -- deviation
# from the offset is what signals a questionable premium or thin listings.
# Folded in from the standalone afford prototype, 2026-09-04.
# =============================================================================

# ---- internal: value at cumulative share p of a bracket distribution ---------
# Uniform within brackets. If the quantile lands in the open-top bracket the
# floor is returned rather than a point inside an invented width -- the same
# never-fabricate rule as .afi_interp_le, understating heavy-top tracts (which
# makes their ratio conservative, and is said in the docs).
.afi_bracket_quantile <- function(lo, hi, n, p) {
  o <- order(lo)
  lo <- lo[o]; hi <- hi[o]; n <- ifelse(is.na(n[o]), 0, n[o])
  tot <- sum(n)
  if (tot <= 0) return(NA_real_)
  target <- p * tot
  run <- 0
  for (i in seq_along(n)) {
    if (run + n[i] >= target) {
      if (!is.finite(hi[i]) || n[i] <= 0) return(lo[i])
      return(lo[i] + (target - run) / n[i] * (hi[i] - lo[i]))
    }
    run <- run + n[i]
  }
  lo[length(lo)]
}

# ---- internal: ZIP-level 2BR SAFMRs via the HUD API --------------------------
# One call per county entity; SAFMR metros return per-ZIP rows in basicdata,
# non-SAFMR areas return only the MSA-level row (skipped -- a metro-wide FMR is
# not a small-area benchmark). NULL when hudr or the key is unavailable.
.afi_safmr_zip <- function(state, counties, year) {
  if (!requireNamespace("hudr", quietly = TRUE) ||
      !nzchar(Sys.getenv("HUD_API_KEY"))) return(NULL)
  rows <- lapply(counties, function(co) {
    r <- tryCatch(hudr::get_hud_fmr_data(paste0(state, co, "99999"),
                                         as.character(year)),
                  error = function(e) NULL)
    if (!is.list(r)) return(NULL)      # hudr returns an error STRING on failure
    bd <- r$basicdata
    if (is.null(bd) || is.null(bd$zip_code) || is.null(bd$two_bedroom))
      return(NULL)
    keep <- bd$zip_code != "MSA level" & grepl("^[0-9]{5}$", bd$zip_code)
    if (!any(keep)) return(NULL)
    data.frame(zip = bd$zip_code[keep],
               safmr2 = suppressWarnings(as.numeric(bd$two_bedroom[keep])),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out) || nrow(out) == 0) return(NULL)
  out[!is.na(out$safmr2), , drop = FALSE]
}

#' @title Cross-check entry pricing against HUD Small Area FMRs
#' @description
#' Flags tracts where the [afford_entry()] premium implies a current asking
#' 40th-percentile rent far out of line with HUD's Small Area Fair Market Rent
#' for the same ZIPs -- an independent benchmark (SAFMRs are ACS-built, ZORI
#' is listings-built), so agreement earns trust and divergence earns a flag.
#' @details
#' Per tract: the standing 40th-percentile gross rent from the `B25063`
#' brackets, times `entry_premium`, gives the implied asking 40th percentile;
#' the housing-unit-weighted mean of the overlapping ZIPs' 2BR SAFMRs gives
#' the benchmark; their ratio is compared to the **regional median ratio**.
#' The median sits below 1 by construction (an all-bedroom percentile against
#' a 2BR benchmark), so the flag marks deviation from that structural offset,
#' not from 1: `safmr_flag` is `TRUE` outside `band` times the median.
#' Tracts whose ZIP coverage is under `cover_min` get `NA` -- a cross-check
#' must not fabricate its own benchmark, so there is no fallback here (unlike
#' the premium's). Quantiles landing in the open-top bracket return its floor,
#' understating heavy-top tracts and making their ratio conservative.
#'
#' Needs the `hudr` package and a `HUD_API_KEY` (same requirements as
#' `ami_source = "hud"`); without them the function messages and returns `x`
#' with `NA` check columns rather than failing a pipeline.
#' @param x An [afford_entry()] result (needs `entry_premium`; rent rows are
#'   checked, other tenures pass through `NA`).
#' @param weights A [nt_zcta_weights()] crosswalk for the same counties;
#'   `NULL` builds it.
#' @param year FMR year; `NULL` (default) uses `x`'s year.
#' @param band Multiplicative band around the regional median ratio outside
#'   which a tract is flagged (default `c(0.75, 1.33)`).
#' @param cover_min Minimum HU-share of a tract in SAFMR-covered ZIPs for the
#'   benchmark to count (default `0.25`).
#' @return `x` plus `safmr_2br` (the tract's HU-weighted benchmark),
#'   `safmr_p40_ratio` (implied asking p40 over benchmark), and `safmr_flag`.
#' @seealso [afford_entry()], [afford_zori()]
#' @examples \dontrun{
#' ent <- afford_entry(afford_index("06", "081", 2024, tenure = "rent"),
#'                     afford_zori())
#' chk <- afford_safmr_check(ent)
#' table(chk$safmr_flag[chk$ami_tier == "ELI"], useNA = "ifany")
#' }
#' @export
afford_safmr_check <- function(x, weights = NULL, year = NULL,
                               band = c(0.75, 1.33), cover_min = 0.25) {
  req <- c("GEOID", "county", "ami_tier", "tenure", "entry_premium")
  if (!is.data.frame(x) || !all(req %in% names(x)))
    stop("`x` must be an afford_entry() result (missing: ",
         paste(setdiff(req, names(x)), collapse = ", "), ").", call. = FALSE)
  stopifnot(is.numeric(band), length(band) == 2, band[1] < band[2],
            band[1] > 0)

  if (is.null(year)) year <- unique(x$year)[1]
  state <- unique(substr(x$county, 1, 2))
  if (length(state) != 1)
    stop("`x` spans multiple states; run afford_safmr_check() per state.",
         call. = FALSE)
  counties <- unique(substr(x$county, 3, 5))

  x$safmr_2br <- NA_real_
  x$safmr_p40_ratio <- NA_real_
  x$safmr_flag <- NA
  sz <- .afi_safmr_zip(state, counties, year)
  if (is.null(sz)) {
    message("afford_safmr_check(): SAFMRs unavailable (needs hudr + ",
            "HUD_API_KEY, and a SAFMR-designated metro) -- check columns ",
            "left NA.")
    return(x)
  }

  if (is.null(weights)) weights <- nt_zcta_weights(state, counties)
  w <- weights[weights$zcta %in% sz$zip, c("zcta", "tract_geoid", "frac_hu")]
  w <- dplyr::left_join(w, sz, by = c("zcta" = "zip"))
  bench <- dplyr::summarize(dplyr::group_by(w, tract_geoid),
    coverage = sum(frac_hu, na.rm = TRUE),
    safmr_2br = stats::weighted.mean(safmr2, frac_hu, na.rm = TRUE),
    .groups = "drop")
  bench$safmr_2br[bench$coverage < cover_min] <- NA_real_

  # standing p40 once per tract (the premium is constant across tiers)
  br <- .afi_get_brackets("tract", "B25063", state, counties, year)
  p40 <- dplyr::summarize(dplyr::group_by(br, GEOID),
    p40_standing = .afi_bracket_quantile(lo, hi, n, 0.4), .groups = "drop")

  key <- unique(x[x$tenure == "rent" & !is.na(x$entry_premium),
                  c("GEOID", "entry_premium")])
  key <- dplyr::left_join(key, p40, by = "GEOID")
  key <- dplyr::left_join(key, bench, by = c("GEOID" = "tract_geoid"))
  key$safmr_p40_ratio <- ifelse(
    !is.na(key$safmr_2br) & key$safmr_2br > 0 & !is.na(key$p40_standing),
    key$p40_standing * key$entry_premium / key$safmr_2br, NA_real_)

  ok <- is.finite(key$safmr_p40_ratio)
  if (sum(ok) < 10) {
    message("afford_safmr_check(): fewer than 10 tracts with a usable ",
            "benchmark -- no regional offset to flag against; flags left NA.")
  } else {
    med <- stats::median(key$safmr_p40_ratio[ok])
    key$safmr_flag <- ifelse(is.na(key$safmr_p40_ratio), NA,
      key$safmr_p40_ratio < band[1] * med | key$safmr_p40_ratio > band[2] * med)
  }

  i <- match(x$GEOID, key$GEOID)
  is_rent <- x$tenure == "rent"
  for (cl in c("safmr_2br", "safmr_p40_ratio", "safmr_flag"))
    x[[cl]] <- ifelse(is_rent, key[[cl]][i], NA)
  x
}
