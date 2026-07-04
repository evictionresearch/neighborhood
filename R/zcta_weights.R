# ZCTA x tract housing-unit weights on the block-exact surface -- the ZIP->tract
# crosswalk under the availability model's entry-price layer (Zillow ZORI is
# ZIP-level; tracts are the analysis unit). 2020 census blocks nest within both
# 2020 tracts and 2020 ZCTAs, so the fractions are exact, not areal guesses.
# Reuses the block machinery from nt_areal_weight() (R/areal_weight.R).

#' Housing-unit weights from ZCTAs to tracts (block-exact)
#'
#' @description
#' For each (ZCTA, tract) overlap in the study counties, the share of the
#' tract's housing units (population, land area) that falls inside the ZCTA —
#' computed on 2020 census blocks, which nest within both 2020 tracts and 2020
#' ZCTAs, so the fractions are exact rather than areal approximations. This is
#' the crosswalk [afford_entry()] uses to bring ZIP-level asking rents (Zillow
#' ZORI) onto tracts.
#' @details
#' The first call downloads the **national** 2020 ZCTA boundary file through
#' `tigris` (it is not published by state) and filters it to the blocks'
#' bounding box; with `options(tigris_use_cache = TRUE)` that download happens
#' once. By default the full-resolution boundaries are used so block nesting
#' is exact; `cb = TRUE` substitutes the lighter generalized file (small
#' edge-block misassignment possible).
#' @param state State (FIPS or name), single.
#' @param counties County or counties (FIPS or names).
#' @param year Decennial vintage for blocks and ZCTAs (default `2020`).
#' @param cb `FALSE` (default) for full-resolution ZCTAs (exact nesting);
#'   `TRUE` for the generalized cartographic file (lighter, approximate at
#'   block edges).
#' @param crs Projected CRS for the block-in-ZCTA point assignment (default
#'   `3857`). Because blocks nest in ZCTAs, any sane projected CRS gives the
#'   same assignment.
#' @param ... Passed to [tigris::blocks()].
#' @return A tibble, one row per (ZCTA, tract) overlap: `zcta`, `tract_geoid`,
#'   in-ZCTA block totals (`hu_in`, `pop_in`, `area_in`), whole-tract totals
#'   (`hu_t`, `pop_t`, `area_t`), and fractions (`frac_hu`, `frac_pop`,
#'   `frac_area`). `frac_hu` sums to 1 over each tract (up to blocks outside
#'   any ZCTA, which are rare).
#' @seealso [afford_entry()], [nt_areal_weight()]
#' @examples \dontrun{
#' w <- nt_zcta_weights("53", "033")     # King County
#' # tract-level asking-rent premium = HU-weighted mean of its ZIPs' premiums
#' }
#' @export
nt_zcta_weights <- function(state, counties, year = 2020, cb = FALSE,
                            crs = 3857, ...) {
  stopifnot("`state` must be a single state" = length(state) == 1)
  state <- .afi_norm_state(state)
  counties <- .afi_norm_counties(state, counties)

  bl_raw <- do.call(rbind, lapply(counties, function(co)
    tigris::blocks(state = state, county = co, year = year,
                   progress_bar = FALSE, ...)))
  suf <- if ("GEOID20" %in% names(bl_raw)) "20"
         else if ("GEOID10" %in% names(bl_raw)) "10"
         else stop("tigris::blocks(year = ", year,
                   ") returned neither GEOID20 nor GEOID10 columns")
  bl <- bl_raw |>
    sf::st_transform(crs) |>
    dplyr::transmute(
      block       = .data[[paste0("GEOID", suf)]],
      tract_geoid = substr(.data[[paste0("GEOID", suf)]], 1, 11),
      pop         = as.numeric(.data[[paste0("POP", suf)]]),
      hu          = as.numeric(.data[[paste0("HOUSING", suf)]]),
      area        = as.numeric(.data[[paste0("ALAND", suf)]])
    )

  zc <- tigris::zctas(year = year, cb = cb, progress_bar = FALSE,
                      filter_by = sf::st_bbox(sf::st_transform(bl_raw, 4326)))
  zcol <- grep("^ZCTA5CE", names(zc), value = TRUE)[1]
  if (is.na(zcol)) stop("ZCTA id column not found in tigris::zctas() result.")
  zc <- sf::st_transform(zc, crs)
  zc <- dplyr::select(zc, place_name = dplyr::all_of(zcol))
  zc$place_geoid <- zc$place_name

  out <- .aw_block_fractions(bl, zc)
  out <- dplyr::rename(out, zcta = place_name)
  out$place_geoid <- NULL
  dplyr::select(out, zcta, tract_geoid, hu_in, pop_in, area_in,
                hu_t, pop_t, area_t, frac_hu, frac_pop, frac_area)
}
