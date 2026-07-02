#' Population-weighted areal crosswalk from census blocks to places
#'
#' @description
#' Build an exact areal/population weight for how each census tract overlaps each
#' incorporated place (or other place type), using decennial census blocks as
#' the weighting surface. Because 2020 blocks nest within both 2020 tracts and
#' 2020 places, the share of a tract's population (housing units, or land area)
#' that falls inside a place is computed *exactly* — there is no
#' areal-interpolation approximation. Multiply any tract-level estimate
#' (HPRM scores, ACS counts, rates) by the returned `frac_*` column to
#' population-weight it onto place boundaries.
#'
#' Tracts straddle municipal limits, so a jurisdiction's profile is not simply
#' "the tracts inside the city." Using the *incorporated* place boundary
#' (`lsad = "25"`) also drops census-designated places (CDPs) and unincorporated
#' pockets for free: their blocks fall outside every incorporated polygon and
#' therefore contribute zero weight.
#'
#' @param state Two-letter state abbreviation or FIPS code. A single state per
#'   call (passed to tigris).
#' @param county County name(s) or FIPS code(s) within `state`. May be a vector;
#'   blocks are pulled for each and combined. **Pass every county a target place
#'   touches** — a place that spans a county not listed here keeps correct
#'   fractions but silently omits its out-of-county tracts.
#' @param places Optional character vector of place `NAME`s to keep (for example
#'   `c("Redwood City", "San Mateo")`). `NULL` (default) keeps every place of the
#'   requested `lsad`. When supplied, each name must resolve to exactly one
#'   polygon at the chosen `lsad`, so that downstream aggregation by `place_name`
#'   is unambiguous; a name that resolves to several polygons is an error.
#' @param year Vintage for both blocks and places. Default `2020`. The block
#'   schema — and therefore the population/housing counts — is detected from the
#'   data: a `year >= 2020` pull carries 2020-decennial blocks (`*20` columns),
#'   `2010` carries 2010-decennial blocks (`*10`). Counts are decennial-census
#'   totals, not ACS estimates.
#' @param lsad Place LSAD code(s) to keep. Default `"25"` (incorporated places),
#'   which excludes CDPs and unincorporated areas. Set `NULL` to keep all place
#'   types — note a single `NAME` can then resolve to several polygons (e.g. a
#'   city and a like-named CDP), which `places=` will reject.
#' @param crs Projected CRS for the block-to-place point-in-polygon join. Default
#'   `3310` (California Albers). The CRS affects only the spatial assignment of
#'   blocks to places; because 2020 blocks nest within 2020 places, an interior
#'   point lands in the same place under any sane projected CRS, so the returned
#'   fractions are CRS-stable in practice. Outside California, pass a locally
#'   appropriate projected (not lon/lat) CRS for a well-conditioned join.
#' @param ... Passed to the underlying [tigris::blocks()] call.
#'
#' @return A tibble with one row per (place, tract) overlap: `place_name`,
#'   `place_geoid`, `tract_geoid`, the in-place block totals (`pop_in`, `hu_in`,
#'   `area_in`), the whole-tract totals (`pop_t`, `hu_t`, `area_t`), and the
#'   within-tract fractions inside the place (`frac_pop`, `frac_hu`,
#'   `frac_area`). Water / zero-population tracts return a fraction of `0` (no
#'   divide-by-zero). Rows are sorted by place, then by descending `frac_pop`,
#'   for readability only — the fractions are independent of row order, so
#'   aggregate rather than index positionally. With `places=`, `place_name` is
#'   unique per polygon; under `lsad = NULL` it may repeat.
#' @seealso [get_co_puma()] for the PUMA-county crosswalk and [ntdf()] for
#'   tract-level neighborhood typologies.
#' @examples \dontrun{
#' # Share of each tract's population / renters inside San Mateo County's cities
#' w <- nt_areal_weight(
#'   "CA", "San Mateo",
#'   places = c("Redwood City", "San Mateo", "Brisbane", "San Bruno")
#' )
#'
#' # Population-weight a tract-level estimate onto each city. `frac_pop` is the
#' # share of the tract's population inside the city, so renters_e * frac_pop is
#' # the tract's in-city renter households.
#' library(dplyr)
#' w %>%
#'   left_join(tract_estimates, by = c("tract_geoid" = "geoid")) %>%
#'   group_by(place_name) %>%
#'   summarise(renters_in_city = sum(renters_e * frac_pop, na.rm = TRUE))
#' }
#' @export
nt_areal_weight <- function(state, county, places = NULL,
                            year = 2020, lsad = "25", crs = 3310, ...) {
  stopifnot("`state` must be a single state" = length(state) == 1)

  # Places (incorporated by default), filtered + guarded.
  pl <- tigris::places(state = state, year = year, progress_bar = FALSE) |>
    sf::st_transform(crs)
  pl <- .aw_resolve_places(pl, places, lsad)

  # Blocks: the weighting surface (one or more counties, one state). rbind (not
  # dplyr::bind_rows) so the sf class + CRS survive a multi-county pull; the
  # column suffix is detected so off-2020 vintages don't fail on a hardcoded 20.
  bl_raw <- do.call(rbind, lapply(county, function(co)
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

  .aw_block_fractions(bl, pl)
}

# Resolve the raw places layer to the kept (place_name, place_geoid) set:
# apply the LSAD filter, the optional name filter, and the uniqueness / empty
# guards. Split out so the guards are unit-testable without a tigris pull.
.aw_resolve_places <- function(pl, places, lsad) {
  if (!is.null(lsad)) pl <- pl[pl$LSAD %in% lsad, , drop = FALSE]
  pl <- dplyr::select(pl, place_name = NAME, place_geoid = GEOID)
  if (!is.null(places)) {
    missing_pl <- setdiff(places, pl$place_name)
    if (length(missing_pl))
      warning("place(s) not found at lsad = ", paste(lsad, collapse = "/"),
              ": ", paste(missing_pl, collapse = ", "))
    pl <- pl[pl$place_name %in% places, , drop = FALSE]
    dup <- unique(pl$place_name[duplicated(pl$place_name)])
    if (length(dup))
      stop("place name(s) resolve to multiple polygons at lsad = ",
           paste(lsad, collapse = "/"),
           " (disambiguate via `lsad`, or aggregate by place_geoid): ",
           paste(dup, collapse = ", "))
  }
  if (nrow(pl) == 0)
    stop("no places matched (lsad = ", paste(lsad, collapse = "/"), ")")
  pl
}

# The weighting math: assign each block to a place (interior point in polygon),
# then express each (place, tract) overlap as a share of the whole tract. `bl`
# is an sf of blocks (block, tract_geoid, pop, hu, area); `pl` an sf of places
# (place_name, place_geoid). tract_tot is built BEFORE dropping unincorporated
# blocks, so the denominator is the whole tract. distinct() is insurance: a
# block belongs to at most one place even if place polygons touch.
.aw_block_fractions <- function(bl, pl) {
  bl_pl <- sf::st_join(suppressWarnings(sf::st_point_on_surface(bl)), pl,
                       join = sf::st_within) |>
    sf::st_drop_geometry() |>
    dplyr::distinct(block, .keep_all = TRUE)

  tract_tot <- bl_pl |>
    dplyr::group_by(tract_geoid) |>
    dplyr::summarise(pop_t = sum(pop), hu_t = sum(hu), area_t = sum(area),
                     .groups = "drop")

  bl_pl |>
    dplyr::filter(!is.na(place_name)) |>
    dplyr::group_by(place_name, place_geoid, tract_geoid) |>
    dplyr::summarise(pop_in = sum(pop), hu_in = sum(hu), area_in = sum(area),
                     .groups = "drop") |>
    dplyr::left_join(tract_tot, by = "tract_geoid") |>
    dplyr::mutate(
      frac_pop  = ifelse(pop_t  > 0, pop_in  / pop_t,  0),
      frac_hu   = ifelse(hu_t   > 0, hu_in   / hu_t,   0),
      frac_area = ifelse(area_t > 0, area_in / area_t, 0)
    ) |>
    dplyr::arrange(place_name, dplyr::desc(frac_pop))
}
