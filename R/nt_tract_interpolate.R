#' Weight tract-level estimates onto a place boundary
#'
#' @description
#' The apply side of [nt_areal_weight()]: scale tract-level counts down to the
#' share that falls inside a place, so jurisdiction statistics describe only
#' the people (or households) within the boundary. Person counts scale by
#' `frac_pop`, household counts by `frac_hu`, land measures by `frac_area` —
#' match the fraction to the variable's unit.
#'
#' Weighted counts are non-integer by construction (88% of 1,000 people is
#' 880, but 88% of 1,043 is 917.8); compute shares from them directly, and
#' round only for display.
#'
#' @param x A tract-level data frame (or `sf`; geometry is ignored) with an
#'   `id` column of tract GEOIDs and the estimate columns to scale.
#' @param weights A weights table from [nt_areal_weight()] — or any data frame
#'   with `tract_geoid`, `place_name`, and the `frac_*` columns (e.g. the
#'   `hprm_tract_weights.csv` audit trail a pipeline has already verified).
#' @param place Optional place name(s) to keep from `weights` (matched against
#'   `place_name`). `NULL` keeps every place present.
#' @param persons,households,areas Character vectors naming the columns of `x`
#'   to scale by `frac_pop`, `frac_hu`, and `frac_area` respectively. Columns
#'   not named in any of the three pass through unscaled (ids, labels, scores).
#' @param id Name of the tract-id column in `x` (default `"GEOID"`).
#' @param suffix Appended to the scaled columns' names (default `""`, i.e.
#'   scaled in place). Pass e.g. `"_wt"` to keep the raw columns alongside.
#' @return A tibble with one row per (place, tract): the `weights` columns
#'   (`place_name`, `tract_geoid`, `frac_*`, ...) joined to `x`, with the named
#'   columns scaled to their in-place share. Tracts of `x` that touch no kept
#'   place are dropped; tracts in `weights` missing from `x` are kept with `NA`
#'   estimates (so a coverage gap is visible, not silent).
#' @family ern_maps
#' @seealso [nt_areal_weight()] for building the weights.
#' @examples \dontrun{
#' w <- nt_areal_weight("CA", "San Mateo", places = "Redwood City")
#' acs <- tidycensus::get_acs("tract", table = "B05001", state = "CA",
#'                            county = "San Mateo", year = 2022,
#'                            output = "wide")
#' rwc <- nt_tract_interpolate(acs, w, persons = grep("E$", names(acs), value = TRUE))
#' sum(rwc$B05001_001E)   # persons inside the city limit, not whole-tract sums
#' }
#' @export
nt_tract_interpolate <- function(x, weights, place = NULL,
                                 persons = NULL, households = NULL,
                                 areas = NULL, id = "GEOID", suffix = "") {
  if (inherits(x, "sf")) x <- sf::st_drop_geometry(x)
  x <- as.data.frame(x)
  stopifnot(
    "`id` column not found in `x`"                = id %in% names(x),
    "`weights` needs tract_geoid + place_name"    =
      all(c("tract_geoid", "place_name") %in% names(weights))
  )
  w <- as.data.frame(weights)
  if (!is.null(place)) {
    w <- w[w$place_name %in% place, , drop = FALSE]
    if (nrow(w) == 0) stop("no rows in `weights` match place = ",
                           paste(place, collapse = ", "), call. = FALSE)
  }

  spec <- list(persons = persons, households = households, areas = areas)
  frac <- c(persons = "frac_pop", households = "frac_hu", areas = "frac_area")
  used <- unlist(spec, use.names = FALSE)
  missing_cols <- setdiff(used, names(x))
  if (length(missing_cols)) {
    stop("column(s) not in `x`: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
  dup <- used[duplicated(used)]
  if (length(dup)) {
    stop("column(s) assigned to more than one unit: ",
         paste(unique(dup), collapse = ", "), call. = FALSE)
  }
  need_frac <- frac[!vapply(spec, is.null, logical(1))]
  miss_frac <- setdiff(unname(need_frac), names(w))
  if (length(miss_frac)) {
    stop("`weights` is missing: ", paste(miss_frac, collapse = ", "),
         call. = FALSE)
  }

  out <- dplyr::left_join(dplyr::as_tibble(w), dplyr::as_tibble(x),
                          by = stats::setNames(id, "tract_geoid"))
  for (u in names(spec)) {
    for (col in spec[[u]]) {
      out[[paste0(col, suffix)]] <- out[[col]] * out[[frac[[u]]]]
    }
  }
  out
}
