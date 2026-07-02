#' Erase water area from an sf layer (coastline / lake cutout)
#'
#' @description
#' Clip open water out of polygon geometry so coastal and bay-front tracts (or
#' places, counties, any `sf`) display as their land footprint instead of a
#' rectangle that runs out into the ocean. This is the cartographic complement
#' to [nt_areal_weight()]: that function already weights by population and land
#' area (water blocks carry zero), so the *numbers* never count water — this
#' makes the *map* agree, by removing the water polygon from the shapes you draw.
#'
#' It is deliberately lean. Water is fetched (and cut) **only for the area of
#' interest plus a small `buffer`** — never a whole state, let alone the nation:
#' it selects just the counties `x` (buffered) overlaps, and crops each county's
#' water to the area of interest right after download, so it never holds or
#' unions an entire county's ocean. The `buffer` lets it still erase water that
#' borders the edge of `x`, including in an adjacent county.
#'
#' @param x An `sf` object to clip (tracts, places, counties, ...). Attributes
#'   are preserved; only geometry is trimmed.
#' @param state Two-letter state abbreviation or FIPS code for the water pull. A
#'   single state.
#' @param county Optional county name(s) / FIPS within `state`. `NULL` (default)
#'   infers the counties from `x`'s own buffered footprint, so a small or
#'   one-county input only downloads the water it actually needs.
#' @param area_min Minimum water-body area in square meters to erase. Default
#'   `5e5` (0.5 km²) — large enough to drop ponds and creeks, small enough to
#'   keep bays, sloughs, large lakes, and the open coast.
#' @param buffer Distance (CRS units; meters for an equal-area CRS such as 3310,
#'   or via s2 for lon/lat) to extend the area of interest when selecting and
#'   cutting water, so bodies that lap the edge of `x` — including just over a
#'   county line — are still erased. Default `1000` (1 km). Larger considers
#'   more nearby water (and may pull an adjacent county).
#' @param year Vintage for the water (and county-inference) layers. `NULL`
#'   (default) lets tigris choose its default year.
#' @param ... Passed to [tigris::area_water()].
#'
#' @return `x` with water erased, geometry re-extracted to polygons, in `x`'s
#'   original CRS. If no qualifying water is found near `x`, `x` is returned
#'   unchanged.
#' @seealso [nt_areal_weight()] for the population/area weighting this
#'   complements; [tigris::area_water()] for the source water geometry.
#' @examples \dontrun{
#' library(tigris)
#' tr <- tracts("CA", "San Mateo", cb = TRUE, year = 2020)
#' tr_land <- nt_erase_water(tr, "CA", "San Mateo")   # bay + Pacific trimmed off
#' }
#' @export
nt_erase_water <- function(x, state, county = NULL, area_min = 5e5,
                           buffer = 1000, year = NULL, ...) {
  stopifnot("`x` must be an sf object" = inherits(x, "sf"),
            "`state` must be a single state" = length(state) == 1)
  crs0 <- sf::st_crs(x)
  xv   <- sf::st_make_valid(x)

  # Area of interest (+ buffer) — scope EVERYTHING (county selection, download,
  # and the cut) to here, so we never pull or process water far from x.
  region <- sf::st_buffer(sf::st_union(xv), buffer)
  rbox   <- sf::st_bbox(region)

  # Counties to pull water for: only those the region overlaps (typically one).
  if (is.null(county)) {
    co <- tigris::counties(state = state, cb = TRUE, year = year,
                           progress_bar = FALSE) |>
      sf::st_transform(crs0)
    county <- co$COUNTYFP[lengths(sf::st_intersects(co, region)) > 0]
  }
  if (length(county) == 0) return(x)

  # Per county: pull water, then drop small bodies + crop to the region, so we
  # never union/erase a whole county's ocean — only the water near x.
  water <- lapply(county, function(co) {
    w <- tryCatch(
      tigris::area_water(state = state, county = co, year = year,
                         progress_bar = FALSE, ...),
      error = function(e) NULL)
    if (is.null(w) || nrow(w) == 0) return(NULL)
    .ew_prep_water(sf::st_transform(w, crs0), rbox, area_min)
  })
  water <- do.call(c, Filter(Negate(is.null), water))   # concatenate sfc only
  if (is.null(water) || length(water) == 0) return(x)

  .ew_cut(xv, water)
}

# Reduce a county's water layer to the geometry worth erasing near the AOI: keep
# only bodies larger than area_min (skip ponds), crop to the region bbox (the
# "within the area of interest, plus nearby" scope), drop empties. Returns an
# sfc (geometry only) or NULL. Split out so the area filter + crop scoping are
# unit-testable without a tigris pull.
.ew_prep_water <- function(w, rbox, area_min) {
  w <- w[!is.na(w$AWATER) & w$AWATER > area_min, ]
  if (nrow(w) == 0) return(NULL)
  w <- suppressWarnings(sf::st_crop(sf::st_make_valid(w), rbox))
  w <- w[!sf::st_is_empty(w), ]
  if (nrow(w) == 0) NULL else sf::st_geometry(w)
}

# Erase the prepared water from x and re-extract polygons (st_difference can
# leave a GEOMETRYCOLLECTION). `water` is an sfc; `x` an sf in the same CRS.
.ew_cut <- function(x, water) {
  water <- sf::st_union(sf::st_make_valid(water))
  suppressWarnings(
    sf::st_difference(x, water) |>
      sf::st_collection_extract("POLYGON")
  )
}
