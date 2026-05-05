#' US census tract neighborhood racial typologies, 2024
#'
#' Tract-level racial/ethnic composition and `ntdf()` typology assignments for
#' all US states (plus DC and Puerto Rico) using 5-year ACS data centered on
#' 2024 (2020-2024 endpoint).
#'
#' @format A tibble with one row per census tract and the following columns:
#' \describe{
#'   \item{GEOID}{Census tract GEOID (character).}
#'   \item{NAME}{Tract name as returned by tidycensus.}
#'   \item{totraceE, WhiteE, BlackE, AsianE, LatineE}{ACS estimates from B03002.}
#'   \item{pWhite, pAsian, pBlack, pLatine, pOther}{Group share of total race.}
#'   \item{NeighType}{Detailed typology (e.g. "Asian-White").}
#'   \item{nt_conc}{Concatenated typology factor with shared levels.}
#'   \item{state}{Two-letter state postal code.}
#'   \item{year}{ACS endpoint year (2024).}
#' }
#' @source US Census Bureau, American Community Survey 5-year (table B03002).
"us_nt_tracts2024"
