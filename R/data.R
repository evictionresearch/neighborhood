#' US census tract neighborhood racial typologies, 2019
#'
#' Tract-level racial/ethnic composition and `ntdf()` typology assignments for
#' all US states using 5-year ACS data centered on 2019.
#'
#' @format A tibble with one row per census tract and the following columns:
#' \describe{
#'   \item{GEOID}{Census tract GEOID (character).}
#'   \item{NAME}{Tract name as returned by tidycensus.}
#'   \item{totraceE, WhiteE, BlackE, AsianE, LatineE}{ACS estimates from B03002.}
#'   \item{pWhite, pAsian, pBlack, pLatine, pOther}{Group share of total race.}
#'   \item{NeighType}{Detailed typology (e.g. "Asian-White").}
#'   \item{nt_conc}{Concatenated typology factor with shared levels across years.}
#'   \item{state}{Two-letter state postal code.}
#'   \item{year}{ACS endpoint year (2019).}
#' }
#' @source US Census Bureau, American Community Survey 5-year (table B03002).
"us_nt_tracts2019"

#' US census tract neighborhood racial typologies, 2022
#'
#' Tract-level racial/ethnic composition and `ntdf()` typology assignments for
#' all US states using 5-year ACS data centered on 2022.
#'
#' @format See [us_nt_tracts2019]; columns are identical.
#' @source US Census Bureau, American Community Survey 5-year (table B03002).
"us_nt_tracts2022"

#' US census tract neighborhood racial typologies, 2021
#'
#' Tract-level racial/ethnic composition and `ntdf()` typology assignments for
#' all US states using 5-year ACS data centered on 2021.
#'
#' @format See [us_nt_tracts2019]; columns are identical.
#' @source US Census Bureau, American Community Survey 5-year (table B03002).
"us_nt_tracts2021"
