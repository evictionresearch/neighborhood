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

#' Minnesota county eviction filings, monthly
#'
#' Aggregated monthly eviction-court filings for all 87 Minnesota counties — the
#' public data behind the Eviction Research Network state profile at
#' <https://evictionresearch.net/minnesota/>. One row per county-month,
#' 2017-01 through 2026-03 (the newest month is held back under the page's
#' expungement "hold-out rule"). Includes race-imputed filing counts and matching
#' renter denominators so demographic rate trends can be reproduced.
#'
#' @format A data frame with 9,657 rows and the following columns:
#' \describe{
#'   \item{geoid}{5-digit county FIPS code (character).}
#'   \item{county}{County name.}
#'   \item{year, month}{Calendar year and month (integer).}
#'   \item{filings}{Eviction filings recorded that month (court records).}
#'   \item{renters}{Renter households (ACS; constant per county).}
#'   \item{rate}{Raw monthly proportion, `filings / renters`. For the annualized
#'     "filings per 1,000 renter households" figure the profiles report, compute
#'     `1000 * sum(filings) * 12 / n_months / renters` over a window.}
#'   \item{filings_black, filings_white, filings_latine, filings_other}{
#'     Race/ethnicity-imputed filing counts (fractional).}
#'   \item{renters_black, renters_white, renters_latine, renters_other}{
#'     Renter households by race/ethnicity (ACS denominators).}
#' }
#' @source Eviction Research Network / Legal Services Corporation court records,
#'   aggregated to county-month; renter denominators from the ACS.
#' @seealso [nt_chart()] to plot it; `vignette("interactive-charts")`.
"mn_evictions"
