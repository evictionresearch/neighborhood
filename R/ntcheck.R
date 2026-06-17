#' Tabulate neighborhood types
#'
#' @description
#' Counts how many tracts fall into each `NeighType`, sorted from most to least
#' common. Use it after [ntdf()] to see which fine-grained types are rare enough
#' to be worth collapsing — the `nt_conc` column already does a default
#' concatenation, and this table helps you decide whether to go further.
#'
#' @param df A data frame created by [ntdf()] (must contain a `NeighType`
#'   column).
#' @param ... Unused; present for backward compatibility.
#' @return A data frame with one row per `NeighType` and its count `n`, ordered
#'   by descending frequency.
#' @seealso [ntdf()] to create the input, [nt_pal()] / [nt_add_choropleth()] to
#'   map the result.
#' @examples
#' ntcheck(us_nt_tracts2024)
#' @export

ntcheck <- function(df, ...)
  df %>% dplyr::group_by(NeighType) %>% dplyr::count() %>% dplyr::arrange(dplyr::desc(n)) %>% data.frame()
