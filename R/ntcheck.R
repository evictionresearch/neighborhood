#' Investigate possible concatenation
#'
#' @param df \code{ntdf} created dataframe
#' @param ... Other keyword arguments
#' @return Frequncy table of each neighborhood type
#' @examples \dontrun
#' @export

ntcheck <- function(df, ...)
  df %>% dplyr::group_by(NeighType) %>% dplyr::count() %>% dplyr::arrange(dplyr::desc(n)) %>% data.frame()
