#' Suggested Color Palette
#'
#' @param df ntdf object
#' @param ... Other keyword arguments
#' @return Returns a factored color palette
#' @export

nt_pal <- function(df, ...){
  leaflet::colorFactor(c(
    '#33a02c', # 'Mostly Asian', green
    '#1f78b4', # 'Mostly Black', blue
    '#e31a1c', # 'Mostly Latino', red
    '#9b66b0', # 'Mostly Other', purple
    '#C95123', # 'Mostly White',
    '#1fc2ba', # 'Asian-Black',
    '#d6ae5c', # 'Asian-Latino',
    '#91c7b9', # 'Asian-Other',
    '#b2df8a', # 'Asian-White',
    '#de4e4b', # 'Black-Latino',
    '#71a1f5', # 'Black-Other',
    '#a6cee3', # 'Black-White',
    '#f0739b', # 'Latino-Other',
    '#fb9a99', # 'Latino-White',
    '#c28a86', # 'Other-White',
    '#fdbf6f', # '3 Group Mixed',
    '#cab2d6', # '4 Group Mixed',
    '#1d5fd1', # 'Diverse',
    '#FFFFFF'),  # 'Unpopulated Tract'
    domain = df$nt_conc,
    na.color = '#C0C0C0'
  )}
