#' Neighborhood-typology color palette (for leaflet)
#'
#' @description
#' Returns a [leaflet::colorFactor()] palette mapping the 19 concatenated
#' neighborhood-typology levels in the `nt_conc` column (created by [ntdf()])
#' to the Eviction Research Network house colors. Use it to color a `leaflet`
#' choropleth of typologies.
#'
#' For **MapLibre** maps, you do not need this function: [nt_add_choropleth()]
#' applies the same color key automatically when it detects an `nt_conc`
#' column. Both paths read from one internal color vector, so they always
#' agree.
#'
#' @param df An `ntdf()`-created data frame containing an `nt_conc` factor.
#' @param ... Unused; present for backward compatibility.
#' @return A `leaflet` palette function (the result of [leaflet::colorFactor()])
#'   that maps `nt_conc` levels to hex colors, with grey (`#C0C0C0`) for `NA`.
#' @seealso [ntdf()] to create the data, [nt_add_choropleth()] for the MapLibre
#'   equivalent, and [leaflet::colorFactor()].
#' @examples
#' \dontrun{
#' md <- ntdf(state = "MD", county = "Baltimore City", geometry = TRUE)
#' pal <- nt_pal(md)
#' leaflet::leaflet(md) |>
#'   leaflet::addPolygons(fillColor = ~pal(nt_conc), fillOpacity = 0.7,
#'                        weight = 0.3, color = "#ffffff") |>
#'   leaflet::addLegend(pal = pal, values = ~nt_conc)
#' }
#' @export
nt_pal <- function(df, ...) {
  leaflet::colorFactor(
    palette  = unname(.nt_colors),
    domain   = df$nt_conc,
    na.color = .nt_na_color
  )
}
