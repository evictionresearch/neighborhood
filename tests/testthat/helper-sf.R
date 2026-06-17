# Small, deterministic sf fixture for map-function tests: four square "tracts"
# in WGS84 with a typology factor, numeric columns, and id/name/county columns.
nt_test_sf <- function() {
  lv <- c("Mostly Asian", "Mostly Black", "Mostly Latine", "Mostly Other",
          "Mostly White", "Asian-Black", "Asian-Latine", "Asian-Other",
          "Asian-White", "Black-Latine", "Black-Other", "Black-White",
          "Latine-Other", "Latine-White", "Other-White", "3 Group Mixed",
          "4 Group Mixed", "Diverse", "Unpopulated Tract")
  sq <- function(x, y) {
    sf::st_polygon(list(rbind(c(x, y), c(x + 0.1, y), c(x + 0.1, y + 0.1),
                              c(x, y + 0.1), c(x, y))))
  }
  sf::st_sf(
    GEOID   = sprintf("0600100%04d", 1:4),
    NAME    = paste("Census Tract", 1:4),
    county  = c("Alpha", "Alpha", "Beta", "Beta"),
    nt_conc = factor(c("Mostly White", "Asian-White", "Black-White", "Diverse"),
                     levels = lv),
    pBlack   = c(0.05, 0.22, 0.48, 0.90),
    totraceE = c(4231L, 5012L, 3100L, 2750L),
    geometry = sf::st_sfc(sq(-122.4, 37.7), sq(-122.3, 37.7),
                          sq(-122.4, 37.8), sq(-122.3, 37.8)),
    crs = 4326
  )
}
