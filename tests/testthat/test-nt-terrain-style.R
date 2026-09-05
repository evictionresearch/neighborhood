# nt_terrain_style(): the house terrain basemap (light/dark), and its wiring
# through .nt_resolve_style() / nt_maplibre(). All offline, widget-level.

terrain_ids <- c(
  "bg", "hillshade", "landcover-wood", "landcover-scrub", "landcover-ice",
  "park", "landuse-residential", "water", "waterway", "building", "aeroway",
  "rail", "road-path", "road-track", "road-minor", "road-secondary",
  "road-primary", "road-motorway", "boundary-state", "boundary-country",
  "water-name", "road-name", "road-shield", "peak", "place-village",
  "place-town", "place-city", "place-state"
)

layer_ids <- function(s) vapply(s$layers, function(l) l$id, character(1))

test_that("nt_terrain_style() returns a complete v8 style with both sources", {
  s <- nt_terrain_style()
  expect_identical(s$version, 8L)
  expect_identical(s$glyphs,
    "https://tiles.openfreemap.org/fonts/{fontstack}/{range}.pbf")
  expect_identical(names(s$sources), c("omt", "dem"))
  # TileJSON indirection is load-bearing: url, never a hardcoded tiles array
  expect_identical(s$sources$omt$url, "https://tiles.openfreemap.org/planet")
  expect_null(s$sources$omt$tiles)
  expect_match(s$sources$omt$attribution, "OpenFreeMap", fixed = TRUE)
  expect_identical(s$sources$dem$encoding, "terrarium")
  expect_identical(s$sources$dem$tileSize, 256)
  expect_identical(s$sources$dem$maxzoom, 12)
  expect_length(s$sources$dem$tiles, 1L)
  expect_match(s$sources$dem$attribution, "Mapzen", fixed = TRUE)
})

test_that("the layer census matches the canonical 28 ids in order", {
  expect_identical(layer_ids(nt_terrain_style()), terrain_ids)
  expect_identical(layer_ids(nt_terrain_style("dark")), terrain_ids)
})

test_that("light and dark modes actually differ", {
  l <- nt_terrain_style("light")
  d <- nt_terrain_style("dark")
  expect_identical(l$layers[[1]]$paint[["background-color"]], "#eef1f4")
  expect_identical(d$layers[[1]]$paint[["background-color"]], "#16202b")
  expect_identical(l$layers[[2]]$paint[["hillshade-exaggeration"]], 0.48)
  expect_identical(d$layers[[2]]$paint[["hillshade-exaggeration"]], 0.60)
  # roadSoften opacity ramps ride light mode only
  minor <- function(s) s$layers[[which(layer_ids(s) == "road-minor")]]
  expect_false(is.null(minor(l)$paint[["line-opacity"]]))
  expect_null(minor(d)$paint[["line-opacity"]])
})

test_that("hillshade = FALSE drops the dem source and the hillshade layer", {
  f <- nt_terrain_style(hillshade = FALSE)
  expect_identical(names(f$sources), "omt")
  expect_length(f$layers, 27L)
  expect_identical(layer_ids(f), setdiff(terrain_ids, "hillshade"))
})

test_that("serialization keeps arrays as arrays (htmlwidgets unboxing)", {
  json <- as.character(htmlwidgets:::toJSON(nt_terrain_style()))
  # the length-1-array killer: text-font and tiles must never unbox
  expect_match(json, '"text-font":["', fixed = TRUE)
  expect_false(grepl('"text-font":"', json, fixed = TRUE))
  expect_match(json, '"tiles":["https://s3', fixed = TRUE)
  expect_false(grepl('"tiles":"', json, fixed = TRUE))
  expect_match(json, '"line-dasharray":[', fixed = TRUE)
  expect_false(grepl('"line-dasharray":[0-9]', json))
  expect_match(json, '"filter":[', fixed = TRUE)
  expect_false(grepl('"filter":{', json, fixed = TRUE))
  # the shield text expression, verbatim
  expect_match(json, paste0(
    '["case",["==",["get","network"],"us-interstate"],',
    '["concat","I-",["get","ref"]],',
    '["==",["get","network"],"us-highway"],',
    '["concat","US ",["get","ref"]],["get","ref"]]'), fixed = TRUE)
  expect_match(json,
    '"match",["get","class"],["motorway","trunk","primary"],true,false',
    fixed = TRUE)
  # peak label: name, newline, feet via 3.28084 -- all verbatim
  expect_match(json, paste0(
    '["concat",["coalesce",["get","name"],"Peak"],"\\n",',
    '["to-string",["round",["*",["get","ele"],3.28084]]]," ft"]'), fixed = TRUE)
})

test_that(".nt_resolve_style() knows the terrain names and keeps the old ones", {
  skip_if_not_installed("mapgl")
  tl <- .nt_resolve_style("terrain-light")
  td <- .nt_resolve_style("terrain-dark")
  expect_true(is.list(tl) && tl$version == 8L)
  expect_true(is.list(td) && td$version == 8L)
  expect_false(identical(tl$layers[[1]]$paint[["background-color"]],
                         td$layers[[1]]$paint[["background-color"]]))
  # OpenFreeMap names still resolve to URLs -- including the "dark" collision guard
  expect_type(.nt_resolve_style("positron"), "character")
  expect_type(.nt_resolve_style("dark"), "character")
  expect_match(.nt_resolve_style("dark"), "openfreemap", fixed = TRUE)
  # everything else passes through untouched
  url <- "https://example.org/style.json"
  expect_identical(.nt_resolve_style(url), url)
  lst <- list(version = 8L, sources = list(), layers = list())
  expect_identical(.nt_resolve_style(lst), lst)
})

test_that("nt_maplibre() defaults to terrain-light; positron still reachable", {
  skip_if_not_installed("mapgl")
  m <- nt_maplibre(nt_test_sf())
  expect_true(is.list(m$x$style))
  expect_identical(m$x$style$version, 8L)
  expect_type(nt_maplibre(nt_test_sf(), style = "positron")$x$style, "character")
})

test_that("nt_declutter_basemap()'s default pattern hides exactly the terrain tints", {
  pat <- eval(formals(nt_declutter_basemap)$pattern)
  hidden <- terrain_ids[grepl(pat, terrain_ids, ignore.case = TRUE)]
  expect_setequal(hidden, c("hillshade", "landcover-wood", "landcover-scrub",
                            "landcover-ice", "park", "landuse-residential"))
})

test_that("the water layer keeps the before_id fill-sandwich position", {
  ids <- layer_ids(nt_terrain_style())
  expect_true(which(ids == "water") > which(ids == "hillshade"))
  expect_true(which(ids == "water") < which(ids == "road-minor"))
})
