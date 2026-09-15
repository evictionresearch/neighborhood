# The ERN terrain basemap -- the house navy-terrain style, ported from the
# canonical generator in the Boondock map:
#   ~/git/cidrlab/boondock_map/boondock/src/shared/boondockStyle.js
# Client-rendered hillshade under a minimalist vector map: water and forest
# carry the brand navies, roads are cool-blue hairlines, and mountain peaks
# label their elevation in feet. Sources are keyless and free: OpenFreeMap
# vector tiles (OpenMapTiles schema) and the public terrain-tiles DEM on AWS
# (Mapzen terrarium encoding).
#
# The original's Electron-only offline machinery (localStorage tile-template
# cache, boondock:// protocol) is deliberately not ported; the omt source uses
# TileJSON `url` indirection, which MapLibre resolves at load time. Never
# replace it with a hardcoded `tiles` array -- OpenFreeMap's concrete tile
# URLs carry a rotating dated build path and go stale.
#
# Serialization discipline (the correctness contract of this file): the style
# is a plain R list that htmlwidgets serializes to JSON for MapLibre. Named
# lists become objects; unnamed list(...) becomes a JSON array even at length
# one; atomic length-1 vectors unbox to bare scalars. So every value that must
# stay a JSON array -- filters and expressions (interpolate/case/concat/...),
# text-field, text-font, line-dasharray, tiles -- is an unnamed list(), never
# c(). Scalars (colors, sizes, zooms) stay atomic. Conditional keys (the
# light-mode opacity ramps, the shield visibility) are added by if-assignment,
# never NULL placeholders.

# Palettes. "dark" is the original's NIGHT (navy terrain), "light" its DAY
# ("navy ink on paper-light ground for full sun"). Hexes verbatim from
# boondockStyle.js; only light carries roadSoften (opacity ramps that read
# calmer over dense urban grids).
.nt_terrain_palette <- function(mode) {
  if (mode == "dark") {
    list(
      land       = "#16202b",
      landDark   = "#121a23",
      forest     = "#1b2b34",   # wooded ground sits slightly lighter than clearings
      scrub      = "#182631",
      park       = "#1a2c40",
      water      = "#223754",
      waterway   = "#35506e",
      building   = "#222f3d",
      roadMajor  = "#587a9e",
      roadMid    = "#47617e",
      roadMinor  = "#3a536e",
      path       = "#6b87a6",
      rail       = "#31414f",
      boundary   = "rgba(232, 238, 244, 0.28)",
      textBright = "#e8eef4",
      text       = "#c6d4e2",
      textDim    = "#9fb4c8",
      textFaint  = "#6d8098",
      halo       = "#0e141b",
      ice        = "#28394c",
      waterName  = "#7d9bbf",
      peakText   = "#b9c9d9",
      cityText   = "#e8eef4",
      hsExaggeration = 0.60,
      hsShadow    = "#070b10",
      hsHighlight = "#52719b",
      hsAccent    = "#0d131b"
    )
  } else {
    list(
      land       = "#eef1f4",
      landDark   = "#e4e9ee",
      forest     = "#e0e8e0",   # wooded ground a shade darker than clearings
      scrub      = "#e7ece7",
      park       = "#dce8f0",
      water      = "#b9cfe4",
      waterway   = "#9dbcd8",
      building   = "#d9dfe7",
      roadMajor  = "#3f5a78",
      roadMid    = "#55708e",
      roadMinor  = "#8199b1",
      path       = "#6e87a3",
      rail       = "#b6c0cc",
      boundary   = "rgba(25, 34, 44, 0.30)",
      textBright = "#19222C",
      text       = "#2e4054",
      textDim    = "#55708e",
      textFaint  = "#7e91a6",
      halo       = "#f2f5f8",
      ice        = "#e6edf5",
      waterName  = "#5b7ea3",
      peakText   = "#3d5270",
      cityText   = "#26364a",
      roadSoften = TRUE,   # dense urban grids read calmer with opacity ramps
      hsExaggeration = 0.48,
      hsShadow    = "#93a6bc",
      hsHighlight = "#ffffff",
      hsAccent    = "#cdd8e4"
    )
  }
}

# 50/50 mix of two hex colors, for the soft-streets variant: road inks pulled
# halfway toward the ground tone so the grid recedes under a choropleth.
# Base grDevices only; returns an uppercase "#RRGGBB" scalar, which keeps the
# serialization discipline above (colors stay atomic). Half-up rounding via
# floor(x + 0.5), not round(): it matches JavaScript's Math.round, so this
# port and the canonical generator (boondockStyle.js mixHex) emit identical
# hexes even when a channel mix lands on .5.
.nt_mix_hex <- function(hex, with, w = 0.5) {
  a <- grDevices::col2rgb(hex)
  b <- grDevices::col2rgb(with)
  grDevices::rgb(t(floor(a * (1 - w) + b * w + 0.5)), maxColorValue = 255)
}

# Route numbers for the roads you navigate by. `ref` is a separate field from
# `name` in the OpenMapTiles schema, so the road-name layer, which labels
# `name`, never shows a route number. Bold haloed text rather than a shield
# graphic: shields need a sprite sheet and this style deliberately has none,
# and a legible "I-5" beats a fake shield.
.nt_terrain_road_shield <- function(C, id = "road-shield", visibility = NULL) {
  layout <- list(
    # `ref` is the bare number, so an interstate would render as a lone "5".
    # `network` says which shield it would be, so it reads I-5 / US 97, and
    # anything else keeps its own ref, which already carries a letter suffix
    # like 99W.
    "text-field" = list("case",
      list("==", list("get", "network"), "us-interstate"), list("concat", "I-", list("get", "ref")),
      list("==", list("get", "network"), "us-highway"), list("concat", "US ", list("get", "ref")),
      list("get", "ref")
    ),
    "text-font" = list("Noto Sans Bold"),
    "text-size" = list("interpolate", list("linear"), list("zoom"), 8, 10, 12, 12.5),
    "symbol-placement" = "line",
    "symbol-spacing" = 220,
    "text-rotation-alignment" = "viewport",   # shields read upright, not along the curve
    "text-padding" = 3
  )
  if (!is.null(visibility)) layout[["visibility"]] <- visibility
  list(
    id = id,
    type = "symbol",
    source = "omt",
    "source-layer" = "transportation_name",
    minzoom = 8,
    filter = list("all",
      list("has", "ref"),
      list("match", list("get", "class"), list("motorway", "trunk", "primary"), TRUE, FALSE)
    ),
    layout = layout,
    paint = list(
      # shieldText is set by nt_terrain_style(): textBright normally, the
      # faint label tone under streets = "soft"
      "text-color" = C$shieldText,
      "text-halo-color" = C$halo,
      "text-halo-width" = 2.2
    )
  )
}

#' The ERN terrain basemap style (light and dark)
#'
#' @description
#' Builds the house terrain basemap: client-rendered hillshade relief under a
#' minimalist vector map, with water and forest in the brand navies, roads as
#' cool-blue hairlines, haloed-text route shields ("I-5", "US 97"), and
#' mountain peaks labeled with their elevation in feet. Two variants share one
#' design: `"light"` draws navy ink on a paper-light ground (and softens dense
#' urban road grids with opacity ramps), `"dark"` is the navy night terrain.
#'
#' This is the style behind `nt_maplibre(style = "terrain-light")` (the package
#' default) and `"terrain-dark"`; call it directly when you want to tweak the
#' result or drop the hillshade. The design was created for the Boondock map
#' and is shared here as the ERN house basemap.
#'
#' All map resources are keyless and free: OpenFreeMap vector tiles and glyphs
#' (OpenMapTiles schema, OpenStreetMap data) and the public Mapzen terrain
#' tiles on AWS for the hillshade. Attribution for both rides on the style's
#' sources, so MapLibre's attribution control shows it automatically. The
#' style needs no sprite sheet at all.
#'
#' @param mode `"light"` (default) or `"dark"`.
#' @param hillshade Logical; include the terrain-relief hillshade layer
#'   (default `TRUE`). `FALSE` drops both the elevation source and the
#'   hillshade layer, so the map makes no terrain-tile requests at all --
#'   useful under an opaque choropleth. (With the default choropleth pipeline
#'   you rarely need this: [nt_declutter_basemap()] already hides the
#'   hillshade and land tints under the fills.)
#' @param streets `"ink"` (default) draws the roads in the full navy road
#'   inks; `"soft"` pulls every road ink halfway toward the ground tone and
#'   drops the route shields ("I-5", "US 101") to the faint label color, so
#'   dense urban grids recede and a choropleth carries the pane. Born on the
#'   San Mateo rental-vacancy figure (2026-09-15), where the full-ink grid
#'   read as too heavy beside the data. Also available through
#'   [nt_maplibre()] as `style = "terrain-light-soft"` / `"terrain-dark-soft"`.
#' @return A MapLibre style specification as a plain R list, for the `style`
#'   argument of [nt_maplibre()] or [mapgl::maplibre()].
#' @family ern_maps
#' @seealso [nt_maplibre()], [nt_declutter_basemap()]; [mapgl::maplibre()] for
#'   the underlying engine.
#' @examplesIf interactive() && requireNamespace("mapgl", quietly = TRUE)
#' # The two variants, and a flat (no-hillshade) light map
#' nt_maplibre(style = "terrain-dark", center = c(-122.33, 47.6), zoom = 9)
#' nt_maplibre(style = nt_terrain_style("light", hillshade = FALSE))
#' # Soft streets: the road grid recedes under a choropleth
#' nt_maplibre(style = "terrain-light-soft")
#' @export
nt_terrain_style <- function(mode = c("light", "dark"), hillshade = TRUE,
                             streets = c("ink", "soft")) {
  mode <- match.arg(mode)
  streets <- match.arg(streets)
  stopifnot(is.logical(hillshade), length(hillshade) == 1L, !is.na(hillshade))
  C <- .nt_terrain_palette(mode)

  # Soft streets: mix the road inks toward the ground so the grid recedes;
  # the shields follow, dropping to the faint label tone. Applied at the
  # palette level so every layer that borrows a road color (aeroway) follows.
  C$shieldText <- C$textBright
  if (streets == "soft") {
    for (k in c("roadMajor", "roadMid", "roadMinor", "path"))
      C[[k]] <- .nt_mix_hex(C[[k]], C$land)
    C$shieldText <- C$textFaint
  }

  sources <- list(
    omt = list(
      type = "vector",
      # TileJSON indirection is load-bearing: OpenFreeMap's concrete tile URLs
      # carry a rotating dated build path, so a hardcoded tiles array goes
      # stale. MapLibre fetches this TileJSON and resolves the template.
      url = "https://tiles.openfreemap.org/planet",
      attribution = "\u00a9 OpenFreeMap \u00a9 OpenMapTiles \u00a9 OpenStreetMap contributors"
    ),
    dem = list(
      type = "raster-dem",
      tiles = list("https://s3.amazonaws.com/elevation-tiles-prod/terrarium/{z}/{x}/{y}.png"),
      encoding = "terrarium",
      tileSize = 256,
      # z12 relief is plenty when overzoomed; halves DEM requests up close
      maxzoom = 12,
      attribution = "Terrain: Mapzen terrain tiles (USGS 3DEP, SRTM) via AWS"
    )
  )

  # Road paints, built stepwise so light mode can add its opacity ramps
  # (the original's `...(C.roadSoften && {...})` spreads) without dark mode
  # ever carrying a line-opacity key.
  soften <- isTRUE(C$roadSoften)
  p_minor <- list(
    "line-color" = C$roadMinor,
    "line-width" = list("interpolate", list("exponential", 1.4), list("zoom"), 11, 0.5, 18, 5)
  )
  if (soften) p_minor[["line-opacity"]] <- list("interpolate", list("linear"), list("zoom"), 11, 0.45, 14, 0.9)
  p_secondary <- list(
    "line-color" = C$roadMid,
    "line-width" = list("interpolate", list("exponential", 1.4), list("zoom"), 8, 0.6, 18, 7)
  )
  if (soften) p_secondary[["line-opacity"]] <- list("interpolate", list("linear"), list("zoom"), 8, 0.55, 13, 0.9)
  p_primary <- list(
    "line-color" = C$roadMajor,
    "line-width" = list("interpolate", list("exponential", 1.4), list("zoom"), 6, 0.8, 18, 9)
  )
  if (soften) p_primary[["line-opacity"]] <- list("interpolate", list("linear"), list("zoom"), 6, 0.65, 12, 0.95)
  p_motorway <- list(
    "line-color" = C$roadMajor,
    "line-width" = list("interpolate", list("exponential", 1.4), list("zoom"), 5, 1, 18, 11)
  )
  if (soften) p_motorway[["line-opacity"]] <- list("interpolate", list("linear"), list("zoom"), 5, 0.7, 12, 0.95)

  layers <- list(
    list(id = "bg", type = "background", paint = list("background-color" = C$land)),

    # Terrain first: everything else reads on top of the relief. Day runs a
    # lower exaggeration than night because pale shadow on light ground
    # carries further than it does over navy.
    list(
      id = "hillshade", type = "hillshade", source = "dem",
      paint = list(
        "hillshade-exaggeration" = C$hsExaggeration,
        "hillshade-shadow-color" = C$hsShadow,
        "hillshade-highlight-color" = C$hsHighlight,
        "hillshade-accent-color" = C$hsAccent
      )
    ),

    # -- Landcover ------------------------------------------------------
    list(
      id = "landcover-wood", type = "fill", source = "omt", "source-layer" = "landcover",
      filter = list("any", list("==", list("get", "class"), "wood"), list("==", list("get", "class"), "forest")),
      paint = list("fill-color" = C$forest, "fill-opacity" = 0.5)
    ),
    list(
      id = "landcover-scrub", type = "fill", source = "omt", "source-layer" = "landcover",
      filter = list("any", list("==", list("get", "class"), "grass"), list("==", list("get", "class"), "scrub"), list("==", list("get", "class"), "wetland")),
      paint = list("fill-color" = C$scrub, "fill-opacity" = 0.4)
    ),
    list(
      id = "landcover-ice", type = "fill", source = "omt", "source-layer" = "landcover",
      filter = list("==", list("get", "class"), "ice"),
      paint = list("fill-color" = C$ice, "fill-opacity" = 0.55)
    ),
    list(
      id = "park", type = "fill", source = "omt", "source-layer" = "park",
      paint = list("fill-color" = C$park, "fill-opacity" = 0.3)
    ),
    list(
      id = "landuse-residential", type = "fill", source = "omt", "source-layer" = "landuse",
      filter = list("any", list("==", list("get", "class"), "residential"), list("==", list("get", "class"), "commercial"), list("==", list("get", "class"), "industrial")),
      paint = list("fill-color" = C$landDark, "fill-opacity" = 0.6)
    ),

    # -- Water ----------------------------------------------------------
    list(
      id = "water", type = "fill", source = "omt", "source-layer" = "water",
      paint = list("fill-color" = C$water)
    ),
    list(
      id = "waterway", type = "line", source = "omt", "source-layer" = "waterway",
      paint = list(
        "line-color" = C$waterway,
        "line-width" = list("interpolate", list("exponential", 1.4), list("zoom"), 8, 0.5, 14, 2.2)
      )
    ),

    # -- Buildings (close zooms only) -----------------------------------
    list(
      id = "building", type = "fill", source = "omt", "source-layer" = "building", minzoom = 14,
      paint = list("fill-color" = C$building, "fill-opacity" = 0.7)
    ),

    # -- Aeroway / rail -------------------------------------------------
    list(
      id = "aeroway", type = "line", source = "omt", "source-layer" = "aeroway", minzoom = 10,
      paint = list("line-color" = C$roadMinor, "line-width" = 1.5)
    ),
    list(
      id = "rail", type = "line", source = "omt", "source-layer" = "transportation",
      filter = list("==", list("get", "class"), "rail"),
      paint = list("line-color" = C$rail, "line-width" = 1, "line-dasharray" = list(4, 3))
    ),

    # -- Roads: cool-blue hairlines, thinner than anyone else's ---------
    list(
      id = "road-path", type = "line", source = "omt", "source-layer" = "transportation", minzoom = 12,
      filter = list("==", list("get", "class"), "path"),
      paint = list(
        "line-color" = C$path,
        "line-width" = list("interpolate", list("linear"), list("zoom"), 12, 0.6, 16, 1.6),
        "line-dasharray" = list(2.5, 1.6)
      )
    ),
    list(
      id = "road-track", type = "line", source = "omt", "source-layer" = "transportation", minzoom = 10,
      filter = list("==", list("get", "class"), "track"),
      paint = list(
        "line-color" = C$path,
        "line-width" = list("interpolate", list("linear"), list("zoom"), 10, 0.5, 16, 2),
        "line-dasharray" = list(4, 2)
      )
    ),
    list(
      id = "road-minor", type = "line", source = "omt", "source-layer" = "transportation",
      filter = list("in", list("get", "class"), list("literal", list("minor", "service"))),
      minzoom = 11,
      paint = p_minor
    ),
    list(
      id = "road-secondary", type = "line", source = "omt", "source-layer" = "transportation",
      filter = list("in", list("get", "class"), list("literal", list("secondary", "tertiary"))),
      paint = p_secondary
    ),
    list(
      id = "road-primary", type = "line", source = "omt", "source-layer" = "transportation",
      filter = list("in", list("get", "class"), list("literal", list("primary", "trunk"))),
      paint = p_primary
    ),
    list(
      id = "road-motorway", type = "line", source = "omt", "source-layer" = "transportation",
      filter = list("==", list("get", "class"), "motorway"),
      paint = p_motorway
    ),

    # -- Boundaries -----------------------------------------------------
    list(
      id = "boundary-state", type = "line", source = "omt", "source-layer" = "boundary",
      filter = list("==", list("get", "admin_level"), 4),
      paint = list("line-color" = C$boundary, "line-width" = 1, "line-dasharray" = list(3, 2))
    ),
    list(
      id = "boundary-country", type = "line", source = "omt", "source-layer" = "boundary",
      filter = list("==", list("get", "admin_level"), 2),
      paint = list("line-color" = C$boundary, "line-width" = 1.5)
    ),

    # -- Labels ---------------------------------------------------------
    list(
      id = "water-name", type = "symbol", source = "omt", "source-layer" = "water_name",
      layout = list(
        "text-field" = list("get", "name"),
        "text-font" = list("Noto Sans Italic"),
        "text-size" = 11
      ),
      paint = list("text-color" = C$waterName, "text-halo-color" = C$halo, "text-halo-width" = 1)
    ),
    list(
      id = "road-name", type = "symbol", source = "omt", "source-layer" = "transportation_name", minzoom = 12,
      layout = list(
        "text-field" = list("get", "name"),
        "text-font" = list("Noto Sans Regular"),
        "text-size" = 10.5,
        "symbol-placement" = "line"
      ),
      paint = list("text-color" = C$textDim, "text-halo-color" = C$halo, "text-halo-width" = 1)
    ),
    .nt_terrain_road_shield(C),
    list(
      # Peaks label their elevation in feet -- the map carries its own numbers
      id = "peak", type = "symbol", source = "omt", "source-layer" = "mountain_peak", minzoom = 9,
      filter = list("has", "ele"),
      layout = list(
        "text-field" = list("concat",
          list("coalesce", list("get", "name"), "Peak"), "\n",
          list("to-string", list("round", list("*", list("get", "ele"), 3.28084))), " ft"),
        "text-font" = list("Noto Sans Italic"),
        "text-size" = 10,
        "text-line-height" = 1.25
      ),
      paint = list("text-color" = C$peakText, "text-halo-color" = C$halo, "text-halo-width" = 1.1)
    ),
    list(
      id = "place-village", type = "symbol", source = "omt", "source-layer" = "place",
      filter = list("in", list("get", "class"), list("literal", list("village", "hamlet", "suburb"))),
      minzoom = 10,
      layout = list("text-field" = list("get", "name"), "text-font" = list("Noto Sans Regular"), "text-size" = 11),
      paint = list("text-color" = C$textDim, "text-halo-color" = C$halo, "text-halo-width" = 1.2)
    ),
    list(
      id = "place-town", type = "symbol", source = "omt", "source-layer" = "place",
      filter = list("==", list("get", "class"), "town"),
      layout = list("text-field" = list("get", "name"), "text-font" = list("Noto Sans Regular"), "text-size" = 12.5),
      paint = list("text-color" = C$text, "text-halo-color" = C$halo, "text-halo-width" = 1.3)
    ),
    list(
      id = "place-city", type = "symbol", source = "omt", "source-layer" = "place",
      filter = list("==", list("get", "class"), "city"),
      layout = list("text-field" = list("get", "name"), "text-font" = list("Noto Sans Bold"), "text-size" = 14, "text-letter-spacing" = 0.02),
      paint = list("text-color" = C$cityText, "text-halo-color" = C$halo, "text-halo-width" = 1.4)
    ),
    list(
      id = "place-state", type = "symbol", source = "omt", "source-layer" = "place",
      filter = list("==", list("get", "class"), "state"),
      maxzoom = 8,
      layout = list(
        "text-field" = list("upcase", list("get", "name")),
        "text-font" = list("Noto Sans Regular"),
        "text-size" = 11,
        "text-letter-spacing" = 0.25
      ),
      paint = list("text-color" = C$textFaint, "text-halo-color" = C$halo, "text-halo-width" = 1)
    )
  )

  if (!hillshade) {
    sources$dem <- NULL
    ids <- vapply(layers, function(l) l$id, character(1))
    layers <- layers[ids != "hillshade"]
  }

  list(
    version = 8L,
    glyphs = "https://tiles.openfreemap.org/fonts/{fontstack}/{range}.pbf",
    sources = sources,
    layers = layers
  )
}
