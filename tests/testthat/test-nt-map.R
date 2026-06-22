# Object-shape tests for the MapLibre wrappers. They inspect the serialized
# widget payload (layers, legend, controls) rather than rendering in a browser,
# so they run on CI. Guarded on mapgl being installed.

test_that("nt_maplibre returns a maplibre htmlwidget with the control set", {
  skip_if_not_installed("mapgl")
  d <- nt_test_sf()
  m <- nt_maplibre(d)
  expect_s3_class(m, "maplibregl")
  expect_s3_class(m, "htmlwidget")
  expect_true(all(c("navigation_control", "fullscreen_control",
                    "geolocate_control", "geocoder_control") %in% names(m$x)))
})

test_that("nt_maplibre can drop controls and search", {
  skip_if_not_installed("mapgl")
  m <- nt_maplibre(nt_test_sf(), controls = TRUE, search = FALSE)
  expect_false("geocoder_control" %in% names(m$x))
  expect_true("navigation_control" %in% names(m$x))
})

test_that("nt_add_choropleth adds a fill layer with popup, tooltip, and legend", {
  skip_if_not_installed("mapgl")
  d <- nt_test_sf()
  m <- nt_maplibre(d) |> nt_add_choropleth(d, "nt_conc")
  expect_length(m$x$layers, 1L)
  lyr <- m$x$layers[[1]]
  expect_identical(lyr$id, "nt_nt_conc")
  expect_identical(lyr$type, "fill")
  expect_false(is.null(lyr$popup))
  expect_identical(lyr$tooltip, "nt_conc")
  expect_gt(nchar(paste(m$x$legend_html, collapse = "")), 0L)
})

test_that("nt_map one-liner maps nt_conc by default", {
  skip_if_not_installed("mapgl")
  m <- nt_map(nt_test_sf())
  expect_s3_class(m, "htmlwidget")
  expect_identical(m$x$layers[[1]]$id, "nt_nt_conc")
})

test_that("numeric column maps as a binned layer with a chosen id", {
  skip_if_not_installed("mapgl")
  d <- nt_test_sf()
  m <- nt_maplibre(d) |>
    nt_add_choropleth(d, "pBlack", breaks = c(0.25, 0.5, 0.75), id = "afford")
  expect_identical(m$x$layers[[1]]$id, "afford")
})

test_that("nt_add_labels adds a symbol layer", {
  skip_if_not_installed("mapgl")
  d <- nt_test_sf()
  m <- nt_maplibre(d) |>
    nt_add_choropleth(d, "nt_conc") |>
    nt_add_labels(d, "NAME")
  types <- vapply(m$x$layers, function(z) z$type, character(1))
  expect_true("symbol" %in% types)
})

test_that("popup accepts an existing column name", {
  skip_if_not_installed("mapgl")
  d <- nt_test_sf()
  d$mypopup <- nt_popup(d, title = "GEOID", fields = c("Type" = "nt_conc"))
  m <- nt_maplibre(d) |> nt_add_choropleth(d, "nt_conc", popup = "mypopup")
  expect_identical(m$x$layers[[1]]$popup, "mypopup")
})

test_that("visible = FALSE adds a hidden layer", {
  skip_if_not_installed("mapgl")
  d <- nt_test_sf()
  m <- nt_maplibre(d) |>
    nt_add_choropleth(d, "nt_conc", id = "A") |>
    nt_add_choropleth(d, "pBlack", id = "B", visible = FALSE)
  vis <- vapply(m$x$layers, function(z) z$layout$visibility %||% "visible", character(1))
  ids <- vapply(m$x$layers, function(z) z$id, character(1))
  expect_identical(vis[ids == "A"], "visible")
  expect_identical(vis[ids == "B"], "none")
})

test_that("a named column does not corrupt the fill expression (regression)", {
  skip_if_not_installed("mapgl")
  d <- nt_test_sf()
  demos <- c(Black = "pBlack")          # named, as from demos[i] in a loop
  m <- nt_maplibre(d) |> nt_add_choropleth(d, demos[1], id = "Black")
  fc <- unlist(m$x$layers[[1]]$paint[["fill-color"]])
  expect_true("pBlack" %in% fc)          # plain column name present
  expect_false(any(names(fc) %in% "Black"))  # not serialized as a named object
})

test_that("a second choropleth appends its legend tied to its layer", {
  skip_if_not_installed("mapgl")
  d <- nt_test_sf()
  m <- nt_maplibre(d) |>
    nt_add_choropleth(d, "nt_conc", id = "A") |>
    nt_add_choropleth(d, "pBlack", id = "B")
  lh <- paste(m$x$legend_html, collapse = "")
  expect_true(grepl("A", lh) && grepl("B", lh))   # both legends present, tied to layers
})

test_that("nt_layers_control attaches a switcher referencing the layers", {
  skip_if_not_installed("mapgl")
  d <- nt_test_sf()
  m <- nt_maplibre(d) |>
    nt_add_choropleth(d, "nt_conc", id = "Typology") |>
    nt_add_choropleth(d, "pBlack", id = "Share", visible = FALSE) |>
    nt_layers_control(layers = c("Typology", "Share"), title = "Show")
  expect_s3_class(m, "htmlwidget")
  js <- paste(unlist(m$jsHooks$render), collapse = " ")
  expect_match(js, "setLayoutProperty")        # toggles layer visibility
  expect_match(js, "Typology")
  expect_match(js, "Share")
})

test_that("fill_color passthrough uses the custom expression + a manual legend", {
  skip_if_not_installed("mapgl")
  d <- nt_test_sf()
  d$flag <- c(TRUE, FALSE, FALSE, FALSE)
  fc <- list("case", list("==", list("get", "flag"), TRUE), "#c9c9c9",
             mapgl::interpolate(column = "pBlack", values = c(0, 0.5, 0.9),
                                stops = c("#fed976", "#fd8d3c", "#54278f")))
  m <- nt_maplibre(d) |>
    nt_add_choropleth(d, "pBlack", fill_color = fc, id = "custom",
                      colors = c("#fed976", "#54278f"), labels = c("Low", "High"))
  expect_identical(m$x$layers[[1]]$paint[["fill-color"]][[1]], "case")
  expect_gt(nchar(paste(m$x$legend_html, collapse = "")), 0L)   # legend from colors/labels
})

test_that("nt_layers_control validates inputs", {
  skip_if_not_installed("mapgl")
  d <- nt_test_sf()
  m <- nt_maplibre(d) |> nt_add_choropleth(d, "nt_conc", id = "A")
  expect_error(nt_layers_control(m, layers = "A", labels = c("x", "y")),
               "same length")
})

test_that("map functions reject non-spatial data with guidance", {
  d <- sf::st_drop_geometry(nt_test_sf())
  expect_error(nt_map(d), "not spatial|could not be converted")
  expect_error(nt_maplibre(d), "not spatial|could not be converted")
})
