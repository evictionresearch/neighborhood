# nt_qs_convert(): .qs/.qs2 -> Parquet and friends.
# No-network -- small objects written to a per-test tempdir. qs2 is the source
# format throughout because the legacy `qs` package may not be installed; the
# legacy read path is covered by the reader's own fallback, not exercised here.

skip_if_no <- function(...) for (p in c(...)) skip_if_not_installed(p)

# A fresh directory per fixture, so a test never sees another test's outputs.
# Plain tempfile() rather than a scoped temp directory, which would unwind here
# inside the helper and delete the directory before the test opened it.
new_dir <- function() {
  d <- tempfile("ntqs-")
  dir.create(d, recursive = TRUE)
  d
}

qs_fixture <- function(obj, name, dir = new_dir()) {
  skip_if_no("qs2")
  f <- file.path(dir, name)
  qs2::qs_save(obj, f)
  f
}

small_df <- function()
  data.frame(GEOID = c("06081001", "06081002"), rate = c(1.5, 2.5),
             stringsAsFactors = FALSE)

test_that("the default is parquet, written beside the source with the prefix kept", {
  skip_if_no("qs2", "arrow")
  f <- qs_fixture(small_df(), "d6_tract_agg_202512.qs2")

  log <- nt_qs_convert(f, quiet = TRUE)

  expect_equal(nrow(log), 1L)
  expect_equal(log$format, "parquet")
  expect_equal(log$status, "written")
  expect_equal(basename(log$path), "d6_tract_agg_202512.parquet")   # d6 prefix survives
  expect_equal(dirname(log$path), dirname(f))                       # beside the source
  expect_equal(as.data.frame(arrow::read_parquet(log$path)), small_df())
})

test_that("`to` takes any number of formats and writes one file each", {
  skip_if_no("qs2", "arrow", "fst")
  f <- qs_fixture(small_df(), "d6_agg.qs2")

  log <- nt_qs_convert(f, to = c("parquet", "csv", "rds", "fst"), quiet = TRUE)

  expect_equal(nrow(log), 4L)
  expect_true(all(log$status == "written"))
  expect_setequal(basename(log$path),
                  c("d6_agg.parquet", "d6_agg.csv", "d6_agg.rds", "d6_agg.fst"))
  expect_true(all(file.exists(log$path)))
  expect_equal(readRDS(file.path(dirname(f), "d6_agg.rds")), small_df())
})

test_that("format names tolerate case, a leading dot, and aliases", {
  skip_if_no("qs2", "arrow")
  f <- qs_fixture(small_df(), "d6_agg.qs2")

  log <- nt_qs_convert(f, to = c(".Parquet", "GEOPARQUET", "arrow"), quiet = TRUE)

  expect_equal(nrow(log), 2L)                       # parquet de-duplicated
  expect_setequal(log$format, c("parquet", "feather"))
  expect_true(all(log$status == "written"))
})

test_that("an unsupported format errors and lists what is supported", {
  skip_if_no("qs2")
  f <- qs_fixture(small_df(), "d6_agg.qs2")
  expect_error(nt_qs_convert(f, to = "parqet"), "Unsupported output format")
  expect_error(nt_qs_convert(f, to = "parqet"), "parquet")
})

test_that("an object parquet cannot hold errors per file and names the way out", {
  skip_if_no("qs2", "arrow")
  f <- qs_fixture(list(fit = lm(mpg ~ wt, mtcars)), "m1_fit.qs2")

  expect_warning(log <- nt_qs_convert(f, to = c("parquet", "rds"), quiet = TRUE),
                 "rectangular")
  expect_equal(log$status, c("error", "written"))
  expect_true(is.na(log$path[1]))
  expect_match(log$message[1], 'to = "rds"')
  expect_s3_class(readRDS(log$path[2])$fit, "lm")
})

test_that("a mixed directory converts the tables and reports the rest", {
  skip_if_no("qs2", "arrow")
  d <- new_dir()
  qs2::qs_save(small_df(), file.path(d, "d6_agg.qs2"))
  qs2::qs_save(list(fit = lm(mpg ~ wt, mtcars)), file.path(d, "m1_fit.qs2"))

  expect_warning(log <- nt_qs_convert(d, quiet = TRUE), "rectangular")

  expect_equal(nrow(log), 2L)
  expect_setequal(log$status, c("written", "error"))
  expect_true(file.exists(file.path(d, "d6_agg.parquet")))          # batch not aborted
})

test_that("sf goes to GeoParquet with its CRS, and to spatial-only formats", {
  skip_if_no("qs2", "sf", "sfarrow")
  pts <- sf::st_as_sf(data.frame(GEOID = c("a", "b"), v = 1:2,
                                 x = c(-122.3, -122.4), y = c(37.8, 37.9)),
                      coords = c("x", "y"), crs = 4326)
  f <- qs_fixture(pts, "o2_places.qs2")

  log <- suppressWarnings(nt_qs_convert(f, to = c("parquet", "geojson"), quiet = TRUE))
  expect_true(all(log$status == "written"))

  back <- suppressWarnings(sfarrow::st_read_parquet(log$path[log$format == "parquet"]))
  expect_s3_class(back, "sf")
  expect_equal(sf::st_crs(back)$epsg, 4326L)
  expect_equal(nrow(back), 2L)
})

test_that("a format that cannot carry geometry warns and writes the attributes", {
  skip_if_no("qs2", "sf")
  pts <- sf::st_as_sf(data.frame(v = 1:2, x = c(-122.3, -122.4), y = c(37.8, 37.9)),
                      coords = c("x", "y"), crs = 4326)
  f <- qs_fixture(pts, "o2_places.qs2")

  expect_warning(log <- nt_qs_convert(f, to = "csv", quiet = TRUE), "cannot hold geometry")
  expect_equal(log$status, "written")
  expect_equal(names(utils::read.csv(log$path)), "v")
})

test_that("a non-spatial object cannot go to a spatial-only format", {
  skip_if_no("qs2", "sf")
  f <- qs_fixture(small_df(), "d6_agg.qs2")
  expect_warning(log <- nt_qs_convert(f, to = "gpkg", quiet = TRUE), "needs an sf object")
  expect_equal(log$status, "error")
})

test_that("existing outputs are skipped unless overwrite = TRUE", {
  skip_if_no("qs2", "arrow")
  f <- qs_fixture(small_df(), "d6_agg.qs2")

  first <- nt_qs_convert(f, quiet = TRUE)
  expect_equal(first$status, "written")

  again <- nt_qs_convert(f, quiet = TRUE)
  expect_equal(again$status, "skipped")
  expect_match(again$message, "overwrite = TRUE")

  forced <- nt_qs_convert(f, overwrite = TRUE, quiet = TRUE)
  expect_equal(forced$status, "written")
})

test_that("out_dir redirects the outputs and is created when absent", {
  skip_if_no("qs2", "arrow")
  f  <- qs_fixture(small_df(), "d6_agg.qs2")
  od <- file.path(new_dir(), "converted")

  log <- nt_qs_convert(f, out_dir = od, quiet = TRUE)

  expect_true(dir.exists(od))
  expect_equal(normalizePath(dirname(log$path)), normalizePath(od))
  expect_false(file.exists(file.path(dirname(f), "d6_agg.parquet")))
})

test_that("writer_args reach the format's writer", {
  skip_if_no("qs2", "arrow")
  f <- qs_fixture(data.frame(a = c(1, NA)), "d6_agg.qs2")

  log <- nt_qs_convert(f, to = "csv", writer_args = list(csv = list(na = "")),
                       quiet = TRUE)
  expect_equal(log$status, "written")
  expect_equal(readLines(log$path), c('"a"', "1", ""))   # the NA wrote as empty

  expect_error(nt_qs_convert(f, writer_args = list(list(compression = "zstd"))),
               "named list")
  expect_error(nt_qs_convert(f, writer_args = list(parqet = list())),
               "Unsupported output format")
})

test_that("directory search is non-recursive by default", {
  skip_if_no("qs2", "arrow")
  d <- new_dir()
  dir.create(file.path(d, "sub"))
  qs2::qs_save(small_df(), file.path(d, "sub", "d6_agg.qs2"))

  expect_error(nt_qs_convert(d, quiet = TRUE), "No .qs/.qs2 files found")
  expect_equal(nrow(nt_qs_convert(d, recursive = TRUE, quiet = TRUE)), 1L)
})

test_that("a legacy .qs round-trips through parquet", {
  skip_if_no("qs", "arrow")                 # qs is archived on CRAN; see ?nt_qs_convert
  d  <- new_dir()
  f  <- file.path(d, "d6_wa_agg_202512.qs")
  df <- small_df()
  qs::qsave(df, f)
  expect_equal(readBin(f, "raw", 4), as.raw(c(0x0b, 0x0e, 0x0a, 0x0c)))   # legacy magic

  log <- nt_qs_convert(f, quiet = TRUE)

  expect_equal(log$status, "written")
  expect_identical(as.data.frame(arrow::read_parquet(log$path)), df)
})

test_that("the reader falls back when the extension misdescribes the format", {
  skip_if_no("qs", "qs2", "arrow")
  d  <- new_dir()
  df <- small_df()
  qs2::qs_save(df, file.path(d, "a.qs"))    # qs2 content wearing a .qs name
  qs::qsave(df,    file.path(d, "b.qs2"))   # legacy content wearing a .qs2 name

  log <- nt_qs_convert(c(file.path(d, "a.qs"), file.path(d, "b.qs2")), quiet = TRUE)

  expect_equal(log$status, c("written", "written"))
  expect_identical(as.data.frame(arrow::read_parquet(log$path[1])), df)
  expect_identical(as.data.frame(arrow::read_parquet(log$path[2])), df)
})

test_that("a missing path errors before anything is read", {
  expect_error(nt_qs_convert(file.path(tempdir(), "nope.qs")), "No such file")
  expect_error(nt_qs_convert(character(0)), "one or more file")
})

test_that("an unreadable file warns and lands in the log as an error", {
  d <- new_dir()
  f <- file.path(d, "d6_broken.qs2")
  writeLines("not a qs file", f)

  expect_warning(log <- nt_qs_convert(f, to = c("parquet", "rds"), quiet = TRUE),
                 "could not read")
  expect_equal(nrow(log), 2L)                       # one row per requested format
  expect_true(all(log$status == "error"))
  expect_match(log$message[1], "could not read")
})
