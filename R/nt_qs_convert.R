# =============================================================================
# nt_qs_convert(): the migrate-on-touch tool for the ERN serialization rule.
#
# Parquet is the on-disk standard for analytic data -- columnar, compressed,
# and readable from Python / Julia / DuckDB -- so a published number's data file
# outlives the R session that wrote it. .qs/.qs2 is fast but R-only and tied to
# the writing package's version, which is why library/CODE_CONVENTIONS.md
# ("Serialization format -- Parquet, not qs") demotes it to legacy and asks
# pipelines to convert as they are touched, keeping qs/rds only for what Parquet
# cannot hold: fitted models (BART/INLA), nested lists.
#
# So this converts one file, a vector of files, or a whole directory, to one
# format or several in a single pass, and on an object Parquet cannot take it
# records the reason for that file instead of writing a broken table.
# =============================================================================

# ---- internal: drop geometry for formats that cannot carry it ---------------
.nt_conv_flat <- function(x, fmt) {
  if (!inherits(x, "sf")) return(x)
  warning("nt_qs_convert: '", fmt, "' cannot hold geometry; writing attributes ",
          "only. Use 'parquet' (GeoParquet), 'gpkg', or 'geojson' to keep it.",
          call. = FALSE)
  sf::st_drop_geometry(x)
}

.nt_conv_is_table <- function(x) {
  is.data.frame(x) || inherits(x, c("ArrowTabular", "Table", "RecordBatch"))
}

# ---- internal: output-format registry ---------------------------------------
# ext    output extension
# accept "table" (rectangular only), "sf" (spatial only), or "any" (serializers)
# needs  packages required, given the object (parquet wants sfarrow only for sf)
# write  (object, path, extra args) -> writes the file
.nt_conv_formats <- list(
  parquet = list(
    ext = "parquet", accept = "table",
    needs = function(x) if (inherits(x, "sf")) "sfarrow" else "arrow",
    write = function(x, path, a)
      if (inherits(x, "sf")) do.call(sfarrow::st_write_parquet, c(list(x, path), a))
      else                   do.call(arrow::write_parquet, c(list(x, path), a))
  ),
  feather = list(
    ext = "arrow", accept = "table", needs = function(x) "arrow",
    write = function(x, path, a)
      do.call(arrow::write_feather, c(list(.nt_conv_flat(x, "feather"), path), a))
  ),
  fst = list(
    ext = "fst", accept = "table", needs = function(x) "fst",
    write = function(x, path, a)
      do.call(fst::write_fst, c(list(as.data.frame(.nt_conv_flat(x, "fst")), path), a))
  ),
  csv = list(
    ext = "csv", accept = "table", needs = function(x) character(),
    write = function(x, path, a)
      do.call(utils::write.csv,
              c(list(as.data.frame(.nt_conv_flat(x, "csv")), path, row.names = FALSE), a))
  ),
  tsv = list(
    ext = "tsv", accept = "table", needs = function(x) character(),
    write = function(x, path, a)
      do.call(utils::write.table,
              c(list(as.data.frame(.nt_conv_flat(x, "tsv")), path,
                     sep = "\t", row.names = FALSE, quote = FALSE), a))
  ),
  xlsx = list(
    ext = "xlsx", accept = "table", needs = function(x) "writexl",
    write = function(x, path, a)
      do.call(writexl::write_xlsx,
              c(list(as.data.frame(.nt_conv_flat(x, "xlsx")), path), a))
  ),
  dta = list(
    ext = "dta", accept = "table", needs = function(x) "haven",
    write = function(x, path, a)
      do.call(haven::write_dta, c(list(as.data.frame(.nt_conv_flat(x, "dta")), path), a))
  ),
  sav = list(
    ext = "sav", accept = "table", needs = function(x) "haven",
    write = function(x, path, a)
      do.call(haven::write_sav, c(list(as.data.frame(.nt_conv_flat(x, "sav")), path), a))
  ),
  json = list(
    ext = "json", accept = "table", needs = function(x) "jsonlite",
    write = function(x, path, a)
      do.call(jsonlite::write_json,
              c(list(as.data.frame(.nt_conv_flat(x, "json")), path), a))
  ),
  geojson = list(
    ext = "geojson", accept = "sf", needs = function(x) character(),
    write = function(x, path, a)
      do.call(sf::st_write,
              c(list(x, path, delete_dsn = TRUE, quiet = TRUE), a))
  ),
  gpkg = list(
    ext = "gpkg", accept = "sf", needs = function(x) character(),
    write = function(x, path, a)
      do.call(sf::st_write,
              c(list(x, path, delete_dsn = TRUE, quiet = TRUE), a))
  ),
  shp = list(
    ext = "shp", accept = "sf", needs = function(x) character(),
    write = function(x, path, a)
      do.call(sf::st_write,
              c(list(x, path, delete_dsn = TRUE, quiet = TRUE), a))
  ),
  rds = list(
    ext = "rds", accept = "any", needs = function(x) character(),
    write = function(x, path, a) do.call(saveRDS, c(list(x, path), a))
  ),
  qs2 = list(
    ext = "qs2", accept = "any", needs = function(x) "qs2",
    write = function(x, path, a) do.call(qs2::qs_save, c(list(x, path), a))
  )
)

# Spellings that name the same writer. Deliberately absent: "qs" and "rda",
# which name formats this does not write -- asking for one should say so rather
# than quietly hand back a .qs2 or .rds.
.nt_conv_alias <- c(arrow = "feather", ipc = "feather", geoparquet = "parquet",
                    pqt = "parquet", excel = "xlsx", stata = "dta",
                    spss = "sav", shapefile = "shp")

# ---- internal: normalize `to` to registry keys ------------------------------
.nt_conv_resolve <- function(to) {
  if (!is.character(to) || !length(to))
    stop("`to` must be a character vector of output formats.", call. = FALSE)
  key <- tolower(trimws(sub("^\\.", "", to)))
  key <- ifelse(key %in% names(.nt_conv_alias), .nt_conv_alias[key], key)
  bad <- setdiff(key, names(.nt_conv_formats))
  if (length(bad))
    stop("Unsupported output format", if (length(bad) > 1) "s" else "", ": ",
         paste(sQuote(bad), collapse = ", "), ".\n  Supported: ",
         paste(names(.nt_conv_formats), collapse = ", "), ".", call. = FALSE)
  unique(unname(key))
}

# ---- internal: read one .qs / .qs2 file -------------------------------------
# qs2 refuses a legacy file with "qs-legacy format detected, use qs::qread", so
# dispatch on the extension and fall back to the other reader on a mismatch.
.nt_qs_read <- function(path) {
  ext <- tolower(tools::file_ext(path))
  order <- switch(ext,
    qs2 = list(c("qs2", "qs_read"), c("qs", "qread"), c("qs2", "qd_read")),
    qs  = list(c("qs", "qread"), c("qs2", "qs_read"), c("qs2", "qd_read")),
          list(c("qs2", "qs_read"), c("qs2", "qd_read"), c("qs", "qread")))
  notes <- character()
  for (r in order) {
    if (!requireNamespace(r[1], quietly = TRUE)) {
      notes <- c(notes, sprintf("%s is not installed", r[1]))
      next
    }
    out <- tryCatch(getExportedValue(r[1], r[2])(path),
                    error = function(e) structure(conditionMessage(e), class = ".ntfail"))
    if (!inherits(out, ".ntfail")) return(out)
    notes <- c(notes, sprintf("%s::%s(): %s", r[1], r[2], as.character(out)))
  }
  stop("could not read ", basename(path), ".\n  ", paste(notes, collapse = "\n  "),
       "\n  Legacy .qs files need the 'qs' package (install.packages('qs')); ",
       ".qs2 needs 'qs2'.", call. = FALSE)
}

# ---- internal: expand paths/directories to a vector of qs files -------------
.nt_conv_inputs <- function(path, recursive) {
  if (!is.character(path) || !length(path))
    stop("`path` must be one or more file or directory paths.", call. = FALSE)
  path <- path.expand(path)
  miss <- path[!file.exists(path)]
  if (length(miss))
    stop("No such file or directory: ", paste(miss, collapse = ", "), call. = FALSE)
  files <- unlist(lapply(path, function(p) {
    if (!dir.exists(p)) return(p)
    list.files(p, pattern = "\\.(qs|qs2|qd)$", ignore.case = TRUE,
               full.names = TRUE, recursive = recursive)
  }), use.names = FALSE)
  files <- unique(files)
  if (!length(files))
    stop("No .qs/.qs2 files found under ", paste(path, collapse = ", "),
         if (!recursive) " (`recursive = TRUE` searches subdirectories)." else ".",
         call. = FALSE)
  files
}

#' Convert `.qs`/`.qs2` files to Parquet and other formats
#'
#' @description
#' Rewrites legacy `.qs` / `.qs2` files into one or more durable formats,
#' Parquet by default. Point it at a file, several files, or a directory, name
#' the formats you want, and each source file is read once and written out to
#' every requested format beside it.
#'
#' This is the migrate-on-touch tool for the ERN serialization convention:
#' Parquet is the standard on-disk format for analytic data because it is
#' columnar, compressed, and language-agnostic (Python, Julia, and DuckDB read
#' it natively), so a published number's data file outlives the R session that
#' wrote it. `.qs`/`.qs2` is fast but R-only and pinned to the writing package's
#' version. Filenames are preserved apart from the extension, so a `d5_*` output
#' keeps its producing-script prefix.
#'
#' @details
#' **What Parquet cannot hold.** Fitted models (BART, INLA), nested lists, and
#' other non-rectangular objects have no Parquet representation. Asking for a
#' tabular format on one of those records an error for that file and moves on —
#' the convention is to keep those as `qs2`/`rds`, so `to = "rds"` converts them
#' without leaving the family. A mixed directory therefore converts in one call:
#' the tables land as Parquet, the model fits report why they did not.
#'
#' **Spatial objects.** An `sf` object written to `"parquet"` becomes GeoParquet
#' via `sfarrow`, keeping its geometry and CRS. `"geojson"`, `"gpkg"`, and
#' `"shp"` are spatial-only. Formats that cannot carry geometry (`"csv"`,
#' `"fst"`, `"xlsx"`, …) write the attribute table and warn that they dropped it.
#'
#' **Reading.** `.qs2` is read with `qs2::qs_read()` and legacy `.qs` with
#' `qs::qread()`; `qs2` detects a legacy file and refuses it, so the reader falls
#' back to the other package when an extension turns out to be misleading.
#' Reading a legacy `.qs` therefore needs the `qs` package installed, which is a
#' separate package from `qs2`.
#'
#' **And `qs` is getting hard to install, which is the whole argument for
#' converting.** As of September 2026 `qs` is archived on CRAN, and its last
#' version (0.27.3, which is also the GitHub head) does not compile against
#' R 4.6: it calls `SET_FRAME()`, which R 4.5 still declared as "used by mmap
#' qs" and R 4.6 dropped from the API. A binary built under R 4.5 crashes the
#' session if loaded under R 4.6. So a `.qs` file is readable only on an R old
#' enough to still build its reader, and that window is closing. Convert on the
#' oldest R that can still open the file, not later.
#'
#' **Failures do not stop the batch.** A file that cannot be read or written
#' raises a warning and lands in the returned frame with `status = "error"` and
#' the reason, so one unconvertible model fit does not abort a directory of
#' tables. Check `status` rather than assuming every row was written.
#'
#' @param path One or more paths: `.qs`/`.qs2` files, directories, or a mix.
#'   A directory contributes the `.qs`/`.qs2` files it contains (see
#'   `recursive`).
#' @param to Character vector of output formats, in any number. Default
#'   `"parquet"`. Rectangular: `"parquet"` (GeoParquet for `sf`), `"feather"`
#'   (Arrow IPC, alias `"arrow"`), `"fst"`, `"csv"`, `"tsv"`, `"xlsx"`, `"dta"`,
#'   `"sav"`, `"json"`. Spatial only: `"geojson"`, `"gpkg"`, `"shp"`. Any object:
#'   `"rds"`, `"qs2"`. Leading dots and case are ignored, so `".Parquet"` works.
#' @param out_dir Directory for the outputs. `NULL` (default) writes each file
#'   beside its source, which is what an in-place pipeline migration wants. A
#'   path is created if it does not exist, and — because the source tree is
#'   flattened into it — converting several directories at once can collide on a
#'   shared basename; convert one directory per call, or leave this `NULL`.
#' @param recursive Search directories in `path` recursively. Default `FALSE`.
#' @param overwrite Overwrite an existing output file. Default `FALSE`, which
#'   leaves it alone and reports `status = "skipped"`.
#' @param writer_args Named list of extra arguments per format, for example
#'   `list(parquet = list(compression = "zstd"), csv = list(na = ""))`. Each
#'   element is passed to that format's writer
#'   ([arrow::write_parquet()], [utils::write.csv()], and so on).
#' @param quiet Suppress the per-file progress messages. Default `FALSE`.
#'
#' @return A data frame (invisibly), one row per source-file × format, with
#'   `source`, `format`, `path` (the output, `NA` when nothing was written),
#'   `bytes`, `status` (`"written"`, `"skipped"`, or `"error"`), and `message`.
#'   Returned visibly enough to inspect: assign it and check `status`.
#'
#' @seealso [arrow::write_parquet()] and [sfarrow::st_write_parquet()] for the
#'   writers behind the default, and `vignette("geography-utilities")`.
#' @examples \dontrun{
#' # The convention's one-liner, as a function: one file to Parquet beside it
#' nt_qs_convert("d5_dem_est_202512.qs")
#'
#' # Several formats in one read — Parquet for the pipeline, csv for a collaborator
#' nt_qs_convert("d6_tract_agg_202512.qs2", to = c("parquet", "csv"))
#'
#' # A whole tree, with the archival compression Parquet supports
#' log <- nt_qs_convert(
#'   "data/",
#'   recursive = TRUE,
#'   writer_args = list(parquet = list(compression = "zstd"))
#' )
#' subset(log, status != "written")   # model fits and other non-tables
#'
#' # Objects Parquet cannot hold stay in the qs/rds family
#' nt_qs_convert("bartcv_eer_v13_2024-04-09.qs", to = "rds")
#' }
#' @export
nt_qs_convert <- function(path,
                          to = "parquet",
                          out_dir = NULL,
                          recursive = FALSE,
                          overwrite = FALSE,
                          writer_args = list(),
                          quiet = FALSE) {

  fmts  <- .nt_conv_resolve(to)
  files <- .nt_conv_inputs(path, recursive)

  if (length(writer_args)) {
    if (is.null(names(writer_args)) || any(!nzchar(names(writer_args))))
      stop("`writer_args` must be a named list, one element per format.", call. = FALSE)
    names(writer_args) <- .nt_conv_resolve(names(writer_args))
  }
  if (!is.null(out_dir)) {
    out_dir <- path.expand(out_dir)
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  }

  row <- function(src, fmt, out, status, msg)
    data.frame(source = src, format = fmt, path = out,
               bytes = if (is.na(out)) NA_real_ else unname(file.size(out)),
               status = status, message = msg, stringsAsFactors = FALSE)

  out <- list()
  for (f in files) {
    obj <- tryCatch(.nt_qs_read(f),
                    error = function(e) structure(conditionMessage(e), class = ".ntfail"))
    if (inherits(obj, ".ntfail")) {
      warning(as.character(obj), call. = FALSE)
      out[[length(out) + 1L]] <-
        row(f, fmts, NA_character_, "error", as.character(obj))
      next
    }

    for (fmt in fmts) {
      spec <- .nt_conv_formats[[fmt]]
      dest <- file.path(if (is.null(out_dir)) dirname(f) else out_dir,
                        paste0(tools::file_path_sans_ext(basename(f)), ".", spec$ext))

      fail <- NULL
      if (spec$accept == "table" && !.nt_conv_is_table(obj))
        fail <- sprintf(
          "'%s' needs a rectangular object; %s holds <%s>. No tabular format holds a model fit or a nested list -- use to = \"rds\" or \"qs2\".",
          fmt, basename(f), paste(class(obj), collapse = "/"))
      else if (spec$accept == "sf" && !inherits(obj, "sf"))
        fail <- sprintf("'%s' needs an sf object; %s holds <%s>.",
                        fmt, basename(f), paste(class(obj), collapse = "/"))
      if (!is.null(fail)) {
        warning("nt_qs_convert: ", fail, call. = FALSE)
        out[[length(out) + 1L]] <- row(f, fmt, NA_character_, "error", fail)
        next
      }

      need <- setdiff(spec$needs(obj), "")
      miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
      if (length(miss)) {
        fail <- sprintf("'%s' needs the %s package (install.packages(\"%s\")).",
                        fmt, paste(sQuote(miss), collapse = " and "), miss[1])
        warning("nt_qs_convert: ", fail, call. = FALSE)
        out[[length(out) + 1L]] <- row(f, fmt, NA_character_, "error", fail)
        next
      }

      if (file.exists(dest) && !overwrite) {
        out[[length(out) + 1L]] <-
          row(f, fmt, dest, "skipped", "exists; pass overwrite = TRUE to replace")
        next
      }

      err <- tryCatch({
        spec$write(obj, dest, writer_args[[fmt]])
        NULL
      }, error = function(e) conditionMessage(e))

      if (!is.null(err) || !file.exists(dest)) {
        msg <- if (is.null(err)) "writer produced no file" else err
        warning("nt_qs_convert: ", basename(f), " -> ", fmt, ": ", msg, call. = FALSE)
        out[[length(out) + 1L]] <- row(f, fmt, NA_character_, "error", msg)
        next
      }

      if (!quiet)
        message(sprintf("nt_qs_convert: %s -> %s (%s)", basename(f), basename(dest),
                        format(structure(file.size(dest), class = "object_size"),
                               units = "auto")))
      out[[length(out) + 1L]] <- row(f, fmt, dest, "written", NA_character_)
    }
  }

  invisible(do.call(rbind, out))
}
