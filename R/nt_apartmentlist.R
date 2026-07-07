# ==========================================================================
# nt_apartmentlist_rents() — Apartment List rent estimates, latest vintage
# ==========================================================================

#' Apartment List rent estimates, always the latest monthly release
#'
#' @description
#' Reads the newest `Apartment_List_Rent_Estimates_YYYY_MM.csv` in `dir` —
#' and, unless told otherwise, first checks
#' \url{https://www.apartmentlist.com/research/category/data-rent-estimates}
#' for a newer monthly release and downloads it. Apartment List links each
#' release from a CDN; the link is read out of the page source, so no API
#' key or scraping framework is needed. If the site is unreachable, the
#' newest local file is used with a message.
#'
#' The file is the full national release (county, metro, city rows; monthly
#' columns back to 2017) — filter to the rows you need.
#'
#' @param dir Folder holding the CSVs (created if missing). Default
#'   `"~/data/apartment_list"`.
#' @param refresh `"auto"` (default: download only when the site has a newer
#'   vintage than the newest local file), `"never"` (offline: newest local
#'   file only), or `"force"` (re-download the current release).
#' @param quiet Logical; suppress progress messages.
#' @return A data frame of the release, with the vintage string
#'   (`"YYYY_MM"`) in `attr(, "vintage")`.
#' @examplesIf interactive()
#' al <- nt_apartmentlist_rents()
#' attr(al, "vintage")
#' @export
nt_apartmentlist_rents <- function(dir = "~/data/apartment_list",
                                   refresh = c("auto", "never", "force"),
                                   quiet = FALSE) {
  refresh <- match.arg(refresh)
  dir <- path.expand(dir)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  pat <- "^Apartment_List_Rent_Estimates_([0-9]{4}_[0-9]{2})\\.csv$"
  local_vintage <- function() {
    f <- list.files(dir, pattern = pat)
    if (length(f)) max(sub(pat, "\\1", f)) else ""
  }

  if (refresh != "never") {
    remote <- tryCatch({
      page <- paste(readLines(
        "https://www.apartmentlist.com/research/category/data-rent-estimates",
        warn = FALSE), collapse = "\n")
      hits <- regmatches(page, gregexpr(
        "//[a-z0-9.]*ctfassets\\.net/[^\"]*Apartment_List_Rent_Estimates_[0-9]{4}_[0-9]{2}\\.csv",
        page))[[1]]
      hits <- hits[!grepl("Summary", hits)]
      if (!length(hits)) NULL else list(
        url     = paste0("https:", hits[[1]]),
        vintage = sub(".*Estimates_([0-9]{4}_[0-9]{2})\\.csv$", "\\1", hits[[1]])
      )
    }, error = function(e) NULL, warning = function(w) NULL)

    if (is.null(remote)) {
      if (!quiet) message("apartmentlist.com not reachable - using the newest local file")
    } else if (refresh == "force" || remote$vintage > local_vintage()) {
      dest <- file.path(dir, sprintf("Apartment_List_Rent_Estimates_%s.csv",
                                     remote$vintage))
      if (!quiet) message("downloading Apartment List rent estimates ",
                          remote$vintage)
      utils::download.file(remote$url, dest, mode = "wb", quiet = TRUE)
    } else if (!quiet) {
      message("Apartment List data is current (", local_vintage(), ")")
    }
  }

  v <- local_vintage()
  stopifnot("no Apartment List file found (folder empty and site unreachable)" = nzchar(v))
  out <- utils::read.csv(
    file.path(dir, sprintf("Apartment_List_Rent_Estimates_%s.csv", v)),
    check.names = FALSE, stringsAsFactors = FALSE,
    # FIPS codes must stay character: leading zeros are data ("0660102")
    colClasses = c(location_fips_code = "character"))
  out <- dplyr::as_tibble(out)
  attr(out, "vintage") <- v
  out
}
