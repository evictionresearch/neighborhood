#' Build HTML popup content for a map
#'
#' @description
#' Assembles a character vector of HTML — one entry per row of `data` — suitable
#' for use as the `popup` (or `tooltip`) argument of [nt_add_choropleth()] or any
#' `mapgl` layer function. MapLibre renders whatever HTML the popup column
#' contains, so this is the seam where a map grows from a plain label into the
#' rich, multi-panel popups used in the HPRM and state-profile maps.
#'
#' The output is fully **deterministic**: the same data in produces byte-for-byte
#' identical HTML out (no timestamps, no random ids), so popups are reproducible
#' and testable.
#'
#' @details
#' Start simple and graft upward (this is exactly the arc the *Mapping with
#' MapLibre* vignette follows):
#'
#' 1. **A title and a few fields.** Pass `fields` as a named vector mapping the
#'    label you want shown to the column it comes from. Numeric columns are
#'    formatted automatically — values that all fall in `[0, 1]` are shown as
#'    percentages, other numerics get thousands separators.
#' 2. **Custom HTML.** Pass `html` (a length-1 or length-`nrow(data)` character
#'    vector) to append arbitrary markup after the fields — inline `<div>` bar
#'    charts of racial composition, an SVG "bubble track" comparing a tract to
#'    its county and the nation, and so on. Because it is just a string, you
#'    build it in R with `paste0()`/`sprintf()` and stay deterministic.
#'
#' @param data A data frame or `sf` object. Geometry, if present, is ignored.
#' @param fields Optional named vector or list mapping **display label ->
#'   column name**, e.g. `c("County" = "county", "Median rent" = "med_rent")`.
#'   Fields render in the order given.
#' @param title Optional title shown bold at the top. If `title` is the name of
#'   a column in `data`, that column's per-row value is used (e.g.
#'   `title = "GEOID"` yields a different title per tract); otherwise `title` is
#'   treated as a literal string shared by every popup.
#' @param html Optional character vector (length 1 or `nrow(data)`) of extra
#'   HTML appended after the fields. Use it to graft on bar charts, SVG, links,
#'   etc.
#' @param prefix_title Logical; if `TRUE` (default) and `title` names a column,
#'   the title reads like `"Tract: <value>"`. Set `FALSE` to show the bare value.
#' @return A character vector of length `nrow(data)` containing one HTML string
#'   per feature.
#' @seealso [nt_add_choropleth()], which calls this to build a default popup;
#'   the *Mapping with MapLibre* vignette for the full HPRM-style recipe.
#' @examples
#' \dontrun{
#' md <- ntdf(state = "MD", county = "Baltimore City", geometry = TRUE)
#'
#' # Simple: a per-tract title plus two fields
#' md$popup <- nt_popup(
#'   md,
#'   title  = "GEOID",
#'   fields = c("Neighborhood type" = "nt_conc", "Total population" = "totraceE")
#' )
#'
#' # Rich-ready: graft a racial-composition bar chart on top
#' bars <- with(md, paste0(
#'   "<div style='margin-top:6px'>",
#'   "<div style='background:#1f78b4;height:8px;width:", round(pBlack * 100), "%'></div>",
#'   "<div style='background:#33a02c;height:8px;width:", round(pAsian * 100), "%'></div>",
#'   "</div>"
#' ))
#' md$popup <- nt_popup(md, title = "GEOID", fields = c("Type" = "nt_conc"), html = bars)
#'
#' nt_map(md) # picks up the `popup` column automatically
#' }
#' @export
nt_popup <- function(data, fields = NULL, title = NULL, html = NULL,
                     prefix_title = TRUE) {
  if (inherits(data, "sf")) data <- sf::st_drop_geometry(data)
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  n <- nrow(data)
  if (n == 0L) return(character(0))

  parts <- vector("list", 0L)

  # --- Title -----------------------------------------------------------------
  if (!is.null(title)) {
    if (length(title) == 1L && title %in% names(data)) {
      val <- .nt_fmt(data[[title]])
      lab <- if (isTRUE(prefix_title)) paste0(title, ": ") else ""
      parts[[length(parts) + 1L]] <- paste0("<b>", lab, val, "</b>")
    } else {
      parts[[length(parts) + 1L]] <- paste0("<b>", as.character(title[1]), "</b>")
    }
  }

  # --- Fields ----------------------------------------------------------------
  if (!is.null(fields)) {
    labels <- names(fields)
    cols   <- unname(unlist(fields))
    if (is.null(labels) || any(labels == "")) {
      stop("`fields` must be a named vector/list (label = column name).",
           call. = FALSE)
    }
    missing_cols <- setdiff(cols, names(data))
    if (length(missing_cols)) {
      stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "),
           call. = FALSE)
    }
    for (i in seq_along(cols)) {
      parts[[length(parts) + 1L]] <-
        paste0(labels[i], ": ", .nt_fmt(data[[cols[i]]]))
    }
  }

  # --- Custom HTML -----------------------------------------------------------
  if (!is.null(html)) {
    html <- as.character(html)
    if (length(html) == 1L) html <- rep(html, n)
    if (length(html) != n) {
      stop("`html` must be length 1 or nrow(data).", call. = FALSE)
    }
  }

  if (length(parts) == 0L && is.null(html)) {
    stop("Nothing to show: provide `title`, `fields`, or `html`.", call. = FALSE)
  }

  # Stitch the field/title parts with <br>, then append custom HTML.
  body <- if (length(parts)) {
    do.call(paste, c(parts, sep = "<br>"))
  } else {
    rep("", n)
  }
  if (!is.null(html)) {
    body <- ifelse(nzchar(body), paste0(body, "<br>", html), html)
  }
  body
}

# Format a column for display: percentages for [0,1] numerics, thousands
# separators for other numerics, plain character otherwise. Vectorized and
# deterministic.
.nt_fmt <- function(x) {
  if (is.numeric(x)) {
    finite <- x[is.finite(x)]
    if (length(finite) && all(finite >= 0 & finite <= 1)) {
      return(scales::percent(x, accuracy = 0.1))
    }
    acc <- if (length(finite) && all(finite == round(finite))) 1 else 0.01
    return(scales::comma(x, accuracy = acc))
  }
  as.character(x)
}
