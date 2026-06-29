# Precompute afford_index() results for the QA review page (dev/afford_qa.Rmd).
# Decouples the (slow, API-bound) data pulls from rendering: run this once, then
# knit the Rmd off the cached dev/afford_qa_data.rds.
#
#   Rscript dev/afford_qa_data.R
#
# Uses ami_source = "hud" off the bundled snapshot (inst/extdata/hud_il_2024.rds)
# for the QA counties, so it is offline/exact except for FMR pulls in the
# ami_source comparison (which retry).

suppressPackageStartupMessages({
  library(dplyr); library(tidycensus); library(sf)
})
pkgload::load_all(".", quiet = TRUE)
options(tigris_use_cache = TRUE, tigris_progress = FALSE)
YEAR <- 2024

regions <- list(
  bay      = list(state = "06", counties = c("075","001","081","085"),
                  label = "Bay Area (SF, Alameda, San Mateo, Santa Clara)"),
  king     = list(state = "53", counties = "033",
                  label = "King County, WA (Seattle)"),
  hennepin = list(state = "27", counties = "053",
                  label = "Hennepin County, MN (Minneapolis)"),
  atlanta  = list(state = "13", counties = c("121","089","067","135"),
                  label = "Atlanta metro (Fulton, DeKalb, Cobb, Gwinnett)")
)

retry <- function(expr, n = 4) {
  for (i in seq_len(n)) {
    r <- tryCatch(force(expr), error = function(e) NULL)
    if (!is.null(r)) return(r)
    Sys.sleep(3)
  }
  NULL
}
afi <- function(reg, ...) retry(suppressMessages(
  afford_index(reg$state, reg$counties, YEAR, geometry = TRUE, ...)))

# -- 1. Baseline per region: hud limits, matched demand, availability on --------
message("Baseline maps per region (hud / matched / availability)...")
baseline <- lapply(names(regions), function(k) {
  message("  ", k); afi(regions[[k]], ami_source = "hud")
})
names(baseline) <- names(regions)

# -- 2. Parameter variants, all on King County (clean single county) ------------
message("King County parameter variants...")
king <- regions$king
king_demand_all   <- afi(king, ami_source = "hud", demand = "all")
king_demand_owner <- afi(king, ami_source = "hud", demand = "owner")
king_acs          <- afi(king, ami_source = "acs")                 # vs hud (baseline)
king_buyin_fs     <- afi(king, ami_source = "hud", tenure = "own_buyin",
                         buyin_stock = "for_sale")

# -- 3. AMI-source cutoffs comparison: one county per region x 4 sources --------
message("AMI-source cutoffs table...")
cut_targets <- list(
  c("Bay (SF)", "06", "075"), c("King WA", "53", "033"),
  c("Hennepin MN", "27", "053"), c("Atlanta (Fulton)", "13", "121")
)
sources <- c("acs", "acs_fmr", "hud_acs", "hud")
cut_rows <- list()
for (t in cut_targets) for (s in sources) {
  r <- retry(suppressMessages(ami_cutoffs(t[2], t[3], YEAR, ami_source = s)))
  cut_rows[[paste(t[1], s)]] <- if (is.null(r)) {
    data.frame(region = t[1], source = s, ami = NA, ELI = NA, VLI = NA, LI = NA)
  } else {
    data.frame(region = t[1], source = s, ami = r$ami,
               ELI = r$cut_ELI, VLI = r$cut_VLI, LI = r$cut_LI)
  }
}
cutoffs <- dplyr::bind_rows(cut_rows)

qa <- list(
  year = YEAR, regions = regions, baseline = baseline,
  king_demand_all = king_demand_all, king_demand_owner = king_demand_owner,
  king_acs = king_acs, king_buyin_fs = king_buyin_fs, cutoffs = cutoffs,
  built = format(Sys.time(), "%Y-%m-%d %H:%M")
)
saveRDS(qa, "dev/afford_qa_data.rds")
message("Saved dev/afford_qa_data.rds")

# quick integrity echo
ok <- vapply(baseline, function(x) !is.null(x) && inherits(x, "sf"), logical(1))
message("Baseline regions OK: ", paste(names(ok)[ok], collapse = ", "),
        if (any(!ok)) paste0("  FAILED: ", paste(names(ok)[!ok], collapse = ", ")) else "")
message("Variants OK: ",
        paste(c("demand_all","demand_owner","acs","buyin_fs")[c(
          !is.null(king_demand_all), !is.null(king_demand_owner),
          !is.null(king_acs), !is.null(king_buyin_fs))], collapse = ", "))
message("Cutoffs rows: ", nrow(cutoffs), " (",
        sum(!is.na(cutoffs$VLI)), " resolved)")
