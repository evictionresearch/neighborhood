# Rebuild a full-US us_nt_tracts<year>.rda dataset by running ntdf() over
# every state/territory. Used for years where no upstream source CSV/RDS
# exists (e.g. 2021 refresh, 2023, 2024).
#
# Usage:
#   Rscript data-raw/build_full_us.R 2024
#
# Requires CENSUS_API_KEY in the environment.

suppressPackageStartupMessages({
  library(neighborhood)
  library(dplyr)
  library(purrr)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: build_full_us.R <year>")
yr <- as.integer(args[[1]])

states <- c(
  "AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID","IL",
  "IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC","ND","NE",
  "NH","NJ","NM","NV","NY","OH","OK","OR","PA","PR","RI","SC","SD","TN","TX",
  "UT","VA","VT","WA","WI","WV","WY"
)

target_cols <- c(
  "GEOID", "NAME", "totraceE", "WhiteE", "BlackE", "AsianE", "LatineE",
  "pWhite", "pAsian", "pBlack", "pLatine", "pOther",
  "NeighType", "nt_conc", "state", "year"
)

build_state <- function(s) {
  message(sprintf("[%s] %s ...", format(Sys.time(), "%H:%M:%S"), s))
  out <- tryCatch(
    ntdf(state = s, year = yr) %>%
      mutate(state = s, year = yr),
    error = function(e) {
      message(sprintf("  ERROR for %s: %s", s, conditionMessage(e)))
      NULL
    }
  )
  if (is.null(out)) return(NULL)
  out[, target_cols]
}

parts <- map(states, build_state)
ok <- !sapply(parts, is.null)
message(sprintf("Built %d / %d states", sum(ok), length(states)))
if (any(!ok)) message("Failed: ", paste(states[!ok], collapse = ", "))

df <- bind_rows(parts[ok])
message(sprintf("Total rows: %d", nrow(df)))
message(sprintf("Distinct states: %d", length(unique(df$state))))
message(sprintf("NA nt_conc: %d", sum(is.na(df$nt_conc))))

assign(paste0("us_nt_tracts", yr), df)
do.call(usethis::use_data,
        list(as.name(paste0("us_nt_tracts", yr)),
             overwrite = TRUE, compress = "xz"))
message("Saved.")
