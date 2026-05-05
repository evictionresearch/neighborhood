# Rebuild bundled datasets as .rda for proper LazyData loading.
#
# Background
# ----------
# Releases prior to 1.0.6 shipped .csv.bz2 (and parallel .rds) in data/.
# R's lazy-data loader auto-reads .csv.bz2 via read.table(), which mishandled
# embedded commas inside quoted NAME values and broke install_github (#10).
# .rds is not a recognized lazy-data format, so it was silently ignored.
# .rda is the canonical format and is what we ship from 1.0.6 onward.
#
# Source files
# ------------
# The raw source files (full-US ntdf() output for each ACS endpoint year) are
# NOT in this repo — they're large and live in the evictionresearch shared
# storage. To rebuild, point SRC_DIR at a directory that contains:
#   us_nt_tracts2019.rds
#   us_nt_tracts2022.rds
#   us_nt_tracts21.csv.bz2   # 2021 has only csv; nt_conc factor is reconstructed
#
# Schema
# ------
# As of 1.0.6 all three datasets share the same 16 columns in the same order:
#   GEOID, NAME, totraceE, WhiteE, BlackE, AsianE, LatineE,
#   pWhite, pAsian, pBlack, pLatine, pOther,
#   NeighType, nt_conc, state, year
# The 2021 source still uses the older `LatinxE`/`pLatinx` naming and carries
# extra county/state columns; both are normalized below.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

SRC_DIR <- Sys.getenv("NEIGHBORHOOD_SRC_DIR", unset = "data-raw/source")

nt_levels <- c(
  "Mostly Asian", "Mostly Black", "Mostly Latine", "Mostly Other", "Mostly White",
  "Asian-Black", "Asian-Latine", "Asian-Other", "Asian-White",
  "Black-Latine", "Black-Other", "Black-White",
  "Latine-Other", "Latine-White",
  "Other-White",
  "3 Group Mixed", "4 Group Mixed", "Diverse",
  "Unpopulated Tract"
)

target_cols <- c(
  "GEOID", "NAME", "totraceE", "WhiteE", "BlackE", "AsianE", "LatineE",
  "pWhite", "pAsian", "pBlack", "pLatine", "pOther",
  "NeighType", "nt_conc", "state", "year"
)

us_nt_tracts2019 <- readRDS(file.path(SRC_DIR, "us_nt_tracts2019.rds"))[, target_cols]
us_nt_tracts2022 <- readRDS(file.path(SRC_DIR, "us_nt_tracts2022.rds"))[, target_cols]

us_nt_tracts2021 <- read_csv(
  file.path(SRC_DIR, "us_nt_tracts21.csv.bz2"),
  show_col_types = FALSE,
  progress = FALSE
) %>%
  rename(LatineE = LatinxE, pLatine = pLatinx) %>%
  mutate(NeighType = gsub("Latinx", "Latine", NeighType, fixed = TRUE)) %>%
  mutate(nt_conc = case_when(
    NeighType %in% c("Black-Asian-Latine-Other", "Asian-Latine-Other-White",
                     "Black-Latine-Other-White", "Black-Asian-Other-White",
                     "Black-Asian-Latine-White") ~ "4 Group Mixed",
    NeighType %in% c("Black-Asian-Latine", "Black-Asian-Other", "Black-Latine-Other",
                     "Asian-Latine-Other", "Black-Asian-White", "Black-Latine-White",
                     "Black-Other-White", "Asian-Latine-White", "Asian-Other-White",
                     "Latine-Other-White") ~ "3 Group Mixed",
    NeighType %in% c("All White", "White-Shared")   ~ "Mostly White",
    NeighType %in% c("All Black", "Black-Shared")   ~ "Mostly Black",
    NeighType %in% c("All Asian", "Asian-Shared")   ~ "Mostly Asian",
    NeighType %in% c("All Latine", "Latine-Shared") ~ "Mostly Latine",
    NeighType %in% c("All Other", "Other-Shared")   ~ "Mostly Other",
    TRUE ~ NeighType
  )) %>%
  mutate(nt_conc = factor(nt_conc, levels = nt_levels)) %>%
  select(all_of(target_cols))

usethis::use_data(us_nt_tracts2019, overwrite = TRUE, compress = "xz")
usethis::use_data(us_nt_tracts2022, overwrite = TRUE, compress = "xz")
usethis::use_data(us_nt_tracts2021, overwrite = TRUE, compress = "xz")
