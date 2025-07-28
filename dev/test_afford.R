librarian::shelf(tidycensus, dplyr, stats, scales, stringr, tigris, sf)
options(width = Sys.getenv("COLUMNS", unset = 120))


source("R/afford.R")

atlantic_co <- afford("NJ", "Atlantic", .8, year = 2023)

atlantic_co %>% glimpse()
atlantic_co %>% summary()

count(atlantic_co, rent_jenks_cat)

