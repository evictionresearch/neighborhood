# ==========================================================================
# Create neighborhood typology for entire country
# ==========================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("timathomas/neighborhood")
pacman::p_load(sf, tigris, tidycensus, tidyverse)
options(tigris_use_cache = TRUE)

# ==========================================================================
# Create and save data
# ==========================================================================

us_states <- 
  states() %>% 
  st_set_geometry(NULL) %>% 
  filter(!STUSPS %in% c("AS", "GU", "MP", "VI")) %>% 
  pull(STUSPS) %>% 
  sort()

us_tracts <-
  map_df(us_states, function(states){
      ntdf(state = states, year = 2019) %>% 
      mutate(state = states)
    })

saveRDS(us_tracts, "~/git/neighborhood/data/us_nt_tracts.rds")