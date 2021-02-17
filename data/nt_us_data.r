# ==========================================================================
# Create neighborhood typology for entire country
# ==========================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("timathomas/neighborhood")
pacman::p_load(sf, tigris, tidyverse)

# ==========================================================================
# Create data
# ==========================================================================

us_states <- 
	states() %>% 
	st_set_geometry(NULL) %>% 
	pull(STATEFP)

us_tracts <- 
	ntdf(state = us_states)