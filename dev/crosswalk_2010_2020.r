# ==========================================================================
# Create 2010 to 2020 crosswalk
# Author: Tim Thomas - timthomas@berkeley.edu
# Modified: September 23, 2024
# Goal: Create crosswalk file for 2010 to 2020 census tracts and counties.
# ==========================================================================

# group by 2020 tracts
# For counts: sum weighted values
# For medians & averages: sum(weighted values)/sum(weights)
# Check vars for avg and meds

#
# Notes:
# --------------------------------------------------------------------------
# As of 2022, Connecticut uses 9 planning regions instead of the traditional counties
# as used in other states. Unfortunately, crosswalks to only county equivalants will not 
# be made available until 2027.
# ==========================================================================
# Adjustment notes: 
# 1. Check for NA's and see if there needs to be some kind of adjustment for them. 
# 2. add CPI adjustment for monetary values
# ==========================================================================

#
# Load packages
# --------------------------------------------------------------------------
librarian::shelf(qs, tidycensus, tigris, sf, timathomas/colorout, tidyverse)

#
# Options and user defined inputs
# --------------------------------------------------------------------------
options(
  tigris_use_cache = TRUE,
  tigris_class = "sf",
  width = Sys.getenv('COLUMNS')
)

# ==========================================================================
# 1. Create crosswalk for census tracts
# ==========================================================================
# Create a crosswalk using 2010 block data to situate in 2020 tracts

#
# Find pop and tenure variables
# --------------------------------------------------------------------------
vars_dec10sf1 <- tidycensus::load_variables(2010, 'sf1', cache = TRUE)

glimpse(vars_dec10sf1)
count(vars_dec10sf1,concept) %>% data.frame()

vars_dec10sf1 %>% 
  filter(concept %in% c("TOTAL POPULATION", "TENURE")) %>% 
  mutate(title = paste0(label, " = ", name)) %>% 
  pull(title)

# list vars
# ----------
acs_vars <-
  c(tenure = "H004001",
    own_mort = "H004002",
    own_free = "H004003",
    renter = "H004004",
    pop = "P001001")

#
# Download census data by state and save
# --------------------------------------------------------------------------

# ==========================================================================
# DEV VALUES
# states <- c("RI", "CT", "XY") # dev states
# state = "RI"
# ==========================================================================
states <- unique(fips_codes$state)[1:51]

crosswalk_tract <- function(state){
  tryCatch({
    print(paste0("Processing ", state, " 2010 blocks"))
    block10 <-
      get_decennial(
        geography = "block",
        state = state,
        variables = acs_vars,
        year = 2010,
        geometry = TRUE,
        output = "wide"
      ) %>%
      st_centroid() %>%
      group_by(GEOID) %>%
      mutate(
        tract10 = str_sub(GEOID, 1, 11),
        own = sum(own_mort, own_free, na.rm = TRUE)
      ) %>%
      arrange(tract10) %>%
      group_by(tract10) %>%
      mutate(
        share_pop = pop/sum(pop, na.rm = TRUE),
        share_tenure = tenure/sum(tenure, na.rm = TRUE),
        share_own = own/sum(own, na.rm = TRUE),
        share_renter = renter/sum(renter, na.rm = TRUE),
      ) %>%
      ungroup()

    print(paste0("Downloading ", state, " 2022 tracts"))
    tract22 <-
      tracts(
        state = state,
        cb = FALSE
      )

    print(paste0("Joining & weighting ", state, " 2010 blocks and 2022 tracts"))
    cw <-
      st_join(tract22, block10) %>%
      group_by(tract20 = GEOID.x, tract10) %>%
      reframe(
        pop = sum(pop, na.rm = TRUE),
        tenure = sum(tenure, na.rm = TRUE),
        own = sum(own, na.rm = TRUE),
        renter = sum(renter, na.rm = TRUE),
        pop_weight = sum(share_pop, na.rm = TRUE),
        tenure_weight = sum(share_tenure, na.rm = TRUE),
        own_weight = sum(share_own, na.rm = TRUE),
        renter_weight = sum(share_renter, na.rm = TRUE),
      ) %>%
      arrange(tract20, tract10)

    print(paste0("Saving ", state, " crosswalk"))
    qsave(cw, paste0("~/data/evictionresearch/neighborhood/crosswalk/tracts/cw_", state, ".qs"))

    print(summary(cw))
    print("Clean up")
    rm(block10)
    rm(tract22)
    rm(cw)
    gc()

    print(paste0("Success ", state))
      return(NULL)
    }, error = function(e) {
    print(paste0("Failed ", state))
    return(state)
  })
}

failed_states <- map(states, crosswalk_tract) %>% compact()
unlist(failed_states)
# TX failed
map("TX", crosswalk_tract) %>% compact()

# ==========================================================================
# Create complete us tract file
# ==========================================================================
cw_files <- list.files("~/data/evictionresearch/neighborhood/crosswalk/tracts", full.names = TRUE)

us_cw <-
  map_dfr(cw_files, function(x){
    qread(x)
  }
  )

qsave(us_cw, "~/git/evictionresearch/neighborhood/data/cw_tract_us.qs")

# ==========================================================================
# Create crosswalk for counties
# ==========================================================================

states <- unique(fips_codes$state)[1:51]

crosswalk_county <- function(state){
  tryCatch({
    print(paste0("Processing ", state, " 2010 blocks"))
    block10 <-
      get_decennial(
        geography = "block",
        state = state,
        variables = acs_vars,
        year = 2010,
        geometry = TRUE,
        output = "wide"
      ) %>%
      st_centroid() %>%
      group_by(GEOID) %>%
      mutate(
        tract10 = str_sub(GEOID, 1, 11),
        own = sum(own_mort, own_free, na.rm = TRUE)
      ) %>%
      arrange(tract10) %>%
      group_by(tract10) %>%
      mutate(
        share_pop = pop/sum(pop, na.rm = TRUE),
        share_tenure = tenure/sum(tenure, na.rm = TRUE),
        share_own = own/sum(own, na.rm = TRUE),
        share_renter = renter/sum(renter, na.rm = TRUE),
      ) %>%
      ungroup()

    print(paste0("Downloading ", state, " 2022 counties"))
    county22 <-
      counties(
        state = state,
        cb = FALSE
      )

    print(paste0("Joining & weighting ", state, " 2010 blocks and 2022 counties"))
    cw <-
      st_join(county22, block10) %>%
      group_by(county20 = GEOID.x, county10) %>%
      reframe(
        pop = sum(pop, na.rm = TRUE),
        tenure = sum(tenure, na.rm = TRUE),
        own = sum(own, na.rm = TRUE),
        renter = sum(renter, na.rm = TRUE),
        pop_weight = sum(share_pop, na.rm = TRUE),
        tenure_weight = sum(share_tenure, na.rm = TRUE),
        own_weight = sum(share_own, na.rm = TRUE),
        renter_weight = sum(share_renter, na.rm = TRUE),
      ) %>%
      arrange(county20, county10)

    print(paste0("Saving ", state, " crosswalk"))
    qsave(cw, paste0("~/data/evictionresearch/neighborhood/crosswalk/counties/cw_", state, ".qs"))

    print(summary(cw))
    print("Clean up")
    rm(block10)
    rm(county22)
    rm(cw)
    gc()

    print(paste0("Success ", state))
    return(NULL)
    }, error = function(e) {
    print(paste0("Failed ", state))
    return(state)
  })
}

failed_states <- map(states, crosswalk_county) %>% compact()
map("ID", crosswalk_county) %>% compact()

# ==========================================================================
# Create complete us counties file
# ==========================================================================
cw_files <- list.files("~/data/evictionresearch/neighborhood/crosswalk/counties", full.names = TRUE)

us_cw <-
  map_dfr(cw_files, function(x){
    qread(x)
  }
  )

qsave(us_cw, "~/git/evictionresearch/neighborhood/data/cw_county_us.qs")