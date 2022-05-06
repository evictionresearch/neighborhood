# ==========================================================================
# Determine income groups for a specific geography
# ==========================================================================

# ==========================================================================
# Census variable download
# ==========================================================================

#
# Variables
# --------------------------------------------------------------------------

acs_hhincten <-
  c(
  'mhhinc' = 'B19013_001',
  'HHIncTen_Total' = 'B25118_001', # Total
  'HHIncTenRent' = 'B25118_014', # Renter occupied
  'HHIncTenRent_5' = 'B25118_015', # Renter occupied!!Less than $5,000
  'HHIncTenRent_10' = 'B25118_016', # Renter occupied!!$5,000 to $9,999
  'HHIncTenRent_15' = 'B25118_017', # Renter occupied!!$10,000 to $14,999
  'HHIncTenRent_20' = 'B25118_018', # Renter occupied!!$15,000 to $19,999
  'HHIncTenRent_25' = 'B25118_019', # Renter occupied!!$20,000 to $24,999
  'HHIncTenRent_35' = 'B25118_020', # Renter occupied!!$25,000 to $34,999
  'HHIncTenRent_50' = 'B25118_021', # Renter occupied!!$35,000 to $49,999
  'HHIncTenRent_75' = 'B25118_022', # Renter occupied!!$50,000 to $74,999
  'HHIncTenRent_100' = 'B25118_023', # Renter occupied!!$75,000 to $99,999
  'HHIncTenRent_150' = 'B25118_024', # Renter occupied!!$100,000 to $149,999
  'HHIncTenRent_151' = 'B25118_025' # Renter occupied!!$150,000 or more
  )

#
# Tidycensus download data
# --------------------------------------------------------------------------
tr_data <- function(
	geography = 'tract',
    vars = acs_hhincten,
    state,
    county_list,
    geometry = FALSE,
    cache_table = TRUE,
    output = 'tidy',
    year = NULL
	){
  require(dplyr)
  tidycensus::get_acs(
    geography = geography,
    variables = vars,
    state = state,
    county = county_list,
    geometry = geometry,
    cache_table = cache_table,
    output = output,
    year = year,
    cb = TRUE,
    keep_geo_vars = TRUE
    ) %>%
  dplyr::mutate(acs_year = year) %>%
  dplyr::select(-moe) %>%
  tidyr::spread(variable, estimate) %>%
  dplyr::mutate(COUNTY = substr(GEOID, 1, 5))
}

  df <- tr_data(state = 'CA', county_list = 'Alameda', year = 2019) %>% glimpse()

inc_names <- c(
  'HHInc_5' = 4999, # Renter occupied!!Less than $5,000
  'HHInc_10' = 9999, # Renter occupied!!$5,000 to $9,999
  'HHInc_15' = 14999, # Renter occupied!!$10,000 to $14,999
  'HHInc_20' = 19999, # Renter occupied!!$15,000 to $19,999
  'HHInc_25' = 24999, # Renter occupied!!$20,000 to $24,999
  'HHInc_35' = 34999, # Renter occupied!!$25,000 to $34,999
  'HHInc_50' = 49999, # Renter occupied!!$35,000 to $49,999
  'HHInc_75' = 74999, # Renter occupied!!$50,000 to $74,999
  'HHInc_100' = 99999, # Renter occupied!!$75,000 to $99,999
  'HHInc_150' = 149999, # Renter occupied!!$100,000 to $149,999
  'HHInc_151' = 150000 # Renter occupied!!$150,000 or more
)

#
# Create income group dataframe
# --------------------------------------------------------------------------

df_vli <-
  df %>%
  dplyr::select(
    GEOID,
    acs_year,
    COUNTY,
    mhhinc,
    HHIncTenRent_10:HHIncTenRent_75) %>%
  dplyr::group_by(COUNTY, acs_year) %>%
  dplyr::mutate(co_mhhinc = median(mhhinc, na.rm = TRUE),
       co_MI_val = 1.2*co_mhhinc,
       co_LI_val = .8*co_mhhinc,
       co_VLI_val = .5*co_mhhinc,
       co_ELI_val = .3*co_mhhinc) %>%
  dplyr::ungroup() %>%
  tidyr::gather(medinc_cat, medinc_count, HHIncTenRent_10:HHIncTenRent_75) %>%
  mutate(medinc_cat = recode(medinc_cat, !!!inc_names)) %>%
  mutate(bottom_inccat =
      case_when(
       medinc_cat > 0 & medinc_cat <= 24999 ~ medinc_cat - 4999,
       medinc_cat == 34999 ~ medinc_cat - 9999,
       medinc_cat == 49999 ~ medinc_cat - 14999,
       medinc_cat == 74999 | medinc_cat == 99999 ~ medinc_cat - 24999,
       medinc_cat == 149999 ~ medinc_cat - 49999,
       medinc_cat == 150000 ~ medinc_cat,
                   # for owner and renter income
                   # medinc_cat > 9999 &
                   # medinc_cat <= 49999 ~ medinc_cat - 4999,
                   # medinc_cat == 59999 ~ medinc_cat - 9999,
                   # medinc_cat > 59999 &
                   # medinc_cat <= 149999 ~ medinc_cat - 24999,
                   # medinc_cat >= 199999 ~ medinc_cat - 49999,
                  TRUE ~ NA_real_),
    top_inccat = medinc_cat,
    HI = case_when(
      bottom_inccat <= co_MI_val & top_inccat >= co_MI_val ~
        (top_inccat - co_MI_val)/(top_inccat - bottom_inccat),
      bottom_inccat >= co_MI_val ~ 1,
      TRUE ~ 0),
    MI = case_when(
      bottom_inccat <= co_LI_val & top_inccat >= co_LI_val ~
        (top_inccat - co_LI_val)/(top_inccat - bottom_inccat),
      bottom_inccat >= co_LI_val & top_inccat <= co_MI_val ~ 1,
      bottom_inccat <= co_MI_val & top_inccat >= co_MI_val ~
        (co_MI_val - bottom_inccat)/(top_inccat - bottom_inccat),
      TRUE ~ 0),
    LI = case_when(
      bottom_inccat <= co_VLI_val & top_inccat >= co_VLI_val ~
        (top_inccat - co_VLI_val)/(top_inccat - bottom_inccat),
      bottom_inccat >= co_VLI_val & top_inccat <= co_LI_val ~ 1,
      bottom_inccat <= co_LI_val & top_inccat >= co_LI_val ~
        (co_LI_val - bottom_inccat)/(top_inccat - bottom_inccat),
      TRUE ~ 0),
    VLI = case_when(
      bottom_inccat <= co_ELI_val & top_inccat >= co_ELI_val ~
        (top_inccat - co_ELI_val)/(top_inccat - bottom_inccat),
      bottom_inccat >= co_ELI_val & top_inccat <= co_VLI_val ~ 1,
      bottom_inccat <= co_VLI_val & top_inccat >= co_VLI_val ~
        (co_VLI_val - bottom_inccat)/(top_inccat - bottom_inccat),
      TRUE ~ 0),
    ELI = case_when(
      top_inccat <= co_ELI_val ~ 1,
      bottom_inccat <= co_ELI_val & top_inccat >= co_ELI_val ~
        (co_ELI_val - bottom_inccat)/(top_inccat - bottom_inccat),
      TRUE ~ 0)) %>%
  group_by(GEOID, acs_year) %>%
  mutate(
    tot_hh = sum(medinc_count, na.rm = TRUE),
    High = sum(HI*medinc_count, na.rm = TRUE),
    Moderate = sum(MI*medinc_count, na.rm = TRUE),
    Low = sum(LI*medinc_count, na.rm = TRUE),
    `Very Low` = sum(VLI*medinc_count, na.rm = TRUE),
    `Extremely Low` = sum(ELI*medinc_count, na.rm = TRUE)
    # tr_HI_prop = tr_HI_count/tr_totinc_count,
    # tr_MI_prop = tr_MI_count/tr_totinc_count,
    # tr_LI_prop = tr_LI_count/tr_totinc_count,
    # tr_VLI_prop = tr_VLI_count/tr_totinc_count,
    # tr_ELI_prop = tr_ELI_count/tr_totinc_count
  ) %>%
  select(acs_year, GEOID, tot_hh:`Extremely Low`) %>%
  distinct() %>%
  gather(inc_group, num_hh, High:`Extremely Low`) %>%
  mutate(perc_hh = num_hh/tot_hh)