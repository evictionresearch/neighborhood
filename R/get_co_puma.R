#' See which PUMAs intersect which counties.
#' @description This function defines the county associated with a PUMA. In some cases, multiple PUMAs fall within one county, such as in urban areas. In other situations, multiple counties may fall within one PUMA, such as in rural situations. Tracts nest within PUMAs so this
#' @param st Study state or states
#' @param yr PUMA year Default "NULL"
#' @param ... Other keyword arguments
#' @return A data frame of PUMA and county FIPS codes
#' @examples \dontrun{
#' devtools::install_github("timathomas/neighborhood", ref = "main")
#' #
#' # Pull PUMS data
#' # --------------------------------------------------------------------------
#'
#' pums_var <- pums_variables %>% filter(year == 2021, survey == "acs5")
#' pums_var %>%
#'   distinct(var_code, var_label, data_type, level) %>%
#'   filter(level == "housing") %>%
#'   select(-level) %>%
#'   arrange(var_code) %>%
#'   data.frame()
#'
#' pums_var %>% filter(grepl("AGE", var_code)) %>% data.frame()
#' pums_var %>% filter(grepl("code", var_label)) %>% count(var_label) %>% data.frame()
#' pums_var %>% count(var_label) %>% data.frame()
#'
#' #
#' # Create PUMS dataset with PUMA, Sex, Age, Race, Hispanicity, and Tenure in
#' # our study area.
#' # --------------------------------------------------------------------------
#'
#' study_pumas <- get_co_puma("OR", 2021)
#'
#' pums <-
#'   get_pums(
#'     variables = c("PUMA", "SEX", "AGEP", "RAC1P", "HISP", "HHT"),
#'     state = "OR",
#'     survey = "acs5",
#'     year = 2021,
#'     variables_filter = list(
#'       # SPORDER = 1, # first person in house
#'       TEN = 3 # renters
#'     ),
#'     recode = TRUE
#'   )
#'
#' #
#' # PUMS by Householder (i.e., head)
#' #
#' study_pums <-
#'   left_join(study_pumas, pums, by = c("PUMACE10" = "PUMA")) %>%
#'   mutate(
#'     race_eth = case_when(
#'       HISP != "01" ~ "Latine",
#'       HISP == "01" & RAC1P == "1" ~ "White",
#'       HISP == "01" & RAC1P == "2" ~ "Black",
#'       HISP == "01" & RAC1P == "6" ~ "Other", # for OR, Asian is other
#'       TRUE ~ "Other")
#'   ) %>%
#'   filter(AGEP >= 18, SPORDER == "1") %>%
#'   count(county, sex = SEX_label, race = race_eth, wt = WGTP) %>%
#'   group_by(county) %>%
#'   mutate(pums_total = sum(n))
#'
#' data.frame(study_pums)
#'
#' study_pums_head <-
#'   left_join(
#'     study_pums %>%
#'       group_by(county, race) %>%
#'       reframe(n = sum(n), pums_total = unique(pums_total)) %>%
#'       pivot_wider(names_from = race, values_from = n, names_prefix = "pums_"),
#'     study_pums %>%
#'       group_by(county, sex) %>%
#'       reframe(n = sum(n), pums_total = unique(pums_total)) %>%
#'       pivot_wider(names_from = sex, values_from = n, names_prefix = "pums_")
#'   ) %>%
#'   left_join(
#'     study_pums %>%
#'       group_by(county) %>%
#'       pivot_wider(names_from = c(race, sex), values_from = n, names_prefix = "pums_")
#'   )

#' data.frame(study_pums_head)
#' }
#' @export

get_co_puma <-
  function(
  	st,
  	yr = NULL,
  	...
  	){
    us_tracts <-
      purrr::map2(st, yr, function(s, y){
        tigris::tracts(state = s, year = y, ...) %>%
          dplyr::select(STATEFP, COUNTYFP) %>%
          dplyr::mutate(YEAR = y) %>%
          sf::st_centroid()}
        )|> dplyr::bind_rows()

    us_pumas <-
      purrr::map2(st, yr, function(s, y){
        tigris::pumas(state = s, year = y) %>%
          dplyr::select(dplyr::starts_with(c("PUMACE", "NAMELSAD")))
        }
        )|> dplyr::bind_rows()

    us_co_puma <-
      sf::st_join(us_pumas, us_tracts) %>%
      sf::st_drop_geometry() %>%
      dplyr::left_join(
        fips_codes %>%
          dplyr::select(
            STATEFP = state_code,
            COUNTYFP = county_code,
            state,
            state_name,
            county)
        ) %>%
      dplyr::distinct() %>%
      dplyr::arrange(county) %>%
      dplyr::mutate(county = stringr::str_replace(county, " County", ""))

    return(us_co_puma)
  }
