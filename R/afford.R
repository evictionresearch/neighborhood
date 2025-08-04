#' @title Affordable market location
#' @description The \code{afford} function identifies where different income groups can afford to live based on local market value while accounting for the region's count of households of the given income group.
#' @param state Study state.
#' @param counties Study county or counties.
#' @param ami_limit Specified area median income, or AMI, limit of interest such as 3. or .5 AMI.
#' @param year Study year.
#' @param geometry Download spatial data.
#' @param ... Other keyword arguments
#' @return Returns a spatial file
#' @examples \dontrun{
#' bay5 <- afford("06", c("001", "013", "041", "075", "081"), .5, 2021)<br>
#' wasatch5 <- afford("49", c('057', '011', '035', '049'), .5, 2019, geometry = TRUE)
#' }
#' @export

afford <- function(
   state = "state",
   counties = "counties",
   ami_limit = "ami_limit",
   year = NULL,
   geometry = FALSE,
   ...
   ){

  # Input validation warnings
  if (!is.character(state) || nchar(state) != 2 || !grepl("^[0-9]{2}$", state)) {
    warning("'state' should be a 2-digit FIPS code as character (e.g., '06' for California, '53' for Washington), not a state name or abbreviation. Use tigris::fips_codes to find the correct FIPS code.")
  }

  if (!is.character(counties) || any(nchar(counties) != 3) || any(!grepl("^[0-9]{3}$", counties))) {
    warning("'counties' should be 3-digit FIPS codes as character strings (e.g., '001', '013'), not county names")
  }

#### BEGIN TESTING ####

librarian::shelf(tidycensus, dplyr, stats, scales, stringr, tigris, sf)
options(width = Sys.getenv("COLUMNS", unset = 120))
state <- "34"
counties <-
  fips_codes %>%
  filter(
    county %in% # nolint
    c("Atlantic County", "Cape May County", "Cumberland County", # nolint
    "Ocean County", "Gloucester County", "Camden County", "Burlington County"), # nolint
    state == "NJ") %>% # nolint
    pull(county_code) # Atlantic, Cape May, Cumberland, Ocean, Gloucester, Camden, Burlington # nolint
ami_limit <- .8
year <- 2023

  closest <- function(x = "x", limits = "limits") {
    limits[which.min(abs(limits - x))]
  }

# County count of household income

income <-
  tidycensus::get_acs(
    geography = "county",
    table = "B19001",
    state = state,
    year = year,
    cache_table = TRUE
  ) %>%
  dplyr::filter(GEOID %in% paste0(state, counties)) %>%
  dplyr::left_join(
    tidycensus::load_variables(year, "acs5", cache = TRUE),
    by = c("variable" = "name")
  )

# Tract level median income levels
  med_inc <-
    tidycensus::get_acs(
      geography = "tract",
      variables = "B19013_001",
      state = state,
      county = counties,
      year = year,
      cache_table = TRUE
    )

# Get ACS variables to join to data
  acsvars <- tidycensus::load_variables(year, "acs5", cache = TRUE)

# Home asking price
  price <-
    tidycensus::get_acs(
      geography = "tract",
      table = "B25085",
      state = state,
      county = counties,
      year = year,
      cache_table = TRUE
    ) %>%
    dplyr::left_join(acsvars, by = c("variable" = "name")) %>%
    dplyr::arrange(GEOID, variable)

# Home value
  value <-
    tidycensus::get_acs(
      geography = "tract",
      table = "B25075",
      state = state,
      county = counties,
      year = year,
      cache_table = TRUE
    ) %>%
    dplyr::left_join(acsvars, by = c("variable" = "name")) %>%
    dplyr::arrange(GEOID, variable)

  rent <-
    tidycensus::get_acs(
      geography = "tract",
      table = "B25063", # use table because some years have added levels
      state = state,
      county = counties,
      year = year,
      cache_table = TRUE
    ) %>%
    dplyr::left_join(acsvars, by = c("variable" = "name")) %>%
    dplyr::arrange(GEOID, variable) %>%
    dplyr::group_by(GEOID) %>%
    dplyr::filter(
      variable != dplyr::first(variable) & variable != dplyr::last(variable)
    ) %>% # remove first and last variables because they are not income limits
    dplyr::ungroup()
### LEFT OFF
# Double check each limit is represented in the data
  income_limit <- c(0, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000,
                    50000, 60000, 75000, 100000, 125000, 150000, 200000, Inf)
  income$limit <- rep(income_limit, times = nrow(income)/length(income_limit))
  price_limit <- c(0, 10000, 15000, 20000, 25000, 30000, 35000, 40000,
                   50000, 60000, 70000, 80000, 90000, 100000, 125000, 150000, 175000, 200000,
                   250000, 300000, 400000, 500000, 750000, 1000000, 1500000, 2000000, Inf)
  price$limit <- rep(price_limit, times = nrow(price)/length(price_limit))
  value$limit <- rep(price_limit, times = nrow(price)/length(price_limit))
  rent_limit <- c(0, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800,
                  900, 1000, 1250, 1500, 2000, 2500, 3000, 3500, Inf)
  rent$limit <- rep(rent_limit, time = nrow(rent)/25)

  ami <- stats::median(med_inc$estimate, na.rm = TRUE)

  price$income_limit <- price$limit*0.188
  value$income_limit <- value$limit*0.188
  rent$income_limit <- (rent$limit/0.3)*12

  price_counts <-
    price %>%
    dplyr::filter(income_limit <= closest(ami_limit*ami, income_limit) &
             income_limit > 0) %>%
    dplyr::group_by(GEOID) %>%
    dplyr::summarize(tr_own_accessible = sum(estimate)) %>%
    dplyr::left_join(
      price %>%
        dplyr::filter(income_limit == 0) %>%
        dplyr::select(GEOID, tr_own_total = estimate)
    )

  value_counts <-
    value %>%
    dplyr::filter(income_limit <= closest(ami_limit*ami, income_limit) &
             income_limit > 0) %>%
    dplyr::group_by(GEOID) %>%
    dplyr::summarize(tr_own_accessible = sum(estimate)) %>%
    dplyr::left_join(
      value %>%
        dplyr::filter(income_limit == 0) %>%
        dplyr::select(GEOID, tr_own_total = estimate)
    )

  rent_counts <-
    rent %>%
    dplyr::filter(income_limit <= closest(ami_limit*ami, income_limit) &
             income_limit > 0) %>%
    dplyr::group_by(GEOID) %>%
    dplyr::summarize(tr_rent_accessible = sum(estimate)) %>%
    dplyr::left_join(
      rent %>%
        dplyr::filter(income_limit == 0) %>%
        dplyr::select(GEOID, tr_rent_total = estimate)
    )

  total_pop <- sum(income %>%
                  dplyr::filter(limit == 0) %>%
                    dplyr::select(estimate))

  class_pop <- sum(income %>%
                     dplyr::filter(limit <= closest(ami_limit*ami, income_limit) & income_limit > 0) %>%
                     dplyr::select(estimate))

  class_prop <- case_when(
    class_pop == 0 & total_pop == 0 ~ 0,
    class_pop > 0 & total_pop == 0 ~ 0,
    TRUE ~ class_pop/total_pop
  )


  tract_counts <-
    dplyr::bind_rows(price_counts, value_counts) %>%
    dplyr::group_by(GEOID) %>%
    dplyr::summarize_all(~sum(.)) %>%
    dplyr::mutate(
      tr_own_supply = tr_own_accessible/tr_own_total,
      tr_own_ratio = tr_own_supply/class_prop,
      tr_own_rate = (tr_own_accessible/class_pop)*100000) %>%
    dplyr::left_join(
      rent_counts %>%
      dplyr::mutate(
        tr_rent_supply = tr_rent_accessible/tr_rent_total,
        tr_rent_ratio = case_when(
          tr_rent_supply == 0 & class_prop == 0 ~ 0,
          tr_rent_supply > 0 & class_prop == 0 ~ 0,
          TRUE ~ tr_rent_supply/class_prop
        ),
        tr_rent_rate = case_when(
          tr_rent_accessible == 0 & class_prop == 0 ~ 0,
          tr_rent_accessible > 0 & class_prop == 0 ~ 0,
          TRUE ~ (tr_rent_accessible/class_pop)*100000)
        ),
      by = "GEOID") %>%
    dplyr::mutate(
      reg_total_pop = total_pop,
           reg_class_pop = class_pop,
           ami_limit = ami_limit)

# max_rent <- max(tract_counts$tr_rent_rate, na.rm = TRUE)/100000
# max_own <- max(tract_counts$tr_own_rate, na.rm = TRUE)/100000

  tract_counts <-
    tract_counts %>%
    dplyr::mutate(
      rent_jenks_cat =
        factor(
          dplyr::case_when(
            tr_rent_rate <= 100 ~ "Less than 0.1%",
            tr_rent_rate > 100 & tr_rent_rate <= 200  ~ "0.1% to 0.2%",
            tr_rent_rate > 200 ~
              paste0(
                "0.2% to ",
                scales::percent(max(tr_rent_rate, na.rm = TRUE)/100000, accuracy = .1)),
          ),
          levels = c(
            "Less than 0.1%",
            "0.1% to 0.2%",
            paste0("0.2% to ", scales::percent(max(tr_rent_rate, na.rm = TRUE)/100000, accuracy = .1))
          )
        ),
    own_jenks_cat =
      factor(
        dplyr::case_when(
          tr_own_rate <= 100 ~ "Less than 0.1%",
          tr_own_rate > 100 & tr_own_rate <= 200  ~ "0.1% to 0.2%",
          tr_own_rate > 200 ~
            paste0(
              "0.2% to ",
              scales::percent(max(tr_own_rate, na.rm = TRUE)/100000, accuracy = .1)
            ),
        ),
        levels = c(
          "Less than 0.1%",
          "0.1% to 0.2%",
          paste0("0.2% to ", scales::percent(max(tr_own_rate, na.rm = TRUE)/100000, accuracy = .1))
        )
      ),
    popup =
      stringr::str_c(
        "<b>Tract: ", GEOID, "</b>",
                "<br>",
                "Tract rental units: ", scales::comma(tr_rent_total, accuracy = 1),
                "<br>",
                "Rentals affordable to ", scales::percent(ami_limit), " AMI: ", scales::comma(tr_rent_accessible, accuracy = 1), " (", scales::percent(tr_rent_supply, accuracy = 1), ")",
                "<br>",
                scales::percent(tr_rent_rate/100000, accuracy = .1), " of the region's ", scales::percent(ami_limit, accuracy = 1), " AMI households served",
                "<br>",
                "<br>",
                "Tract owned homes: ", scales::comma(tr_own_total, accuracy = 1),
                "<br>",
                "Homes affordable to ", scales::percent(ami_limit), " AMI: ", scales::comma(tr_own_accessible, accuracy = 1), " (", scales::percent(tr_own_supply, accuracy = 1), ")",
                "<br>",
                scales::percent(tr_own_rate/100000, accuracy = .1), " of the region's ", scales::percent(ami_limit, accuracy = 1), " AMI households served",
                "<br>",
                "<br>",
                "Region household count: ", scales::comma(reg_total_pop, accuracy = 1), "<br>",
                "Region Households at ", scales::percent(ami_limit), " AMI: ", scales::comma(reg_class_pop, accuracy = 1)
        )
      )

if(geometry == TRUE){
  print("Return sf dataframe")
  tigris::tracts(state = state, county = counties, cb = TRUE, year = year) %>% dplyr::left_join(tract_counts) %>% sf::st_transform(crs = 4326)
} else {
  print("Return dataframe")
  return(tract_counts)
}
}

# marin_co_afford <- afford("CA", "Marin", .8, year = 2022)
# marin_co_afford %>% glimpse()
# marin_co_afford %>% summary()


atlantic_co <- afford("NJ", "Atlantic", .8, year = 2023)

tidycensus
dplyr
stats
scales
stringr
tigris
sf