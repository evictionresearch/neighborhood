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

#### BEGIN TESTING ####

librarian::shelf(tidycensus, dplyr, stats, scales, stringr, tigris, sf)
options(width = Sys.getenv("COLUMNS", unset = 120))
state <- "NJ"
counties <- "Atlantic"
ami_limit <- .8
year <- 2023

  closest <- function(x = "x", limits = "limits") {
    limits[which.min(abs(limits - x))]
  }
  state_to_fips <- function(state) {
    # List of state names, abbreviations, and FIPS codes
    state_table <- data.frame(
      name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
      abbr = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
      fips = c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56"),
      stringsAsFactors = FALSE
    )
    # If already a 2-digit FIPS code, return as character
    if (is.character(state) && nchar(state) == 2 && state %in% state_table$fips) {
      return(state)
    }
    # Try matching by abbreviation (case-insensitive)
    abbr_match <- which(toupper(state) == state_table$abbr)
    if (length(abbr_match) == 1) {
      return(state_table$fips[abbr_match])
    }
    # Try matching by full name (case-insensitive, ignore spaces)
    name_match <- which(tolower(gsub("\\s+", "", state)) == tolower(gsub("\\s+", "", state_table$name)))
    if (length(name_match) == 1) {
      return(state_table$fips[name_match])
    }
    stop("State not recognized. Please provide a valid state name, abbreviation, or 2-digit FIPS code.")
  }

  income <-
    tidycensus::get_acs(geography = "county",
            table = "B19001",
            state = state_to_fips(state),
            year = year,
            cache_table = TRUE) %>%
    dplyr::filter(GEOID %in% paste0(state, counties)) %>%
    dplyr::left_join(tidycensus::load_variables(year, "acs5", cache = TRUE), by = c("variable" = "name"))
### LEFT OFF HERE: RECHECK ABOVE FUNCTION

  med_inc <-
    tidycensus::get_acs(geography = "tract",
            variables = "B19013_001",
            state = state,
            county = counties,
            year = year,
            cache_table = TRUE)

  acsvars <- tidycensus::load_variables(year, "acs5", cache = TRUE)

  price <- dplyr::bind_rows(
    tidycensus::get_acs(geography = "tract",
            variables = c("B25085_001", "B25085_002", "B25085_003", "B25085_004", "B25085_005",
                          "B25085_006", "B25085_007", "B25085_008", "B25085_009", "B25085_010",
                          "B25085_011", "B25085_012", "B25085_013", "B25085_014", "B25085_015",
                          "B25085_016", "B25085_017", "B25085_018", "B25085_019", "B25085_020",
                          "B25085_021", "B25085_022", "B25085_023", "B25085_024"),
            state = state,
            county = counties,
            year = year,
            cache_table = TRUE),
    tidycensus::get_acs(geography = "tract",
            variables = c("B25085_025", "B25085_026", "B25085_027"),
            state = state,
            county = counties,
            year = year,
            cache_table = TRUE)) %>%
    dplyr::left_join(acsvars, by = c("variable" = "name")) %>%
    dplyr::arrange(GEOID, variable)

  value <- dplyr::bind_rows(
    tidycensus::get_acs(geography = "tract",
            variables = c("B25075_001", "B25075_002", "B25075_003", "B25075_004", "B25075_005",
                          "B25075_006", "B25075_007", "B25075_008", "B25075_009", "B25075_010",
                          "B25075_011", "B25075_012", "B25075_013", "B25075_014", "B25075_015",
                          "B25075_016", "B25075_017", "B25075_018", "B25075_019", "B25075_020",
                          "B25075_021", "B25075_022", "B25075_023", "B25075_024"),
            state = state,
            county = counties,
            year = year,
            cache_table = TRUE),
    tidycensus::get_acs(geography = "tract",
            variables = c("B25075_025", "B25075_026", "B25075_027"),
            state = state,
            county = counties,
            year = year,
            cache_table = TRUE)) %>%
    dplyr::left_join(acsvars, by = c("variable" = "name")) %>%
    dplyr::arrange(GEOID, variable)

  rent <- dplyr::bind_rows(
    tidycensus::get_acs(geography = "tract",
            variables = c("B25063_002", "B25063_003", "B25063_004", "B25063_005",
                          "B25063_006", "B25063_007", "B25063_008", "B25063_009", "B25063_010",
                          "B25063_011", "B25063_012", "B25063_013", "B25063_014", "B25063_015",
                          "B25063_016", "B25063_017", "B25063_018", "B25063_019", "B25063_020",
                          "B25063_021", "B25063_022", "B25063_023", "B25063_024", "B25063_025"),
            state = state,
            county = counties,
            year = year,
            cache_table = TRUE),
    tidycensus::get_acs(geography = "tract",
            variables = "B25063_026",
            state = state,
            county = counties,
            year = year,
            cache_table = TRUE)) %>%
    dplyr::left_join(acsvars, by = c("variable" = "name")) %>%
    dplyr::arrange(GEOID, variable)

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