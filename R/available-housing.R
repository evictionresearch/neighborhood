# ==========================================================================
# Available Housing Index
# Developed by Alex Ramiller and Tim Thomas
# 2020.09.18
# This pulls the appropriate data to determine the tract rate of homes that
# are available to various income groups.
# ==========================================================================

# # census_api_key(read_yaml("/Users/ajramiller/census.yaml"))
# census_api_key('4c26aa6ebbaef54a55d3903212eabbb506ade381', install = TRUE)

#
# Function dev.
# --------------------------------------------------------------------------
closest <- function(x = "x", limits = "limits") {
  limits[which.min(abs(limits - x))]
}

afford <- function(
   state = "state",
   counties = "counties",
   ami_limit = .3,
   year = NULL,
   GEOID = "GEOID",
   acsvars = "acsvars",
   variable = "variable",
   estimate = "estimate",
   limit = "limit",
   tr_own_accessible = "tr_own_accessible",
   tr_own_total = "tr_own_total",
   tr_own_supply = "tr_own_supply",
   tr_rent_accessible = "tr_rent_accessible",
   tr_rent_total = "tr_rent_total",
   tr_rent_supply = "tr_rent_supply") {
  require(dplyr)
  income <-
    tidycensus::get_acs(geography = "county",
            table = "B19001",
            state = state,
            year = year,
            cache_table = TRUE) %>%
    dplyr::filter(GEOID %in% paste0(state, counties)) %>%
    dplyr::left_join(tidycensus::load_variables(year, "acs5", cache = TRUE), by = c("variable" = "name"))

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
  income$limit <- rep(income_limit, times = nrow(income)/17)
  price_limit <- c(0, 10000, 15000, 20000, 25000, 30000, 35000, 40000,
                   50000, 60000, 70000, 80000, 90000, 100000, 125000, 150000, 175000, 200000,
                   250000, 300000, 400000, 500000, 750000, 1000000, 1500000, 2000000, Inf)
  price$limit <- rep(price_limit, times = nrow(price)/27)
  value$limit <- rep(price_limit, times = nrow(price)/27)
  rent_limit <- c(0, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800,
                  900, 1000, 1250, 1500, 2000, 2500, 3000, 3500, Inf)
  rent$limit <- rep(rent_limit, time = nrow(rent)/25)

  #ami <- closest(stats::median(med_inc$estimate, na.rm = TRUE), income$income_limit)
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

  class_prop <- class_pop/total_pop

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
        tr_rent_ratio = tr_rent_supply/class_prop,
        tr_rent_rate = (tr_rent_accessible/class_pop)*100000),
      by = "GEOID") %>%
    dplyr::mutate(
      reg_total_pop = total_pop,
           reg_class_pop = class_pop,
           ami_limit = ami_limit)

# Get breaks for rent and own categories
    # rent_breaks <- c(round(BAMMtools::getJenksBreaks(puget$tr_rent_rate, 4)[-4], digits = -1), max(puget$tr_rent_rate, na.rm = TRUE))
    # own_breaks <- c(round(BAMMtools::getJenksBreaks(tract_counts$tr_own_rate, 6)[-6], digits = -1), max(tract_counts$tr_own_rate, na.rm = TRUE))

  tract_counts <-
    tract_counts %>%
    dplyr::mutate(
    rent_jenks_cat =
      factor(
        case_when(
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
      # factor(
      #   case_when(
      #     tr_rent_rate <= rent_breaks[2] ~
      #       paste0(rent_breaks[1], ' - ', rent_breaks[2]),
      #     tr_rent_rate > rent_breaks[2] & tr_rent_rate <= rent_breaks[3] ~
      #       paste0(rent_breaks[2], ' - ', rent_breaks[3]),
      #     tr_rent_rate > rent_breaks[3] & tr_rent_rate <= rent_breaks[4] ~
      #       paste0(rent_breaks[3], ' - ', rent_breaks[4]),
      #     tr_rent_rate > rent_breaks[4] & tr_rent_rate <= rent_breaks[5] ~
      #       paste0(rent_breaks[4], ' - ', rent_breaks[5]),
      #     tr_rent_rate > rent_breaks[5] ~
      #       paste0(rent_breaks[5], ' - ', scales::comma(round(rent_breaks[6], digits = 0)))
      #     ),
      #   levels = c(
      #     paste0(rent_breaks[1], ' - ', rent_breaks[2]),
      #     paste0(rent_breaks[2], ' - ', rent_breaks[3]),
      #     paste0(rent_breaks[3], ' - ', rent_breaks[4]),
      #     paste0(rent_breaks[4], ' - ', rent_breaks[5]),
      #     paste0(rent_breaks[5], ' - ', scales::comma(round(rent_breaks[6], digits = 0)))
      #   )
      # ),
    own_jenks_cat =
      factor(
        case_when(
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
      # factor(
      #   case_when(
      #     tr_own_rate <= own_breaks[2] ~
      #       paste0(own_breaks[1], ' - ', own_breaks[2]),
      #     tr_own_rate > own_breaks[2] & tr_own_rate <= own_breaks[3] ~
      #       paste0(own_breaks[2], ' - ', own_breaks[3]),
      #     tr_own_rate > own_breaks[3] & tr_own_rate <= own_breaks[4] ~
      #       paste0(own_breaks[3], ' - ', own_breaks[4]),
      #     tr_own_rate > own_breaks[4] & tr_own_rate <= own_breaks[5] ~
      #       paste0(own_breaks[4], ' - ', own_breaks[5]),
      #     tr_own_rate > own_breaks[5] ~
      #       paste0(own_breaks[5], ' - ', scales::comma(round(own_breaks[6], digits = 0)))
      #     ),
      #   levels = c(
      #     paste0(own_breaks[1], ' - ', own_breaks[2]),
      #     paste0(own_breaks[2], ' - ', own_breaks[3]),
      #     paste0(own_breaks[3], ' - ', own_breaks[4]),
      #     paste0(own_breaks[4], ' - ', own_breaks[5]),
      #     paste0(own_breaks[5], ' - ', scales::comma(round(own_breaks[6], digits = 0)))
      #   )
      # ),
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

  tigris::tracts(state = state, county = counties, cb = TRUE, year = year) %>% dplyr::left_join(tract_counts) %>% sf::st_transform(crs = 4326)
}

#
# Create measure example
# --------------------------------------------------------------------------
# options(tigris_use_cache = TRUE)
# library(tidyverse)
# king <- afford("53", c("Pierce", "King", "Snohomish"), .8, 2020)
# puget <- afford("53", c("033", "053", "061"), .8, 2020)
# wasatch <- afford("49", c('057', '011', '035', '049'), .5, 2019)
# bay5 <- afford("06", c("001", "013", "041", "075", "081"), .5, 2019)
# bay9 <- afford("06", c("001", "013", "041", "055", "075", "081", "085", "095", "097"), .5, 2019)
# bay21 <- afford("06", c("001", "013", "017", "041", "047", "053", "055", "061", "067", "069", "075", "077", "081", "085", "087", "095", "097", "099", "101", "113", "115"), .3, 2018)

#
# Plot measures
# --------------------------------------------------------------------------
# tmap::tmap_mode("view")
#       tmap::tm_shape(bay5) +
#       tmap::tm_fill(
#         "rent_jenks_cat",
#         title = c("Rent", "Own"),
#         style = "fixed",
#         alpha = .5,
#         # breaks = c(0, 20, 40, 60, 80, 100, max(king %>% sf::st_drop_geometry() %>% dplyr::select(tr_rent_rate), na.rm = TRUE)),
#         palette = "-RdYlBu",
#         # id = c("tr_rent_rate", "tr_own_rate")
#         popup.vars = c("GEOID", "tr_rent_rate", "tr_own_rate")
#       ) +
#       tmap::tm_borders(alpha = .5) +
      # tmap::tm_facets(sync = TRUE, nrow = 2)


# plot_exclusivity_ratio <- function(data, variable = c("tr_rent_rate", "tr_own_rate")){
#   require(sf)
#   require(RColorBrewer)
#   require(tmap)
#   require(dplyr)
#   if(!(FALSE %in% (variable %in% names(data)))){

#     # plot(data[variable],
#     #      breaks = c(0, 20, 40, 60, 80, 100, max(data %>% st_drop_geometry() %>% select(variable), na.rm = TRUE)),
#     #      pal = brewer.pal(6, "RdBu"),
#     #      lwd = 0.01)
#   } else {
#     stop("Incorrect variable name.")
#   }
# }

# # plot_exclusivity_ratio(bay5, variable = "tr_rent_rate")
# # plot_exclusivity_ratio(bay9)
# # plot_exclusivity_ratio(bay21)
# plot_exclusivity_ratio(king)

# ==========================================================================
# Leaflet
# ==========================================================================

# rentpal <- colorNumeric(
#   palette = "Blues",
#   domain = king$tr_rent_rate)

# ownpal <- colorNumeric(
#   palette = "Blues",
#   domain = king$tr_own_rate)

# map_it <- function(data){


#   leaflet(data = king) %>%
#     addMapPane(name = "polygons", zIndex = 410) %>%
#     addMapPane(name = "maplabels", zIndex = 420) %>% # higher zIndex rendered on top
#     addProviderTiles("CartoDB.PositronNoLabels") %>%
#     addProviderTiles("CartoDB.PositronOnlyLabels",
#                    options = leafletOptions(pane = "maplabels"),
#                    group = "map labels") %>%
#     addPolygons(
#       group = 'Accessible Rental Market',
#       label = ~tr_rent_rate,
#       labelOptions = labelOptions(textsize = '12px'),
#       # fillOpacity. = .5,
#       color = ~rentpal(tr_rent_rate),
#       stroke = TRUE,
#       weight = .7,
#       # opacity = .6,
#       highlightOptions =
#         highlightOptions(
#           color = "#ff4a4a",
#           weight = 5,
#           bringToFront = TRUE
#           ),
#       ) %>%
#     addPolygons(
#       group = 'Accessible Housing Market',
#       label = ~tr_own_rate,
#       labelOptions = labelOptions(textsize = '12px'),
#       # fillOpacity. = .5,
#       color = ~colorQuantile("Blues", king$tr_own_rate, n = 5),
#       stroke = TRUE,
#       weight = .7,
#       opacity = .6,
#       highlightOptions =
#         highlightOptions(
#           color = "#ff4a4a",
#           weight = 5,
#           bringToFront = TRUE
#           ),
#       ) %>%
#   addLayersControl(
#     baseGroups = c(
#       'Accessible Rental Market',
#       'Accessible Housing Market'),
#     options = layersControlOptions(collapsed = FALSE))

#   }

#   map_it(king)
