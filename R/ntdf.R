#' Create segregation measure
#' @description Drawn from Hall, Matthew, Kyle Crowder, and Amy Spring. 2015. “Neighborhood Foreclosures, Racial/Ethnic Transitions, and Residential Segregation.” American Sociological Review 80:526–549.
#' @param state Study state or states
#' @param geography Default "tract", see tidycensus for options
#' @param county Study county or counties
#' @param geometry Download sf spatial feature. Default is FALSE
#' @param cache_table Default TRUE
#' @param output Default "wide", other option is "tidy"
#' @param year Default "NULL"
#' @param ... Other keyword arguments
#' @return A dataframe of racial segregated neighborhoods
#' @examples \dontrun{
#' Baltimore_nt <- ntdf(state = "MD", county = "Baltimore City", geometry = TRUE)
#' dplyr::glimpse(Baltimore_nt)
#' }
#' @export

ntdf <- function(
	state,
	geography = "tract",
	county = NULL,
	geometry = FALSE,
	cache_table = TRUE,
	output = "wide",
	year = 2019,
	...
){
race_vars <-
	c('totrace' = 'B03002_001',
	  'White' = 'B03002_003',
	  'Black' = 'B03002_004',
	  'Asian' = 'B03002_006',
	  'Latine' = 'B03002_012')

acs_data <-
	tidycensus::get_acs(
		geography = geography,
		variables = race_vars,
		state = state,
		county = county,
		geometry = geometry,
		cache_table = cache_table,
		output = output,
		year = year,
		cb = TRUE
		) %>%
	dplyr::select(-dplyr::ends_with("M")) %>%
	dplyr::group_by(GEOID) %>%
	dplyr::mutate(pWhite = WhiteE/totraceE,
		   pAsian = AsianE/totraceE,
		   pBlack = BlackE/totraceE,
		   pLatine = LatineE/totraceE,
		   pOther = (totraceE - sum(WhiteE, AsianE, BlackE, LatineE, na.rm = TRUE))/totraceE)

	acs_data %>%
	dplyr::group_by(GEOID) %>%
	dplyr::mutate(NeighType =
		dplyr::case_when(
			pWhite >=.9 ~ "All White",
			pBlack >=.9 ~ "All Black",
			pAsian >=.9 ~ "All Asian",
			pLatine >=.9 ~ "All Latine",
			pOther >=.9 ~ "All Other",
			pWhite < .9 & pBlack < .1 & pAsian < .1 & pLatine < .1 & pOther < .1 ~ "White-Shared",
			pBlack < .9 & pWhite < .1 & pAsian < .1 & pLatine < .1 & pOther < .1 ~ "Black-Shared",
			pAsian < .9 & pBlack < .1 & pWhite < .1 & pLatine < .1 & pOther < .1 ~ "Asian-Shared",
			pLatine < .9 & pBlack < .1 & pAsian < .1 & pWhite < .1 & pOther < .1 ~ "Latine-Shared",
			pOther < .9 & pBlack < .1 & pAsian < .1 & pLatine < .1 & pWhite < .1 ~ "Other-Shared",
			pWhite >=.1 & pWhite < .9 & pBlack >=.1 & pBlack < .9 & pAsian < .1 & pLatine < .1 & pOther < .1 ~ "Black-White",
			pWhite >=.1 & pWhite < .9 & pLatine >=.1 & pLatine < .9 & pAsian < .1 & pBlack < .1 & pOther < .1 ~ "Latine-White",
			pWhite >=.1 & pWhite < .9 & pAsian >=.1 & pAsian < .9 & pBlack < .1 & pLatine < .1 & pOther < .1 ~ "Asian-White",
			pWhite >= .1 & pWhite < .9 & pOther >=.1 & pOther < .9 & pBlack < .1 & pLatine < .1 & pAsian < .1 ~ "Other-White",
			pBlack >=.1 & pBlack < .9 & pOther >=.1 & pOther < .9 & pWhite < .1 & pLatine < .1 & pAsian < .1 ~ "Black-Other",
			pBlack >=.1 & pBlack < .9 & pLatine >=.1 & pLatine < .9 & pWhite < .1 & pAsian < .1 & pOther < .1 ~ "Black-Latine",
			pAsian >=.1 & pAsian < .9 & pBlack >=.1 & pBlack < .9 & pWhite < .1 & pLatine < .1 & pOther < .1 ~ "Asian-Black",
			pAsian >=.1 & pAsian < .9 & pLatine >=.1 & pLatine < .9 & pWhite < .1 & pBlack < .1 & pOther < .1 ~ "Asian-Latine",
			pAsian >= .1 & pAsian < .9 & pOther >=.1 & pOther < .9 & pWhite < .1 & pLatine < .1 & pBlack < .1 ~ "Asian-Other",
			pLatine >= .1 & pLatine < .9 & pOther >=.1 & pOther < .9 & pWhite < .1 & pAsian < .1 & pBlack < .1 ~ "Latine-Other",
			pBlack >=.1 & pAsian >=.1 & pLatine >=.1 & pWhite < .1 & pOther < .1 ~ "Black-Asian-Latine",
			pBlack >=.1 & pAsian >=.1 & pOther >=.1 & pWhite < .1 & pLatine < .1 ~ "Black-Asian-Other",
			pBlack >=.1 & pLatine >=.1 & pOther >=.1 & pWhite < .1 & pAsian < .1 ~ "Black-Latine-Other",
			pAsian >=.1 & pLatine >=.1 & pOther >=.1 & pWhite < .1 & pBlack < .1 ~ "Asian-Latine-Other",
			pWhite >=.1 & pBlack >=.1 & pAsian >=.1 & pLatine < .1 & pOther < .1 ~ "Black-Asian-White",
			pWhite >=.1 & pBlack >=.1 & pLatine >=.1 & pAsian < .1 & pOther < .1 ~ "Black-Latine-White",
			pWhite >=.1 & pBlack >=.1 & pOther >=.1 & pLatine < .1 & pAsian < .1 ~ "Black-Other-White",
			pWhite >=.1 & pAsian >=.1 & pLatine >=.1 & pBlack < .1 & pOther < .1 ~ "Asian-Latine-White",
			pWhite >=.1 & pAsian >=.1 & pOther >=.1 & pLatine < .1 & pBlack < .1 ~ "Asian-Other-White",
			pWhite >=.1 & pLatine >=.1 & pOther >=.1 & pAsian < .1 & pBlack < .1 ~ "Latine-Other-White",
			pWhite >=.1 & pLatine >=.1 & pOther >=.1 & pAsian < .1 & pBlack < .1 ~ "Latine-Other-White",
			pBlack >=.1 & pAsian >=.1 & pLatine >=.1 & pOther >=.1 & pWhite < .1 ~ "Black-Asian-Latine-Other",
			pWhite >=.1 & pAsian >=.1 & pLatine >=.1 & pOther >=.1 & pBlack < .1 ~ "Asian-Latine-Other-White",
			pWhite >=.1 & pBlack >=.1 & pLatine >=.1 & pOther >=.1 & pAsian < .1 ~ "Black-Latine-Other-White",
			pWhite >=.1 & pBlack >=.1 & pAsian >=.1 & pOther >=.1 & pLatine < .1 ~ "Black-Asian-Other-White",
			pWhite >=.1 & pBlack >=.1 & pAsian >=.1 & pLatine >=.1 & pOther < .1 ~ "Black-Asian-Latine-White",
			pWhite >=.1 & pWhite <=.7 & pBlack >=.1 & pAsian >=.1 & pLatine >=.1 & pOther >=.1 ~ "Diverse",
			totraceE == 0 ~ "Unpopulated Tract"
			)
	) %>%
	dplyr::ungroup() %>%
	dplyr::mutate(nt_conc =
		dplyr::case_when(
			NeighType == "Black-Asian-Latine-Other" ~ "4 Group Mixed",
			NeighType == "Asian-Latine-Other-White" ~ "4 Group Mixed",
			NeighType == "Black-Latine-Other-White" ~ "4 Group Mixed",
			NeighType == "Black-Asian-Other-White" ~ "4 Group Mixed",
			NeighType == "Black-Asian-Latine-White" ~ "4 Group Mixed",
			NeighType == "Black-Asian-Latine" ~ "3 Group Mixed",
			NeighType == "Black-Asian-Other" ~ "3 Group Mixed",
			NeighType == "Black-Latine-Other" ~ "3 Group Mixed",
			NeighType == "Asian-Latine-Other" ~ "3 Group Mixed",
			NeighType == "Black-Asian-White" ~ "3 Group Mixed",
			NeighType == "Black-Latine-White" ~ "3 Group Mixed",
			NeighType == "Black-Other-White" ~ "3 Group Mixed",
			NeighType == "Asian-Latine-White" ~ "3 Group Mixed",
			NeighType == "Asian-Other-White" ~ "3 Group Mixed",
			NeighType == "Latine-Other-White" ~ "3 Group Mixed",
			NeighType == "Latine-Other-White" ~ "3 Group Mixed",
			NeighType == "All White" ~ "Mostly White",
			NeighType == "All Black" ~ "Mostly Black",
			NeighType == "All Asian" ~ "Mostly Asian",
			NeighType == "All Latine" ~ "Mostly Latine",
			NeighType == "All Other" ~ "Mostly Other",
			NeighType == "White-Shared" ~ "Mostly White",
			NeighType == "Black-Shared" ~ "Mostly Black",
			NeighType == "Asian-Shared" ~ "Mostly Asian",
			NeighType == "Latine-Shared" ~ "Mostly Latine",
			NeighType == "Other-Shared" ~ "Mostly Other",
			TRUE ~ NeighType
		)
	) %>%
	dplyr::mutate(nt_conc =
		factor(nt_conc,
			levels = c(
				"Mostly Asian",
				"Mostly Black",
				"Mostly Latine",
				"Mostly Other",
				"Mostly White",

				"Asian-Black",
				"Asian-Latine",
				"Asian-Other",
				"Asian-White",

				"Black-Latine",
				"Black-Other",
				"Black-White",

				"Latine-Other",
				"Latine-White",

				"Other-White",
				"3 Group Mixed",
				"4 Group Mixed",
				"Diverse",
				"Unpopulated Tract"
				)
		)
	)

}
