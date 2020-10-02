# ==========================================================================
# Neighborhood Typology
# By: Tim Thomas - timothyathomas@gmail.com
# Measures drawn from:
# 	Hall, Matthew, Kyle Crowder, and Amy Spring. 2015. “Neighborhood
# 		Foreclosures, Racial/Ethnic Transitions, and Residential
# 		Segregation.” American Sociological Review 80:526–549.
#
# Process:
# 	1. Create a dataframe with the proportions for White, Black, Asian,
# 		Latinx, and Other.
# 	2. Run the `nt` function on that dataset to create a `NeighType` field
# 	3. Look at your neighborhood typology frequency and consider merging
# 		similar, or low frequency, neighborhoods into one or more common
# 		categories (e.g., some of the four category cases could be considered
# 		white-mixed--see the article to see how the authors consolidated
# 		categories).
# ==========================================================================

# ==========================================================================
# NT Formula
# ==========================================================================

nt <- function(
	df, 
	GEOID = "GEOID", 
	totraceE = "toteraceE", 
	pWhite = "pWhite", 
	pBlack = "pBlack", 
	pAsian = "pAsian", 
	pLatinx = "pLatinx", 
	pOther = "pOther", 
	NeighType = "NeighType",
	nt_conc = "nt_conc"
	){

	df %>%
	dplyr::group_by(GEOID) %>%
			# create neighborhood typology
	dplyr::mutate(NeighType =
		dplyr::case_when(
			pWhite >=.9 ~ "All White",
			pBlack >=.9 ~ "All Black",
			pAsian >=.9 ~ "All Asian",
			pLatinx >=.9 ~ "All Latinx",
			pOther >=.9 ~ "All Other",

			# pBlack >=.7 ~ "Mostly Black",
			# pAsian >=.7 ~ "Mostly Asian",
			# pLatinx >=.7 ~ "Mostly Latinx",
			# pOther >=.7 ~ "Mostly Other",

			pWhite < .9 & pBlack < .1 & pAsian < .1 & pLatinx < .1 & pOther < .1 ~ "White-Shared",
			pBlack < .9 & pWhite < .1 & pAsian < .1 & pLatinx < .1 & pOther < .1 ~ "Black-Shared",
			pAsian < .9 & pBlack < .1 & pWhite < .1 & pLatinx < .1 & pOther < .1 ~ "Asian-Shared",
			pLatinx < .9 & pBlack < .1 & pAsian < .1 & pWhite < .1 & pOther < .1 ~ "Latinx-Shared",
			pOther < .9 & pBlack < .1 & pAsian < .1 & pLatinx < .1 & pWhite < .1 ~ "Other-Shared",

			pWhite >=.1 & pWhite < .9 & pBlack >=.1 & pBlack < .9 & pAsian < .1 & pLatinx < .1 & pOther < .1 ~ "Black-White",
			pWhite >=.1 & pWhite < .9 & pLatinx >=.1 & pLatinx < .9 & pAsian < .1 & pBlack < .1 & pOther < .1 ~ "Latinx-White",
			pWhite >=.1 & pWhite < .9 & pAsian >=.1 & pAsian < .9 & pBlack < .1 & pLatinx < .1 & pOther < .1 ~ "Asian-White",
			pWhite >= .1 & pWhite < .9 & pOther >=.1 & pOther < .9 & pBlack < .1 & pLatinx < .1 & pAsian < .1 ~ "Other-White",

			pBlack >=.1 & pBlack < .9 & pOther >=.1 & pOther < .9 & pWhite < .1 & pLatinx < .1 & pAsian < .1 ~ "Black-Other",
			pBlack >=.1 & pBlack < .9 & pLatinx >=.1 & pLatinx < .9 & pWhite < .1 & pAsian < .1 & pOther < .1 ~ "Black-Latinx",

			pAsian >=.1 & pAsian < .9 & pBlack >=.1 & pBlack < .9 & pWhite < .1 & pLatinx < .1 & pOther < .1 ~ "Asian-Black",
			pAsian >=.1 & pAsian < .9 & pLatinx >=.1 & pLatinx < .9 & pWhite < .1 & pBlack < .1 & pOther < .1 ~ "Asian-Latinx",
			pAsian >= .1 & pAsian < .9 & pOther >=.1 & pOther < .9 & pWhite < .1 & pLatinx < .1 & pBlack < .1 ~ "Asian-Other",

			pLatinx >= .1 & pLatinx < .9 & pOther >=.1 & pOther < .9 & pWhite < .1 & pAsian < .1 & pBlack < .1 ~ "Latinx-Other",

			pBlack >=.1 & pAsian >=.1 & pLatinx >=.1 & pWhite < .1 & pOther < .1 ~ "Black-Asian-Latinx",
			pBlack >=.1 & pAsian >=.1 & pOther >=.1 & pWhite < .1 & pLatinx < .1 ~ "Black-Asian-Other",
			pBlack >=.1 & pLatinx >=.1 & pOther >=.1 & pWhite < .1 & pAsian < .1 ~ "Black-Latinx-Other",

			pAsian >=.1 & pLatinx >=.1 & pOther >=.1 & pWhite < .1 & pBlack < .1 ~ "Asian-Latinx-Other",

			pWhite >=.1 & pBlack >=.1 & pAsian >=.1 & pLatinx < .1 & pOther < .1 ~ "Black-Asian-White",
			pWhite >=.1 & pBlack >=.1 & pLatinx >=.1 & pAsian < .1 & pOther < .1 ~ "Black-Latinx-White",
			pWhite >=.1 & pBlack >=.1 & pOther >=.1 & pLatinx < .1 & pAsian < .1 ~ "Black-Other-White",
			pWhite >=.1 & pAsian >=.1 & pLatinx >=.1 & pBlack < .1 & pOther < .1 ~ "Asian-Latinx-White",
			pWhite >=.1 & pAsian >=.1 & pOther >=.1 & pLatinx < .1 & pBlack < .1 ~ "Asian-Other-White",
			pWhite >=.1 & pLatinx >=.1 & pOther >=.1 & pAsian < .1 & pBlack < .1 ~ "Latinx-Other-White",
			pWhite >=.1 & pLatinx >=.1 & pOther >=.1 & pAsian < .1 & pBlack < .1 ~ "Latinx-Other-White",

			pBlack >=.1 & pAsian >=.1 & pLatinx >=.1 & pOther >=.1 & pWhite < .1 ~ "Black-Asian-Latinx-Other",
			pWhite >=.1 & pAsian >=.1 & pLatinx >=.1 & pOther >=.1 & pBlack < .1 ~ "Asian-Latinx-Other-White",
			pWhite >=.1 & pBlack >=.1 & pLatinx >=.1 & pOther >=.1 & pAsian < .1 ~ "Black-Latinx-Other-White",
			pWhite >=.1 & pBlack >=.1 & pAsian >=.1 & pOther >=.1 & pLatinx < .1 ~ "Black-Asian-Other-White",
			pWhite >=.1 & pBlack >=.1 & pAsian >=.1 & pLatinx >=.1 & pOther < .1 ~ "Black-Asian-Latinx-White",

			pWhite >=.1 & pWhite <=.7 & pBlack >=.1 & pAsian >=.1 & pLatinx >=.1 & pOther >=.1 ~ "Diverse",
			totraceE == 0 ~ "Unpopulated Tract")) %>%
		dplyr::ungroup() %>% 
		dplyr::mutate(nt_conc = 
			factor(
				dplyr::case_when(
					NeighType == "Black-Asian-Latinx-Other" ~ "4 Group Mixed", 
					NeighType == "Asian-Latinx-Other-White" ~ "4 Group Mixed", 
					NeighType == "Black-Latinx-Other-White" ~ "4 Group Mixed", 
					NeighType == "Black-Asian-Other-White" ~ "4 Group Mixed", 
					NeighType == "Black-Asian-Latinx-White" ~ "4 Group Mixed", 
					NeighType == "Black-Asian-Latinx" ~ "3 Group Mixed",
					NeighType == "Black-Asian-Other" ~ "3 Group Mixed",
					NeighType == "Black-Latinx-Other" ~ "3 Group Mixed",
					NeighType == "Asian-Latinx-Other" ~ "3 Group Mixed",
					NeighType == "Black-Asian-White" ~ "3 Group Mixed",
					NeighType == "Black-Latinx-White" ~ "3 Group Mixed",
					NeighType == "Black-Other-White" ~ "3 Group Mixed",
					NeighType == "Asian-Latinx-White" ~ "3 Group Mixed",
					NeighType == "Asian-Other-White" ~ "3 Group Mixed",
					NeighType == "Latinx-Other-White" ~ "3 Group Mixed",
					NeighType == "Latinx-Other-White" ~ "3 Group Mixed",
					NeighType == "All White" ~ "Mostly White",
					NeighType == "All Black" ~ "Mostly Black",
					NeighType == "All Asian" ~ "Mostly Asian",
					NeighType == "All Latinx" ~ "Mostly Latinx",
					NeighType == "All Other" ~ "Mostly Other",
					NeighType == "White-Shared" ~ "Mostly White",
					NeighType == "Black-Shared" ~ "Mostly Black",
					NeighType == "Asian-Shared" ~ "Mostly Asian",
					NeighType == "Latinx-Shared" ~ "Mostly Latinx",
					NeighType == "Other-Shared" ~ "Mostly Other", 
					TRUE ~ NeighType
				), 
			levels = c(
				"Mostly White",
				"Mostly Asian",
				"Mostly Latinx",								
				"Mostly Black",
				"Mostly Other",
				"Asian-White",				
				"Latinx-White",
				"Black-White",				
				"Other-White",
				"Asian-Black",
				"Asian-Latinx",
				"Asian-Other",
				"Latinx-Other",
				"Black-Other",
				"Black-Latinx",
				"3 Group Mixed", 
				"4 Group Mixed", 
				"Diverse", 
				"Unpopulated Tract"
				)
			)
		)
	}

# ==========================================================================
# Create data frame
# ==========================================================================

ntdf <- function(
	state, 
	geography = "tract", 
	county = NULL, 
	geometry = FALSE, 
	cache_table = TRUE, 
	output = "wide", 
	year = 2018, 
	GEOID = "GEOID",
	WhiteE = "WhiteE",
	AsianE = "AsianE",
	BlackE = "BlackE",
	LatinxE = "LatinxE",
	totraceE = "totraceE"
){

race_vars <-
	c('totrace' = 'B03002_001',
	  'White' = 'B03002_003',
	  'Black' = 'B03002_004',
	  'Asian' = 'B03002_006',
	  'Latinx' = 'B03002_012')

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
		   pLatinx = LatinxE/totraceE,
		   pOther = (totraceE - sum(WhiteE, AsianE, BlackE, LatinxE, na.rm = TRUE))/totraceE)

 nt(df = acs_data)

}

# ==========================================================================
# Check nt categories
# ==========================================================================

ntcheck <- function(df, NeighType = "NeighType", n = "n")
	df %>% dplyr::group_by(NeighType) %>% dplyr::count() %>% dplyr::arrange(dplyr::desc(n)) %>% data.frame()