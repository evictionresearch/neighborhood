<!-- Read this: 
https://www.r-bloggers.com/2022/09/the-package-learning-how-to-build-an-r-package/

library(pkgdown)
usethis::use_pkgdown()
pkgdown::build_site()
usethis::use_pkgdown_github_pages() -->
# The Neighborhood Package

[![DOI](https://zenodo.org/badge/300691126.svg)](https://zenodo.org/doi/10.5281/zenodo.10642018)


An R package with various functions to study neighborhood dynamics. 

## Install

``` r
devtools::install_github("evictionresearch/neighborhood", ref = "main")
```

## Neighborhood Racial Typologies Function

The Neighborhood Racial Typologies function is a descriptive, categorical tool to help identify racial and ethnic divides within a region (e.g. city, county, state, etc.). Traditional segregation measures, such as the [disimilarity index](https://en.wikipedia.org/wiki/Index_of_dissimilarity), provide a single measure for a large geographical area. This descripitve function is useful for mapping tract level, small area (e.g. neighborhood level) divisions between ethnic and racial groups.  

The neighborhood typology is designated when a tract's racial/ethnic group share is more than 10%. For example, if a tract is 60% white, 30% Asian, 5% Black, and 5% Latinx then that tract will be called a majority Asian-White tract. This definition can be found in the `NeighType` field.  

Because there are so many different combinations, I created the `nt_conc` field to show concatinated fields of 1, 2, or more groups. 


#### Data download
All U.S. tract definitions can be downloaded in the [`data`](https://github.com/evictionresearch/neighborhood/tree/main/data) directory above. 

#### Credits
This function is based off this paper:  
[Hall, Matthew, Kyle Crowder, and Amy Spring. 2015. “Neighborhood Foreclosures, Racial/Ethnic Transitions, and Residential Segregation.” American Sociological Review 80:526–549.](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4479290/)

1. `nt`: Neighborhood Racial Typologies
2. `ntdf`: create the dataframe for nt
3. `ntcheck`: identify counts that can be concatenated

### Example
You can view an interactive map using this function [here](https://evictionresearch.net/maryland/maps/baltimore.html). Choose the "Neighborhood Segregation" layer

``` r
Baltimore_nt <- ntdf(state = "MD", county = "Baltimore City", geometry = TRUE)
cal <- ntdf(state = "CA")
ny <- ntdf(state = "NY")
glimpse(Baltimore_nt)

ps_nt <- ntdf(state = "WA", county = c("Snohomish", "King", "Pierce"), geometry = TRUE)


# Check to see if there are duplicate tract assumptions. 
Baltimore_nt %>% 
st_set_geometry(NULL) %>% 
mutate(val = 1) %>%
spread(NeighType, val, fill = 0) %>% 
mutate_at(vars(`All Black`:`White-Shared`), list(as.numeric)) %>% 
select(13:ncol(.)) %>% 
mutate(rowsum = rowSums(.)) %>% 
filter(rowsum > 1) %>% 
glimpse()
```

### Concatenation
After running the above code, look at the counts and consider concatenating and/or reducing outlying (small count) neighborhood types. 

``` r
ntcheck(Baltimore_nt)
ntcheck(cal)
ntcheck(ny)
ntcheck(ps_nt)
```

The `nt_conc` field concatenates the `NeighType` field automatically and may satisfy most people. 

## Get County and PUMA cross sections
The `get_co_puma` function defines the county associated with a PUMA. In some cases, multiple PUMAs fall within one county, such as in urban areas. In other situations, multiple counties may fall within one PUMA, such as in rural situations. Tracts nest within PUMAs so this
