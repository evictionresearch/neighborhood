# The Neighborhood Package

An R package with various functions to study neighborhood dynamics. 

## Install

```
devtools::install_github("NeighborhoodResearch/neighborhood")
```

## Neighborhood Racial Typologies Function

The Neighborhood Racial Typologies function is a descriptive, categorical tool to help identify racial and ethnic divides within a region (e.g. city, county, state, etc.). Traditional segregation measures, such as the [disimilarity index](https://en.wikipedia.org/wiki/Index_of_dissimilarity), provide a single measure for a large geographical area. This descripitve function is useful for mapping tract level, small area (e.g. neighborhood level) divisions between ethnic and racial groups.  

This function is based off this paper:  
[Hall, Matthew, Kyle Crowder, and Amy Spring. 2015. “Neighborhood Foreclosures, Racial/Ethnic Transitions, and Residential Segregation.” American Sociological Review 80:526–549.](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4479290/)

1. `nt`: Neighborhood Racial Typologies
2. `ntdf`: create the dataframe for nt
3. `ntcheck`: identify counts that can be concatenated

### Example
You can view an interactive map using this function [here](https://evictions.study/maryland/maps/baltimore.html).

```
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

```
ntcheck(Baltimore_nt)
ntcheck(cal)
ntcheck(ny)
ntcheck(ps_nt)
```

The `nt_conc` field concatenates the `NeighType` field automatically and may satisfy most people. 
