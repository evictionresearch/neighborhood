# Testing the Afford Function

This directory contains scripts to test and debug the `afford` function from the `neighborhood` package.

## Quick Start

1. **Set up your environment:**
   ```r
   source("dev/setup.R")
   ```

2. **Get a Census API key** (if you don't have one):
   - Visit: https://api.census.gov/data/key_signup.html
   - Get a free API key
   - Set it in R: `tidycensus::census_api_key("YOUR_API_KEY_HERE", install = TRUE)`

3. **Test the function step by step:**
   ```r
   source("dev/debug_afford.R")
   ```

4. **Run the full test:**
   ```r
   source("dev/test_afford.R")
   ```

## Files Explained

### `setup.R`
- Installs and loads all required packages
- Checks if Census API key is set
- Tests basic Census data access

### `debug_afford.R`
- Tests individual components of the `afford` function
- Runs step-by-step debugging
- Identifies where issues occur

### `test_afford.R`
- Runs the full `afford` function with different parameters
- Tests both with and without geometry
- Shows results and statistics

## Common Issues and Solutions

### Error: "could not find function '%>%'"
**Solution:** Load the tidyverse package:
```r
library(tidyverse)
```

### Error: "No Census API key found"
**Solution:** Get and set a Census API key:
```r
tidycensus::census_api_key("YOUR_API_KEY_HERE", install = TRUE)
```

### Error: "Internet connection issues"
**Solution:** Check your internet connection and try again.

### Error: "Package not found"
**Solution:** Install missing packages:
```r
install.packages(c("tidyverse", "tidycensus", "sf", "tigris", "scales", "stringr"))
```

## Testing Different Scenarios

### Single County Test
```r
atlantic_co <- afford("NJ", "Atlantic", 0.8, year = 2023, geometry = FALSE)
```

### Multiple Counties Test
```r
nj_counties <- c("Atlantic", "Cape May", "Cumberland")
nj_multi <- afford("NJ", nj_counties, 0.5, year = 2023, geometry = FALSE)
```

### With Geometry Test
```r
atlantic_co_sf <- afford("NJ", "Atlantic", 0.8, year = 2023, geometry = TRUE)
```

## Expected Output

The `afford` function should return a data frame with columns like:
- `GEOID`: Census tract identifier
- `tr_rent_accessible`: Number of accessible rental units
- `tr_own_accessible`: Number of accessible owned units
- `tr_rent_supply`: Proportion of rental units that are accessible
- `tr_own_supply`: Proportion of owned units that are accessible
- `tr_rent_rate`: Rate per 100,000 households
- `tr_own_rate`: Rate per 100,000 households
- `popup`: HTML popup text for mapping

## Troubleshooting

If you encounter issues:

1. **Run the setup script first** to ensure all dependencies are installed
2. **Check your Census API key** is properly set
3. **Test with a simple case** (single county, no geometry) first
4. **Check your internet connection** as the function downloads Census data
5. **Look at the debug output** to identify where the issue occurs

## Notes

- The function downloads data from the US Census Bureau, so it requires internet access
- Some Census variables may not be available for all years or geographies
- The function caches data to improve performance on subsequent runs
- Geometry operations require the `sf` package and can be slower than non-geometry operations