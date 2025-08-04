# Debug script for the afford function
# This script helps identify and fix issues with the afford function

# Load required libraries
librarian::shelf(tidyverse, tidycensus, sf, tigris, scales, stringr)

# Check if Census API key is set
if (is.null(tidycensus::census_api_key())) {
  cat("WARNING: No Census API key found. Please set one using:\n")
  cat("tidycensus::census_api_key('YOUR_API_KEY_HERE', install = TRUE)\n")
  cat("You can get a free API key from: https://api.census.gov/data/key_signup.html\n\n")
}

# Function to test individual components
test_components <- function() {
  cat("Testing individual components...\n")

  # Test 1: Basic tidycensus call
  cat("1. Testing basic tidycensus call...\n")
  tryCatch({
    test_data <- tidycensus::get_acs(
      geography = "county",
      table = "B19001",
      state = "NJ",
      year = 2023,
      cache_table = TRUE
    )
    cat("✓ Basic tidycensus call successful\n")
  }, error = function(e) {
    cat("✗ Basic tidycensus call failed:", e$message, "\n")
  })

  # Test 2: County filtering
  cat("2. Testing county filtering...\n")
  tryCatch({
    test_data <- tidycensus::get_acs(
      geography = "county",
      table = "B19001",
      state = "NJ",
      year = 2023,
      cache_table = TRUE
    ) %>%
      dplyr::filter(GEOID %in% paste0("34", "001"))  # Atlantic County
    cat("✓ County filtering successful\n")
  }, error = function(e) {
    cat("✗ County filtering failed:", e$message, "\n")
  })

  # Test 3: Median income calculation
  cat("3. Testing median income calculation...\n")
  tryCatch({
    med_inc <- tidycensus::get_acs(
      geography = "tract",
      variables = "B19013_001",
      state = "NJ",
      county = "Atlantic",
      year = 2023,
      cache_table = TRUE
    )
    ami <- stats::median(med_inc$estimate, na.rm = TRUE)
    cat("✓ Median income calculation successful:", ami, "\n")
  }, error = function(e) {
    cat("✗ Median income calculation failed:", e$message, "\n")
  })
}

# Function to test the afford function step by step
test_afford_step_by_step <- function() {
  cat("\nTesting afford function step by step...\n")

  # Set parameters
  state <- "NJ"
  counties <- "Atlantic"
  ami_limit <- 0.8
  year <- 2023
  geometry <- FALSE

  cat("Parameters: state =", state, ", counties =", counties,
      ", ami_limit =", ami_limit, ", year =", year, "\n")

  # Test each major step
  tryCatch({
    cat("1. Getting income data...\n")
    income <- tidycensus::get_acs(
      geography = "county",
      table = "B19001",
      state = state,
      year = year,
      cache_table = TRUE
    ) %>%
      dplyr::filter(GEOID %in% paste0(state, counties)) %>%
      dplyr::left_join(tidycensus::load_variables(year, "acs5", cache = TRUE),
                       by = c("variable" = "name"))
    cat("✓ Income data retrieved\n")

    cat("2. Getting median income...\n")
    med_inc <- tidycensus::get_acs(
      geography = "tract",
      variables = "B19013_001",
      state = state,
      county = counties,
      year = year,
      cache_table = TRUE
    )
    cat("✓ Median income data retrieved\n")

    cat("3. Getting price data...\n")
    price <- dplyr::bind_rows(
      tidycensus::get_acs(
        geography = "tract",
        variables = c("B25085_001", "B25085_002", "B25085_003", "B25085_004", "B25085_005",
                      "B25085_006", "B25085_007", "B25085_008", "B25085_009", "B25085_010",
                      "B25085_011", "B25085_012", "B25085_013", "B25085_014", "B25085_015",
                      "B25085_016", "B25085_017", "B25085_018", "B25085_019", "B25085_020",
                      "B25085_021", "B25085_022", "B25085_023", "B25085_024"),
        state = state,
        county = counties,
        year = year,
        cache_table = TRUE
      ),
      tidycensus::get_acs(
        geography = "tract",
        variables = c("B25085_025", "B25085_026", "B25085_027"),
        state = state,
        county = counties,
        year = year,
        cache_table = TRUE
      )
    )
    cat("✓ Price data retrieved\n")

    cat("4. Calculating AMI...\n")
    ami <- stats::median(med_inc$estimate, na.rm = TRUE)
    cat("✓ AMI calculated:", ami, "\n")

    cat("All steps completed successfully!\n")

  }, error = function(e) {
    cat("✗ Error in step-by-step test:", e$message, "\n")
  })
}

# Run the tests
cat("=== AFFORD FUNCTION DEBUGGING ===\n\n")

# Test components
test_components()

# Test step by step
test_afford_step_by_step()

cat("\n=== DEBUGGING COMPLETE ===\n")
cat("If all tests pass, you can run the full afford function.\n")
cat("If tests fail, check your Census API key and internet connection.\n")