# Setup script for testing the afford function
# This script installs and configures all necessary dependencies

cat("=== SETTING UP ENVIRONMENT FOR AFFORD FUNCTION TESTING ===\n\n")

# Install required packages if not already installed
required_packages <- c(
  "tidyverse",
  "tidycensus",
  "sf",
  "tigris",
  "scales",
  "stringr"
)

cat("Checking and installing required packages...\n")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg)
  } else {
    cat("✓", pkg, "already installed\n")
  }
}

# Load all packages
cat("\nLoading packages...\n")
library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(scales)
library(stringr)

cat("✓ All packages loaded successfully\n")

# Check Census API key
cat("\nChecking Census API key...\n")
if (is.null(tidycensus::census_api_key())) {
  cat("⚠️  No Census API key found!\n")
  cat("To get a free API key, visit: https://api.census.gov/data/key_signup.html\n")
  cat("Then set it using: tidycensus::census_api_key('YOUR_API_KEY_HERE', install = TRUE)\n")
} else {
  cat("✓ Census API key is set\n")
}

# Test basic functionality
cat("\nTesting basic functionality...\n")
tryCatch({
  # Test if we can access Census data
  test_data <- tidycensus::get_acs(
    geography = "county",
    variables = "B19013_001",
    state = "NJ",
    year = 2023
  )
  cat("✓ Basic Census data access successful\n")
}, error = function(e) {
  cat("✗ Basic Census data access failed:", e$message, "\n")
  cat("This might be due to missing API key or internet connection issues.\n")
})

cat("\n=== SETUP COMPLETE ===\n")
cat("Next steps:\n")
cat("1. If you don't have a Census API key, get one from the link above\n")
cat("2. Run: source('code/debug_afford.R') to test the function step by step\n")
cat("3. Run: source('code/test_afford.R') to run the full test\n")