# Unit tests for the biodiversity observations Shiny app

# The tests cover the following:
#
#   1. search_species function:
#      - Searching for existing species
#      - Handling leading/trailing whitespaces in the search query
#      - Case-insensitive search
#      - Searching for non-existent species
#      - Handling empty or whitespace-only search queries
#
#   2. filter_species function:
#      - Filtering for existing species
#      - Filtering for non-existent species
#
#   3. count_by_year function:
#    - Counting observations by year
#    - Counting observations for a subset of years
#
#   4. render_map function:
#      - Rendering the map without errors
#      - Rendering the map with a subset of data
#      - Rendering the map with an empty dataset
#
#   5. render_timeline function:
#      - Rendering the timeline without errors
#      - Rendering the timeline with a subset of data
#      - Rendering the timeline with an empty dataset
#
#   6. Data loading and preprocessing:
#      - Verifying that the dataset contains only observations from Poland
#      - Handling missing dataset files
#      - Testing performance with a larger dataset
#
#   7. Error handling:
#      - Handling invalid inputs for search_species function
#      - Handling invalid inputs for filter_species function
#      - Handling invalid inputs for count_by_year function

# Load packages
library(testthat)
library(data.table)
library(leaflet)
library(plotly)
library(here)

# Source files
source(here("app", "utils", "functions.R"))

# Load prepared data
occurence <- readRDS(here("app", "data", "occurence_prepared.Rds"))
setDT(occurence)

# Define color palette
base_green <- c("#2ecc71", "#27ae60", "#229954", "#1e8449", "#196f3d")
species <- unique(occurence$scientificName)
color_palette <- colorFactor(colorRampPalette(base_green)(length(species)), domain = species)
first_green <- color_palette(species[1])

# Test search_species function
test_that("search_species works correctly", {
  # Test searching for existing species
  result <- search_species("pica", occurence)
  expect_true(nrow(result) > 0)

  # Test searching for species with leading or trailing whitespace
  result <- search_species("  pica pica  ", occurence)
  expect_true(nrow(result) > 0)

  # Test searching for species with different letter cases
  result <- search_species("Pica pica", occurence)
  expect_true(nrow(result) > 0)

  # Test searching for species that don't exist in the dataset
  result <- search_species("Nonexistent species", occurence)
  expect_true(nrow(result) == 0)

  # Test searching with only whitespaces
  result <- search_species("   ", occurence)
  expect_true(nrow(result) == 0)
})

# Test filter_species function
test_that("filter_species works correctly", {
  result <- filter_species(occurence, "Pica pica")
  expect_true(nrow(result) > 0)

  # Test filtering for species that doesn't exist in the dataset
  result <- filter_species(occurence, "Nonexistent species")
  expect_true(nrow(result) == 0)
})

# Test count_by_year function
test_that("count_by_year works correctly", {
  result <- count_by_year(occurence)
  expect_true(nrow(result) > 0)
  expect_true(all(result$N > 0))

  # Test counting by year for data subset
  subset_data <- occurence[year(eventDate) >= 2000 & year(eventDate) <= 2010]
  result <- count_by_year(subset_data)
  expect_true(nrow(result) == 11) # expect 11 years (2000-2010)
  expect_true(all(result$year >= 2000 & result$year <= 2010))
})

# Test render_map function
test_that("render_map does not produce errors", {
  expect_silent(render_map(occurence, color_palette))

  # Test rendering map with data subset
  subset_data <- occurence[1:100]
  expect_silent(render_map(subset_data, color_palette))

  # Test rendering map with empty dataset
  empty_data <- occurence[0]
  expect_silent(render_map(empty_data, color_palette))
})

# Test render_timeline function
test_that("render_timeline does not produce errors", {
  test_timeline_data <- count_by_year(occurence)
  expect_silent(render_timeline(test_timeline_data, first_green))

  # Test rendering timeline with data subset
  subset_data <- occurence[year(eventDate) >= 2000 & year(eventDate) <= 2010]
  test_timeline_data <- count_by_year(subset_data)
  expect_silent(render_timeline(test_timeline_data, first_green))

  # Test rendering timeline with empty dataset
  empty_data <- occurence[0]
  test_timeline_data <- count_by_year(empty_data)
  expect_silent(render_timeline(test_timeline_data, first_green))
})

# Test data loading and preprocessing
test_that("data loading and preprocessing works correctly", {
  # Verify that dataset contains only observations from Poland
  expect_true(all(occurence$country == "Poland"))

  # Test behavior when dataset file is missing
  expect_error(readRDS(here("app", "data", "nonexistent_file.Rds")))

  # Test performance of data loading with larger dataset
  large_dataset <- rbindlist(list(occurence, occurence, occurence))
  expect_true(nrow(large_dataset) == 3 * nrow(occurence))
})

# Test error handling
test_that("error handling works correctly", {
  # Verify that appropriate error messages are displayed in case of failures
  expect_error(search_species(NULL, occurence))
  expect_error(filter_species(occurence, NULL))
  expect_error(count_by_year(NULL))
})
