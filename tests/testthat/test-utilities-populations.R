# Tests for utilities-populations.R
# Note: Many functions in this file are complex and require significant setup
# These tests focus on exported functions and basic validation

test_that("setupVirtualTwinPopConfig requires valid inputs", {
  expect_error(
    setupVirtualTwinPopConfig(
      projectConfiguration = projectConfiguration,
      dataObserved = NULL,
      groups = c("invalid", NA)
    )
  )
})

test_that("exportVirtualTwinPopulations validates populationNames", {
  # Test with character vector
  expect_silent(
    exportVirtualTwinPopulations(
      projectConfiguration = projectConfiguration,
      modelFile = list.files(projectConfiguration$modelFolder, pattern = ".pkml")[1],
      overwrite = FALSE,
      populationNames = character(0)
    )
  )
})

test_that("exportRandomPopulations handles empty population list", {
  # Should return silently when no populations to export
  expect_silent(
    exportRandomPopulations(
      projectConfiguration = projectConfiguration,
      populationNames = character(0),
      overwrite = FALSE
    )
  )
})

test_that("exportRandomPopulations validates customParameters", {
  # Invalid customParameter - not a list
  expect_error(
    exportRandomPopulations(
      projectConfiguration = projectConfiguration,
      populationNames = NULL,
      customParameters = "invalid",
      overwrite = FALSE
    )
  )
  
  # Invalid customParameter - missing required fields
  expect_error(
    exportRandomPopulations(
      projectConfiguration = projectConfiguration,
      populationNames = NULL,
      customParameters = list(list(wrongField = "value")),
      overwrite = FALSE
    )
  )
})

test_that("exportRandomPopulations handles valid customParameters structure", {
  # Valid structure should not error during validation
  # Create a custom parameter with proper structure
  customParam <- list(
    list(
      path = "Organism|Weight",
      values = "70"
    )
  )
  
  # Should not error during parameter validation
  # Using expect_silent to verify function completes without error when given empty population list
  expect_silent(
    exportRandomPopulations(
      projectConfiguration = projectConfiguration,
      populationNames = character(0),  # Empty list to avoid actual export
      customParameters = customParam,
      overwrite = FALSE
    )
  )
})
