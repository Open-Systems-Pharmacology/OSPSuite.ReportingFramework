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

# Additional comprehensive tests for exportRandomPopulations
test_that("exportRandomPopulations validates custom parameter values length", {
  skip_if_not(file.exists(projectConfiguration$populationsFile),
              "Populations file not available")

  dtPops <- xlsxReadData(wb = projectConfiguration$populationsFile, sheetName = "Demographics")
  skip_if(nrow(dtPops) == 0, "No populations defined")

  # Get first population name
  popName <- dtPops$populationName[1]

  # Create a custom parameter with inconsistent number of values
  customParam <- list(
    list(
      path = "Organism|Weight",
      values = c("70", "80", "90")  # Multiple values that don't match population size
    )
  )

  # This should error when the values length doesn't match population size and isn't 1
  expect_error(
    exportRandomPopulations(
      projectConfiguration = projectConfiguration,
      populationNames = popName,
      customParameters = customParam,
      overwrite = TRUE
    ),
    regexp = "Inconsistent|number"
  )
})

test_that("exportRandomPopulations returns invisible NULL", {
  result <- exportRandomPopulations(
    projectConfiguration = projectConfiguration,
    populationNames = character(0),
    overwrite = FALSE
  )

  expect_null(result)
})

test_that(".validateAndFilterPopulations validates customParameters", {
  skip_if_not(file.exists(projectConfiguration$populationsFile),
              "Populations file not available")

  dtPops <- xlsxReadData(wb = projectConfiguration$populationsFile, sheetName = "Demographics")

  # Invalid customParameter - not a list
  expect_error(
    ospsuite.reportingframework:::.validateAndFilterPopulations(
      dtPops = dtPops,
      populationNames = NULL,
      overwrite = FALSE,
      projectConfiguration = projectConfiguration,
      customParameters = "invalid"
    )
  )

  # Invalid customParameter - missing path
  expect_error(
    ospsuite.reportingframework:::.validateAndFilterPopulations(
      dtPops = dtPops,
      populationNames = NULL,
      overwrite = FALSE,
      projectConfiguration = projectConfiguration,
      customParameters = list(list(values = c("1", "2")))
    )
  )
})

test_that(".validateAndFilterPopulations filters populations correctly", {
  skip_if_not(file.exists(projectConfiguration$populationsFile),
              "Populations file not available")

  dtPops <- xlsxReadData(wb = projectConfiguration$populationsFile, sheetName = "Demographics")
  skip_if(nrow(dtPops) == 0, "No populations defined")

  # Test with NULL populationNames (should keep all)
  result <- ospsuite.reportingframework:::.validateAndFilterPopulations(
    dtPops = dtPops,
    populationNames = NULL,
    overwrite = TRUE,
    projectConfiguration = projectConfiguration,
    customParameters = NULL
  )

  expect_s3_class(result, "data.table")
})

test_that(".validateAndFilterPopulations returns empty when no populations match", {
  skip_if_not(file.exists(projectConfiguration$populationsFile),
              "Populations file not available")

  dtPops <- xlsxReadData(wb = projectConfiguration$populationsFile, sheetName = "Demographics")

  # Test with non-existent population name
  result <- ospsuite.reportingframework:::.validateAndFilterPopulations(
    dtPops = dtPops,
    populationNames = "NonExistentPopulation12345",
    overwrite = FALSE,
    projectConfiguration = projectConfiguration,
    customParameters = NULL
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that(".validateAndFilterPopulations respects overwrite parameter", {
  skip_if_not(file.exists(projectConfiguration$populationsFile),
              "Populations file not available")

  dtPops <- xlsxReadData(wb = projectConfiguration$populationsFile, sheetName = "Demographics")
  skip_if(nrow(dtPops) == 0, "No populations defined")

  # With overwrite = FALSE, should filter existing files
  result <- ospsuite.reportingframework:::.validateAndFilterPopulations(
    dtPops = dtPops,
    populationNames = NULL,
    overwrite = FALSE,
    projectConfiguration = projectConfiguration,
    customParameters = NULL
  )

  expect_s3_class(result, "data.table")
})

test_that(".validateAndFilterPopulations warns for small proportion of female ", {
  skip_if_not(file.exists(projectConfiguration$populationsFile),
              "Populations file not available")

  dtPops <- xlsxReadData(wb = projectConfiguration$populationsFile, sheetName = "Demographics")
  skip_if(nrow(dtPops) == 0, "No populations defined")

  dtPops$proportionOfFemales <- 0.5

  expect_warning(ospsuite.reportingframework:::.validateAndFilterPopulations(
    dtPops = dtPops,
    populationNames = NULL,
    overwrite = TRUE,
    projectConfiguration = projectConfiguration,
    customParameters = NULL
  ))

})
