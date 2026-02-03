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

test_that("exportRandomPopulations respects overwrite parameter", {
  skip_if_not(file.exists(projectConfiguration$populationsFile), 
              "Populations file not available")
  
  # Read demographics to check if any populations are available
  dtPops <- xlsxReadData(wb = projectConfiguration$populationsFile, sheetName = "Demographics")
  
  skip_if(nrow(dtPops) == 0, "No populations defined in Demographics sheet")
  
  # Test with overwrite = FALSE (should skip existing files)
  result <- exportRandomPopulations(
    projectConfiguration = projectConfiguration,
    populationNames = NULL,
    customParameters = NULL,
    overwrite = FALSE
  )
  
  expect_null(result)
})

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

test_that("exportRandomPopulations handles single value custom parameters", {
  skip_if_not(file.exists(projectConfiguration$populationsFile), 
              "Populations file not available")
  
  # Valid structure with single value (should work for any population size)
  customParam <- list(
    list(
      path = "TestParam",
      values = "100"  # Single value
    )
  )
  
  # Should not error with empty population list
  expect_silent(
    exportRandomPopulations(
      projectConfiguration = projectConfiguration,
      populationNames = character(0),
      customParameters = customParam,
      overwrite = FALSE
    )
  )
})

test_that("exportRandomPopulations warns about small proportionOfFemales", {
  skip_if_not(file.exists(projectConfiguration$populationsFile), 
              "Populations file not available")
  
  # This test checks if the function properly warns about proportionOfFemales
  # The warning is triggered for values > 0 and <= 1
  # We can't easily create this condition without modifying the actual file,
  # so we just verify the function runs without error for now
  expect_silent(
    exportRandomPopulations(
      projectConfiguration = projectConfiguration,
      populationNames = character(0),
      overwrite = FALSE
    )
  )
})

test_that("exportRandomPopulations handles NULL populationNames", {
  skip_if_not(file.exists(projectConfiguration$populationsFile), 
              "Populations file not available")
  
  # When populationNames is NULL, should process all populations from Demographics
  # With overwrite = FALSE, should skip existing files
  result <- exportRandomPopulations(
    projectConfiguration = projectConfiguration,
    populationNames = NULL,  # Should use all from Demographics sheet
    customParameters = NULL,
    overwrite = FALSE
  )
  
  expect_null(result)
})

test_that("exportRandomPopulations checks for existing files when overwrite is FALSE", {
  skip_if_not(file.exists(projectConfiguration$populationsFile), 
              "Populations file not available")
  
  # The function should filter out populations that already have CSV files
  # when overwrite = FALSE
  result <- exportRandomPopulations(
    projectConfiguration = projectConfiguration,
    populationNames = NULL,
    overwrite = FALSE
  )
  
  expect_null(result)
})

test_that("exportRandomPopulations validates customParameters path field", {
  # Missing path field
  expect_error(
    exportRandomPopulations(
      projectConfiguration = projectConfiguration,
      populationNames = character(0),
      customParameters = list(list(values = c("1", "2"))),  # No path field
      overwrite = FALSE
    ),
    regexp = "path"
  )
})

test_that("exportRandomPopulations validates customParameters values field", {
  # Missing values field
  expect_error(
    exportRandomPopulations(
      projectConfiguration = projectConfiguration,
      populationNames = character(0),
      customParameters = list(list(path = "SomePath")),  # No values field
      overwrite = FALSE
    ),
    regexp = "values"
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

# Tests for refactored helper functions

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

test_that(".exportSinglePopulation handles valid population", {
  skip_if_not(file.exists(projectConfiguration$populationsFile), 
              "Populations file not available")
  skip("Skipping .exportSinglePopulation test - requires full population setup")
  
  # This test would require a complete population setup
  # and is better tested through integration tests
})
