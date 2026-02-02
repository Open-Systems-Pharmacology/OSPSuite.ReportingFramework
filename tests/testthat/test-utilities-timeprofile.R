# Tests for utilities-timeprofile.R
# Note: Many functions in this file are internal and complex, requiring significant setup
# These tests focus on the exported function and basic validation

test_that("getSimulatedTimeprofile validates input parameters", {
  skip_if(length(scenarioResults) == 0, "No scenario results available")
  
  scenarioName <- names(scenarioResults)[1]
  simulatedResult <- scenarioResults[[scenarioName]]
  outputPaths <- head(simulatedResult$results$allQuantityPaths, 2)
  
  # Test invalid simulatedResult
  expect_error(
    getSimulatedTimeprofile(
      simulatedResult = NULL,
      outputPaths = outputPaths,
      aggregationFun = NULL,
      individualMatch = NULL
    ),
    "Assertion on 'simulatedResult' failed"
  )
  
  expect_error(
    getSimulatedTimeprofile(
      simulatedResult = list(),  # Missing 'results' element
      outputPaths = outputPaths,
      aggregationFun = NULL,
      individualMatch = NULL
    ),
    "Must include the elements.*results"
  )
  
  # Test invalid outputPaths
  expect_error(
    getSimulatedTimeprofile(
      simulatedResult = simulatedResult,
      outputPaths = NULL,
      aggregationFun = NULL,
      individualMatch = NULL
    ),
    "Assertion on 'outputPaths' failed"
  )
  
  expect_error(
    getSimulatedTimeprofile(
      simulatedResult = simulatedResult,
      outputPaths = c("valid", NA),
      aggregationFun = NULL,
      individualMatch = NULL
    ),
    "Contains missing values"
  )
  
  # Test invalid aggregationFun
  expect_error(
    getSimulatedTimeprofile(
      simulatedResult = simulatedResult,
      outputPaths = outputPaths,
      aggregationFun = "not a function",
      individualMatch = NULL
    ),
    "Assertion on 'aggregationFun' failed"
  )
  
  # Test invalid individualMatch
  expect_error(
    getSimulatedTimeprofile(
      simulatedResult = simulatedResult,
      outputPaths = outputPaths,
      aggregationFun = NULL,
      individualMatch = list()  # Should be data.frame or NULL
    ),
    "Assertion on 'individualMatch' failed"
  )
})

test_that("getSimulatedTimeprofile works with valid parameters", {
  skip_if(length(scenarioResults) == 0, "No scenario results available")
  
  # Get first scenario result
  scenarioName <- names(scenarioResults)[1]
  simulatedResult <- scenarioResults[[scenarioName]]
  
  # Get output paths from the simulation
  outputPaths <- head(simulatedResult$results$allQuantityPaths, 2)
  
  # Test with valid parameters
  result <- getSimulatedTimeprofile(
    simulatedResult = simulatedResult,
    outputPaths = outputPaths,
    aggregationFun = NULL,
    individualMatch = NULL
  )
  
  expect_s3_class(result, "data.table")
  expect_true("xValues" %in% names(result))
  expect_true("yValues" %in% names(result))
  expect_true("dataType" %in% names(result))
  expect_true("dataClass" %in% names(result))
})

test_that("getSimulatedTimeprofile handles empty outputPaths", {
  skip_if(length(scenarioResults) == 0, "No scenario results available")
  
  scenarioName <- names(scenarioResults)[1]
  simulatedResult <- scenarioResults[[scenarioName]]
  
  # Test with empty output paths
  result <- getSimulatedTimeprofile(
    simulatedResult = simulatedResult,
    outputPaths = character(0),
    aggregationFun = NULL,
    individualMatch = NULL
  )
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("getSimulatedTimeprofile filters to available paths only", {
  skip_if(length(scenarioResults) == 0, "No scenario results available")
  
  scenarioName <- names(scenarioResults)[1]
  simulatedResult <- scenarioResults[[scenarioName]]
  
  # Mix of valid and invalid paths
  availablePath <- simulatedResult$results$allQuantityPaths[1]
  outputPaths <- c(availablePath, "InvalidPath1", "InvalidPath2")
  
  result <- getSimulatedTimeprofile(
    simulatedResult = simulatedResult,
    outputPaths = outputPaths,
    aggregationFun = NULL,
    individualMatch = NULL
  )
  
  expect_s3_class(result, "data.table")
  # Should only include the valid path
  if (nrow(result) > 0) {
    expect_true(all(result$paths == availablePath))
  }
})
