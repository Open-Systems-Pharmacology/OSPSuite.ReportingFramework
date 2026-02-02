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
    )
  )

  expect_error(
    getSimulatedTimeprofile(
      simulatedResult = list(),  # Missing 'results' element
      outputPaths = outputPaths,
      aggregationFun = NULL,
      individualMatch = NULL
    )
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
  aggregationFun <- function(y) {
    list(
      yValues = mean(y),
      yMin = mean(y) - sd(y),
      yMax = mean(y) + sd(y),
      yErrorType = "mean | standard deviation"
    )
  }

  expect_error(
    getSimulatedTimeprofile(
      simulatedResult = simulatedResult,
      outputPaths = outputPaths,
      aggregationFun = aggregationFun,
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
  aggregationFun <- function(y) {
    list(
      yValues = mean(y),
      yMin = mean(y) - sd(y),
      yMax = mean(y) + sd(y),
      yErrorType = "mean | standard deviation"
    )
  }

  # Get output paths from the simulation
  outputPaths <- head(simulatedResult$results$allQuantityPaths, 2)

  # Test with valid parameters
  result <- getSimulatedTimeprofile(
    simulatedResult = simulatedResult,
    outputPaths = outputPaths,
    aggregationFun = aggregationFun,
    individualMatch = NULL
  )

  expect_s3_class(result, "data.table")
  expect_true("xValues" %in% names(result))
  expect_true("yValues" %in% names(result))
  expect_true("dataType" %in% names(result))
  expect_true("dataClass" %in% names(result))
})

