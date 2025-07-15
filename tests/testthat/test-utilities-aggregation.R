# testproject with variable projectconfiguration is set up by the setup.R for all tests simulataneously

test_that("getAggregationFunction works correctly", {
  # Test GeometricStdDev
  aggregationFunction <- getAggregationFunction("GeometricStdDev", percentiles = NULL, customFunction = NULL)
  expect_type(aggregationFunction, "closure")

  # Test ArithmeticStdDev
  aggregationFunction <- getAggregationFunction("ArithmeticStdDev", percentiles = NULL, customFunction = NULL)
  expect_type(aggregationFunction, "closure")

  # Test Percentiles
  aggregationFunction <- getAggregationFunction("Percentiles", percentiles = c(0.25, 0.5, 0.75), customFunction = NULL)
  expect_type(aggregationFunction, "closure")

  # Test Custom
  expect_error(getAggregationFunction("Custom", percentiles = NULL, customFunction = NULL))
})


# Test for getErrorTypeForPercentiles
test_that("getErrorTypeForPercentiles works correctly", {
  # Test with median and valid percentiles
  percentiles <- c(0.25, 0.5, 0.75)
  result <- getErrorTypeForPercentiles(percentiles, legendsize = 2)
  expect_equal(result, "median | 25th - 75th percentile")

  # Test with only min and max percentiles
  percentiles <- c(0, 0.5, 1)
  result <- getErrorTypeForPercentiles(percentiles, legendsize = 2)
  expect_equal(result, "median | min - max")

  # Test with percentiles including median
  percentiles <- c(0.1, 0.5, 0.9)
  result <- getErrorTypeForPercentiles(percentiles, legendsize = 2)
  expect_equal(result, "median | 10th - 90th percentile")

  # Test with all percentiles equal (edge case)
  percentiles <- c(0.5, 0.5, 0.5)
  result <- getErrorTypeForPercentiles(percentiles, legendsize = 2)
  expect_equal(result, "median | median - median")

  # Test with non-standard percentiles (should be handled correctly)
  percentiles <- c(0.2, 0.4, 0.6)
  result <- getErrorTypeForPercentiles(percentiles, legendsize = 2)
  expect_equal(result, "40th percentile | 20th - 60th percentile")
})

# Test for getAggregationFunction
test_that("getAggregationFunction handles invalid inputs correctly", {
  # Test invalid aggregationFlag
  expect_error(getAggregationFunction("InvalidFlag", percentiles = NULL, customFunction = NULL))

  # Test invalid percentiles for Percentiles aggregation
  expect_error(getAggregationFunction("Percentiles", percentiles = c(0.25, 0.5), customFunction = NULL)) # Not length 3
  expect_error(getAggregationFunction("Percentiles", percentiles = c(1.1, 0.5, 0.75), customFunction = NULL)) # Out of range
  expect_error(getAggregationFunction("Percentiles", percentiles = c(0.5, 0.25, 0.75), customFunction = NULL)) # Not sorted

  # Test invalid legendsize
  expect_error(getAggregationFunction("Percentiles", percentiles = c(0.25, 0.5, 0.75), customFunction = NULL, legendsize = 4))

  # Test with a valid custom function
  customFunc <- function(y) {
    return(list(yValues = mean(y), yErrorValues = sd(y), yErrorType = "Custom"))
  }
  aggregationFunction <- getAggregationFunction("Custom", percentiles = NULL, customFunction = customFunc)
  expect_type(aggregationFunction, "closure")
})

# Test for getErrorTypeForPercentiles
test_that("getErrorTypeForPercentiles handles invalid legendsize correctly", {
  # Test invalid legendsize
  expect_error(getErrorTypeForPercentiles(c(0.25, 0.5, 0.75), legendsize = 4))
  expect_error(getErrorTypeForPercentiles(c(0.25, 0.5, 0.75), legendsize = 0))
})
