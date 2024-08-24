# Test for getAggregationFunction
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
  result <- getErrorTypeForPercentiles(percentiles)
  expect_equal(result, "median and 25th - 75th percentile")

  # Test with only min and max percentiles
  percentiles <- c(0, 0.5,1)
  result <- getErrorTypeForPercentiles(percentiles)
  expect_equal(result, "median and min - max")

  # Test with percentiles including median
  percentiles <- c(0.1, 0.5, 0.9)
  result <- getErrorTypeForPercentiles(percentiles)
  expect_equal(result, "median and 10th - 90th percentile")

  # Test with all percentiles equal (edge case)
  percentiles <- c(0.5, 0.5, 0.5)
  result <- getErrorTypeForPercentiles(percentiles)
  expect_equal(result, "median and 50th - 50th percentile")

  # Test with non-standard percentiles (should be handled correctly)
  percentiles <- c(0.2, 0.4, 0.6)
  result <- getErrorTypeForPercentiles(percentiles)
  expect_equal(result, "median and 20th percentile - 60th percentile")

})
