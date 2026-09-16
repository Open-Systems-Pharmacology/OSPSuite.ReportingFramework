# testproject with variable projectconfiguration is set up by the setup.R for all tests simulataneously

test_that("getAggregationFunction works correctly", {
  # Test GeometricStdDev
  aggregationFunction <- getAggregationFunction(
    "GeometricStdDev",
    percentiles = NULL,
    customFunction = NULL
  )
  expect_type(aggregationFunction, "closure")

  # Test ArithmeticStdDev
  aggregationFunction <- getAggregationFunction(
    "ArithmeticStdDev",
    percentiles = NULL,
    customFunction = NULL
  )
  expect_type(aggregationFunction, "closure")

  # Test Percentiles
  aggregationFunction <- getAggregationFunction(
    "Percentiles",
    percentiles = c(0.25, 0.5, 0.75),
    customFunction = NULL
  )
  expect_type(aggregationFunction, "closure")

  # Test Custom
  expect_error(getAggregationFunction(
    "Custom",
    percentiles = NULL,
    customFunction = NULL
  ))
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
  expect_error(getAggregationFunction(
    "InvalidFlag",
    percentiles = NULL,
    customFunction = NULL
  ))

  # Test invalid percentiles for Percentiles aggregation
  expect_error(getAggregationFunction(
    "Percentiles",
    percentiles = c(0.25, 0.5),
    customFunction = NULL
  )) # Not length 3
  expect_error(getAggregationFunction(
    "Percentiles",
    percentiles = c(1.1, 0.5, 0.75),
    customFunction = NULL
  )) # Out of range
  expect_error(getAggregationFunction(
    "Percentiles",
    percentiles = c(0.5, 0.25, 0.75),
    customFunction = NULL
  )) # Not sorted

  # Test invalid legendsize
  expect_error(getAggregationFunction(
    "Percentiles",
    percentiles = c(0.25, 0.5, 0.75),
    customFunction = NULL,
    legendsize = 4
  ))
})

# Edge case tests for getAggregationFunction
test_that("getAggregationFunction returns closure for GeometricStdDev", {
  agg <- getAggregationFunction(
    "GeometricStdDev",
    percentiles = NULL,
    customFunction = NULL
  )
  expect_type(agg, "closure")
  expect_true(is.function(agg))
})

test_that("getAggregationFunction returns closure for ArithmeticStdDev", {
  agg <- getAggregationFunction(
    "ArithmeticStdDev",
    percentiles = NULL,
    customFunction = NULL
  )
  expect_type(agg, "closure")
  expect_true(is.function(agg))
})

test_that("getAggregationFunction with custom percentiles 5, 50, 95", {
  agg <- getAggregationFunction(
    "Percentiles",
    percentiles = c(0.05, 0.5, 0.95),
    customFunction = NULL
  )
  expect_type(agg, "closure")
  expect_true(is.function(agg))
})

test_that("getAggregationFunction with custom percentiles 25, 50, 75", {
  agg <- getAggregationFunction(
    "Percentiles",
    percentiles = c(0.25, 0.5, 0.75),
    customFunction = NULL
  )
  expect_type(agg, "closure")
  expect_true(is.function(agg))
})

test_that("getAggregationFunction with custom function", {
  customFun <- function(x) mean(x, na.rm = TRUE)
  agg <- getAggregationFunction(
    "Custom",
    percentiles = NULL,
    customFunction = customFun
  )
  expect_type(agg, "closure")
})

# Edge case tests for getErrorTypeForPercentiles
test_that("getErrorTypeForPercentiles with 5th and 95th percentiles", {
  percentiles <- c(0.05, 0.5, 0.95)
  result <- getErrorTypeForPercentiles(percentiles, legendsize = 2)
  expect_type(result, "character")
  expect_true(grepl("median", result, ignore.case = TRUE))
})

test_that("getErrorTypeForPercentiles with 1st and 99th percentiles", {
  percentiles <- c(0.01, 0.5, 0.99)
  result <- getErrorTypeForPercentiles(percentiles, legendsize = 2)
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("getErrorTypeForPercentiles with legendsize = 2 and 25, 50, 75", {
  percentiles <- c(0.25, 0.5, 0.75)
  result <- getErrorTypeForPercentiles(percentiles, legendsize = 2)
  expect_type(result, "character")
})

test_that("getErrorTypeForPercentiles with legendsize = 3", {
  percentiles <- c(0.25, 0.5, 0.75)
  result <- getErrorTypeForPercentiles(percentiles, legendsize = 3)
  expect_type(result, "character")
})

test_that("getErrorTypeForPercentiles with symmetric percentiles", {
  percentiles <- c(0.15, 0.5, 0.85)
  result <- getErrorTypeForPercentiles(percentiles, legendsize = 2)
  expect_type(result, "character")
  expect_true(grepl("median", result, ignore.case = TRUE))
})

test_that("getErrorTypeForPercentiles handles edge case percentiles", {
  # Test with very close percentiles
  percentiles <- c(0.49, 0.5, 0.51)
  result <- getErrorTypeForPercentiles(percentiles, legendsize = 2)
  expect_type(result, "character")
})

test_that("getErrorTypeForPercentiles with only lower and upper bounds", {
  percentiles <- c(0, 0.5, 1)
  result <- getErrorTypeForPercentiles(percentiles, legendsize = 2)
  expect_equal(result, "median | min - max")
})

test_that("getErrorTypeForPercentiles with asymmetric percentiles", {
  percentiles <- c(0.1, 0.5, 0.9)
  result <- getErrorTypeForPercentiles(percentiles, legendsize = 2)
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})
