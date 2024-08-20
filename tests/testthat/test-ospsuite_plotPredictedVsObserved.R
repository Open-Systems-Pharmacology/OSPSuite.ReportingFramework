
# Example data for testing
dtSimulated <- data.table(
  xValues = c(1, 2, 3, 4, 5),
  yValues = c(2, 4, 6, 8, 10),
  PlotTag = rep("A", 5),
  OutputPathId = rep("1", 5)
)

dtObserved <- data.table(
  xValues = c(1.5, 2.5, 3.5, 4.5),
  PlotTag = rep("A", 4),
  OutputPathId = rep("1", 4)
)

# Define unit tests
test_that("Extrapolation for increasing yValues works correctly", {
  result <- addPredictedValues(dtObserved, dtSimulated, c("PlotTag", "OutputPathId"))

  expect_equal(result$predicted, c(3, 5, 7, 9))  # Expected linear predictions
})

# Test with decreasing yValues
dtSimulated_decreasing <- data.table(
  xValues = c(1, 2, 3, 4, 5),
  yValues = c(10000, 100, 1, 0.01, 0.0001),
  PlotTag = rep("B", 5),
  OutputPathId = rep("1", 5)
)

dtObserved_decreasing <- data.table(
  xValues = c(1.5, 2.5, 3.5, 4.5),
  PlotTag = rep("B", 4),
  OutputPathId = rep("1", 4)
)

test_that("Logarithmic interpolation for decreasing yValues works correctly", {
  result <- addPredictedValues(dtObserved_decreasing, dtSimulated_decreasing, c("PlotTag", "OutputPathId"))

  expect_equal(result$predicted, c(1000, 10, 0.1, 0.001))  # Expected log predictions
})

# Test with insufficient data
test_that("Function handles insufficient data gracefully", {
  dtSimulated_insufficient <- data.table(
    xValues = c(1),
    yValues = c(2),
    PlotTag = rep("C", 1),
    OutputPathId = rep("1", 1)
  )

  dtObserved_insufficient <- data.table(
    xValues = c(1.5),
    PlotTag = rep("C", 1),
    OutputPathId = rep("1", 1)
  )

  expect_warning(
    result <- addPredictedValues(dtObserved_insufficient, dtSimulated_insufficient, c("PlotTag", "OutputPathId")),
    "Not enough data points for"
  )
})

# Test with mixed data
dtSimulated_mixed <- data.table(
  xValues = c(1, 2, 3, 4, 5),
  yValues = c(1, 3, 5, 4, 2),  # Mixed increasing and decreasing
  PlotTag = rep("D", 5),
  OutputPathId = rep("1", 5)
)

dtObserved_mixed <- data.table(
  xValues = c(1.5, 2.5, 3.5, 4.5),
  PlotTag = rep("D", 4),
  OutputPathId = rep("1", 4)
)

test_that("Function handles mixed trends correctly", {
  result <- addPredictedValues(dtObserved_mixed, dtSimulated_mixed, c("PlotTag", "OutputPathId"))

  expect_equal(result$predicted, c(2, 4, exp(mean(c(log(5),log(4)))), exp(mean(c(log(2),log(4))))))  # Expected log predictions
})



dtObserved_mixed <- data.table(
  xValues = c(2, 3, 4, 5),
  PlotTag = rep("D", 4),
  OutputPathId = rep("1", 4)
)

test_that("Function handles mixed trends correctly", {
  result <- addPredictedValues(dtObserved_mixed, dtSimulated_mixed, c("PlotTag", "OutputPathId"))

  expect_equal(result$predicted, c(3, 5, 4, 2))  # Expected log predictions
})


# Example data for testing
dtSimulated <- data.table(
  xValues = c(1, 2, 3, 4, 5),
  yValues = c(-2, -4, -6, -8, -10),
  PlotTag = rep("A", 5),
  OutputPathId = rep("1", 5)
)

dtObserved <- data.table(
  xValues = c(1.5, 2.5, 3.5, 4.5),
  PlotTag = rep("A", 4),
  OutputPathId = rep("1", 4)
)

# Define unit tests
test_that("Extrapolation for negative yValues works correctly", {
  result <- addPredictedValues(dtObserved, dtSimulated, c("PlotTag", "OutputPathId"))

  expect_equal(result$predicted, c(-3, -5, -7, -9))  # Expected linear predictions
})


# Example data for testing
dtSimulated <- data.table(
  xValues = rep(seq(1,5),4),
  yValues = seq(1,20),
  PlotTag = rep(c("A","B","A","B"), each = 5),
  OutputPathId = rep(c("1","2"), each = 10)
)

dtObserved <- data.table(
  xValues = rep(c(1.5, 2.5, 3.5, 4.5),4),
  PlotTag = rep(c("A","B","A","B"), each = 4),
  OutputPathId = rep(c("1","2"), each = 8)
)

# Define unit tests
test_that("Extrapolation for more then one group", {
  result <- addPredictedValues(dtObserved, dtSimulated, c("PlotTag", "OutputPathId"))

  expect_equal(result$predicted, setdiff(seq(1.5,20.5),seq(5.5,20.5,5)))  # Expected linear predictions
})


