# Self-contained unit tests for time-profile plotting utilities.
# No external project configuration, simulation files or vdiffr required.

# ---------------------------------------------------------------------------
# Shared minimal data factories
# ---------------------------------------------------------------------------

makeSimDT <- function(group = "grp1", n = 20, slope = "decreasing") {
  x <- seq(0, 10, length.out = n)
  y <- if (slope == "decreasing") exp(-0.3 * x) + 0.01 else 0.1 * x + 0.5
  data.table::data.table(
    xValues = x,
    yValues = y,
    group = group,
    dataType = "simulated",
    xUnit = "h",
    yUnit = "mg/l"
  )
}

makeObsDT <- function(group = "grp1", xVals = c(1, 3, 5, 7)) {
  data.table::data.table(
    xValues = xVals,
    yValues = c(0.75, 0.40, 0.22, 0.12),
    group = group,
    dataType = "observed",
    xUnit = "h",
    yUnit = "mg/l"
  )
}

makeCombinedDT <- function(group = "grp1") {
  rbind(makeSimDT(group), makeObsDT(group))
}

# ---------------------------------------------------------------------------
# addPredictedValues
# ---------------------------------------------------------------------------

test_that("addPredictedValues adds predicted column to observed rows", {
  dtObs <- makeObsDT()
  dtSim <- makeSimDT()

  result <- addPredictedValues(
    dtObserved = dtObs,
    dtSimulated = dtSim,
    identifier = "group"
  )

  expect_true("predicted" %in% names(result))
  expect_equal(nrow(result), nrow(dtObs))
})

test_that("addPredictedValues interpolates within range", {
  dtSim <- data.table::data.table(
    xValues = c(0, 5, 10),
    yValues = c(1, 0.5, 0.25),
    group = "g"
  )
  dtObs <- data.table::data.table(xValues = 5, group = "g")

  result <- addPredictedValues(dtObs, dtSim, identifier = "group")

  expect_equal(result$predicted, 0.5, tolerance = 1e-6)
})

test_that("addPredictedValues returns NA for x outside simulated range", {
  dtSim <- data.table::data.table(
    xValues = c(0, 2, 4),
    yValues = c(1, 0.6, 0.4),
    group = "g"
  )
  dtObs <- data.table::data.table(xValues = 10, group = "g")

  result <- addPredictedValues(dtObs, dtSim, identifier = "group")

  expect_true(is.na(result$predicted))
})

test_that("addPredictedValues skips rows with no matching simulated group", {
  dtSim <- data.table::data.table(
    xValues = c(1, 2, 3, 4, 5),
    yValues = c(1, 0.8, 0.6, 0.4, 0.2),
    group = "A"
  )
  dtObs <- data.table::data.table(xValues = c(2, 3), group = c("A", "B"))

  result <- addPredictedValues(dtObs, dtSim, identifier = "group")

  expect_equal(nrow(result), 2)
  expect_false(is.na(result$predicted[1]))
  expect_true(is.na(result$predicted[2]))
})

test_that("addPredictedValues validates inputs", {
  expect_error(
    addPredictedValues(
      dtObserved = list(x = 1),
      dtSimulated = makeSimDT(),
      identifier = "group"
    )
  )
  expect_error(
    addPredictedValues(
      dtObserved = makeObsDT(),
      dtSimulated = makeSimDT(),
      identifier = 123
    )
  )
})

test_that("addPredictedValues handles multiple identifier columns", {
  dtSim <- data.table::data.table(
    xValues = c(1, 2, 3, 4, 5),
    yValues = c(2, 4, 6, 8, 10),
    group = "g",
    outputPathId = "p1"
  )
  dtObs <- data.table::data.table(
    xValues = 3,
    group = "g",
    outputPathId = "p1"
  )

  result <- addPredictedValues(
    dtObs,
    dtSim,
    identifier = c("group", "outputPathId")
  )

  expect_true("predicted" %in% names(result))
  expect_equal(result$predicted, 6, tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# PlotDataTimeProfile helpers (from former test-PlotDataTimeProfile.R)
# ---------------------------------------------------------------------------

configTable <- data.table::data.table(
  scenario = c("A", "A", "B", "B"),
  scenarioIndex = c(1, 2, 3, 4),
  individualIds = c("1,2", "3", "*", "4"),
  outputPathIds = c("Path1", "Path2", "Path1,Path2", "Path3"),
  timeUnit = c("day(s)", "day(s)", "day(s)", "day(s)"),
  timeOffset_Reference = c(0, 1, 2, 3),
  timeOffset = c(1, 1, 1, 1),
  timeRange_Tag1 = c("firstApplication", rep(NA, 3)),
  timeRange_2 = rep("total", 4),
  referenceScenario = c(
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_
  )
)

timeTags <- factor(
  c("Tag1", "Tag2"),
  levels = c("Tag1", "Tag2"),
  ordered = TRUE
)

dtOutputPaths <- data.table::data.table(
  outputPathId = c("Path1", "Path2", "Path3"),
  OutputPaths = c("model|Path1", "model|Path2", "model|Path3")
)

test_that("getPlotIdForColumns adds plotId column", {
  result <- getPlotIdForColumns(configTable, "outputPathIds")
  expect_true("plotId" %in% names(result))
  expect_equal(nrow(result), 5)
})

test_that("splitCaptionByIndividuals adds individualId column", {
  dtCaption <- getPlotIdForColumns(configTable, "outputPathIds")
  result <- splitCaptionByIndividuals(
    configTable,
    individualIdVector = c("1", "2"),
    dtCaption
  )
  expect_true("individualId" %in% names(result))
  expect_equal(nrow(result), 8)
})

test_that("addTimeTagsToCaption adds timeRangeTag column and multiplies rows", {
  dtCaption <- getPlotIdForColumns(configTable, "outputPathIds")
  result <- addTimeTagsToCaption(
    dtCaption,
    timeTags,
    splitPlotsPerTimeRange = 0
  )
  expect_true("timeRangeTag" %in% names(result))
  expect_equal(nrow(result), length(timeTags) * nrow(dtCaption))
})

test_that("setTimeRangeFilter returns one filter per tag when split=1", {
  result <- setTimeRangeFilter(splitPlotsPerTimeRange = 1, timeTags)
  expect_equal(length(result), 2)
})

test_that("setTimeRangeFilter returns single filter when split=0", {
  result <- setTimeRangeFilter(splitPlotsPerTimeRange = 0, timeTags)
  expect_equal(length(result), 1)
})

test_that("restructureApplicationTimeByScenarioIndex returns one entry per row", {
  applicationTimes <- list(A = c(1, 2), B = c(3, 4))
  result <- restructureApplicationTimeByScenarioIndex(
    applicationTimes,
    configTable
  )
  expect_equal(length(result), nrow(configTable))
})

test_that("getOutputPathsPerScenario returns one list entry per unique scenario", {
  result <- getOutputPathsPerScenario(configTable, dtOutputPaths)
  expect_true(is.list(result))
  expect_equal(length(result), length(unique(configTable$scenario)))
})

test_that("getObservedUnitConversionDT returns unitFactor column", {
  dataObserved <- data.table::data.table(
    outputPathId = c("Path1", "Path2"),
    yUnit = c("g", "kg")
  )
  dtUnit <- data.table::data.table(
    outputPathId = c("Path1", "Path2"),
    dimension = c("Mass", "Mass"),
    yUnit = c("kg", "kg"),
    displayUnit = c("g", "kg"),
    unitFactor = c(1000, 1)
  )
  result <- getObservedUnitConversionDT(dataObserved, dtUnit)
  expect_true("unitFactor" %in% names(result))
  expect_equal(nrow(result), 2)
})
