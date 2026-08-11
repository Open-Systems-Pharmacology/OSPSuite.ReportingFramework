# Unit tests for pure helper functions in plotPKBoxwhisker.R.
# Integration tests (runPlot, vdiffr, real project setup) belong in the
# integration-test package.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

makeBoxwhiskerCaptionData <- function(
  scenarioLongName = "Adults",
  displayNameOutput = "Liver",
  plotTag = "A",
  displayNamePKParameter = "AUC",
  displayUnit = "mg*h/l"
) {
  data.table::data.table(
    scenarioLongName = scenarioLongName,
    displayNameOutput = displayNameOutput,
    plotTag = plotTag,
    displayNamePKParameter = displayNamePKParameter,
    displayUnit = displayUnit
  )
}

makeBoxwhiskerPlotData <- function(
  scenarios = c("s1", "s1", "s2", "s2"),
  values = c(1.0, 1.2, 0.9, 1.1),
  colorIndex = factor(c("A", "A", "B", "B")),
  plotTag = "A"
) {
  data.table::data.table(
    scenarioShortName = factor(scenarios),
    value = values,
    colorIndex = colorIndex,
    plotTag = plotTag
  )
}

# ---------------------------------------------------------------------------
# getCaptionForBoxwhiskerPlot
# ---------------------------------------------------------------------------

test_that("getCaptionForBoxwhiskerPlot builds table caption (isPlotCaption=FALSE)", {
  plotDataPk <- makeBoxwhiskerCaptionData()
  result <- ospsuite.reportingframework:::getCaptionForBoxwhiskerPlot(
    plotDataPk = plotDataPk,
    plotCaptionAddon = NA,
    isPlotCaption = FALSE,
    asRatio = FALSE
  )
  expect_match(result, "Population summary statistics")
  expect_false(grepl("ratios", result))
  expect_false(grepl("box-whisker", result))
})

test_that("getCaptionForBoxwhiskerPlot includes 'ratios' for ratio plots", {
  plotDataPk <- makeBoxwhiskerCaptionData()
  result <- ospsuite.reportingframework:::getCaptionForBoxwhiskerPlot(
    plotDataPk = plotDataPk,
    plotCaptionAddon = NA,
    isPlotCaption = FALSE,
    asRatio = TRUE
  )
  expect_match(result, "ratios")
})

test_that("getCaptionForBoxwhiskerPlot includes scale and percentiles for plot caption", {
  plotDataPk <- makeBoxwhiskerCaptionData()
  result <- ospsuite.reportingframework:::getCaptionForBoxwhiskerPlot(
    plotDataPk = plotDataPk,
    percentiles = c(0.05, 0.5, 0.95),
    yScale = "log",
    plotCaptionAddon = NA,
    isPlotCaption = TRUE,
    asRatio = FALSE
  )
  expect_match(result, "box-whisker")
  expect_match(result, "logarithmic")
  expect_match(result, "5th")
  expect_match(result, "95th")
})

test_that("getCaptionForBoxwhiskerPlot appends plotCaptionAddon", {
  plotDataPk <- makeBoxwhiskerCaptionData()
  result <- ospsuite.reportingframework:::getCaptionForBoxwhiskerPlot(
    plotDataPk = plotDataPk,
    plotCaptionAddon = "See protocol.",
    isPlotCaption = FALSE,
    asRatio = FALSE
  )
  expect_match(result, "See protocol")
})

test_that("getCaptionForBoxwhiskerPlot includes output name", {
  plotDataPk <- makeBoxwhiskerCaptionData(displayNameOutput = "Plasma")
  result <- ospsuite.reportingframework:::getCaptionForBoxwhiskerPlot(
    plotDataPk = plotDataPk,
    plotCaptionAddon = NA,
    isPlotCaption = FALSE,
    asRatio = FALSE
  )
  expect_match(result, "Plasma")
})

# ---------------------------------------------------------------------------
# getSummaryTable
# ---------------------------------------------------------------------------

test_that("getSummaryTable returns expected columns", {
  plotDataPk <- makeBoxwhiskerPlotData()
  result <- ospsuite.reportingframework:::getSummaryTable(
    plotDataPk = plotDataPk,
    onePlotConfig = data.table::data.table(),
    percentiles = c(0.05, 0.5, 0.95)
  )
  expect_true("N" %in% names(result))
  expect_true("arith mean" %in% names(result))
  expect_true("geo mean" %in% names(result))
  expect_true("5th percentile" %in% names(result))
  expect_true("95th percentile" %in% names(result))
})

test_that("getSummaryTable groups by scenarioShortName", {
  plotDataPk <- makeBoxwhiskerPlotData()
  result <- ospsuite.reportingframework:::getSummaryTable(
    plotDataPk = plotDataPk,
    onePlotConfig = data.table::data.table(),
    percentiles = c(0.5)
  )
  expect_equal(nrow(result), 2)
})

test_that("getSummaryTable computes correct N per group", {
  plotDataPk <- makeBoxwhiskerPlotData(
    scenarios = c("s1", "s1", "s1", "s2"),
    values = c(1, 2, 3, 4),
    colorIndex = factor(c("A", "A", "A", "B"))
  )
  result <- ospsuite.reportingframework:::getSummaryTable(
    plotDataPk = plotDataPk,
    onePlotConfig = data.table::data.table(),
    percentiles = c(0.5)
  )
  expect_equal(result[scenarioShortName == "s1"]$N, 3)
  expect_equal(result[scenarioShortName == "s2"]$N, 1)
})

# ---------------------------------------------------------------------------
# validateExistenceOfReferenceForRatio
# ---------------------------------------------------------------------------

test_that("validateExistenceOfReferenceForRatio passes when all plots have reference", {
  dt <- data.table::data.table(
    plotName = c("p1", "p1"),
    referenceScenario = c("ref", "ref")
  )
  expect_invisible(
    ospsuite.reportingframework:::validateExistenceOfReferenceForRatio(dt, NULL)
  )
})

test_that("validateExistenceOfReferenceForRatio errors when a plot has no reference", {
  dt <- data.table::data.table(
    plotName = c("p1", "p2"),
    referenceScenario = c("ref", NA)
  )
  expect_error(
    ospsuite.reportingframework:::validateExistenceOfReferenceForRatio(dt, NULL)
  )
})

test_that("validateExistenceOfReferenceForRatio passes on empty table", {
  dt <- data.table::data.table(
    plotName = character(0),
    referenceScenario = character(0)
  )
  expect_invisible(
    ospsuite.reportingframework:::validateExistenceOfReferenceForRatio(dt, NULL)
  )
})

# ---------------------------------------------------------------------------
# validateIsCrossOverStudy
# ---------------------------------------------------------------------------

test_that("validateIsCrossOverStudy passes when scenario and reference share same population", {
  configTablePlots <- data.table::data.table(
    plotName = "p1",
    scenario = "s1",
    referenceScenario = "s2"
  )
  pkDT <- data.table::data.table(
    scenario = c("s1", "s2"),
    populationId = c("pop1", "pop1")
  )
  expect_invisible(
    ospsuite.reportingframework:::validateIsCrossOverStudy(
      configTablePlots,
      pkDT
    )
  )
})

test_that("validateIsCrossOverStudy errors when populations differ", {
  configTablePlots <- data.table::data.table(
    plotName = "p1",
    scenario = "s1",
    referenceScenario = "s2"
  )
  pkDT <- data.table::data.table(
    scenario = c("s1", "s2"),
    populationId = c("pop1", "pop2")
  )
  expect_error(
    suppressMessages(
      suppressWarnings(
        ospsuite.reportingframework:::validateIsCrossOverStudy(
          configTablePlots,
          pkDT
        )
      )
    )
  )
})
