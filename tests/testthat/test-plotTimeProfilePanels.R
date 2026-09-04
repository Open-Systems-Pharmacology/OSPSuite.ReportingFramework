# helper-only unit tests for plotTimeProfilePanels -----------

test_that("isPlotTypeNeededAndPossible handles TP and observed-dependent plot types", {
  plotDataTp <- list(
    configTable = data.table::data.table(
      plot_TimeProfiles = TRUE,
      plot_PredictedVsObserved = TRUE
    ),
    hasObservedData = function() FALSE
  )
  expect_true(isPlotTypeNeededAndPossible("TP", plotDataTp))
  expect_false(isPlotTypeNeededAndPossible("PvO", plotDataTp))

  plotDataObserved <- list(
    configTable = data.table::data.table(
      plot_TimeProfiles = FALSE,
      plot_PredictedVsObserved = TRUE
    ),
    hasObservedData = function() TRUE
  )
  expect_true(isPlotTypeNeededAndPossible("PvO", plotDataObserved))
})

test_that("checkAndAdjustYlimits returns NULL for non-TP plots", {
  expect_null(checkAndAdjustYlimits(
    plotData = list(),
    yScale = "log",
    timeRangeFilter = "allTimeRanges",
    plotType = "PvO",
    plotCounter = 1
  ))
})

test_that("checkAndAdjustYlimits throws informative error for invalid ylimit expression", {
  plotData <- list(
    configTable = data.table::data.table(ylimit_log = "c(1, )")
  )

  expect_error(
    checkAndAdjustYlimits(
      plotData = plotData,
      yScale = "log",
      timeRangeFilter = "allTimeRanges",
      plotType = "TP",
      plotCounter = 1
    ),
    regexp = "Invalid ylimit expression"
  )
})

# getGroupbyMapping -----------
test_that("getGroupbyMapping does not include shape for simulated data", {
  makePlotData <- function(useColorIndex = TRUE, useShapeIndex = TRUE) {
    list(
      useColorIndex = function() useColorIndex,
      useShapeIndex = function() useShapeIndex
    )
  }

  testCases <- list(
    list(plotType = "TP", useColorIndex = TRUE, useShapeIndex = TRUE),
    list(plotType = "TP", useColorIndex = TRUE, useShapeIndex = FALSE),
    list(plotType = "PvO", useColorIndex = TRUE, useShapeIndex = TRUE),
    list(plotType = "PvO", useColorIndex = TRUE, useShapeIndex = FALSE),
    list(plotType = "PvO", useColorIndex = FALSE, useShapeIndex = TRUE),
    list(plotType = "PvO", useColorIndex = FALSE, useShapeIndex = FALSE)
  )

  for (testCase in testCases) {
    plotData <- makePlotData(
      useColorIndex = testCase$useColorIndex,
      useShapeIndex = testCase$useShapeIndex
    )

    mapping <- getGroupbyMapping(
      plotData = plotData,
      plotType = testCase$plotType,
      dataType = "simulated"
    )

    expect_true("groupby" %in% names(mapping))
    expect_false("shape" %in% names(mapping))
  }
})

test_that("getGroupbyMapping includes shape for observed TP data", {
  makePlotData <- function(useShapeIndex = TRUE) {
    list(
      useColorIndex = function() TRUE,
      useShapeIndex = function() useShapeIndex
    )
  }

  mapping <- getGroupbyMapping(
    plotData = makePlotData(useShapeIndex = TRUE),
    plotType = "TP",
    dataType = "observed"
  )

  expect_true("groupby" %in% names(mapping))
  expect_true("shape" %in% names(mapping))
  expect_equal(rlang::as_label(mapping$shape), "shapeIndex")
})

test_that("getGroupbyMapping includes fixed observed shape for TP without shape index", {
  makePlotData <- function(useShapeIndex = FALSE) {
    list(
      useColorIndex = function() TRUE,
      useShapeIndex = function() useShapeIndex
    )
  }

  mapping <- getGroupbyMapping(
    plotData = makePlotData(useShapeIndex = FALSE),
    plotType = "TP",
    dataType = "observed"
  )

  expect_true("groupby" %in% names(mapping))
  expect_true("shape" %in% names(mapping))
  expect_equal(rlang::as_label(mapping$shape), '"Observed data"')
})

test_that("getGroupbyMapping includes shape for observed non-TP data when shape index is used", {
  makePlotData <- function(useColorIndex = TRUE, useShapeIndex = TRUE) {
    list(
      useColorIndex = function() useColorIndex,
      useShapeIndex = function() useShapeIndex
    )
  }

  mapping <- getGroupbyMapping(
    plotData = makePlotData(useColorIndex = TRUE, useShapeIndex = TRUE),
    plotType = "PvO",
    dataType = "observed"
  )

  expect_true("groupby" %in% names(mapping))
  expect_true("shape" %in% names(mapping))
  expect_equal(rlang::as_label(mapping$shape), "shapeIndex")
})

test_that("getGroupbyMapping does not include shape for observed non-TP data without shape index", {
  makePlotData <- function(useColorIndex = TRUE, useShapeIndex = FALSE) {
    list(
      useColorIndex = function() useColorIndex,
      useShapeIndex = function() useShapeIndex
    )
  }

  mapping <- getGroupbyMapping(
    plotData = makePlotData(useColorIndex = TRUE, useShapeIndex = FALSE),
    plotType = "PvO",
    dataType = "observed"
  )

  expect_true("groupby" %in% names(mapping))
  expect_false("shape" %in% names(mapping))
})

test_that("getGroupAesthetics standardizes names and returns only supported aesthetics", {
  plotData <- list(
    scaleVectors = list(color = c(a = "black"), fill = c(a = "grey"), size = 2)
  )

  aesthetics <- getGroupAesthetics(plotData)

  expect_equal(aesthetics, c("colour", "fill"))
})

test_that("getFoldDistanceForPvO uses default when missing and custom when provided", {
  plotDataDefault <- list(
    configTable = data.table::data.table(foldDistance_PvO = NA_real_)
  )
  plotDataCustom <- list(
    configTable = data.table::data.table(foldDistance_PvO = 3)
  )

  expect_equal(
    getFoldDistanceForPvO(plotDataDefault),
    ospsuite.plots::getFoldDistanceList(2)
  )
  expect_equal(
    getFoldDistanceForPvO(plotDataCustom),
    ospsuite.plots::getFoldDistanceList(3)
  )
})

test_that("getGeomLineAttributesForTP and getGeomLLOQAttributesForTP depend on observed range", {
  withRange <- list(hasObservedDataRange = function() TRUE)
  withoutRange <- list(hasObservedDataRange = function() FALSE)

  expect_equal(
    getGeomLineAttributesForTP(withRange),
    list(linetype = "solid", show.legend = TRUE)
  )
  expect_equal(getGeomLineAttributesForTP(withoutRange), list())

  expect_equal(
    getGeomLLOQAttributesForTP(withRange),
    list(linewidth = 0.5, show.legend = TRUE)
  )
  expect_equal(getGeomLLOQAttributesForTP(withoutRange), list())
})

test_that("getFootNoteLines returns source line for reference data", {
  dataObserved <- data.table::data.table(
    dataClass = "individual",
    yErrorType = NA_character_
  )
  dtDataReference <- data.table::data.table(reference = c("A", "B", "A"))

  lines <- getFootNoteLines(dataObserved, dtDataReference)
  expect_true(any(grepl("Data source: [A, B]", lines, fixed = TRUE)))
})

test_that("getFootNoteLines includes aggregated-data description", {
  dataObserved <- data.table::data.table(
    dataClass = DATACLASS$tpAggregated,
    yErrorType = ospsuite::DataErrorType$ArithmeticStdDev
  )

  lines <- getFootNoteLines(
    dataObserved = dataObserved,
    dtDataReference = data.table::data.table(reference = character(0))
  )

  expect_true(any(grepl("Observed data is displayed as", lines, fixed = TRUE)))
})

test_that("getMapSimulatedAndObserved returns NULL when no observed data", {
  plotData <- list(hasObservedData = function() FALSE)
  expect_null(getMapSimulatedAndObserved(plotData))
})

test_that("getMapSimulatedAndObserved returns mapping table for simulated and observed", {
  plotData <- list(
    hasObservedData = function() TRUE,
    data = data.table::data.table(
      dataType = c("simulated", "observed"),
      colorIndex = c("Sim", "Obs")
    ),
    scaleVectors = list(colour = c("black"), fill = c("grey"))
  )

  result <- getMapSimulatedAndObserved(plotData)

  expect_true(is.data.table(result))
  expect_equal(result$simulated, "Sim")
  expect_equal(result$observed, "Obs")
  expect_equal(result$color, "black")
  expect_equal(result$fill, "grey")
})

test_that("checkAndAdjustYlimits returns explicit limits when expression is valid", {
  simulated <- data.table::data.table(
    xValues = c(0, 5, 10),
    yValues = c(10, 6, 2),
    yUnit = "mg/l",
    dataClass = DATACLASS$tpIndividual,
    yErrorType = NA_character_
  )
  observed <- data.table::data.table(
    xValues = c(1, 3),
    yValues = c(3, 4),
    yUnit = "mg/l",
    lloq = NA_real_
  )
  plotData <- list(
    configTable = data.table::data.table(ylimit_linear = "c(1, 8)"),
    getDataForTimeRange = function(
      filterName,
      plotCounter,
      yScale,
      typeFilter = NULL
    ) {
      if (identical(typeFilter, "simulated")) {
        return(simulated)
      }
      return(observed)
    }
  )

  limits <- checkAndAdjustYlimits(
    plotData = plotData,
    yScale = "linear",
    timeRangeFilter = "allTimeRanges",
    plotType = "TP",
    plotCounter = 1
  )

  expect_equal(limits, c(1, 8))
})

test_that("checkAndAdjustYlimits throws when observed values are outside explicit limits", {
  simulated <- data.table::data.table(
    xValues = c(0, 5, 10),
    yValues = c(10, 6, 2),
    yUnit = "mg/l",
    dataClass = DATACLASS$tpIndividual,
    yErrorType = NA_character_
  )
  observed <- data.table::data.table(
    xValues = c(1, 3),
    yValues = c(0.5, 12),
    yUnit = "mg/l",
    lloq = NA_real_
  )
  plotData <- list(
    configTable = data.table::data.table(ylimit_linear = "c(1, 8)"),
    getDataForTimeRange = function(
      filterName,
      plotCounter,
      yScale,
      typeFilter = NULL
    ) {
      if (identical(typeFilter, "simulated")) {
        return(simulated)
      }
      return(observed)
    }
  )

  expect_error(
    checkAndAdjustYlimits(
      plotData = plotData,
      yScale = "linear",
      timeRangeFilter = "allTimeRanges",
      plotType = "TP",
      plotCounter = 1
    )
  )
})

test_that("checkAndAdjustYlimits auto-computes lower bound for log scale", {
  simulated <- data.table::data.table(
    xValues = c(0, 2, 4, 6, 8, 10),
    yValues = c(10, 8, 4, 2, 1, 0.5),
    yUnit = "mg/l",
    dataClass = DATACLASS$tpIndividual,
    yErrorType = NA_character_
  )
  observed <- data.table::data.table(
    xValues = c(1, 3),
    yValues = c(6, 3),
    yUnit = "mg/l",
    lloq = c(NA_real_, NA_real_)
  )
  plotData <- list(
    configTable = data.table::data.table(ylimit_log = NA_character_),
    getDataForTimeRange = function(
      filterName,
      plotCounter,
      yScale,
      typeFilter = NULL
    ) {
      if (identical(typeFilter, "simulated")) {
        return(simulated)
      }
      return(observed)
    }
  )

  limits <- checkAndAdjustYlimits(
    plotData = plotData,
    yScale = "log",
    timeRangeFilter = "allTimeRanges",
    plotType = "TP",
    plotCounter = 1
  )

  expect_true(is.numeric(limits))
  expect_equal(length(limits), 2)
  expect_true(is.finite(limits[1]))
  expect_true(limits[1] > 0)
  expect_true(is.na(limits[2]))
})

test_that("setManualScaleVectors handles non-TP branch without color and shape indices", {
  skip_if_not_installed("ggplot2")
  basePlot <- ggplot2::ggplot(
    data.frame(x = 1, y = 1, grp = "Observed data"),
    ggplot2::aes(x = x, y = y, colour = grp)
  ) +
    ggplot2::geom_point()

  plotData <- list(
    scaleVectors = list(colour = c("Observed data" = "black")),
    useColorIndex = function() FALSE,
    useShapeIndex = function() FALSE
  )

  result <- setManualScaleVectors(
    plotObject = basePlot,
    plotData = plotData,
    plotType = "PvO"
  )

  expect_s3_class(result, "gg")
})

test_that("updateGuides works for TP without simulated population and without observed data", {
  skip_if_not_installed("ggplot2")
  basePlot <- ggplot2::ggplot(
    data.frame(x = 1, y = 1, grp = "A"),
    ggplot2::aes(x = x, y = y, colour = grp)
  ) +
    ggplot2::geom_point()

  plotData <- list(
    useColorIndex = function() TRUE,
    useShapeIndex = function() FALSE,
    hasSimulatedPop = function() FALSE,
    hasObservedData = function() FALSE,
    tpLabelSimulatedMean = "mean",
    tpLabelSimulatedRange = "range",
    reverseLegend = FALSE
  )

  result <- updateGuides(
    plotData = plotData,
    plotObject = basePlot,
    plotType = "TP"
  )

  expect_s3_class(result, "gg")
})

test_that("getCaptionForPlot creates TP caption with subject and scale text", {
  plotData <- list(
    dtCaption = data.table::data.table(
      timeRangeTag = "all",
      counter = 1,
      displayNameOutput = "Plasma Concentration",
      scenarioLongName = "Scenario 1",
      timeRangeCaption = "",
      individualId = "42"
    ),
    timeRangeTagFilter = list(allTimeRanges = 'timeRangeTag == "all"'),
    configTable = data.table::data.table(plotCaptionAddon = NA_character_)
  )

  caption <- getCaptionForPlot(
    plotData = plotData,
    yScale = "linear",
    timeRangeFilter = "allTimeRanges",
    plotType = "TP",
    plotCounter = 1
  )

  expect_true(grepl("Concentration-time profiles", caption, fixed = TRUE))
  expect_true(grepl("for subject", caption, fixed = TRUE))
  expect_true(grepl("linear", caption, fixed = TRUE))
})
