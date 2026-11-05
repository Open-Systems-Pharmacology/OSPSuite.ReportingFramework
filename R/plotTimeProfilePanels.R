#' Generate Time Profile Panels
#'
#' The `plotTimeProfiles` function creates a series of time profile plots as facet panels
#' from the provided simulated and observed data. This function is designed to visualize complex time-dependent
#' data by allowing for multiple plots organized in a user-defined layout.
#'
#' @param projectConfiguration An object of class `ProjectConfiguration` containing project settings,
#' including file paths and scenario definitions. This is essential for the function to access the necessary data.
#'
#' @param onePlotConfig A configuration table for the specific plot, detailing plot parameters.
#'
#' @param dataObserved A `data.table` (formatted as produced by `readObservedDataByDictionary`)
#' or `DataCombined` object containing the observed data to be plotted.
#'
#' @param scenarioResults A list containing simulated scenario results.
#'
#' @param nMaxFacetRows An integer specifying the maximum number of facet rows (default is 4).
#'
#' @param facetAspectRatio A numeric value specifying the aspect ratio for the facets (default is 0.5).
#'
#' @param aggregationFlag A character vector indicating the type of aggregation function to use
#' for simulated data. Options include "GeometricStdDev" (default), "ArithmeticStdDev", "Percentiles",
#' and "Custom".
#'
#' @param percentiles A numeric vector of percentiles to consider if the aggregation method is set
#' to "Percentiles" (default is c(5, 50, 95)).
#'
#' @param customFunction An optional custom function for aggregation.
#' A custom function should take a numeric vector `y` as input and return a list containing:
#'
#' - `yValues`: The aggregated value (e.g., mean).
#' - `yMin`: The lower value of the aggregated data (e.g., mean - sd).
#' - `yMax`: The upper value of the aggregated data (e.g., mean + sd).
#' - `yErrorType`: A string indicating the type of error associated with the aggregation,
#' it is used in plot legends and captions. It must be a concatenation of the descriptor of `yValues`
#' and the descriptor of `yMin - yMax` range separated by "|" (e.g., "mean | standard deviation"
#' or "median | 5th - 95th percentile").
#'
#' @param referenceScaleVector A named list that configures colors for the display of
#' reference scenarios. The names must be consistent with the labels defined in the config table column
#' `colorLegend`. The first entry corresponds to aesthetic 'color', and the second entry to aesthetic 'fill'.
#'
#' Example:
#' referenceScaleVector = list(
#'   Simulation = c('blue', 'lightblue'),
#'   Reference = c('darkred', 'red')
#' )
#' # Default is list()
#'
#' @param checkForUnusedData A boolean indicating whether to perform quality control on data usage.
#' If TRUE, `plotList` contains an additional entry `unusedData`.
#'
#' @param ... Additional arguments passed to `ospsuite.plots::plotTimeprofile`.
#'
#' @return A list of generated time profile plots
#'
#' @details The function primarily focuses on simulated scenarios, with each panel displaying the
#' result of one scenario, or one scenario versus a reference scenario.
#' Plots containing more than one scenario or only observed data are not supported.
#'
#' The function is intended to be used in combination with the `runPlot` function.
#' See the vignette `Plot and Report Generation` for more details.
#'
#' There exists a helper function `addDefaultConfigForTimeProfilePlots` which is a helpful utility designed to
#' streamline the process of setting up your Excel configuration sheet for time profile plots.
#' This function automatically populates a new sheet with default values that you can
#' customize according to your needs.
#'
#' There is a tutorial for this function with many examples
#' in the accompanying vignette: `Time Profile Plotting Tutorial`.
#'
#' @examples
#' \dontrun{
#' # Example of using the referenceScaleVector parameter for a DDI Analysis
#' referenceScaleVector <- list(
#'   DDI = c("#843C39FF", "#AD494AFF"),
#'   Control = c("#3182BDFF", "#6BAED6FF")
#' )
#' # The scenario would be labeled as 'DDI', with aesthetic color as dark red ("#843C39FF") and
#' # aesthetic fill as light red ("#AD494AFF").
#' # The reference scenario would be labeled 'Control',
#' # with aesthetic color as dark blue ("#3182BDFF") and
#' # aesthetic fill as light blue ("#6BAED6FF").
#'
#' # Example of using the referenceScaleVector parameter for a comparison between a pediatric
#' # and an adult simulation using the default colors
#' referenceScaleVector <- list(
#'   "Pediatric population" = c(NA, NA),
#'   "Adult population" = c(NA, NA)
#' )
#'
#' # The scenario would be labeled as 'Pediatric population', using the default colors with aesthetic
#' # color dark blue ("#3182BDFF") and aesthetic fill as light blue ("#6BAED6FF").
#' # The reference scenario would be labeled 'Adult population', with both aesthetic color and fill
#' # as 'grey'.
#'
#' # Example of using the aggregationFlag parameter "Custom"
#' plotList <- runPlot(
#'   nameOfplotFunction = "plotTimeProfiles",
#'   configTableSheet = "myTimeProfile",
#'   projectConfiguration = projectConfiguration,
#'   suppressExport = TRUE,
#'   inputs = list(
#'     scenarioResults = scenarioResults,
#'     dataObserved = dataObserved,
#'     aggregationFlag = c("Custom"),
#'     customFunction = function(y) {
#'       list(
#'         yValues = mean(y),
#'         yMin = mean(y) - sd(y),
#'         yMax = mean(y) + sd(y),
#'         yErrorType = "mean | standard deviation"
#'       )
#'     }
#'   )
#' )
#'
#' # Example of using the aggregationFlag parameter "Percentiles"
#' plotList <- runPlot(
#'   nameOfplotFunction = "plotTimeProfiles",
#'   configTableSheet = "myTimeProfile",
#'   projectConfiguration = projectConfiguration,
#'   suppressExport = TRUE,
#'   inputs = list(
#'     scenarioResults = scenarioResults,
#'     dataObserved = observedData,
#'     aggregationFlag = c("Percentiles"),
#'     percentiles = c(0.025, 0.5, 0.975)
#'   )
#' )
#' }
#'
#' @export
#' @family plot functions
plotTimeProfiles <- function(
  projectConfiguration,
  onePlotConfig,
  dataObserved = NULL,
  scenarioResults,
  nMaxFacetRows = 4,
  facetAspectRatio = 0.5,
  aggregationFlag = c(
    "GeometricStdDev",
    "ArithmeticStdDev",
    "Percentiles",
    "Custom"
  ),
  percentiles = getOspsuite.plots.option(
    optionKey = OptionKeys$defaultPercentiles
  ),
  customFunction = NULL,
  checkForUnusedData = FALSE,
  referenceScaleVector = list(),
  ...
) {
  # initialize data.table variables
  checkmate::assertIntegerish(nMaxFacetRows, lower = 1, len = 1)
  checkmate::assertDouble(facetAspectRatio, lower = 0, len = 1)

  # use data.table format for dataObserved
  if (!is.null(dataObserved)) {
    if ("DataCombined" %in% class(dataObserved)) {
      dataObserved <- convertDataCombinedToDataTable(dataObserved)
    }

    # add identifier column to pick entries used in this function evaluation
    dataObserved[, .Id := .I]
    data.table::setattr(dataObserved[[".Id"]], "columnType", "identifier")
  }

  # read aggregation function for simulated populations
  aggregationFlag <- match.arg(aggregationFlag)
  aggregationFun <- .getAggregationFunction(
    aggregationFlag,
    percentiles,
    customFunction
  )

  checkmate::assertDataTable(onePlotConfig)
  if (dplyr::n_distinct(onePlotConfig$plotName) > 1) {
    stop(messages$errorplotTimeProfilePanelsMultiplePlotNames())
  }

  ospsuite.utils::logInfo(paste("Create Plot", onePlotConfig$plotName[1]))
  plotData <- PlotDataTimeProfile$new(
    projectConfiguration = projectConfiguration,
    onePlotConfig = onePlotConfig
  )

  # read simulated data and filter observed
  plotData$loadSimulatedResults(
    projectConfiguration = projectConfiguration,
    aggregationFun = aggregationFun,
    scenarioResults = scenarioResults
  )
  plotData$filterObservedDataForPlot(
    dataObserved = dataObserved
  )

  if (
    any(plotData$data$dataClass == DATACLASS$tpAggregated) &&
      plotData$data[dataClass == DATACLASS$tpAggregated, uniqueN(yErrorType)] >
        1
  ) {
    tmp <- unique(plotData$data[, c("dataType", "yErrorType")])
    stop(messages$errorplotTimeProfilePanelsInconsistentAggregationMethods())
  }

  # replicate and filter data for each Time Range Tag
  plotData$addTimeRangeTags()

  # make sure everything will be plotted in correct order
  plotData$setOrderAndFactors()

  # split data to plot panels
  plotData$splitDataToPanels(
    nMaxFacetRows = nMaxFacetRows
  )

  # add columns for indices
  plotData$setIndexColumns(referenceScaleVector = referenceScaleVector)
  # add predicted observed
  plotData$addPredictedForObserved()

  plotList <- list()
  for (plotType in c("TP", "PvO", "ResvT", "ResvO", "ResH", "QQ")) {
    plotList <- c(
      plotList,
      .generatePlotForPlotType(
        plotData = plotData,
        facetAspectRatio = facetAspectRatio,
        plotType = plotType,
        ...
      )
    )
  }

  if (checkForUnusedData) {
    # this is a QC functionality which should not done in a valid Run
    .stopHelperFunction()

    if (plotData$hasObservedData()) {
      plotList[["unusedDataRows"]] <-
        dataObserved[
          !(.Id %in%
            plotData$data[dataType == "observed"][[".Id"]])
        ]
    }
  }

  return(plotList)
}

#' Generate plots for a specific plot type
#'
#' @param plotData Object containing the data for the plot.
#' @param facetAspectRatio Aspect ratio for the facets.
#' @param plotType Type of the plot to generate.
#' @param ... Additional arguments forwarded to the underlying ospsuite plotting functions.
#'
#' @details
#' Iterates through configured time ranges, y-scales, and caption counters to build
#' a plot object per combination, then applies shared legend/scaling/facet/export logic.
#'
#' @return List of plots.
#' @keywords internal
#' @noRd
.generatePlotForPlotType <- function(
  plotData,
  facetAspectRatio,
  plotType,
  ...
) {
  checkmate::assertChoice(
    plotType,
    choices = c("TP", "PvO", "ResvT", "ResvO", "ResH", "QQ")
  )
  plotList <- list()
  if (!.isPlotTypeNeededAndPossible(plotType, plotData)) {
    return(plotList)
  }
  for (timeRangeFilter in names(plotData$timeRangeTagFilter)) {
    for (yScale in splitInputs(plotData$configTable$yScale[1])) {
      for (plotCounter in unique(plotData$dtCaption$counter)) {
        yLimits <- .checkAndAdjustYlimits(
          plotData = plotData,
          yScale = yScale,
          timeRangeFilter = timeRangeFilter,
          plotType = plotType,
          plotCounter = plotCounter
        )
        plotObject <-
          switch(
            plotType,
            TP = ospsuite::plotTimeProfile(
              plotData = plotData$getDataForTimeRange(
                timeRangeFilter,
                plotCounter = plotCounter,
                yScale = yScale
              ),
              yScale = yScale,
              mapping = .getGroupbyMapping(
                plotData,
                plotType,
                dataType = "simulated"
              ),
              observedMapping = .getGroupbyMapping(
                plotData,
                plotType,
                dataType = "observed"
              ),
              groupAesthetics = .getGroupAesthetics(plotData),
              yScaleArgs = list(limits = yLimits),
              geomLineAttributes = .getGeomLineAttributesForTP(plotData),
              geomLLOQAttributes = .getGeomLLOQAttributesForTP(plotData),
              showLegendPerDataset = "none",
              ...
            ),
            PvO = ospsuite::plotPredictedVsObserved(
              plotData = plotData$getDataForTimeRange(
                timeRangeFilter,
                typeFilter = "observed",
                plotCounter = plotCounter,
                yScale = yScale
              ),
              xyScale = yScale,
              mapping = .getGroupbyMapping(plotData, plotType),
              groupAesthetics = .getGroupAesthetics(plotData),
              comparisonLineVector = .getFoldDistanceForPvO(plotData),
              showLegendPerDataset = "none",
              ...
            ),
            ResvT = ospsuite::plotResidualsVsCovariate(
              plotData = plotData$getDataForTimeRange(
                timeRangeFilter,
                typeFilter = "observed",
                plotCounter = plotCounter,
                yScale = yScale
              ),
              residualScale = yScale,
              xAxis = "time",
              mapping = .getGroupbyMapping(plotData, plotType),
              groupAesthetics = .getGroupAesthetics(plotData),
              showLegendPerDataset = "none",
              ...
            ),
            ResvO = ospsuite::plotResidualsVsCovariate(
              plotData = plotData$getDataForTimeRange(
                timeRangeFilter,
                typeFilter = "observed",
                plotCounter = plotCounter,
                yScale = yScale
              ),
              residualScale = yScale,
              xAxis = "observed",
              mapping = .getGroupbyMapping(plotData, plotType),
              groupAesthetics = .getGroupAesthetics(plotData),
              showLegendPerDataset = "none",
              ...
            ),
            ResH = ospsuite::plotResidualsAsHistogram(
              plotData = plotData$getDataForTimeRange(
                timeRangeFilter,
                typeFilter = "observed",
                plotCounter = plotCounter,
                yScale = yScale
              ),
              residualScale = yScale,
              mapping = .getGroupbyMapping(plotData, plotType),
              distribution = "normal",
              ...
            ),
            QQ = ospsuite::plotQuantileQuantilePlot(
              plotData = plotData$getDataForTimeRange(
                timeRangeFilter,
                typeFilter = "observed",
                plotCounter = plotCounter,
                yScale = yScale
              ),
              residualScale = yScale,
              mapping = .getGroupbyMapping(plotData, plotType),
              groupAesthetics = .getGroupAesthetics(plotData),
              ...
            )
          )
        plotObject <- .setManualScaleVectors(
          plotObject = plotObject,
          plotData = plotData,
          plotType = plotType
        )
        plotObject <- .updateGuides(
          plotData = plotData,
          plotObject = plotObject,
          plotType = plotType
        )

        # add Facet Columns
        plotObject <- .addFacets(
          plotObject = plotObject,
          nFacetColumns = plotData$nFacetColumns,
          facetScale = plotData$configTable$facetScale[1],
          facetAspectRatio = ifelse(
            plotType %in% c("PvO", "QQ"),
            1,
            facetAspectRatio
          )
        )

        # adjust time labels
        if (plotType %in% c("TP", "ResvT")) {
          plotObject <- plotObject +
            ggplot2::labs(
              x = plotData$getTimeLabelForTimeRange(timeRangeFilter)
            )
        }

        # prepare for export
        plotObject <- setExportAttributes(
          object = plotObject,
          caption = .getCaptionForPlot(
            plotData,
            yScale = yScale,
            timeRangeFilter = timeRangeFilter,
            plotType = plotType,
            plotCounter = plotCounter
          ),
          footNoteLines = .getFootNoteLines(
            dataObserved = plotData$getDataForTimeRange(
              filterName = timeRangeFilter,
              typeFilter = "observed",
              plotCounter = plotCounter,
              yScale = yScale
            ),
            dtDataReference = plotData$dataReference
          )
        )
        figureKey <- paste0(
          paste(
            plotData$plotName,
            plotType,
            ifelse(yScale == "log", "log", "linear"),
            timeRangeFilter,
            sep = "-"
          ),
          ifelse(
            max(plotData$dtCaption$counter) == 1,
            "",
            paste0("-", plotCounter)
          )
        )
        plotList[[figureKey]] <- plotObject
      }
    }
  }

  return(plotList)
}


# plotHelpers ---------
#' Check if a specific plot type is needed and possible
#'
#' @param plotType Type of the plot to check.
#' @param plotData Object containing the data for the plot.
#'
#' @return Logical indicating if the plot type is needed and possible.
#' @keywords internal
#' @noRd
.isPlotTypeNeededAndPossible <- function(plotType, plotData) {
  configColumn <- switch(
    plotType,
    TP = "plot_TimeProfiles",
    PvO = "plot_PredictedVsObserved",
    ResvT = "plot_ResidualsVsTime",
    ResvO = "plot_ResidualsVsObserved",
    ResH = "plot_ResidualsAsHistogram",
    QQ = "plot_QQ",
    stop(messages$errorplotTimeProfilePanelsUnknownPlotType())
  )

  isNeededAndPossible <- FALSE
  if (plotType == "TP") {
    isNeededAndPossible <- as.logical(plotData$configTable[[configColumn]][1])
  } else {
    isNeededAndPossible <-
      as.logical(plotData$configTable[[configColumn]][1]) &&
      plotData$hasObservedData()
    # warning is thrown during data preparation
  }

  return(isNeededAndPossible)
}


#' Get mapping for simulated and observed data
#'
#' @param plotData Object containing the data for the plot.
#'
#' @return Data table mapping simulated and observed data.
#' @keywords internal
#' @noRd
.getMapSimulatedAndObserved <- function(plotData) {
  # initialize data.table variables

  if (!plotData$hasObservedData()) {
    return(NULL)
  }

  mapSimulatedAndObserved <- data.table(
    simulated = as.character(
      plotData$data[dataType == "simulated"]$colorIndex
    ) %>%
      unique(),
    observed = as.character(
      plotData$data[dataType == "observed"]$colorIndex
    ) %>%
      unique(),
    color = plotData$scaleVectors$colour,
    fill = plotData$scaleVectors$fill
  )

  return(mapSimulatedAndObserved)
}

#' Check and adjust Y limits for plots
#'
#' @param ylimitsRaw Raw y-limit expression from configuration.
#' @return Parsed y-limits or NULL.
#' @keywords internal
#' @noRd
.parseYlimitsExpression <- function(ylimitsRaw) {
  if (is.na(ylimitsRaw) || trimws(ylimitsRaw) == "") {
    return(NULL)
  }

  ylimits <- tryCatch(
    eval(parse(text = ylimitsRaw)),
    error = function(e) {
      stop(paste0(
        "Invalid ylimit expression '",
        ylimitsRaw,
        "': ",
        conditionMessage(e)
      ))
    }
  )

  return(ylimits)
}

#' Restrict observed data to simulated unit
#'
#' @param observedData Observed data for a time range.
#' @param simulatedData Simulated data for the same time range.
#' @return Observed data restricted to matching y-unit when needed.
#' @keywords internal
#' @noRd
.restrictObservedToSimulatedUnit <- function(observedData, simulatedData) {
  if (nrow(observedData) > 1) {
    observedData <- observedData[yUnit == unique(simulatedData$yUnit)[1]]
  }

  return(observedData)
}

#' Validate observed data against explicit y-limits
#'
#' @param observedData Observed data for a time range.
#' @param ylimits Explicit y-limits.
#' @return NULL (invisible).
#' @keywords internal
#' @noRd
.assertObservedWithinYlimits <- function(observedData, ylimits) {
  if (is.null(ylimits) || nrow(observedData) == 0) {
    return(invisible())
  }

  if (
    any(observedData$yValues <= ylimits[1], na.rm = TRUE) ||
      any(observedData$yValues >= ylimits[2], na.rm = TRUE)
  ) {
    stop(messages$errorplotTimeProfilePanelsDataOutsideYLimit())
  }

  return(invisible())
}

#' Compute automatic lower y-limit for log-scale TP plots
#'
#' @param simulatedData Simulated data for a time range.
#' @param observedData Observed data for a time range.
#' @return Numeric vector with lower limit and NA upper limit.
#' @keywords internal
#' @noRd
.computeAutoLogYlimits <- function(simulatedData, observedData) {
  # replace Inf and omit first 10% of time range
  timeRangeSim <- range(simulatedData$xValues)
  timeRangeSim[1] <- timeRangeSim[1] + 0.1 * diff(timeRangeSim)

  minY <- Inf
  if (nrow(observedData) > 0) {
    minY <- min(minY, min(observedData[yValues > 0]$yValues))
    if (any(!is.na(observedData$lloq))) {
      minY <- min(minY, min(observedData[!is.na(lloq)]$lloq))
    }
  }

  tmp <- simulatedData[
    xValues >= timeRangeSim[1] &
      xValues <= timeRangeSim[2]
  ]
  if (simulatedData$dataClass[1] == DATACLASS$tpAggregated) {
    if (
      simulatedData$yErrorType[1] == ospsuite::DataErrorType$ArithmeticStdDev
    ) {
      tmp <- tmp[, yMin := yValues - yErrorValues]
    }
    if (
      simulatedData$yErrorType[1] == ospsuite::DataErrorType$GeometricStdDev
    ) {
      tmp <- tmp[, yMin := yValues / yErrorValues]
    }
    minY <- min(c(
      minY,
      tmp[
        !is.na(yMin) &
          yMin > 0
      ]$yMin
    ))
  } else {
    minY <- min(c(
      minY,
      tmp[!is.na(yValues) & yValues > 0]$yValues
    ))
  }

  return(c(minY / 2, NA))
}

#' Check and adjust Y limits for plots
#'
#' @param yScale Y scale of the plot.
#' @param plotData Object of class `PlotDataTimeProfile`.
#' @param timeRangeFilter Name of the time range filter.
#' @param plotType Type of the plot.
#' @param plotCounter Counter selecting the caption subgroup to plot.
#'
#' @details
#' For time-profile panels, this validates user-provided y-limit expressions,
#' ensures observed points are not clipped, and derives robust defaults for log scale.
#'
#' @return Numeric vector of Y limits.
#' @keywords internal
#' @noRd
.checkAndAdjustYlimits <- function(
  plotData,
  yScale,
  timeRangeFilter,
  plotType,
  plotCounter
) {
  # initialize data.table variables

  if (plotType != "TP") {
    return(NULL)
  }

  ylimitsRaw <- plotData$configTable[[paste0("ylimit_", yScale)]][1]
  ylimits <- .parseYlimitsExpression(ylimitsRaw)

  simulatedData <- plotData$getDataForTimeRange(
    timeRangeFilter,
    typeFilter = "simulated",
    plotCounter = plotCounter,
    yScale = yScale
  )

  observedData <- plotData$getDataForTimeRange(
    timeRangeFilter,
    typeFilter = "observed",
    plotCounter = plotCounter,
    yScale = yScale
  )
  observedData <- .restrictObservedToSimulatedUnit(observedData, simulatedData)

  .assertObservedWithinYlimits(observedData, ylimits)

  # for log scale set always limit to cut very low simulated values at the beginning
  if (is.null(ylimits) && yScale == "log") {
    ylimits <- .computeAutoLogYlimits(simulatedData, observedData)
  }

  return(ylimits)
}

#' Get mapping for "groupby" aesthetics
#'
#' @param plotData Object containing the data for the plot.
#' @param plotType Type of the plot.
#' @param dataType Type of the data, either "observed" or "simulated".
#'
#' @details
#' The mapping logic distinguishes observed and simulated layers so legend behavior
#' and grouping remain consistent across TP, PvO, residual, and QQ plot types.
#'
#' @return Aesthetic mappings for grouping.
#' @keywords internal
#' @noRd
.getGroupbyMapping <- function(plotData, plotType, dataType = "observed") {
  checkmate::assertChoice(
    plotType,
    choices = c("TP", "PvO", "ResvT", "ResvO", "ResH", "QQ")
  )
  checkmate::assertChoice(
    dataType,
    choices = c("observed", "simulated")
  )

  # avoid warning for global variable
  shapeMapping <- NULL

  # Base (groupby) mapping â€” this is all that "simulated" ever needs
  if (
    plotType != "TP" && plotData$useShapeIndex() && !plotData$useColorIndex()
  ) {
    mapping <- ggplot2::aes(groupby = shapeIndex)
  } else {
    mapping <- ggplot2::aes(groupby = colorIndex)
  }

  # Only "observed" data gets a shape aesthetic added on top
  if (dataType == "observed") {
    if (plotType == "TP") {
      shapeMapping <- if (plotData$useShapeIndex()) {
        ggplot2::aes(shape = shapeIndex)
      } else {
        ggplot2::aes(shape = "Observed data")
      }
    } else if (plotData$useShapeIndex()) {
      shapeMapping <- ggplot2::aes(shape = shapeIndex)
    } else {}

    if (!is.null(shapeMapping)) {
      mapping <- utils::modifyList(mapping, shapeMapping)
    }
  }

  return(mapping)
}
#' Get group aesthetics from scale vectors
#'
#' @param plotData Object containing the data for the plot.
#'
#' @return Vector with aesthetics to group by.
#' @keywords internal
#' @noRd
.getGroupAesthetics <- function(plotData) {
  return(intersect(
    c("colour", "fill", "shape"),
    ggplot2::standardise_aes_names(c(
      names(plotData$scaleVectors)
    ))
  ))
}

#' Resolve color legend title for time-profile panel plots
#'
#' @param plotData Object containing the data for the plot.
#' @param plotType Type of the plot.
#' @param legendTitleObservedData Default legend title for observed data.
#'
#' @details
#' The color legend title depends on plot type and on whether simulated population
#' summaries or observed-only groupings are being shown.
#' @return Character scalar for color/fill legend title.
#' @keywords internal
#' @noRd
.getLegendTitleColor <- function(plotData, plotType, legendTitleObservedData) {
  if (plotData$useColorIndex()) {
    if (plotType == "TP") {
      if (plotData$hasSimulatedPop()) {
        return(paste0(
          "Simulated data\n",
          plotData$tpLabelSimulatedMean,
          " with ",
          plotData$tpLabelSimulatedRange
        ))
      }

      return(paste("Simulated", plotData$tpLabelSimulatedMean))
    }

    return(legendTitleObservedData)
  }

  if (plotData$useShapeIndex() && plotType != "TP") {
    return(legendTitleObservedData)
  }

  return("")
}

#' Resolve shape legend title for time-profile panel plots
#'
#' @param plotData Object containing the data for the plot.
#' @param plotType Type of the plot.
#' @param legendTitleObservedData Default legend title for observed data.
#' @return Character scalar for shape legend title.
#' @keywords internal
#' @noRd
.getLegendTitleShape <- function(plotData, plotType, legendTitleObservedData) {
  if (
    plotData$useShapeIndex() || (plotType != "TP" && plotData$useColorIndex())
  ) {
    return(legendTitleObservedData)
  }

  return("")
}

#' Add observed-data shape and linetype guides
#'
#' @param plotObject The ggplot object to update.
#' @param plotType Type of the plot.
#' @param legendTitleShape Title for shape legend.
#' @return Updated ggplot object.
#' @keywords internal
#' @noRd
.addObservedGuides <- function(plotObject, plotType, legendTitleShape) {
  plotObject <- plotObject +
    ggplot2::guides(
      shape = ggplot2::guide_legend(
        title = legendTitleShape,
        order = ifelse(plotType == "TP", 2, 1),
        override.aes = list(
          alpha = 0.7,
          color = "black",
          linewidth = 0.5,
          fill = "black"
        )
      )
    )

  if (plotType == "TP") {
    plotObject <- plotObject +
      ggplot2::guides(
        linetype = ggplot2::guide_legend(
          title = "",
          order = 3,
          override.aes = list(
            color = "black",
            linewidth = 0.5,
            linetype = "dashed",
            shape = NA
          )
        )
      )
  }

  return(plotObject)
}

#' Update guides and scales
#'
#' @param plotData Object containing the data for the plot.
#' @param plotObject The ggplot object to update.
#' @param plotType Type of the plot.
#'
#' @details
#' Guide composition depends on whether color and shape indices are active and on
#' the selected panel type; this function centralizes those conditional legend rules.
#'
#' @return Updated ggplot object with guides.
#' @keywords internal
#' @noRd
.updateGuides <- function(plotData, plotObject, plotType) {
  # Initialize legend titles
  legendTitleObservedData <- "Observed data"
  legendTitleColor <- .getLegendTitleColor(
    plotData = plotData,
    plotType = plotType,
    legendTitleObservedData = legendTitleObservedData
  )

  # Override aesthetics for shape if plotType is "TP"
  overrideAes <- if (plotType == "TP") list(shape = NA) else list()

  # Add guides for color and fill
  plotObject <- plotObject +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        title = legendTitleColor,
        order = 1,
        override.aes = overrideAes,
        reverse = plotData$reverseLegend
      ),
      fill = ggplot2::guide_legend(
        title = legendTitleColor,
        order = 1,
        reverse = plotData$reverseLegend
      )
    )

  if (!plotData$hasSimulatedPop() & plotType == "TP") {
    plotObject <- plotObject +
      ggplot2::guides(fill = "none")
  }

  # Check if there is observed data to add shape guide
  if (plotData$hasObservedData()) {
    legendTitleShape <- .getLegendTitleShape(
      plotData = plotData,
      plotType = plotType,
      legendTitleObservedData = legendTitleObservedData
    )
    plotObject <- .addObservedGuides(
      plotObject = plotObject,
      plotType = plotType,
      legendTitleShape = legendTitleShape
    )
  }

  return(plotObject)
}

#' Set manual scale vectors for plots
#'
#' @param plotObject The ggplot object to update.
#' @param plotData Object containing the data for the plot.
#' @param plotType Type of the plot.
#'
#' @return Updated ggplot object with manual scale vectors.
#' @keywords internal
#' @noRd
.setManualScaleVectors <- function(plotObject, plotData, plotType) {
  for (aesthetic in names(plotData$scaleVectors)) {
    scaleVector <- plotData$scaleVectors[[aesthetic]]
    labels <- waiver()

    if (
      plotType != "TP" &
        !plotData$useColorIndex() &
        !plotData$useShapeIndex()
    ) {
      labels <- "Observed data"
    } else if (
      plotType != "TP" &
        !plotData$useColorIndex() &
        plotData$useShapeIndex() &
        aesthetic != "shape"
    ) {
      scaleVector <-
        stats::setNames(
          rep(
            plotData$scaleVectors[[aesthetic]],
            length(plotData$scaleVectors$shape)
          ),
          names(plotData$scaleVectors$shape)
        )
    }

    plotObject <- plotObject +
      ggplot2::scale_discrete_manual(
        aesthetic,
        values = scaleVector,
        labels = labels
      )
  }

  return(plotObject)
}

#' Generate a caption for the plot
#'
#' @param plotData Object containing the data for the plot.
#' @param yScale Y scale of the plot.
#' @param timeRangeFilter Name of the time range filter.
#' @param plotType Type of the plot.
#' @param plotCounter counter of plot
#'
#' @return Caption text for the plot.
#' @keywords internal
#' @noRd
.getCaptionForPlot <- function(
  plotData,
  yScale,
  timeRangeFilter,
  plotType,
  plotCounter
) {
  checkmate::assertChoice(
    plotType,
    choices = c("TP", "PvO", "ResvT", "ResvO", "ResH", "QQ")
  )
  # avoid warning for global variable
  dtCaption <-
    plotData$dtCaption[
      eval(parse(text = plotData$timeRangeTagFilter[[timeRangeFilter]])) &
        counter == plotCounter
    ]

  plotTypeTxt <- switch(
    plotType,
    TP = "Concentration-time profiles",
    PvO = "Predicted vs Observed",
    ResvT = "Residuals vs time values",
    ResvO = "Residuals vs observed values",
    ResH = "Residuals distribution ",
    QQ = "Residuals as quantile-quantile plot"
  )

  scaleName <- ""
  if (plotType %in% c("TP", "PvO")) {
    scaleName <- paste0(
      " ",
      .captionScaleText(
        scale = yScale,
        axisLabel = ifelse(plotType == "TP", "y", "x and y")
      )
    )
  }

  if (
    "individualId" %in% names(dtCaption) && !all(is.na(dtCaption$individualId))
  ) {
    individualtext <- .pasteFigureTags(
      dtCaption,
      captionColumn = "individualId"
    )
    if (individualtext != "") {
      individualtext <- paste(
        " for subject",
        individualtext
      )
    }
  } else {
    individualtext <- ""
  }
  captiontext <- paste0(
    .captionPrefixSimulated(),
    " ",
    plotTypeTxt,
    " for ",
    .pasteFigureTags(dtCaption, captionColumn = "displayNameOutput"),
    " for ",
    .pasteFigureTags(dtCaption, captionColumn = "scenarioLongName"),
    individualtext,
    scaleName,
    .pasteFigureTags(
      dtCaption,
      captionColumn = "timeRangeCaption",
      endWithDot = TRUE,
      startWithBlank = TRUE
    )
  )
  captiontext <- .addCaptionTextAddon(
    captiontext,
    plotData$configTable$plotCaptionAddon[1]
  )

  return(captiontext)
}
#' Constructs footnote lines for aggregated data and data references
#'
#' @param dataObserved Data observed for the plot.
#' @param dtDataReference Data reference table.
#'
#' @return Vector of characters, each entry is one footnote line.
#' @keywords internal
#' @noRd
.getFootNoteLines <- function(dataObserved, dtDataReference) {
  if (nrow(dataObserved) == 0) {
    return(NULL)
  }

  footnoteLines <- character(0)

  if (any(dataObserved$dataClass == DATACLASS$tpAggregated)) {
    errorLabels <- .getErrorLabels(dataObserved$yErrorType[1])

    footnoteLines <- paste(
      "Observed data is displayed as",
      paste(errorLabels, collapse = " and ")
    )
  }

  # filter used data
  if (nrow(dtDataReference) > 0) {
    footnoteLines <- c(
      footnoteLines,
      paste0(
        "Data source: [",
        paste(
          dtDataReference$reference %>%
            unique(),
          collapse = ", "
        ),
        "]   "
      )
    )
  }
  return(footnoteLines)
}
#' Get fold distance for plotType `PvO`
#'
#' @param plotData Object containing the data for the plot.
#'
#' @return List which can be used as input for comparisonLineVector.
#' @keywords internal
#' @noRd
.getFoldDistanceForPvO <- function(plotData) {
  foldDistance <- ifelse(
    !is.na(plotData$configTable$foldDistance_PvO[1]),
    as.double(plotData$configTable$foldDistance_PvO[1]),
    2
  )

  return(ospsuite.plots::getFoldDistanceList(foldDistance))
}
#' get additional inputs for `TP` plotType to enable lines in shape legend
#'
#' @param plotData `data.table` with plot data
#'
#' @return list with additional attributes for geom_line
#' @keywords internal
#' @noRd
.getGeomLineAttributesForTP <- function(plotData) {
  attributes <- list()
  if (plotData$hasObservedDataRange()) {
    attributes <- list(linetype = "solid", show.legend = TRUE)
  }

  return(attributes)
}

#' get additional inputs for `TP` plotType to enable lines in shape legend
#'
#' @param plotData A `PlotDataTimeProfile` object.
#'
#' @return list with additional attributes for lloq line
#' @keywords internal
#' @noRd
.getGeomLLOQAttributesForTP <- function(plotData) {
  attributes <- list()
  if (plotData$hasObservedDataRange()) {
    attributes <- list(linewidth = 0.5, show.legend = TRUE)
  }

  return(attributes)
}

# validation ----------------

#' Validation of config table for time profiles plots
#'
#' @param configTable Plot configuration table.
#' @param dataObserved Optional observed dataset used for group/id validation.
#' @param scenarioResults Named list of loaded scenario results.
#' @param ... Additional validation arguments forwarded to helper validators.
#' @return NULL (invisible).
#' @export
#' @family plot configuration validation function
validateTimeProfilesConfig <- function(
  configTable,
  dataObserved = NULL,
  scenarioResults,
  ...
) {
  # avoid warning for global variable

  configTablePlots <- validateHeaders(configTable)
  .validateOutputIdsForPlot()
  .validateDataGroupIdsForPlot()

  checkmate::assertCharacter(
    configTablePlots$scenarioLongName,
    any.missing = FALSE,
    .var.name = "longName for relevant scenarios"
  )
  checkmate::assertList(
    scenarioResults,
    any.missing = FALSE,
    null.ok = FALSE,
    min.len = 1
  )
  validateConfigTablePlots(
    configTablePlots = configTablePlots,
    charactersWithoutMissing = c(
      "plotName",
      "scenario",
      "outputPathIds",
      "timeUnit",
      "facetScale",
      "yScale"
    ),
    charactersWithMissing = c(
      "dataGroupIds",
      "plotCaptionAddon",
      "referenceScenario",
      "individualIds"
    ),
    numericColumns = c(
      "timeOffset",
      "timeOffset_Reference",
      "nFacetColumns",
      "foldDistance_PvO"
    ),
    logicalColumns = c(
      "splitPlotsPerTimeRange",
      "plot_TimeProfiles",
      "plot_PredictedVsObserved",
      "plot_ResidualsAsHistogram",
      "plot_ResidualsVsTime",
      "plot_ResidualsVsObserved",
      "plot_QQ"
    ),
    numericRangeColumns = c("ylimit_linear", "ylimit_log"),
    subsetList = list(
      scenario = list(
        cols = c("scenario", "referenceScenario"),
        allowedValues = names(scenarioResults)
      ),
      dataGroupId = list(
        cols = c("dataGroupIds"),
        allowedValues = unique(dataObserved$group)
      ),
      outputPathId = list(
        cols = c("outputPathId"),
        allowedValues = unique(configEnv$outputPaths$outputPathId)
      ),
      individualIds = list(
        cols = "individualIds",
        allowedValues = c(
          "*",
          unique(dataObserved[!is.na(individualId)]$individualId)
        )
      ),
      yscale = list(
        cols = c("yscale"),
        allowedValues = c("linear", "log")
      ),
      timeUnit = list(
        cols = c("timeUnit"),
        allowedValues = ospsuite::getUnitsForDimension("Time")
      ),
      facetScale = list(
        cols = c("facetScale"),
        allowedValues = c("fixed", "free", "free_x", "free_y")
      )
    )
  )

  .validateTimeRangeColumns(configTablePlots)

  validateGroupConsistency(
    dt = configTablePlots,
    valueColumns = c(
      "timeUnit",
      "yScale",
      "ylimit_linear",
      "ylimit_log",
      "colorLegend",
      "plotCaptionAddon",
      "splitPlotsPerTimeRange",
      "nFacetColumns",
      "facetScale",
      "plot_TimeProfiles",
      "plot_PredictedVsObserved",
      "plot_ResidualsAsHistogram",
      "plot_ResidualsVsTime",
      "plot_ResidualsVsObserved",
      "plot_QQ"
    )
  )

  .validateUnitConsistency(configTablePlots = configTablePlots)

  .validateOutputPathIdFormat(configTablePlots = configTablePlots)

  .validateOutputPathIdFormat(
    configTablePlots = configTablePlots,
    column = "individualIds"
  )

  .validateVirtualTwinPop(
    configTablePlots = configTablePlots,
    scenarioResults = scenarioResults
  )

  configTablePlots[,
    invalid := !is.na(referenceScenario) &
      (length(grep("\\(", outputPathIds)) > 0),
    by = .I
  ]
  tmp <- configTablePlots[invalid == TRUE]
  if (nrow(tmp) > 0) {
    stop(messages$errorplotTimeProfilePanelsBracketedOutputsWithReferenceScenarios())
  }

  configTablePlots[,
    invalid := !is.na(referenceScenario) & is.na(colorLegend),
    by = .I
  ]
  tmp <- configTablePlots[invalid == TRUE]
  if (nrow(tmp) > 0) {
    stop(messages$errorplotTimeProfilePanelsReferenceScenariosNeedColorLegend())
  }

  return(invisible())
}


#' Check if panel columns are filled consistently
#'
#' @param configTablePlots `data.table` configuration table without header lines
#'
#' @return NULL (invisible).
#' @keywords internal
#' @noRd
.validateUnitConsistency <- function(
  configTablePlots
) {
  # avoid warning for global variable

  # check if more than two different units are combined in one panel
  configTableList <- split(configTablePlots, by = "plotName")
  for (configPanel in configTableList) {
    outputs <- gsub("[()]", "", splitInputs(configPanel$outputPathIds))
    if (
      dplyr::n_distinct(
        configEnv$outputPaths[outputPathId %in% outputs]$displayUnit
      ) >
        2
    ) {
      stop(messages$errorplotTimeProfilePanelsL2TooManyUnits())
    }
  }

  return(invisible())
}


#' Validates time range columns
#'
#' Time range columns must be character and must contain NA
#' 'total','firstApplication','lastApplication'
#' or a string which evaluates in R to a numeric vector length 2  (e.g. 'c(2,3)' or 'c(2,NA)'
#'
#' @param configTablePlots `data.table` configuration table without header lines
#'
#' @details
#' Custom time ranges are parsed as R expressions and must evaluate to a numeric
#' vector of length two with finite values.
#'
#' @return NULL (invisible).
#' @keywords internal
#' @noRd
.validateTimeRangeColumns <- function(configTablePlots) {
  timeRangeColumns <-
    names(configTablePlots)[grepl("^timeRange_", names(configTablePlots))]

  if (length(timeRangeColumns) == 0) {
    stop(messages$errorplotTimeProfilePanelsL2MissingTimeRange())
  }

  validateAtleastOneEntry(configTablePlots, columnVector = timeRangeColumns)

  validateConfigTablePlots(
    configTablePlots,
    charactersWithMissing = timeRangeColumns
  )

  tryCatch(
    {
      if (
        !all(sapply(
          configTablePlots %>%
            dplyr::select(dplyr::all_of(timeRangeColumns)),
          function(x) {
            valid <- x %in%
              c(NA, "total", "firstApplication", "lastApplication")
            for (entry in x[!valid]) {
              tmp <- tryCatch(
                eval(parse(text = entry)),
                error = function(e) NULL
              )
              valid[x == entry] <-
                !is.null(tmp) &&
                is.numeric(tmp) &&
                length(tmp) == 2 &&
                all(!is.na(tmp))
            }
            return(all(valid))
          }
        ))
      ) {
        stop(messages$errorplotTimeProfilePanelsL2InvalidTimeRangeFormat())
      }
    },
    error = function(err) {
      stop(paste0(
        messages$errorplotTimeProfilePanelsL2TimeRangeParseError(),
        ": ",
        conditionMessage(err)
      ))
    }
  )

  return(invisible())
}

#' Validates output path ID format
#'
#' @param configTablePlots `data.table` configuration table without header lines
#' @param column Column name to validate for balanced parentheses.
#' @return NULL (invisible).
#' @keywords internal
#' @noRd
.validateOutputPathIdFormat <- function(
  configTablePlots,
  column = "outputPathIds"
) {
  # avoid warning for global variable

  tmp <- configTablePlots[,
    .(
      nBracketOpen = sum(grepl("\\(", get(column))),
      nBracketClosed = sum(grepl("\\)", get(column)))
    ),
    by = column
  ]

  tmp <- tmp[nBracketOpen != nBracketClosed]

  if (nrow(tmp) > 0) {
    stop(messages$errorplotTimeProfilePanelsL2UnmatchedBrackets())
  }

  return(invisible())
}

#' Validate Virtual Twin Population in Plot Configuration
#'
#' This function validates the virtual twin population specified in the plot configuration table.
#' It checks for the presence of `individualIds` in scenarios with virtual twin populations,
#' ensures that brackets are not used in `individualIds` for time profile plots, and warns
#' if `individualIds` is filled without a corresponding data group.
#'
#' @param configTablePlots A data table containing the plot configuration, including scenario names and individual IDs.
#' @param scenarioResults A list with scenario results.
#' @return NULL (invisible).
#' @keywords internal
#' @noRd
.validateVirtualTwinPop <- function(configTablePlots, scenarioResults) {
  # avoid warning for global variable

  popScenarios <- scenarioResults[unlist(lapply(
    scenarioResults,
    function(scenarioResult) {
      (!is.null(scenarioResult$population) &&
        "Population" %in% class(scenarioResult$population))
    }
  ))]

  indScenariosNames <- names(popScenarios[unlist(lapply(
    popScenarios,
    function(scenario) {
      "ObservedIndividualId" %in% scenario$population$allCovariateNames
    }
  ))])

  # individualIds is filled
  if (
    any(is.na(configTablePlots[scenario %in% indScenariosNames]$individualIds))
  ) {
    stop(messages$errorplotTimeProfilePanelsL2VirtualPopMissingIndividualIds())
  }

  tmp <- configTablePlots[
    scenario %in% indScenariosNames & as.logical(plot_TimeProfiles)
  ]
  errorRows <- which(
    grepl("\\(", tmp$individualIds) | grepl("\\)", tmp$individualIds)
  )
  if (length(errorRows) > 0) {
    stop(messages$errorplotTimeProfilePanelsL2BracketsDisallowedInIndividualIds())
  }

  tmp <- configTablePlots[
    !(scenario %in% indScenariosNames) &
      !is.na(individualIds) &
      is.na(dataGroupIds)
  ]
  if (nrow(tmp) > 0) {
    warning(messages$warningplotTimeProfilePanelsIndividualIdsIgnored())
  }

  popScenariosNames <- setdiff(names(popScenarios), indScenariosNames)
  tmp <- configTablePlots[
    !is.na(referenceScenario) &
      ((!(scenario %in% popScenariosNames) &
        (referenceScenario %in% popScenariosNames)) |
        ((scenario %in% popScenariosNames) &
          !(referenceScenario %in% popScenariosNames)))
  ]
  if (nrow(tmp) > 0) {
    stop(messages$errorplotTimeProfilePanelsL2ScenarioTypeConflict())
  }

  return(invisible())
}

# support usability --------------------

#' Add Default Configuration for Time Profile Plots
#'
#' This function adds a default configuration sheet for time profile plots to the plot configuration table.
#' It can either create a new sheet or overwrite an existing one based on the specified parameters.
#'
#' @param projectConfiguration A `ProjectConfiguration` class object containing configuration details, including:
#'   - `addOns$reportsFile`: A string representing the file path to the Excel workbook containing the plot configurations.
#'
#' @param dataObserved Optional. A data object containing observed data, if available.
#'
#' @param sheetName A character string specifying the name of the sheet in the plot configuration table.
#'   Default is `TimeProfiles`.
#'
#' @param overwrite A boolean indicating whether existing configurations should be overwritten.
#'   Default is FALSE.
#'
#' @details
#' The function retrieves scenario definitions, output path IDs, and data groups from the project configuration.
#' It checks if the specified sheet already exists and whether to overwrite it. If not, it creates a new header
#' and fills in the default configuration values for the time profile plots.
#'
#' Additionally, the function performs a validity check to ensure that it is not executed during a context
#' where helper functions are prohibited (`validRun`). If such a context is detected, an error is raised to prevent execution.
#'
#'
#' @return invisible(NULL). Updates the Excel workbook in place.
#'
#' @examples
#' \dontrun{
#' addDefaultConfigForTimeProfilePlots(
#'   projectConfiguration = projectConfiguration,
#'   dataObserved = dataObserved,
#'   sheetName = "TimeProfiles"
#' )
#' }
#' @export
#' @family plot functions
#' @family plot configuration helper function
addDefaultConfigForTimeProfilePlots <- function(
  projectConfiguration,
  dataObserved = NULL,
  sheetName = "TimeProfiles",
  overwrite = FALSE
) {
  # avoid warnings for global variables during check

  # this function stops in valid runs
  .stopHelperFunction()

  # use data.table format for dataObserved
  if (!is.null(dataObserved)) {
    if ("DataCombined" %in% class(dataObserved)) {
      dataObserved <- convertDataCombinedToDataTable(dataObserved)
    }
  }

  # update configenvironmant
  loadConfigTableEnvironment(projectConfiguration)
  scenarios <- copy(configEnv$scenarios)

  wb <- openxlsx::loadWorkbook(projectConfiguration$addOns$reportsFile)

  if (sheetName %in% wb$sheet_names & !overwrite) {
    dtNewHeader <- xlsxReadData(
      wb,
      sheetName = sheetName,
      skipDescriptionRow = TRUE
    )
    scenarios <- scenarios[!(scenarioName %in% unique(dtNewHeader$scenario))]
  } else {
    dtNewHeader <- data.table()
  }

  if (nrow(dtNewHeader) == 0) {
    dtNewHeader <- data.table(
      level = 1,
      header = "Concentration time profiles"
    )
  }

  dtNewConfig <- .createNewConfig(scenarios, dataObserved)

  wb <- xlsxAddDataUsingTemplate(
    wb = wb,
    templateSheet = "TimeProfiles",
    sheetName = sheetName,
    dtNewData = rbind(
      dtNewHeader,
      dtNewConfig,
      fill = TRUE
    )
  )

  openxlsx::saveWorkbook(
    wb,
    projectConfiguration$addOns$reportsFile,
    overwrite = TRUE
  )
}


#' Create New Configuration for Time Profile Plots
#'
#' This internal function generates a new configuration data table for time profile plots based on the provided scenarios and project configuration.
#'
#' @param scenarios A data.table containing scenario definitions.
#' @param dataObserved Optional. A data object containing observed data, if available.
#'
#' @return A data.table containing the new configuration for time profile plots.
#' @keywords internal
#' @noRd
.createNewConfig <- function(scenarios, dataObserved) {
  dtNewConfig <- data.table(
    plotName = scenarios$scenarioName,
    scenario = scenarios$scenarioName,
    outputPathIds = paste(
      unique(configEnv$outputPaths$outputPathId),
      collapse = ", "
    ),
    timeUnit = "h",
    timeOffset = 0,
    timeOffset_Reference = 0,
    timeRange_total = TIMERANGE$total,
    timeRange_firstApplication = TIMERANGE$firstApplication,
    timeRange_lastApplication = TIMERANGE$lastApplication,
    splitPlotsPerTimeRange = 1,
    nFacetColumns = 3,
    yScale = "linear, log",
    facetScale = "fixed",
    plot_TimeProfiles = 1,
    plot_PredictedVsObserved = 0,
    plot_ResidualsAsHistogram = 0,
    plot_ResidualsVsTime = 0,
    plot_ResidualsVsObserved = 0,
    plot_QQ = 0
  )

  if (any(!is.na(configEnv$dataGroupIds$defaultScenario))) {
    dtNewConfig <- .mergeDataGroups(
      dtNewConfig,
      configEnv$dataGroupIds,
      dataObserved
    )
  }

  return(dtNewConfig)
}

#' Merge Data Groups into Configuration
#'
#' This internal function merges data group information into the configuration table based on the provided scenarios and observed data.
#'
#' @param dtNewConfig A data.table containing the new configuration for time profile plots.
#' @param dtDataGroups A data.table containing data group information.
#' @param dataObserved Optional. A data object containing observed data, if available.
#'
#' @return A data.table with merged data group information.
#' @keywords internal
#' @noRd
.mergeDataGroups <- function(dtNewConfig, dtDataGroups, dataObserved) {
  # initialize data.table variables

  tmp <- dtDataGroups[,
    .(dataGroupIds = paste(as.character(group), collapse = ", ")),
    by = "defaultScenario"
  ]
  data.table::setnames(tmp, old = c("defaultScenario"), new = c("scenario"))

  dtNewConfig <- dtNewConfig %>%
    merge(tmp, by = "scenario", all.x = TRUE, sort = FALSE)

  dtNewConfig[!is.na(dataGroupIds), individualIds := "*"]

  if (!is.null(dataObserved)) {
    for (sc in unique(dtNewConfig[!is.na(dataGroupIds)]$scenario)) {
      dataGroupIds <- splitInputs(dtNewConfig[scenario == sc]$dataGroupIds)
      availableOutputs <- unique(
        dataObserved[group %in% dataGroupIds]$outputPathId
      )
      dtNewConfig[
        scenario == sc,
        outputPathIds := paste(availableOutputs, collapse = ", ")
      ]
    }
  }

  return(dtNewConfig)
}
