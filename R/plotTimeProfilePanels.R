#' Generate Time Profile Panels
#'
#' The `plotTimeProfilePanels` function creates a series of time profile plots as facet panels
#' from the provided observed data. This function is designed to visualize complex time-dependent
#' data by allowing for multiple plots organized in a user-defined layout.
#'
#' This function is particularly useful for researchers who need to visualize
#' concentration-time profiles of various scenarios, compare predicted versus observed data,
#' and analyze residuals cohesively. For more detailed information, please refer to the
#' accompanying vignettes:
#' - **TimeProfilePlots**
#' - **Time Profile Plotting Tutorial**
#'
#' @param projectConfiguration An object of class ProjectConfiguration containing the project settings,
#' including file paths and scenario definitions. This is essential for the function to access the necessary data.
#'
#' @param configTableSheet The name of the sheet in the Excel plot configuration file that defines
#' the plot settings.
#'
#' @param subfolder A character string specifying the subfolder within the output directory where
#' the plots will be saved. Default is name of configTableSheet
#'
#' @param dataObserved A data.table or DataCombined object containing the observed data to be plotted.
#'
#' @param scenarioResults A list with simulated scenario results. This parameter may be an empty list or
#' contain only some of all scenarios defined in the project configuration, in which
#' case the function will attempt to load missing scenario results from the specified folder.
#'
#' @param nFacetColumns An integer specifying the maximum number of facet columns (default is 2)
#' used for the facet type "by Order".
#'
#' @param nMaxFacetRows An integer specifying the maximum number of facet rows (default is 4)
#' used for all facet types.
#'
#' @param facetAspectRatio A numeric value specifying the aspect ratio for the facets (default is 0.5).
#'
#' @param aggregationFlag A character vector indicating the type of aggregation function to use
#' for simulated data. Options include "GeometricStdDev" (Default), "ArithmeticStdDev", "Percentiles",
#' and "Custom".
#'
#' @param percentiles A numeric vector of percentiles to consider if the aggregation method is set
#' to "Percentiles" (default is c(5, 50, 95)).
#'
#' @param customFunction An optional custom function for aggregation.
#' A custom function should take a numeric vector `y` as input and return a list containing:
#'
#' - `yValues`: The aggregated value (e.g., mean).
#'
#' - `yMin`: The lower value of the aggregated data, (e.g. mean - sd).
#'
#' - `yMax`: The upper value of the aggregated data, (e.g. mean + sd).
#'
#' - `yErrorType`: A string indicating the type of error associated with the aggregation,
#' it is used in plot legends and captions. It must be a concatenation of the descriptor of yValues and the descriptor of yMin - yMax range
#' separated by "|" (e.g., "mean | standard deviation" or "median | 5th - 95th percentile").
#'
#' @param referenceScaleVector A named list that configures labels and colors for the display of
#' reference scenarios. The names of the list are displayed as labels in the legend.
#'
#' Default:
#' referenceScaleVector = list(
#'   Simulation = c(NA, NA),
#'   Reference = c(NA, NA)
#' )
#'
#' @return Generates time profile plots and saves them to the specified output directory.
#' The function does not return any value.
#'
#' @details The function primarily focuses on simulated scenarios, with each panel displaying the
#' result of one scenario. Plots containing more than one scenario or only observed data are not
#' supported.
#'
#' @examples
#' \dontrun{
#' # Example of using the referenceScaleVector parameter for a DDI Analysis
#' referenceScaleVector = list(
#'   DDI = c("#843C39FF", "#AD494AFF"),
#'   Control = c("#3182BDFF", "#6BAED6FF")
#' )
#'
#' # The scenario would be labeled as 'DDI', with aesthetic color as dark red ("#843C39FF") and
#' # aesthetic fill as light red ("#AD494AFF").
#' # The reference scenario would be labeled 'Control', with aesthetic color as dark blue ("#3182BDFF") and
#' # aesthetic fill as light blue ("#6BAED6FF").
#'
#' # Example of using the referenceScaleVector parameter for a comparison between a pediatric
#' # and an adult simulation using the default colors
#' referenceScaleVector = list(
#'   'Pediatric population' = c(NA, NA),
#'   'Adult population' = c(NA, NA)
#' )
#'
#' # The scenario would be labeled as 'Pediatric population', with aesthetic color as dark blue ("#3182BDFF") and
#' # aesthetic fill as light blue ("#6BAED6FF").
#' # The reference scenario would be labeled 'Adult population', with both aesthetic color and fill as 'grey'.
#'
#' # Example of using the aggregationFlag parameter "Custom"
#' plotTimeProfilePanels(
#'   projectConfiguration = myProjectConfig,
#'   configTableSheet = "PlotSettings",
#'   dataObserved = myObservedData,
#'   scenarioResults = myScenarioResults,
#'   aggregationFlag = c("Custom"),
#'   customFunction = function(y) {
#' list(yValues = mean(y), yMin = mean(y) - sd(y), yMax = mean(y) + sd(y), yErrorType = "mean | standard deviation")
#'   }
#' )
#'
#' # Example of using the aggregationFlag parameter "Percentiles"
#' plotTimeProfilePanels(
#'   projectConfiguration = myProjectConfig,
#'   configTableSheet = "PlotSettings",
#'   dataObserved = myObservedData,
#'   scenarioResults = myScenarioResults,
#'   aggregationFlag = c("Percentiles"),
#'   percentiles = c(0.025, 0.5, 0.975)
#' )
#' }
#'
#' @export
plotTimeProfilePanels <- function(projectConfiguration,
                                  subfolder,
                                  configTableSheet,
                                  dataObserved,
                                  scenarioResults = list(),
                                  nFacetColumns = 2,
                                  nMaxFacetRows = 4,
                                  facetAspectRatio = 0.5,
                                  aggregationFlag = c(
                                    "GeometricStdDev",
                                    "ArithmeticStdDev",
                                    "Percentiles",
                                    "Custom"
                                  ),
                                  percentiles = c(5, 50, 95),
                                  customFunction = NULL,
                                  referenceScaleVector = list(
                                    Simulation = c(NA, NA),
                                    Reference = c(NA, NA)
                                  )) {
  checkmate::assert_path_for_output(file.path(projectConfiguration$outputFolder, subfolder), overwrite = TRUE)
  checkmate::assertIntegerish(nFacetColumns, lower = 1, len = 1)
  checkmate::assertDouble(facetAspectRatio, lower = 0, len = 1)

  # use data.table format for dataObserved
  if ("DataCombined" %in% class(dataObserved)) {
    dataObserved <- convertDataCombinedToDataTable(dataObserved)
  }

  # add identifier column to pick entries used in this rmdtask
  dataObserved[,.Id := .I]
  data.table::setattr(dataObserved[['.Id']], "columnType", 'identifier')

  # read aggregation function for simulated populations
  aggregationFlag <- match.arg(aggregationFlag)
  aggregationFun <- getAggregationFunction(aggregationFlag, percentiles, customFunction)

  # read configuration tables
  configTable <-
    readTimeprofileConfigTable(
      sheetName = configTableSheet,
      projectConfiguration = projectConfiguration,
      dataObserved = dataObserved
    )

  # call plot function per plotName
  rmdContainer <- generateRmdContainer(
    projectConfiguration = projectConfiguration,
    subfolder = subfolder,
    configTable = configTable,
    plotFunction =
      function(onePlotConfig, rmdContainer, ...) {
        createPanelPlotsForPlotName(
          onePlotConfig = onePlotConfig,
          rmdContainer = rmdContainer,
          projectConfiguration = projectConfiguration,
          scenarioResults = scenarioResults,
          dataObserved = dataObserved,
          nFacetColumns = nFacetColumns,
          nMaxFacetRows = nMaxFacetRows,
          facetAspectRatio = facetAspectRatio,
          aggregationFun = aggregationFun,
          referenceScaleVector = referenceScaleVector)
      }
  )

  # save configTable to evaluate for ePackage
  rmdContainer$configTable <- configTable


  return(rmdContainer)
}


#' Read the configuration table for time profiles
#'
#' @inheritParams plotTimeProfilePanels
#'
#' @return `data.table` with plot configurations.
#' @keywords internal
readTimeprofileConfigTable <- function(projectConfiguration, sheetName, dataObserved) {
  # initialize variable used in data.tables
  level <- NULL


  # read configuration tables
  configTable <- xlsxReadData(
    wb = projectConfiguration$plotsFile,
    sheetName = sheetName,
    skipDescriptionRow = TRUE
  )

  # add scenario names
  configTable <-
    merge(
      configTable,
      configEnv$scenarios[, c('scenarioName', 'longName')],
      by.x = 'scenario',
      by.y = 'scenarioName',
      all.x = TRUE,
      sort = FALSE
    ) %>%
    data.table::setnames('longName','scenarioLongName')

  validateConfigTableForTimeProfiles(
    configTable = configTable,
    dataObserved = dataObserved,
    projectConfiguration = projectConfiguration
  )



  return(configTable)
}
#' Generate one plot as facet panels
#'
#' @inheritParams plotTimeProfilePanels
#' @param rmdContainer Object of class `RmdContainer`.
#' @param aggregationFun Function to aggregate simulated data.
#' @param onePlotConfig configuration table for this plot
#'
#' @return Object of class `rmdContainer` with added figures.
#' @export
createPanelPlotsForPlotName <- function(onePlotConfig,
                                        rmdContainer,
                                        projectConfiguration,
                                        dataObserved,
                                        scenarioResults,
                                        nFacetColumns,
                                        nMaxFacetRows,
                                        facetAspectRatio,
                                        aggregationFun,
                                        referenceScaleVector) {
  message(paste("Create Plot", onePlotConfig$plotName[1]))

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

  # replicate and filter data for each Time Range Tag
  plotData$addTimeRangeTags()

  # make sure everything will be plotted in correct order
  plotData$setOrderAndFactors()

  # split data to plot panels
  plotData$splitDataToPanels(
    nFacetColumns = nFacetColumns,
    nMaxFacetRows = nMaxFacetRows
  )

  # add columns for indices
  plotData$setIndexColumns(referenceScaleVector = referenceScaleVector)

  # add predicted observed
  plotData$addPredictedForObserved()

  for (plotType in c("TP", "PvO", "ResvT", "ResvO", "ResH", "QQ")) {
    rmdContainer <- generatePlotForPlotType(
      plotData = plotData,
      rmdContainer = rmdContainer,
      facetAspectRatio = facetAspectRatio,
      plotType = plotType
    )
  }


  if(getOption('OSPSuite.RF.withEPackage')){
    if (plotData$hasObservedData()){
      rmdContainer$dataObserved <-
        plotData$data[dataType == 'observed', c('.Id', 'scenarioIndex')] %>%
        unique() %>%
        merge(plotData$configTable[, c('scenario', 'scenarioIndex')],
              by = 'scenarioIndex') %>%
        .[, c('.Id', 'scenario')]
    }
  }

  return(rmdContainer)
}

#' Generate plots for a specific plot type
#'
#' @param plotData Object containing the data for the plot.
#' @param rmdContainer Object of class `RmdContainer`.
#' @param facetAspectRatio Aspect ratio for the facets.
#' @param plotType Type of the plot to generate.
#'
#' @return Updated `rmdContainer` with the generated plot.
#' @keywords internal
generatePlotForPlotType <- function(plotData,
                                    rmdContainer,
                                    facetAspectRatio,
                                    plotType) {
  if (!isPlotTypeNeededAndPossible(plotType, plotData)) {
    return(rmdContainer)
  }

  for (timeRangeFilter in names(plotData$timeRangeTagFilter)) {
    for (yScale in splitInputs(plotData$configTable$yScale[1])) {
      for (plotCounter in unique(plotData$dtCaption$counter)) {
        yLimits <- checkAndAdjustYlimits(
          plotData = plotData,
          yScale = yScale,
          timeRangeFilter = timeRangeFilter,
          plotType = plotType,
          plotCounter = plotCounter
        )
        plotObject <-
          switch(plotType,
                 TP = ospsuite_plotTimeProfile( # nolint indentation_linter
                   plotData = plotData$getDataForTimeRange(timeRangeFilter, plotCounter = plotCounter,yScale = yScale),
                   yscale = yScale,
                   mapping = getGroupbyMapping(plotData, plotType),
                   groupAesthetics = getGroupAesthetics(plotData),
                   yscale.args = list(limits = yLimits),
                   geomLineAttributes = getGeomLineAttributesForTP(plotData),
                   geomLLOQAttributes = getGeomLLOQAttributesForTP(plotData)
                 ),
                 PvO = ospsuite_plotPredictedVsObserved(
                   plotData = plotData$getDataForTimeRange(timeRangeFilter, typeFilter = "observed", plotCounter = plotCounter,yScale = yScale),
                   xyscale = yScale,
                   mapping = getGroupbyMapping(plotData, plotType),
                   groupAesthetics = getGroupAesthetics(plotData),
                   comparisonLineVector = getFoldDistanceForPvO(plotData)
                 ),
                 ResvT = ospsuite_plotResidualsVsTime(
                   plotData = plotData$getDataForTimeRange(timeRangeFilter, typeFilter = "observed", plotCounter = plotCounter,yScale = yScale),
                   residualScale = yScale,
                   mapping = getGroupbyMapping(plotData, plotType),
                   groupAesthetics = getGroupAesthetics(plotData)
                 ),
                 ResvO = ospsuite_plotResidualsVsObserved(
                   plotData = plotData$getDataForTimeRange(timeRangeFilter, typeFilter = "observed", plotCounter = plotCounter,yScale = yScale),
                   residualScale = yScale,
                   xscale = yScale,
                   mapping = getGroupbyMapping(plotData, plotType),
                   groupAesthetics = getGroupAesthetics(plotData)
                 ),
                 ResH = ospsuite_plotResidualsAsHistogram(
                   plotData = plotData$getDataForTimeRange(timeRangeFilter, typeFilter = "observed", plotCounter = plotCounter,yScale = yScale),
                   residualScale = yScale,
                   mapping = getGroupbyMapping(plotData, plotType),
                   distribution = "normal"
                 ),
                 QQ = ospsuite_plotQuantileQuantilePlot(
                   plotData = plotData$getDataForTimeRange(timeRangeFilter, typeFilter = "observed", plotCounter = plotCounter,yScale = yScale),
                   residualScale = yScale,
                   mapping = getGroupbyMapping(plotData, plotType),
                   groupAesthetics = getGroupAesthetics(plotData)
                 )
          )

        plotObject <- setManualScalevectors(
          plotObject = plotObject,
          plotData = plotData,
          plotType = plotType
        )
        plotObject <- updateGuides(
          plotData = plotData,
          plotObject = plotObject,
          plotType = plotType
        )

        # add Facet Columns
        plotObject <- addFacets(
          plotObject = plotObject,
          nFacetColumns = plotData$nFacetColumns,
          facetScale = plotData$configTable$facetScale,
          facetAspectRatio =
            ifelse(plotType %in% c("PvO", "QQ "),
                   1, # nolint indentation_linter
                   facetAspectRatio
            )
        )

        # adjust time labels
        if (plotType %in% c("TP", "ResvT ")) {
          plotObject <- plotObject +
            ggplot2::labs(x = plotData$getTimeLabelForTimeRange(timeRangeFilter))
        }
        # export
        rmdContainer$addAndExportFigure(
          plotObject = plotObject,
          caption = getCaptionForPlot(plotData,
                                      yScale = yScale, # nolint indentation_linter
                                      timeRangeFilter = timeRangeFilter,
                                      plotType = plotType,
                                      plotCounter = plotCounter
          ),
          footNoteLines = getFootNoteLines(
            dataObserved = plotData$getDataForTimeRange(
              filterName = timeRangeFilter,
              typeFilter = "observed",
              plotCounter = plotCounter,
              yScale = yScale
            ),
            dtDataReference = plotData$dataReference
          ),
          figureKey = paste0(
            paste(plotData$plotName,
                  plotType, # nolint indentation_linter
                  ifelse(yScale == "log", "log", "linear"),
                  timeRangeFilter,
                  sep = "-"
            ),
            ifelse(max(plotData$dtCaption$counter) == 1, "", paste0("-", plotCounter))
          )
        )
      }
    }
  }

  return(rmdContainer)
}




# plotHelpers ---------
#' Check if a specific plot type is needed and possible
#'
#' @param plotType Type of the plot to check.
#' @param plotData Object containing the data for the plot.
#'
#' @return Logical indicating if the plot type is needed and possible.
#' @keywords internal
isPlotTypeNeededAndPossible <- function(plotType, plotData) {
  configColumn <- switch(plotType,
                         TP = "plot_TimeProfiles", # nolint indentation_linter
                         PvO = "plot_PredictedVsObserved",
                         ResvT = "plot_ResidualsVsTime",
                         ResvO = "plot_ResidualsVsObserved",
                         ResH = "plot_ResidualsAsHistogram",
                         QQ = "plot_QQ",
                         stop(paste("unknown plottype:", plotType))
  )

  if (plotType == "TP") { # nolint: line_length
    return(as.logical(plotData$configTable[[configColumn]][1]))
  } else {
    return(as.logical(plotData$configTable[[configColumn]][1]) &
             plotData$hasObservedData()) # warning is thrown during data preparation # nolint indentation_linter
  }
}


#' Get mapping for simulated and observed data
#'
#' @param plotType Type of the plot.
#' @param plotData Object containing the data for the plot.
#'
#' @return Data table mapping simulated and observed data.
#' @keywords internal
getMapSimulatedAndObserved <- function(plotData) {
  # initialize data.table variables
  dataType <- NULL

  if (!plotData$hasObservedData()) {
    return(NULL)
  }

  mapSimulatedAndObserved <- data.table(
    simulated = as.character(plotData$data[dataType == "simulated"]$colorIndex) %>% unique(),
    observed = as.character(plotData$data[dataType == "observed"]$colorIndex) %>% unique(),
    color = plotData$scaleVectors$colour,
    fill = plotData$scaleVectors$fill
  )

  return(mapSimulatedAndObserved)
}

#' Check and adjust Y limits for plots
#'
#' @param yScale Y scale of the plot.
#' @param plotData Object of class `PlotDataTimeProfile`.
#' @param timeRangeFilter Name of the time range filter.
#' @param plotType Type of the plot.
#'
#' @return Numeric vector of Y limits.
#' @keywords internal
checkAndAdjustYlimits <- function(plotData,
                                  yScale,
                                  timeRangeFilter,
                                  plotType,
                                  plotCounter) {
  # initialize data.table variables
  yUnit <- yValues <- lloq <- xValues <- yMin <- yErrorValues <- NULL

  if (plotType != "TP") {
    return(NULL)
  }

  ylimits <- plotData$configTable[[paste0("ylimit_", yScale)]][1]

  if (is.na(ylimits) || trimws(ylimits) == "") {
    ylimits <- NULL
  } else {
    ylimits <- eval(parse(text = ylimits))
  }

  simulatedData <- plotData$getDataForTimeRange(timeRangeFilter, typeFilter = "simulated", plotCounter = plotCounter,yScale = yScale)

  observedData <- plotData$getDataForTimeRange(timeRangeFilter, typeFilter = "observed", plotCounter = plotCounter,yScale = yScale)
  if (nrow(observedData) > 1) {
    observedData <- observedData[yUnit == unique(simulatedData$yUnit)[1]]
  }

  # make sure no observed data are missed by setting y limits
  if (!is.null(ylimits) & nrow(observedData) > 0) {
    if (any(observedData$yValues
            <= ylimits[1]) |  # nolint indentation_linter
        any(observedData$yValues  # nolint indentation_linter
            >= ylimits[2], na.rm = TRUE)) {
      stop(paste(
        "data outside ylimit for", plotData$plotName,
        "time range", timeRangeFilter
      ))
    }
  }

  # for log scale set always limit to cut very low simulated values at the beginning
  if (is.null(ylimits) & yScale == "log") {
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

    tmp <- simulatedData[xValues >= timeRangeSim[1] &
                           xValues <= timeRangeSim[2]] # nolint indentation_linter
    if (simulatedData$dataClass[1] == DATACLASS$tpAggregated) {
      if (simulatedData$yErrorType[1] == ospsuite::DataErrorType$ArithmeticStdDev) {
        tmp <- tmp[, yMin := yValues - yErrorValues]
      }
      if (simulatedData$yErrorType[1] == ospsuite::DataErrorType$GeometricStdDev) {
        tmp <- tmp[, yMin := yValues / yErrorValues]
      }
      minY <- min(c(minY,
                    tmp[!is.na(yMin) &
                          yMin > 0]$yMin))
    } else {
      minY <- min(c(minY,
                    tmp[!is.na(yValues) & yValues > 0]$yValues))
    }

    ylimits <- c(minY / 2, NA)
  }

  return(ylimits)
}

#' Get mapping for "groupby" aesthetics
#'
#' @param plotData Object containing the data for the plot.
#' @param plotType Type of the plot.
#'
#' @return Aesthetic mappings for grouping.
#' @keywords internal
getGroupbyMapping <- function(plotData, plotType) {
  # avoid warning for global variable
  colorIndex <- shapeIndex <- NULL

  if (plotType == "TP"){
    if (plotData$useShapeIndex()) {
      mapping <- ggplot2::aes(groupby = colorIndex, shape = shapeIndex)
    } else {
      mapping <- ggplot2::aes(groupby = colorIndex, shape = "Observed data")
    }
  } else{
    if (plotData$useColorIndex() & plotData$useShapeIndex()){
      mapping <- ggplot2::aes(groupby = colorIndex, shape = shapeIndex)
    } else if (plotData$useShapeIndex()){
      mapping <- ggplot2::aes(groupby = shapeIndex, shape = shapeIndex)
    } else{
      mapping <- ggplot2::aes(groupby = colorIndex)
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
getGroupAesthetics <- function(plotData) {
  return(intersect(
    c("colour", "fill", "shape"),
    ggplot2::standardise_aes_names(c(
      names(plotData$scaleVectors)
    ))
  ))
}
#' Update guides and scales
#'
#' @param plotData Object containing the data for the plot.
#' @param plotObject The ggplot object to update.
#' @param plotType Type of the plot.
#'
#' @return Updated ggplot object with guides.
#' @keywords internal
updateGuides <- function(plotData, plotObject, plotType) {
  # Initialize legend titles
  legendTitleObservedData <- "Observed data"
  legendTitleColor <- ""

  # Determine the color legend title based on conditions
  if (plotData$useColorIndex()) {
    if (plotType == "TP") {
      if (plotData$hasSimulatedPop()) {
        legendTitleColor <- paste0(
          "Simulated data\n",
          plotData$tpLabelSimulatedMean,
          " with ", plotData$tpLabelSimulatedRange
        )
      } else {
        legendTitleColor <- paste("Simulated", plotData$tpLabelSimulatedMean)
      }
    } else {
      legendTitleColor <- legendTitleObservedData
    }
  } else if (plotData$useShapeIndex() && plotType != "TP") {
    legendTitleColor <- legendTitleObservedData
  }

  # Override aesthetics for shape if plotType is "TP"
  overrideAes <- if (plotType == "TP") list(shape = NA) else list()

  # Add guides for color and fill
  plotObject <- plotObject +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        title = legendTitleColor,
        order = 1,
        override.aes = overrideAes
      ),
        fill = ggplot2::guide_legend(
          title = legendTitleColor,
          order = 1
        )
      )

  if (!plotData$hasSimulatedPop() & plotType == "TP") {
    plotObject <- plotObject +
    ggplot2::guides(fill = "none")
  }

  # Check if there is observed data to add shape guide
  if (plotData$hasObservedData()) {
    legendTitleShape <- ""
    if (plotData$useShapeIndex() || (plotType != "TP" && plotData$useColorIndex())) {
      legendTitleShape <- legendTitleObservedData
    }

    # Add guide for shape
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

    # Add guide for linetype if plotType is "TP"
    if (plotType == "TP") {
      plotObject <- plotObject +
        ggplot2::guides(
          linetype = ggplot2::guide_legend(
            title = "",
            order = 3,
            override.aes = list(color = "black", linewidth = 0.5, linetype = "dashed", shape = NA)
          )
        )
    }
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
setManualScalevectors <- function(plotObject, plotData, plotType) {
  for (aesthetic in names(plotData$scaleVectors)) {

    scaleVector <- plotData$scaleVectors[[aesthetic]]
    labels <- waiver()

    if (plotType != "TP" &
        !plotData$useColorIndex() & # nolint indentation_linter
        !plotData$useShapeIndex()) {
      labels <- "Observed data"
    } else  if (plotType != "TP" &
                !plotData$useColorIndex() & # nolint indentation_linter
                plotData$useShapeIndex() & aesthetic != 'shape') {
      scaleVector <-
        stats::setNames(rep(
          plotData$scaleVectors[[aesthetic]],
          length(plotData$scaleVectors$shape)
        ),
        names(plotData$scaleVectors$shape))
    }

    plotObject <- plotObject +
      ggplot2::scale_discrete_manual(aesthetic,
                                     values = scaleVector, # nolint indentation_linter
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
getCaptionForPlot <- function(plotData, yScale, timeRangeFilter, plotType, plotCounter) {
  # avoid warning for global variable
  counter <- NULL

  dtCaption <-
    plotData$dtCaption[eval(parse(text = plotData$timeRangeTagFilter[[timeRangeFilter]])) &
                         counter == plotCounter] # nolint indentation_linter


  plotTypeTxt <- switch(plotType,
                        TP = "Concentration-time profiles", # nolint indentation_linter
                        PvO = "Predicted vs Observed",
                        ResvT = "Residuals vs time values",
                        ResvO = "Residuals vs observed values",
                        ResH = "Residuals distribution ",
                        QQ = "Residuals as quantile-quantile plot"
  )

  if ("individualId" %in% names(dtCaption)) {
    individualtext <- pasteFigureTags(dtCaption, captionColumn = "individualId")
    if (individualtext != "")
      individualtext <- paste(individualtext,
        "for subject",
        pasteFigureTags(dtCaption, captionColumn = "individualId")
      )
  } else {
    individualtext <- ""
  }

  captiontext <- paste(
    plotTypeTxt,
    "for",
    pasteFigureTags(dtCaption, captionColumn = "displayNameOutputs"),
    "for",
    pasteFigureTags(dtCaption, captionColumn = "scenarioLongName"),
    individualtext,
    "on a", ifelse(yScale == "linear", "linear", "logarithmic"),
    "y-scale.",
    pasteFigureTags(dtCaption, captionColumn = "timeRangeCaption", endWithDot = TRUE),
    ifelse(!is.na(plotData$configTable$plotCaptionAddon[1]),
           plotData$configTable$plotCaptionAddon[1],
           '')
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
getFootNoteLines <- function(dataObserved, dtDataReference) {
  if (nrow(dataObserved) == 0) {
    return(NULL)
  }

  footnoteLines <- NULL
  if (any(dataObserved$dataClass == DATACLASS$tpAggregated)) {
    errorLabels <- getErrorLabels(dataObserved$yErrorType[1])

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
#' Get fold distance for plotType PvO
#'
#' @param plotData Object containing the data for the plot.
#'
#' @return List which can be used as input for comparisonLineVector.
#' @keywords internal
getFoldDistanceForPvO <- function(plotData) {
  foldDistance <- ifelse(!is.na(plotData$configTable$foldDistance_PvO[1]),
                         as.double(plotData$configTable$foldDistance_PvO[1]), # nolint indentation_linter
                         2
  )

  return(ospsuite.plots::getFoldDistanceList(foldDistance))
}

#' get additional inputs for TP plotType to enable lines in shape legend
#'
#' @param plotData object of class PlotData
#'
#' @return list with additional attributes for geom_line
#' @keywords internal
getGeomLineAttributesForTP <- function(plotData) {
  if (plotData$hasObservedDataRange()) {
    return(list(linetype = "solid", show.legend = TRUE))
  } else {
    return(list())
  }
}

#' get additional inputs for TP plotType to enable lines in shape legend
#'
#' @param plotData
#'
#' @return list with additional attributes for lloq line
#' @keywords internal
getGeomLLOQAttributesForTP <- function(plotData) {
  if (plotData$hasObservedDataRange()) {
    return(list(linewidth = 0.5, show.legend = TRUE))
  } else {
    return(list())
  }
}

# validation ----------------

#' Validation of config table for time profiles plots
#'
#' @template projectConfig
#' @param configTable Plot configuration table.
#' @template observedDataDT
#' @keywords internal
validateConfigTableForTimeProfiles <- function(configTable, dataObserved, projectConfiguration) {
  # avoid warning for global variable
  individualId <- NULL

  configTablePlots <- validateHeaders(configTable)
  validateOutputIdsForPlot()
  validateDataGroupIdsForPlot()

  checkmate::assertCharacter(configTablePlots$scenarioLongName,any.missing = FALSE,.var.name = 'longName for relevant scenarios')

  validateConfigTablePlots(
    configTablePlots = configTablePlots,
    charactersWithoutMissing = c(
      "plotName",
      "scenario",
      "outputPathIds",
      "timeUnit",
      "facetScale",
      "facetType",
      "yScale"
    ),
    charactersWithMissing =
      c(
        "dataGroupIds",
        "plotCaptionAddon",
        "referenceScenario",
        "individualIds"
      ),
    numericColumns = c(
      "timeOffset",
      "timeOffset_Reference",
      "foldDistance_PvO"
    ),
    logicalColumns = c(
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
        allowedValues = configEnv$scenarios$scenarioName
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
        allowedValues = c("*", unique(dataObserved[!is.na(individualId)]$individualId))
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
      ),
      facetType = list(
        cols = c("facetType"),
        allowedValues = unname(unlist(FACETTYPE))
      )
    )
  )

  validateTimeRangeColumns(configTablePlots)

  validatePanelConsistency(
    configTablePlots = configTablePlots,
    panelColumns = c(
      "timeUnit",
      "yScale",
      "ylimit_linear",
      "ylimit_log",
      "plotCaptionAddon",
      "facetType",
      "facetScale",
      "plot_TimeProfiles",
      "plot_PredictedVsObserved",
      "plot_ResidualsAsHistogram",
      "plot_ResidualsVsTime",
      "plot_ResidualsVsObserved",
      "plot_QQ"
    )
  )

  validateUnitConsistency(configTablePlots = configTablePlots)

  validateOutputPathIdFormat(configTablePlots = configTablePlots)

  validateOutputPathIdFormat(configTablePlots = configTablePlots, column = "individualIds")

  validateVirtualTwinPop(
    configTablePlots = configTablePlots,
    projectConfiguration = projectConfiguration
  )

  return(invisible())
}



#' Check if panel columns are filled consistently
#'
#' @template configTablePlots
#' @param panelColumns Vector of columns which should be consistent.
#' @keywords internal
validateUnitConsistency <- function(
    configTablePlots) {
  # avoid warning for global variable
  plotName <- outputPathId <- NULL

  # check if more than two different units are combined in one panel
  configTableList <- split(configTablePlots, by = "plotName")
  for (configPanel in configTableList) {
    outputs <- gsub("[()]", "", splitInputs(configPanel$outputPathIds))
    if (dplyr::n_distinct(configEnv$outputPaths[outputPathId %in% outputs]$displayUnit) > 2) {
      stop("do not combine more than two yUnits in one Panel")
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
#' @template configTablePlots
#' @keywords internal
validateTimeRangeColumns <- function(configTablePlots) {
  timeRangeColumns <-
    names(configTablePlots)[grepl("^timeRange_", names(configTablePlots))]

  if (length(timeRangeColumns) == 0) stop("You need at least one TimeRange Column")

  validateAtleastOneEntry(configTablePlots, columnVector = timeRangeColumns)

  validateConfigTablePlots(configTablePlots,
                           charactersWithMissing = timeRangeColumns # nolint indentation_linter
  )

  tryCatch(
    {
      if (!all(sapply(configTablePlots %>%
                      dplyr::select(dplyr::all_of(timeRangeColumns)), function(x) {
        valid <- x %in% c(NA, "total", "firstApplication", "lastApplication")
        if (!all(valid)) {
          tmp <- eval(parse(text = x[!valid]))
          valid <-
            is.numeric(tmp) &&
            length(tmp) == 2 &&
            all(!is.na(tmp))
        }
        return(all(valid))
      }))) {
        stop('invalid inputs in one of the "TimeRange" columns')
      }
    },
    error = function(err) {
      stop('invalid inputs in one of the "TimeRange" columns')
    }
  )

  return(invisible())
}

#' Validates output path ID format
#'
#' @template configTablePlots
#' @keywords internal
validateOutputPathIdFormat <- function(configTablePlots, column = "outputPathIds") {
  # avoid warning for global variable
  nBracketOpen <- nBracketClosed <- NULL

  tmp <- configTablePlots[, .(
    nBracketOpen = sum(grepl("\\(", get(column))),
    nBracketClosed = sum(grepl("\\)", get(column)))
  ), by = column]

  tmp <- tmp[nBracketOpen != nBracketClosed]

  if (nrow(tmp) > 0) {
    stop(paste("Please check the brackets in column", column, paste(tmp[[column]], collapse = ";")))
  }
}

#' Validate Virtual Twin Population in Plot Configuration
#'
#' This function validates the virtual twin population specified in the plot configuration table.
#' It checks for the presence of `individualIds` in scenarios with virtual twin populations,
#' ensures that brackets are not used in `individualIds` for time profile plots, and warns
#' if `individualIds` is filled without a corresponding data group.
#'
#' @param configTablePlots A data table containing the plot configuration, including scenario names and individual IDs.
#' @param projectConfiguration A configuration object containing project-specific settings.
#' @keywords internal
validateVirtualTwinPop <- function(configTablePlots, projectConfiguration) {
  # avoid warning for global variable
  scenario <- dataGroupIds <- plot_TimeProfiles <- individualIds <- NULL # nolint object_name_linter

  scenarioNames <- unique(configTablePlots$scenario)
  individualMatches <-
    lapply(
      stats::setNames(as.list(scenarioNames), scenarioNames),
      function(scenario) {
        getIndividualMatchForScenario(
          projectConfiguration = projectConfiguration,
          scenario = scenario,
          dtScenarios = configEnv$scenarios
        )
      }
    )

  indPopScenarios <- scenarioNames[!unlist(lapply(individualMatches, is.null))]

  # individualIds is filled
  if (any(is.na(configTablePlots[scenario %in% indPopScenarios]$individualIds))) {
    stop(paste(
      "For scenarios with virtual twin populations, column individualIds has to be filled.",
      'Use "*" or "(*)", if you want to plot all. (Brackets not allowed for Timeprofile Plots)',
      "Check Scenarios:", paste(indPopScenarios, collapse = ", ")
    ))
  }

  tmp <- configTablePlots[scenario %in% indPopScenarios & as.logical(plot_TimeProfiles)]
  errorRows <- which(grepl("\\(", tmp$individualIds) | grepl("\\)", tmp$individualIds))
  if (length(errorRows) > 0) {
    stop(paste(
      "For scenarios with virtual twin populations and selected Plot_TimeProfiles,
               brackets are not allowed in column individualIds.",
      "Check Plots:", paste(tmp$plotName[errorRows], collapse = ", ")
    ))
  }

  tmp <- configTablePlots[!(scenario %in% indPopScenarios) & !is.na(individualIds) & is.na(dataGroupIds)]
  if (nrow(tmp) > 0) {
    warning(paste(
      'Column "individualIds" is filled but no data group is selected and
    scenario is not an virtual twin population scenario. "individualIds" will be ignored.',
      "Check Plots:", paste(tmp$plotName, collapse = ", ")
    ))
  }

  return(invisible())
}

# support usability --------------------

#' Add Default Configuration for Time Profile Plots
#'
#' This function adds a default configuration sheet for time profile plots to the plot configuration table.
#' It can either create a new sheet or overwrite an existing one based on the specified parameters.
#'
#' @param projectConfiguration A ProjectConfiguration class object containing configuration details, including:
#'   - `plotsFile`: A string representing the file path to the Excel workbook containing the plot configurations.
#'
#' @param dataObserved Optional. A data object containing observed data, if available.
#'
#' @param sheetName A character string specifying the name of the sheet in the plot configuration table.
#'   Default is "TimeProfiles".
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
#' where helper functions are prohibited (validRun). If such a context is detected, an error is raised to prevent execution.
#'
#'
#' @return NULL This function updates the Excel workbook in place and does not return a value.
#' It is called for its side effects.
#'
#' @export
addDefaultConfigForTimeProfilePlots <- function(projectConfiguration,
                                                dataObserved = NULL,
                                                sheetName = "TimeProfiles",
                                                overwrite = FALSE) {
  # avoid warnings for global variables during check
  scenarioName <- NULL

  # this function stops in valid runs
  stopHelperFunction()

  # use data.table format for dataObserved
  if (!is.null(dataObserved)){
    if ("DataCombined" %in% class(dataObserved)) {
      dataObserved <- convertDataCombinedToDataTable(dataObserved)
    }
  }

  # update configenvironmant
  loadConfigTables(projectConfiguration)
  scenarios <- copy(configEnv$scenarios)

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  if (sheetName %in% wb$sheet_names & !overwrite) {
    dtNewHeader <- xlsxReadData(wb, sheetName = sheetName, skipDescriptionRow = TRUE)
    scenarios <- scenarios[!(scenarioName %in% unique(dtNewHeader$scenario))]
  } else {
    dtNewHeader <- data.table(
      level = 1,
      header = "Concentration time profiles"
    )
  }

  dtNewConfig <- createNewConfig(scenarios, dataObserved)

  wb <- addDataAsTemplateToXlsx(
    wb = wb,
    templateSheet = "TimeProfile_Panel",
    sheetName = sheetName,
    dtNewData = rbind(dtNewHeader,
                        dtNewConfig, # nolint indentation_linter
                        fill = TRUE
    )
  )

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)
}


#' Create New Configuration for Time Profile Plots
#'
#' This internal function generates a new configuration data table for time profile plots based on the provided scenarios and project configuration.
#'
#' @param scenarios A data.table containing scenario definitions.
#' @param projectConfiguration A ProjectConfiguration class object.
#' @param dataObserved Optional. A data object containing observed data, if available.
#'
#' @return A data.table containing the new configuration for time profile plots.
#' @keywords internal
createNewConfig <- function(scenarios, dataObserved) {

  dtNewConfig <- data.table(
    plotName = scenarios$scenarioName,
    scenario = scenarios$scenarioName,
    outputPathIds = paste(unique(configEnv$outputPaths$outputPathId), collapse = ", "),
    timeUnit = "h",
    timeOffset = 0,
    timeOffset_Reference = 0,
    timeRange_total = TIMERANGE$total,
    timeRange_firstApplication = TIMERANGE$firstApplication,
    timeRange_lastApplication = TIMERANGE$lastApplication,
    yScale = "linear, log",
    facetScale = "fixed",
    facetType = FACETTYPE[[1]],
    plot_TimeProfiles = TRUE,
    plot_PredictedVsObserved = FALSE,
    plot_ResidualsAsHistogram = FALSE,
    plot_ResidualsVsTime = FALSE,
    plot_ResidualsVsObserved = FALSE,
    plot_QQ = FALSE
  )

  if (any(!is.na(configEnv$dataGroupIds$defaultScenario))) {
    dtNewConfig <- mergeDataGroups(dtNewConfig, configEnv$dataGroupIds, dataObserved)
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
mergeDataGroups <- function(dtNewConfig, dtDataGroups, dataObserved) {
  tmp <- dtDataGroups[, .(dataGroupIds = paste(as.character(group), collapse = ', ')), by = 'defaultScenario']
  data.table::setnames(tmp, old = c("defaultScenario"), new = c("scenario"))

  dtNewConfig <- dtNewConfig %>%
    merge(tmp, by = "scenario", all.x = TRUE, sort = FALSE)

  dtNewConfig[!is.na(dataGroupIds), individualIds := '*']

  if (!is.null(dataObserved)) {
    for (sc in unique(dtNewConfig[!is.na(dataGroupIds)]$scenario)) {
      dataGroupIds <- splitInputs(dtNewConfig[scenario == sc]$dataGroupIds)
      availableOutputs <- unique(dataObserved[group %in% dataGroupIds]$outputPathId)
      dtNewConfig[scenario == sc, outputPathIds := paste(availableOutputs, collapse = ', ')]
    }
  }

  return(dtNewConfig)
}
