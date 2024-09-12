#' Generate Time Profile Panels
#'
#' The `plotTimeProfilePanels` function creates a series of time profile plots as facet panels
#' from the provided observed data. This function is designed to visualize complex time-dependent
#' data by allowing for multiple plots organized in a user-defined layout.
#'
#' This function is particularly useful for researchers and analysts who need to visualize
#' concentration-time profiles of various scenarios, compare predicted versus observed data,
#' and analyze residuals cohesively. For more detailed information, please refer to the
#' accompanying vignettes:
#' - **TimeProfilePlots**
#' - **Time Profile Plotting Tutorial**
#
#'
#' @param projectConfiguration An object of class ProjectConfiguration containing the project settings,
#' including file paths and scenario definitions. This is essential for the function to access the necessary data.
#'
#' @param subfolder A character string specifying the subfolder within the output directory where
#' the plots will be saved.
#'
#' @param configTableSheet The name of the sheet in the Excel plot configuration file that defines
#' the plot settings.
#'
#' @param dataObserved A data.table or DataCombined object containing the observed data to be plotted.
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
#' for simulated data. Options include "GeometricStdDev", "ArithmeticStdDev", "Percentiles",
#' and "Custom" (default is "GeometricStdDev").
#'
#' @param percentiles A numeric vector of percentiles to consider if the aggregation method is set
#' to "Percentiles" (default is c(5, 50, 95)).
#'
#' @param customFunction An optional custom function for aggregation. This function should take a
#' numeric vector as input and return a list containing aggregated values and error types.
#'
#' @param referenceScaleVector A named list that configures labels and colors for the display of
#' reference scenarios. The names of the list are displayed as labels in the legend.
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
#' plotTimeProfilePanels(
#'   projectConfiguration = myProjectConfig,
#'   subfolder = "TimeProfiles",
#'   configTableSheet = "TimeProfiles",
#'   dataObserved = myObservedData
#' )
#' }
#'
#' @export
plotTimeProfilePanels <- function(projectConfiguration,
                                  subfolder,
                                  configTableSheet,
                                  dataObserved,
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
                                    Control = c(NA, NA),
                                    Reference = c(NA, NA)
                                  )) {
  # Function implementation goes here
}
plotTimeProfilePanels <- function(projectConfiguration,
                                  subfolder,
                                  configTableSheet,
                                  dataObserved,
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
                                    Control = c(NA, NA),
                                    Reference = c(NA, NA)
                                  )) {
  checkmate::assert_path_for_output(file.path(projectConfiguration$outputFolder, subfolder), overwrite = TRUE)
  checkmate::assertIntegerish(nFacetColumns, lower = 1, len = 1)
  checkmate::assertDouble(facetAspectRatio, lower = 0, len = 1)

  # use data.table format for dataObserved
  if ("DataCombined" %in% class(dataObserved)) {
    dataObserved <- convertDataCombinedToDataTable(dataObserved)
  }

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


  # initialize Container for RMD generation for .Rmd generation
  rmdContainer <-
    RmdContainer$new(
      rmdfolder = file.path(projectConfiguration$outputFolder),
      subfolder = subfolder
    )

  iRow <- 1
  levelLines <- which(!is.na(configTable$Level))
  while (iRow <= nrow(configTable)) {
    if (!is.na(configTable$Level[iRow])) {
      # add  section headers
      rmdContainer$addHeader(configTable$Header[iRow],
                             level = configTable$Level[iRow] # nolint indentation_linter
      )
      iRow <- iRow + 1
    } else {
      # execute plot section
      iEndX <- utils::head(which(levelLines > iRow), 1)
      if (length(iEndX) == 0) {
        iEnd <- nrow(configTable)
      } else {
        iEnd <- levelLines[iEndX] - 1
      }

      for (onePlotConfig in split(configTable[seq(iRow, iEnd)], by = "PlotName")) {
        tryCatch(
          {
            rmdContainer <- createPanelPlotsForPlotName(
              projectConfiguration = projectConfiguration,
              onePlotConfig = onePlotConfig,
              dataObserved = dataObserved,
              rmdContainer = rmdContainer,
              nFacetColumns = nFacetColumns,
              nMaxFacetRows = nMaxFacetRows,
              facetAspectRatio = facetAspectRatio,
              aggregationFun = aggregationFun,
              referenceScaleVector = referenceScaleVector
            )
          },
          error = function(err) {
            if (!getOption("OSPSuite.RF.skipFailingPlots", default = FALSE)) {
              stop(err) # Re-throw the error if "skipFailingPlots" is FALSE
            } else {
              warning(paste(
                "Error during creation of:", onePlotConfig$PlotName[1],
                "Message:", conditionMessage(err)
              ))
            }
          }
        )
      }

      iRow <- iEnd + 1
    }
  }

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
  Level <- NULL  # nolint object_name_linter

  # read configuration tables
  configTable <- xlsxReadData(
    wb = projectConfiguration$plotsFile,
    sheetName = sheetName,
    skipDescriptionRow = TRUE
  )

  validateConfigTableForTimeProfiles(
    configTable = configTable,
    dataObserved = dataObserved,
    projectConfiguration = projectConfiguration
  )


  plotInputColumns <-
    names(configTable)[grepl("^PlotInputs_", names(configTable))]

  for (col in plotInputColumns) {
    configTable[, (col) := as.character(get(col))]
    configTable[is.na(get(col)) & is.na(Level), (col) := ""]
  }

  return(configTable)
}
#' Generate one plot as facet panels
#'
#' @param rmdContainer Object of class `RmdContainer`.
#' @param aggregationFun Function to aggregate simulated data.
#' @template projectConfig
#' @param onePlotConfig configuration table for this plot
#' @template observedDataDT
#' @param nFacetColumns Maximal number of facet columns (default 2) used for facet type "by Order".
#' @param facetAspectRatio Aspect ratio for the facets (default 0.5).
#' @param nMaxFacetRows  maximal number of facet rows
#' @param referenceScaleVector scale vector to scale aesthetics color and fill for scenarios with reference scenario
#'
#' @return Object of class `rmdContainer` with added figures.
#' @export
createPanelPlotsForPlotName <- function(projectConfiguration,
                                        onePlotConfig,
                                        dataObserved,
                                        nFacetColumns,
                                        nMaxFacetRows,
                                        rmdContainer,
                                        facetAspectRatio,
                                        aggregationFun,
                                        referenceScaleVector) {
  message(paste("Create Plot", onePlotConfig$PlotName[1]))

  plotData <- PlotDataTimeProfile$new(
    projectConfiguration = projectConfiguration,
    onePlotConfig = onePlotConfig
  )

  # read simulated data and filter observed
  plotData$loadSimulatedResults(
    projectConfiguration = projectConfiguration,
    aggregationFun = aggregationFun
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
                   plotData = plotData$getDataForTimeRange(timeRangeFilter, plotCounter = plotCounter),
                   yscale = yScale,
                   mapping = getGroupbyMapping(plotData, plotType),
                   groupAesthetics = getGroupAesthetics(plotData),
                   yscale.args = list(limits = yLimits),
                   geomLineAttributes = getGeomLineAttributesForTP(plotData),
                   geomLLOQAttributes = getGeomLLOQAttributesForTP(plotData)
                 ),
                 PvO = ospsuite_plotPredictedVsObserved(
                   plotData = plotData$getDataForTimeRange(timeRangeFilter, typeFilter = "observed", plotCounter = plotCounter),
                   xyscale = yScale,
                   mapping = getGroupbyMapping(plotData, plotType),
                   groupAesthetics = getGroupAesthetics(plotData),
                   comparisonLineVector = getFoldDistanceForPvO(plotData)
                 ),
                 ResvT = ospsuite_plotResidualsVsTime(
                   plotData = plotData$getDataForTimeRange(timeRangeFilter, typeFilter = "observed", plotCounter = plotCounter),
                   residualScale = yScale,
                   mapping = getGroupbyMapping(plotData, plotType),
                   groupAesthetics = getGroupAesthetics(plotData)
                 ),
                 ResvO = ospsuite_plotResidualsVsObserved(
                   plotData = plotData$getDataForTimeRange(timeRangeFilter, typeFilter = "observed", plotCounter = plotCounter),
                   residualScale = yScale,
                   xscale = yScale,
                   mapping = getGroupbyMapping(plotData, plotType),
                   groupAesthetics = getGroupAesthetics(plotData)
                 ),
                 ResH = ospsuite_plotResidualsAsHistogram(
                   plotData = plotData$getDataForTimeRange(timeRangeFilter, typeFilter = "observed", plotCounter = plotCounter),
                   residualScale = yScale,
                   mapping = getGroupbyMapping(plotData, plotType),
                   distribution = "normal"
                 ),
                 QQ = ospsuite_plotQuantileQuantilePlot(
                   plotData = plotData$getDataForTimeRange(timeRangeFilter, typeFilter = "observed", plotCounter = plotCounter),
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
          plotData = plotData,
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
              plotCounter = plotCounter
            ),
            dtDataReference = plotData$dataReference
          ),
          figureKey = paste0(
            paste(plotData$configTable$PlotName[1],
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
                         TP = "Plot_TimeProfiles", # nolint indentation_linter
                         PvO = "Plot_PredictedVsObserved",
                         ResvT = "Plot_ResidualsVsTime",
                         ResvO = "Plot_ResidualsVsObserved",
                         ResH = "Plot_ResidualsAsHistogram",
                         QQ = "Plot_QQ",
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

  simulatedData <- plotData$getDataForTimeRange(timeRangeFilter, typeFilter = "simulated", plotCounter = plotCounter)

  observedData <- plotData$getDataForTimeRange(timeRangeFilter, typeFilter = "observed", plotCounter = plotCounter)
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
        "data outside ylimit for", plotData$configTable$PlotName[1],
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

  if ("shapeIndex" %in% names(plotData$data)) {
    ggplot2::aes(groupby = colorIndex, shape = shapeIndex)
  } else if (plotType == "TP") {
    ggplot2::aes(groupby = colorIndex, shape = "Observed data")
  } else {
    ggplot2::aes(groupby = colorIndex)
  }
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
    ggplot2::standardise_aes_names(unique(c(
      names(plotData$scaleVectors),
      names(plotData$scaleVectorsObserved)
    )))
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
  legendTitleObservedData <- "Observed data"

  legendTitleColor <- ""
  if (plotData$useColorIndex()) {
    if (plotType == "TP" & !plotData$hasSimulatedPop()) {
      legendTitleColor <- paste("Simulated", plotData$tpLabelSimulatedMean)
    } else if (plotType == "TP" & plotData$hasSimulatedPop()) {
      legendTitleColor <- paste0(
        "Simulated data\n",
        plotData$tpLabelSimulatedMean,
        " with ", plotData$tpLabelSimulatedRange
      )
    } else {
      legendTitleColor <- legendTitleObservedData
    }
  }

  plotObject <-
    plotObject +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        title = legendTitleColor,
        order = 1,
        override.aes = ifelse(plotType == "TP", list(shape = NA), list())
      ),
      fill = ggplot2::guide_legend(
        title = legendTitleColor,
        order = 1,
        override.aes = ifelse(plotType == "TP", list(shape = NA), list())
      )
    )

  if (plotData$hasObservedData()) {
    legendTitleShape <- ""
    if (plotData$useShapeIndex()) legendTitleShape <- legendTitleObservedData
    if (plotType != "TP" & plotData$useColorIndex()) legendTitleShape <- legendTitleObservedData

    plotObject <-
      plotObject +
      ggplot2::guides(
        shape = ggplot2::guide_legend(
          title = legendTitleShape,
          order = ifelse(plotType == "TP", 2, 1),
          override.aes = list(
            alpha = 0.7,
            color = "black",
            linewidth = 0.5,
            fill = "black"
          ),
        ),
        linetype = ggplot2::guide_legend(
          title = "",
          order = ifelse(plotType == "TP", 3, 2),
          override.aes = list(color = "black", linewidth = 0.5, linetype = "dashed")
        )
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
setManualScalevectors <- function(plotObject, plotData, plotType) {
  for (aesthetic in names(plotData$scaleVectors)) {
    if (plotType != "TP" &
        !plotData$useColorIndex() & # nolint indentation_linter
        !plotData$useShapeIndex()) {
      labels <- "Observed data"
    } else {
      labels <- names(plotData$scaleVectors[[aesthetic]])
    }

    plotObject <- plotObject +
      ggplot2::scale_discrete_manual(aesthetic,
                                     values = plotData$scaleVectors[[aesthetic]], # nolint indentation_linter
                                     labels = labels
      )
  }

  return(plotObject)
}
#' Add facets to a ggplot object
#'
#' @param plotObject ggplot object to which the facets should be added.
#' @inheritParams createPanelPlotsForPlotName
#' @param nFacetColumns Maximal number of facets used for facet type by Order.
#'
#' @return Updated ggplot object with facets.
#' @keywords internal
addFacets <- function(plotObject,
                      plotData,
                      facetAspectRatio = 0.5,
                      nFacetColumns = 3) {
  # avoid warnings for global variables during check
  PlotTag <- NULL # nolint object_name_linter

  nFacetColumns <- plotData$nFacetColumns

  if (!is.null(nFacetColumns)) {
    plotObject <- plotObject +
      ggplot2::facet_wrap(
        facets = ggplot2::vars(PlotTag),
        scales = plotData$configTable$FacetScale[1],
        ncol = nFacetColumns
      ) +
      ggplot2::theme(aspect.ratio = facetAspectRatio)
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
    individualtext <-
      paste(
        "for subject",
        pasteFigureTags(dtCaption, captionColumn = "individualId")
      )
  } else {
    individualtext <- ""
  }

  captiontext <- paste(
    plotTypeTxt,
    "for",
    pasteFigureTags(dtCaption, captionColumn = "outputDisplayName"),
    "for",
    pasteFigureTags(dtCaption, captionColumn = "ScenarioCaptionName"),
    individualtext,
    "on a", ifelse(yScale == "linear", "linear", "logarithmic"),
    "y-scale.",
    pasteFigureTags(dtCaption, captionColumn = "timeRangeCaption", endWithDot = TRUE),
    plotData$configTable$PlotCaptionAddon[1]
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
          dtDataReference$Reference %>%
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
  foldDistance <- ifelse(!is.na(plotData$configTable$FoldDistance_PvO[1]),
                         as.double(plotData$configTable$FoldDistance_PvO[1]), # nolint indentation_linter
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

  dtScenarios <- getScenarioDefinitions(projectConfiguration)
  dtOutputPaths <- getOutputPathIds(projectConfiguration)
  validateOutputIdsForPlot(dtOutputPaths)

  validateConfigTablePlots(
    configTablePlots = configTablePlots,
    charactersWithoutMissing = c(
      "PlotName",
      "Scenario",
      "ScenarioCaptionName",
      "OutputPathIds",
      "TimeUnit",
      "FacetScale",
      "FacetType",
      "yScale"
    ),
    charactersWithMissing =
      c(
        "DataGroupIds",
        "PlotCaptionAddon",
        "ReferenceScenario",
        "IndividualIds"
      ),
    numericColumns = c(
      "TimeOffset",
      "TimeOffset_Reference",
      "FoldDistance_PvO"
    ),
    logicalColumns = c(
      "Plot_TimeProfiles",
      "Plot_PredictedVsObserved",
      "Plot_ResidualsAsHistogram",
      "Plot_ResidualsVsTime",
      "Plot_ResidualsVsObserved",
      "Plot_QQ"
    ),
    numericRangeColumns = c("ylimit_linear", "ylimit_log"),
    subsetList = list(
      scenario = list(
        cols = c("Scenario", "ReferenceScenario"),
        allowedValues = dtScenarios$Scenario_name
      ),
      dataGroupId = list(
        cols = c("DataGroupIds"),
        allowedValues = unique(dataObserved$group)
      ),
      outputPathId = list(
        cols = c("outputPathId"),
        allowedValues = unique(dtOutputPaths$outputPathId)
      ),
      IndividualIds = list(
        cols = "IndividualIds",
        allowedValues = c("*", unique(dataObserved[!is.na(individualId)]$individualId))
      ),
      yscale = list(
        cols = c("yscale"),
        allowedValues = c("linear", "log")
      ),
      TimeUnit = list(
        cols = c("TimeUnit"),
        allowedValues = ospsuite::getUnitsForDimension("Time")
      ),
      FacetScale = list(
        cols = c("FacetScale"),
        allowedValues = c("fixed", "free", "free_x", "free_y")
      ),
      FacetType = list(
        cols = c("FacetType"),
        allowedValues = unname(unlist(FACETTYPE))
      )
    )
  )

  validateTimeRangeColumns(configTablePlots)

  validatePanelConsistency(
    configTablePlots = configTablePlots,
    panelColumns = c(
      "TimeUnit",
      "yScale",
      "ylimit_linear",
      "ylimit_log",
      "PlotCaptionAddon",
      "FacetType",
      "FacetScale",
      "Plot_TimeProfiles",
      "Plot_PredictedVsObserved",
      "Plot_ResidualsAsHistogram",
      "Plot_ResidualsVsTime",
      "Plot_ResidualsVsObserved",
      "Plot_QQ"
    ),
    dtOutputPaths = dtOutputPaths
  )


  validateOutputPathIdFormat(configTablePlots = configTablePlots)

  validateOutputPathIdFormat(configTablePlots = configTablePlots, column = "IndividualIds")

  validateVirtualTwinPop(
    configTablePlots = configTablePlots,
    projectConfiguration = projectConfiguration,
    dtScenarios = dtScenarios
  )

  return(invisible())
}

#' Check if panel columns are filled consistently
#'
#' @template configTablePlots
#' @param panelColumns Vector of columns which should be consistent.
#' @keywords internal
validatePanelConsistency <- function(
    configTablePlots,
    panelColumns,
    dtOutputPaths) {
  # avoid warning for global variable
  PlotName <- outputPathId <- NULL   # nolint object_name_linter

  # Check for unique values of panel columns for each `PlotName`
  uniquePanelValues <-
    configTablePlots[, lapply(.SD, function(x) {
      length(unique(x))
    }), by = PlotName, .SDcols = panelColumns]
  tmp <- lapply(panelColumns, function(col) {  # nolint object_usage_linter
    if (any(uniquePanelValues[[col]] > 1)) stop(paste("values for", col, "should be the same within each panel"))
  })

  # check if more than two different units are combined in one panel
  configTableList <- split(configTablePlots, by = "PlotName")
  for (configPanel in configTableList) {
    outputs <- gsub("[()]", "", splitInputs(configPanel$OutputPathIds))
    if (dplyr::n_distinct(dtOutputPaths[outputPathId %in% outputs]$DisplayUnit) > 2) {
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
    names(configTablePlots)[grepl("^TimeRange_", names(configTablePlots))]

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
validateOutputPathIdFormat <- function(configTablePlots, column = "OutputPathIds") {
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
#' It checks for the presence of `IndividualIds` in scenarios with virtual twin populations,
#' ensures that brackets are not used in `IndividualIds` for time profile plots, and warns
#' if `IndividualIds` is filled without a corresponding data group.
#'
#' @param configTablePlots A data table containing the plot configuration, including scenario names and individual IDs.
#' @param projectConfiguration A configuration object containing project-specific settings.
#' @param dtScenarios A data table containing scenario details.
#' @keywords internal
validateVirtualTwinPop <- function(configTablePlots, projectConfiguration, dtScenarios) {
  # avoid warning for global variable
  Scenario <- DataGroupIds <- Plot_TimeProfiles <- IndividualIds <- NULL # nolint object_name_linter

  scenarioNames <- unique(configTablePlots$Scenario)
  individualMatches <-
    lapply(
      stats::setNames(as.list(scenarioNames), scenarioNames),
      function(scenario) {
        getIndividualMatchForScenario(
          projectConfiguration = projectConfiguration,
          scenario = scenario,
          dtScenarios = dtScenarios
        )
      }
    )

  indPopScenarios <- scenarioNames[!unlist(lapply(individualMatches, is.null))]

  # IndividualIds is filled
  if (any(is.na(configTablePlots[Scenario %in% indPopScenarios]$IndividualIds))) {
    stop(paste(
      "For scenarios with virtual twin populations, column IndividualIds has to be filled.",
      'Use "*" or "(*)", if you want to plot all. (Brackets not allowed for Timeprofile Plots)',
      "Check Scenarios:", paste(indPopScenarios, collapse = ", ")
    ))
  }

  tmp <- configTablePlots[Scenario %in% indPopScenarios & as.logical(Plot_TimeProfiles)]
  errorRows <- which(grepl("\\(", tmp$IndividualIds) | grepl("\\)", tmp$IndividualIds))
  if (length(errorRows) > 0) {
    stop(paste(
      "For scenarios with virtual twin populations and selected Plot_TimeProfiles,
               brackets are not allowed in column IndividualIds.",
      "Check Plots:", paste(tmp$PlotName[errorRows], collapse = ", ")
    ))
  }

  tmp <- configTablePlots[!(Scenario %in% indPopScenarios) & !is.na(IndividualIds) & is.na(DataGroupIds)]
  if (nrow(tmp) > 0) {
    warning(paste(
      'Column "IndividualIds" is filled but no data group is selected and
    scenario is not an virtual twin population scenario. "IndividualIds" will be ignored.',
      "Check Plots:", paste(tmp$PlotName, collapse = ", ")
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
#' @return NULL This function updates the Excel workbook in place and does not return a value.
#' It is called for its side effects.
#'
#' @export
addDefaultConfigForTimeProfilePlots <- function(projectConfiguration,
                                                sheetName = "TimeProfiles", overwrite = FALSE) {
  # avoid warnings for global variables during check
  Scenario_name <- NULL # nolint object_name_linter

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  # this function stops in valid runs
  stopHelperFunction()

  scenarios <- getScenarioDefinitions(projectConfiguration)
  dtOutputPaths <- getOutputPathIds(projectConfiguration)
  dtDataGroups <- getDataGroups(projectConfiguration)

  if (sheetName %in% wb$sheet_names & !overwrite) {
    dtNewHeader <- xlsxReadData(wb, sheetName = sheetName, skipDescriptionRow = TRUE)
    scenarios <- scenarios[!(Scenario_name %in% unique(dtNewHeader$Scenario))]
  } else {
    dtNewHeader <- data.table(
      Level = 1,
      Header = "Concentration time profiles"
    )
  }

  dtNewConfig <- data.table(
    PlotName = scenarios$Scenario_name,
    Scenario = scenarios$Scenario_name,
    ScenarioCaptionName = scenarios$Scenario_name,
    OutputPathIds = paste(unique(dtOutputPaths$outputPathId), collapse = ", "),
    TimeUnit = "h",
    TimeOffset = 0,
    TimeOffset_Reference = 0,
    TimeRange_total = TIMERANGE$total,
    TimeRange_firstApplication = TIMERANGE$firstApplication,
    TimeRange_lastApplication = TIMERANGE$lastApplication,
    yScale = "linear, log",
    FacetScale = "fixed",
    FacetType = FACETTYPE[[1]],
    Plot_TimeProfiles = TRUE,
    Plot_PredictedVsObserved = FALSE,
    Plot_ResidualsAsHistogram = FALSE,
    Plot_ResidualsVsTime = FALSE,
    Plot_ResidualsVsObserved = FALSE,
    Plot_QQ = FALSE
  )

  dtNewConfig <- dtNewConfig %>%
    merge(
      dtDataGroups %>%
        dplyr::select(c("group", "DefaultScenario")) %>%
        data.table::setnames(
          old = c("group", "DefaultScenario"),
          new = c("DataGroupIds", "Scenario")
        ),
      by = "Scenario",
      all.x = TRUE, sort = FALSE
    )


  wb <- addConfigToTemplate(
    wb = wb,
    templateSheet = "TimeProfile_Panel",
    sheetName = sheetName,
    dtNewConfig = rbind(dtNewHeader,
                        dtNewConfig, # nolint indentation_linter
                        fill = TRUE
    )
  )

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)
}
