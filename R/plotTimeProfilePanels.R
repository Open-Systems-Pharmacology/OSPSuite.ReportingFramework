#' Generate a list of time profiles as facet panels
#'
#' @template projectConfig
#' @param subfolder Name of the subfolder in the projectconfiguration$outputFolder directory.
#' @param configTableSheet Name of the sheet in the plot configuration xlsx that defines the plot.
#' @template observedData
#' @param nFacetColumns Maximal number of facet columns (default 2) used for facet type "by Order".
#' @param facetAspectRatio Aspect ratio for the facets (default 0.5).
#' @param aggregationFlag Type of aggregation function to use (default is "GeometricStdDev").
#' @param percentiles Percentiles to consider (default is c(5, 50, 95)).
#' @param customFunction Optional custom function for aggregation.
#'
#' @return Object of class `rmdContainer`.
#' @export
plotTimeProfilePanels <- function(projectConfiguration,
                                  subfolder,
                                  configTableSheet,
                                  dataObserved,
                                  nFacetColumns = 2,
                                  facetAspectRatio = 0.5,
                                  aggregationFlag = c(
                                    "GeometricStdDev",
                                    "ArithmeticStdDev",
                                    "Percentiles",
                                    "Custom"
                                  ),
                                  percentiles = c(5, 50, 95),
                                  customFunction = NULL) {
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
  configTable <- readTimeprofileConfigTable(projectConfiguration = projectConfiguration,
                                            sheetName = configTableSheet)

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
        level = configTable$Level[iRow]
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
              facetAspectRatio = facetAspectRatio,
              aggregationFun = aggregationFun
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
readTimeprofileConfigTable <- function(projectConfiguration,sheetName,dataObserved) {
  #initialize variable used in data.tables
  Level <- NULL

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
#' @template projectConfig
#' @template onePlotConfig
#' @template observedDataDT
#' @param rmdContainer Object of class `RmdContainer`.
#' @param aggregationFun Function to aggregate simulated data.
#'
#' @return Object of class `rmdContainer` with added figures.
#' @export
createPanelPlotsForPlotName <- function(projectConfiguration,
                                        onePlotConfig,
                                        dataObserved,
                                        nFacetColumns,
                                        rmdContainer,
                                        facetAspectRatio,
                                        aggregationFun) {
  message(paste("Create Plot", onePlotConfig$PlotName[1]))

  plotData <- PlotDataTimeProfile$new(
    projectConfiguration = projectConfiguration,
    onePlotConfig = onePlotConfig,
    dataObserved = dataObserved,
    aggregationFun = aggregationFun,
    nFacetColumns = nFacetColumns
  )

  for (plotType in names(plotTypesTimeprofile())) {
    rmdContainer <- generatePlotForPlotType(plotData,
      rmdContainer,
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
      yLimits <- checkAndAdjustYlimits(
        plotData,
        yScale,
        timeRangeFilter,
        plotType
      )

      plotObject <-
        switch(plotType,
          TP = ospsuite_plotTimeProfile(
            plotData = plotData$getDataForTimeRange(timeRangeFilter),
            yscale = yScale,
            mapping = getGroupbyMapping(plotData),
            groupAesthetics = getGroupAesthetics(plotData),
            mapSimulatedAndObserved = getMapSimulatedAndObserved(plotData),
            yscale.args = list(limits = yLimits)
          ),
          PvO = ospsuite_plotPredictedVsObserved(
            plotData = plotData$getDataForTimeRange(timeRangeFilter, typeFilter = "observed"),
            xyscale = yScale,
            mapping = getGroupbyMapping(plotData),
            groupAesthetics = getGroupAesthetics(plotData),
            comparisonLineVector = getFoldDistanceForPvO(plotData)
          ),
          ResvT = ospsuite_plotResidualsVsTime(
            plotData = plotData$getDataForTimeRange(timeRangeFilter, typeFilter = "observed"),
            residualScale = yScale,
            mapping = getGroupbyMapping(plotData),
            groupAesthetics = getGroupAesthetics(plotData)
          ),
          ResvO = ospsuite_plotResidualsVsObserved(
            plotData = plotData$getDataForTimeRange(timeRangeFilter, typeFilter = "observed"),
            residualScale = yScale,
            xscale = yScale,
            mapping = getGroupbyMapping(plotData),
            groupAesthetics = getGroupAesthetics(plotData)
          ),
          ResH = ospsuite_plotResidualsAsHistogram(
            plotData = plotData$getDataForTimeRange(timeRangeFilter, typeFilter = "observed"),
            residualScale = yScale,
            mapping = getGroupbyMapping(plotData),
            distribution = "normal"
          ),
          QQ = ospsuite_plotQuantileQuantilePlot(
            plotData = plotData$getDataForTimeRange(timeRangeFilter, typeFilter = "observed"),
            residualScale = yScale,
            mapping = getGroupbyMapping(plotData),
            groupAesthetics = getGroupAesthetics(plotData)
          )
        )

      plotObject <- updateGuides(
        plotData = plotData,
        plotObject = plotObject,
        plotType = plotType
      )

      plotObject <- setManualScalevectors(
        plotObject = plotObject,
        plotData = plotData,
        plotType = plotType
      )

      # add Facet Columns
      plotObject <- addFacets(
        plotObject = plotObject,
        plotData = plotData,
        facetAspectRatio =
          ifelse(plotType %in% c("PvO", "QQ "),
            1,
            facetAspectRatio
          )
      )

      # adjust time labels
      if (plotType %in% c("TP", "ResvT ")) {
        plotObject <- plotObject +
          labs(x = plotData$getTimeLabelForTimeRange(timeRangeFilter))
      }

      # export
      rmdContainer$addAndExportFigure(
        plotObject = plotObject,
        caption = getCaptionForPlot(plotData,
          yScale = yScale,
          timeRangeFilter = timeRangeFilter,
          plotType = plotType
        ),
        footNoteLines = getFootNoteLines(
          dataObserved = plotData$getDataForTimeRange(
            filterName = timeRangeFilter,
            typeFilter = "observed"
          ),
          dtDataReference = plotData$dataReference
        ),
        figureKey = paste(plotData$configTable$PlotName[1],
          plotType,
          ifelse(yScale == "log", "log", "linear"),
          timeRangeFilter,
          sep = "-"
        )
      )
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
    TP = "Plot_TimeProfiles",
    PvO = "Plot_PredictedVsObserved",
    ResvT = "Plot_ResidualsVsTime",
    ResvO = "Plot_ResidualsVsObserved",
    ResH = "Plot_ResidualsAsHistogram",
    QQ = "Plot_QQ",
    stop(paste("unknown plottype:", plotType))
  )

  if (plotType == "TP") {  # nolint: line_length
    return(as.logical(plotData$configTable[[configColumn]][1]))
  } else {
    return(as.logical(plotData$configTable[[configColumn]][1]) &
      plotData$hasObservedData()) # warning is thrown during data preparation
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
                                  plotType) {
  # initialize data.table variables
  dataType <- yUnit <- yValues <- lloq <- xValues <- yMin <- yerrorValues <- NULL

  if (plotType != "TP") {
    return(NULL)
  }

  ylimits <- plotData$configTable[[paste0("ylimit_", yScale)]][1]

  if (is.na(ylimits) || trimws(ylimits) == "") {
    ylimits <- NULL
  } else {
    ylimits <- eval(parse(text = ylimits))
  }

  simulatedData <- plotData$getDataForTimeRange(timeRangeFilter)[dataType == "simulated"]

  observedData <- plotData$getDataForTimeRange(timeRangeFilter)[dataType == "observed"]
  if (nrow(observedData) > 1) {
    observedData <- observedData[yUnit == unique(simulatedData$yUnit)[1]]
  }

  # make sure no observed data are missed by setting y limits
  if (!is.null(ylimits) & nrow(observedData) > 0) {
    if (any(observedData$yValues
    <= ylimits[1]) |
      any(observedData$yValues
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
      xValues <= timeRangeSim[2]]
    if (simulatedData$dataClass[1] == DATACLASS$tpAggregated) {
      if (simulatedData$yErrorType[1] == ospsuite::DataErrorType$ArithmeticStdDev) {
        tmp <- tmp[, yMin := yValues - yErrorValues]
      }
      if (simulatedData$yErrorType[1] == ospsuite::DataErrorType$GeometricStdDev) {
        tmp <- tmp[, yMin := yValues / yErrorValues]
      }
      minY <- min(c(
        minY,
        tmp[!is.na(yMin) &
          yMin > 0]$yMin
      ))
    } else {
      minY <- min(c(
        minY,
        tmp[!is.na(yValues) & yValues > 0]$yValues
      ))
    }

    ylimits <- c(minY / 2, NA)
  }

  return(ylimits)
}

#' Get mapping for "groupby" aesthetics
#'
#' @param plotData Object containing the data for the plot.
#'
#' @return Aesthetic mappings for grouping.
#' @keywords internal
getGroupbyMapping <- function(plotData) {
  if ("shapeIndex" %in% names(plotData$data)) {
    aes(groupby = colorIndex, shape = shapeIndex)
  } else {
    aes(groupby = colorIndex)
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
#' Update guides for time profile plots
#'
#' @param plotData Object containing the data for the plot.
#' @param plotObject The ggplot object to update.
#' @param plotType Type of the plot.
#'
#' @return Updated ggplot object with guides.
#' @keywords internal
updateGuides <- function(plotData, plotObject, plotType) {
  if (plotType != "TP") {
    return(plotObject)
  }

  # if mapSimulatedAdObserved is not available, colors hav to be set by scalingVectors
  if (!plotData$hasObservedData()) {
    for (aesthetic in names(plotData$scaleVectors)) {
      plotObject <- plotObject +
        scale_discrete_manual(aesthetic,
          values = plotData$scaleVectors[[aesthetic]]
        )
    }
  }

  if (plotData$useColorIndex() & !plotData$hasSimulatedPop()) {
    legendTitleSimulated <- paste("Simulated", plotData$tpLabelSimulatedMean)
  } else if (plotData$useColorIndex() & plotData$hasSimulatedPop()) {
    legendTitleSimulated <- paste0(
      "Simulated data\n",
      plotData$tpLabelSimulatedMean,
      " with ", plotData$tpLabelSimulatedRange
    )
  } else {
    legendTitleSimulated <- NULL
  }

  if (plotData$hasObservedData() &
    !plotData$useColorIndex() &
    !plotData$useShapeIndex()) {
    legendTitleObserved <- NULL
  } else {
    legendTitleObserved <- plotData$tpLabelObserved
  }

  aestheticsSuffix <- ifelse(plotData$hasObservedData(), "_ggnewscale_1", "")
  guidesList <- setNames(
    lapply(c("colour", "fill"), function(aesthetic) {
      guide_legend(order = 1, title = legendTitleSimulated)
    }),
    paste0(c("colour", "fill"), aestheticsSuffix)
  )
  if (plotData$hasObservedData()) {
    guidesListObserved <- setNames(
      lapply(c("colour", "fill"), function(aesthetic) {
        guide_legend(order = 2, title = legendTitleObserved)
      }),
      c("colour", "fill")
    )
    guidesList <- c(
      guidesList,
      guidesListObserved
    )
  }
  if (plotData$useShapeIndex()) {
    if (plotData$useColorIndex()) {
      guidesList[["shape"]] <- guide_legend(order = 3, title = "")
    } else {
      guidesList[["shape"]] <- guide_legend(order = 2, title = legendTitleObserved)
    }
  }

  plotObject <- plotObject + guides(!!!guidesList)

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
  if (plotType == "TP") {
    return(plotObject)
  }

  if (!plotData$useColorIndex() &
    !plotData$useShapeIndex()) {
    legendTitleObserved <- NULL
  } else {
    legendTitleObserved <- plotData$tpLabelObserved
  }

  for (aesthetic in names(plotData$scaleVectorsObserved)) {
    plotObject <- plotObject +
      scale_discrete_manual(aesthetic,
        values = plotData$scaleVectorsObserved[[aesthetic]],
        guide = guide_legend(order = 1, title = legendTitleObserved)
      )
  }

  plotObject <-
    plotObject + guides(linetype = guide_legend(order = 10, title = NULL))

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
                      facetAspectRatio = 0.5) {
  if (!is.null(plotData$nFacetColumns)) {
    plotObject <- plotObject +
      facet_wrap(
        facets = vars(PlotTag),
        scales = plotData$configTable$FacetScale[1],
        ncol = plotData$nFacetColumns
      ) +
      theme(aspect.ratio = facetAspectRatio)
  }

  return(plotObject)
}
#' Generate a caption for the plot
#'
#' @param plotData Object containing the data for the plot.
#' @param yScale Y scale of the plot.
#' @param timeRangeFilter Name of the time range filter.
#' @param plotType Type of the plot.
#'
#' @return Caption text for the plot.
#' @keywords internal
getCaptionForPlot <- function(plotData, yScale, timeRangeFilter, plotType) {
  dtCaption <-
    plotData$dtCaption[eval(parse(text = plotData$timeRangeTagFilter[[timeRangeFilter]]))]


  plotTypeTxt <- switch(plotType,
    TP = "Concentration-time profiles",
    PvO = "Predicted vs Observed",
    ResvT = "Residuals vs time values",
    ResvO = "Residuals vs observed values",
    ResH = "Residuals distribution ",
    QQ = "Residuals as quantile-quantile plot"
  )

  captiontext <- paste(
    plotTypeTxt,
    "for",
    pasteFigureTags(dtCaption, captionColumn = "outputDisplayName"),
    "for",
    pasteFigureTags(dtCaption, captionColumn = "ScenarioCaptionName"),
    "on a", ifelse(yScale == "linear", "linear", "logarithmic"),
    "y-scale.",
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
    as.double(plotData$configTable$FoldDistance_PvO[1]),
    2
  )

  return(ospsuite.plots::getFoldDistanceList(foldDistance))
}
#' @return Names list of plot types.
#' @keywords internal
plotTypesTimeprofile <- function() {
  c(
    TP = "TimeProfiles",
    PvO = "PredictedVsObserved",
    ResvT = "ResidualsVsTime",
    ResvO = "Plot_ResidualsVsObserved",
    ResH = "Plot_ResidualsAsHistogram",
    QQ = "QQ"
  )
}


# validation ----------------

#' Validation of config table for time profiles plots
#'
#' @template projectConfig
#' @param configTable Plot configuration table.
#' @template observedDataDT
#' @keywords internal
validateConfigTableForTimeProfiles <- function(configTable, dataObserved, projectConfiguration) {
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
        "ReferenceScenario"
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
  # Check for unique values of panel columns for each `PlotName`
  uniquePanelValues <-
    configTablePlots[, lapply(.SD, function(x) {
      length(unique(x))
    }), by = PlotName, .SDcols = panelColumns]
  tmp <- lapply(panelColumns, function(col) {
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
  TimeRangeColumns <-
    names(configTablePlots)[grepl("^TimeRange_", names(configTablePlots))]

  if (length(TimeRangeColumns) == 0) stop("You need at least one TimeRange Column")

  validateAtleastOneEntry(configTablePlots, columnVector = TimeRangeColumns)

  validateConfigTablePlots(configTablePlots,
    charactersWithMissing = TimeRangeColumns
  )

  tryCatch(
    {
      if (!all(sapply(configTablePlots[, ..TimeRangeColumns], function(x) {
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
validateOutputPathIdFormat <- function(configTablePlots) {
  if (any(length(stringr::str_extract_all(configTablePlots$outputPathId, "\\([^)]*$")) > 0) ||
    any(length(stringr::str_extract_all(configTablePlots$outputPathId, "(?<!\\()\\([^)]+")) > 0)) {
    stop("Please check the brackets in column outputPathId")
  }
}



# support usability --------------------

#' Adds a default sheet to the plot configuration table
#'
#' @template projectConfig
#' @param sheetName Name of the sheet in the plot configuration table.
#' @param overwrite Boolean indicating if existing configurations will be overwritten.
#' @export
addDefaultConfigForTimeProfilePlots <- function(projectConfiguration,
                                                sheetName = "TimeProfile_Panel", overwrite = FALSE) {
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)
  if (sheetName %in% wb$sheet_names & !overwrite) {
    stop(paste(sheetName, "already exist"))
  }

  scenarios <- getScenarioDefinitions(projectConfiguration)
  dtOutputPaths <- getOutputPathIds(projectConfiguration)
  dtDataGroups <- getDataGroups(projectConfiguration)

  dtNewHeader <- data.table(
    Level = 1,
    Header = "Concentration time profiles"
  )

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
      dtNewConfig,
      fill = TRUE
    )
  )

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)
}
