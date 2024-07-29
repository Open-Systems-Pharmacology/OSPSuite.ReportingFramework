plotTimeProfilePanels <- function(projectConfiguration,
                                  subfolder,
                                  configTableSheet,
                                  observedData,
                                  prepareElectronicPackage = TRUE) {
  checkmate::assert_path_for_output(file.path(projectConfiguration$outputFolder, subfolder), overwrite = TRUE)
  checkmate::assertFlag(prepareElectronicPackage)
  facetType <- match.arg(facetType)

  # use data.table format for observedData
  if ("DataCombined" %in% class(observedData)) {
    observedData <- convertDataCombinedToDataTable(observedData)
  }

  # read configuration tables
  configTable <- xlsxReadData(
    wb = projectConfiguration$plotsFile,
    sheetName = configTableSheet,
    skipDescriptionRow = TRUE,
    asLogicals
  )
  validateConfigTableForTimeProfiles(
    configTable = configTable,
    observedData = observedData,
    projectConfiguration = projectConfiguration
  )


  # initialize Container for RMD generation for .Rmd generation
  rmdContainer <-
    RmdContainer$new(
      rmdfolder = file.path(projectConfiguration$outputFolder),
      subfolder = subfolder
    )

  iRow = 1
  while (iRow < nrow(configPanel)){

  }

  # add  section headers
  rmdContainer$addHeader("Concentration time profiles", level = 1)

  for (panelConfig in split(configTable, by = "PlotName")) {
    rmdContainer <-
      plotOfOneTimeProfilePanel(
        panelConfig = panelConfig,
        observedData = observedData,
        rmdContainer = rmdContainer
      )
  }


  # add a figure
  plotObject <-
    ospsuite.plots::plotHistogram(
      data = data.frame(x = rnorm(100)),
      mapping = ggplot2::aes(x = x) # nolint
    )

  rmdContainer$addAndExportFigure(
    plotObject = plotObject,
    caption = "My captiontxt",
    footNoteLines = NULL,
    figureKey = "myHistogram"
  )

  # add a table
  figurekey <- "quantiles"
  dt <- data.table(
    x = rnorm(1000),
    class = sample(c("Female", "Male"), size = 1000, replace = TRUE)
  ) %>%
    .[, as.list(quantile(x)), by = "class"]

  # change digits of significance from 3 (default) to 4
  rmdContainer$digitsOfSignificance <- 4

  rmdContainer$addAndExportTable(
    dt = dt,
    path = resultDirectory,
    caption = "My captiontxt",
    tablename = "quantiles"
  )

  return(rmdContainer)
}


#' Title
#'
#' @param projectConfiguration
#' @param panelConfig
#' @param observedData
#' @param rmdContainer
#'
#' @return
#' @export
#'
#' @examples
plotOfOneTimeProfilePanel <- function(projectConfiguration,
                                      panelConfig,
                                      observedData,
                                      rmdContainer) {
  # collect all data
  plotData <- collectPlotData(
    projectConfiguration = projectConfiguration,
    panelConfig = panelConfig,
    observedData = observedData
  )

  ospsuite.plots::plotTimeProfile(
    data = simulatedData,
    mapping = aes(x = xValues, y = yValues),
    metaData = metaData,
    observedData = observedDataPanel
  )

  # #create the plot
  # for (timeRangeTag in grep('TimeRange_',names(panelConfig),value = TRUE)) {
  #
  #   simulatedDataFiltered <-
  #     filterDataForTimeRange(dt = copy(simulatedData),
  #                            timeRangeTag = timeRangeTag,
  #                            panelConfig = panelConfig,
  #                            timeRangeConfig = timeRangeConfig[[timeRangeTag]],
  #                            applicationTime = applicationTime)
  #
  #   if(nrow(simulatedDataFiltered) > 1) {
  #
  #     if (nrow(observedData) > 0) {
  #       observedDataFiltered <-
  #         filterDataForTimeRange(dt = observedData[!is.na(dv)],
  #                                timeRangeTag = timeRangeTag,
  #                                panelConfig = panelConfig,
  #                                timeRangeConfig = timeRangeConfig[[timeRangeTag]],
  #                                applicationTime = applicationTime)
  #     } else{
  #       observedDataFiltered <- copy(observedData)
  #     }
  #
  #     # make sure correct x label is set, by adjusting metadata
  #     metaData$time$dimension = ifelse(timeRangeConfig[[timeRangeTag]]$timeAfterDose,
  #                                      'Time after dose',
  #                                      'Time')
  #
  #
  #     tagData = generateTagData(
  #       panelConfig = panelConfig,
  #       simulatedData = simulatedDataFiltered,
  #       observedData = observedDataFiltered
  #     )
  #
  #     for (scaleToPlot  in scalesToPlot ) {
  #
  #       ylimits =checkAndAdjustYlimits(ylimits = panelConfig[[paste0('ylimit_',scaleToPlot)]][1],
  #                                      scaleToPlot = scaleToPlot,
  #                                      observedData = observedDataFiltered,
  #                                      simulatedData = simulatedDataFiltered)
  #
  #       # generate Plot
  #       plotObject <-
  #         plotOneTimeProfilePanel(
  #           simulatedData = simulatedDataFiltered,
  #           observedData = observedDataFiltered,
  #           tagData = tagData,
  #           scaleToPlot = scaleToPlot,
  #           metaData = metaData,
  #           colorScaleTP,
  #           ylimits =  ylimits,
  #           panelConfig = panelConfig)
  #
  #
  #
  #       # export
  #       fileNameOfPlot = paste0(paste(panelConfig$PlotName[1],
  #                                     'timeProfile',
  #                                     ifelse(scaleToPlot == 'log','Log','Linear'),
  #                                     timeRangeTag,
  #                                     'Concentration',
  #                                     sep = '-'),
  #                               '.png')
  #
  #
  #       suppressWarnings(plotExport(plotObject = plotObject,
  #                                   filepath = file.path(reportFolder,taskfolder),
  #                                   filename = fileNameOfPlot,
  #                                   height = NULL))
  #
  #
  #       captiontxt = getCaptionTimeProfile(
  #         tagData,
  #         scaleToPlot = scaleToPlot,
  #         timeRangeTag = timeRangeConfig[[timeRangeTag]]$captionTxt,
  #         panelAddon = panelConfig$PanelCaptionAddon[1]
  #       )
  #
  #       footnoteLines = getFootNoteLines(observedAsAggregated = observedAsAggregated,
  #                                        panelConfig = panelConfig,
  #                                        dataFiles = dataFiles)
  #
  #       # write to md file
  #       addFigureChunk(
  #         fileName = fileNameOfTask,
  #         figureFileRelativePath = file.path(reportFolder,taskfolder,fileNameOfPlot),
  #         figureFileRootDirectory = reportFolder,
  #         figureCaption = captiontxt,
  #         footnoteLines = footnoteLines)
  #
  #
  #     }
  #   }
  # }
}



#' COllects all data neede for this plots
#'
#' add colums for facet
#'
#' @template projectConfig
#' @param panelConfig
#' @param observedData
#'
#' @return `data.table` with plotData and attribute metaData
#' @export
collectPlotData <- function(projectConfiguration,
                            panelConfig,
                            observedData,
                            facetType) {
  simulatedData <-
    loadSimulatedResultsForTimeProfilePanelPlot(
      projectConfiguration,
      panelConfig
    ) %>%
    dplyr::mutate(dataType = "simulated")

  metaData <- constructMetadata(
    timeUnit = panelConfig$TimeUnit,
    yUnits = unique(attr(simulatedData, "dtUnit")$DisplayUnit)
  )

  observedDataPanel <-
    getObservedDataForTimeProfilePanelPlot(
      observedData = observedData,
      panelConfig = panelConfig,
      dtUnit = attr(simulatedData, "dtUnit")
    )

  plotData <- rbind(
    simulatedData,
    observedDataPanel
  )

  plotData <- addFacetColumns(plotData, facetType)

  data.table:setattr(plotData, "metaData", "metaData")

  return(plotData)
}

#' addFacetColum
#'
#' @param plotData
#' @param facetType
#'
#' @return
#' @export
addFacetColumns <- function(plotData, facetType) {
  return(plotData)
}


#' load simulated results for all scenarios of this panel
#'
#' @template projectConfig
#' @param panelConfig
#'
#' @return `data.table` with simulated results
#' @export
loadSimulatedResultsForTimeProfilePanelPlot <- function(projectConfiguration,
                                                        panelConfig) {
  simulatedData <- data.table()
  dtUnit <- data.table()

  simulatedResults <-
    esqlabsR::loadScenarioResults(
      scenarioNames = c(
        panelConfig$Scenario,
        panelConfig[!is.na(ReferenceScenario)]$ReferenceScenario
      ),
      resultsFolder = file.path(projectConfiguration$outputFolder, "SimulationResults")
    )

  # use index and not scenario_name,
  # it may be necessary to duplicate scenarios, e.g. to plot with different data,or units
  for (scenarioIndex in seq_len(nrow(panelConfig))) {
    configList <- as.list(panelConfig[scenarioIndex, ])

    timeprofile <-
      getSimulatedTimeprofile(
        projectConfiguration = projectConfiguration,
        simulatedResult = simulatedResults[[configList$Scenario]],
        targetTimeUnit = configList$TimeUnit,
        outputs = gsub("[()]", "", splitInputs(configList$OutputPathIds)),
        timeOffset = as.double(configList$TimeOffset_Reference) +
          as.double(configList$TimeOffset)
      ) %>%
      dplyr::mutate(scenarioIndex = scenarioIndex)


    simulatedData <- rbind(
      simulatedData,
      timeprofile
    )

    dtUnit <- rbind(
      dtUnit,
      attr(timeprofile, "dtUnit")
    ) %>%
      unique()

    if (!is.na(configList$ReferenceScenario) && as.logical(configList$ReferenceScenario)) {
      simulatedData <- rbind(
        simulatedData,
        getSimulatedTimeprofile(
          projectConfiguration = projectConfiguration,
          simulatedResult = simulatedResults[[configList$ReferenceScenario]],
          targetTimeUnit = configList$TimeUnit,
          outputs = gsub("[()]", "", splitInputs(configList$OutputPathIds)),
          timeOffset = as.double(configList$TimeOffset)
        ) %>%
          dplyr::mutate(scenarioIndex = scenarioIndex)
      )
    }
  }


  data.table::setattr(simulatedData, "dtUnit", dtUnit)

  return(simulatedData)
}


#' load observed data used in this panel
#'
#' @param panelConfig
#' @param observedData
#' @param dtUnit
#'
#' @return
#' @export
getObservedDataForTimeProfilePanelPlot <- function(observedData,
                                                   panelConfig,
                                                   dtUnit) {
  observedDataPanel <- data.table()

  for (scenarioIndex in seq_len(nrow(panelConfig))) {
    configList <- as.list(panelConfig[scenarioIndex, ])

    dataGroupIds <- splitInputs(configList$DataGroupIds)
    # load observed data
    if (!is.null(dataGroupIds)) {
      observedDataTmp <-
        observedData[group %in% dataGroupIds &
          OutputPathId %in% gsub("[()]", "", splitInputs(configList$OutputPathIds))] %>%
        dplyr::select(dplyr::any_of(getColumnsForColumnType(columnTypes = c("identifier", "timeprofile")))) %>%
        dplyr::mutate(scenarioIndex = scenarioIndex)

      observedDataTmp[, xValues := toUnit(
        quantityOrDimension = "Time",
        sourceUnit = observedDataTmp$xUnit[1],
        values = as.double(xValues),
        targetUnit = configList$TimeUnit,
      )]

      dtUnitObserved <- observedDataTmp %>%
        dplyr::select("OutputPathId", "yUnit") %>%
        unique() %>%
        merge(dtUnit,
          by = "OutputPathId"
        )
      dtUnitObserved[, unitFactor := toUnit(
        quantityOrDimension = dimension,
        values = 1,
        sourceUnit = yUnit,
        targetUnit = DisplayUnit,
        molWeight = molWeight, molWeightUnit = "g/mol"
      )]

      observedDataTmp <- observedDataTmp %>%
        merge(dtUnitObserved,
          by = "OutputPathId"
        )
      observedDataTmp[, yValues := yValues * unitFactor]

      observedDataPanel <-
        rbind(observedDataPanel, observedDataTmp)
    }
  }

  return(observedDataPanel)
}


#' loads simulated result and convert for plotting
#'
#' if population scenario, time profiles are aggregated
#' Units are converted to desired units
#'
#' @template projectConfig
#' @param simulatedResult simulation result for one scenario
#' @param outputs `outputPathsIds` selected for scenario
#' @param targetTimeUnit target unit for time display
#' @param timeOffset offset which is subtracted from time to adjust to Reference or data
#'
#' @return `data.table` with converted output
#' @export
getSimulatedTimeprofile <- function(projectConfiguration,
                                    simulatedResult,
                                    outputs,
                                    targetTimeUnit,
                                    timeOffset) {
  dtOutputPaths <- getOutputPathIds(projectConfiguration)
  dt <- ospsuite::simulationResultsToDataFrame(
    simulationResults = simulatedResult$results,
    quantitiesOrPaths = unique(dtOutputPaths[OutputPathId %in% outputs]$OutputPath)
  ) %>%
    data.table::setDT()

  # unit conversion
  dt[, Time := toUnit(
    quantityOrDimension = "Time",
    values = as.double(Time),
    targetUnit = targetTimeUnit,
  )]

  dtUnit <- dt %>%
    dplyr::select("paths", "dimension", "unit", "molWeight") %>%
    unique() %>%
    merge(
      dtOutputPaths %>%
        dplyr::select("OutputPathId", "DisplayUnit", "OutputPath"),
      by.x = "paths",
      by.y = "OutputPath"
    )
  dtUnit[, unitFactor := toUnit(
    quantityOrDimension = dimension,
    values = 1,
    sourceUnit = unit,
    targetUnit = DisplayUnit,
    molWeight = molWeight, molWeightUnit = "g/mol"
  )]

  dt <- dt %>%
    merge(dtUnit[, c("paths", "OutputPathId", "unitFactor")],
      by = "paths"
    )
  dt[, simulationValues := simulationValues * unitFactor]

  if (dplyr::n_distinct(dt$IndividualId) > 1) {
    stop("aggregate")
  } else {
    dt <- dt %>%
      dplyr::select(c("OutputPathId", "Time", "simulationValues")) %>%
      data.table::setnames(
        old = c("Time", "simulationValues"),
        new = c("xValues", "yValues")
      )
  }

  dt[, xValues := xValues - timeOffset]


  # add full list of metadata to data set
  data.table::setattr(dt, "dtUnit", dtUnit %>% dplyr::select(c(
    "OutputPathId", "dimension", "DisplayUnit", "molWeight"
  )))

  return(dt)
}

# auxiliaries ----------

constructMetadata <- function(timeUnit,
                              yUnits) {
  if (length(yUnits) > 2) {
    stop("to many y units for one plot")
  }

  metaData <- list(
    xValues = list(
      unit = timeUnit,
      dimension = "Time"
    ),
    yValues = list(
      unit = yUnits[1],
      dimension = ospsuite::getDimensionForUnit(yUnits[1])
    )
  )

  if (length(yUnits) == 2) {
    metaData[["y2"]] <- list(
      unit = yUnits[2],
      dimension = ospsuite::getDimensionForUnit(yUnits[2])
    )
  }

  return(metaData)
}


#' Creates PlotID for each scenario outputs group
#'
#' @param configTable
#'
#' @return `data.table` with columns 'Scenario','OutputPathId','PlotID'
#' @export
getPlotIdTable <- function(configTable) {

  plotIDTable <-
    tidyr::separate_rows(data.table::setDF(configTable), OutputPathId, sep = ",\\s*|(?<=\\)),\\s*|\\s(?=\\()")  %>%
    data.table::setDT()


  # set Plot ID as cumulated sum over difference off number of open and closed brackets
  plotIDTable[, nBracketOpen := cumsum(grepl("\\(", OutputPathId)),]
  plotIDTable[, nBracketClosed := cumsum(grepl("\\)", OutputPathId)),]
  plotIDTable[, countAdd := nBracketClosed-nBracketOpen+1,]
  plotIDTable[, countPlot := cumsum(data.table::shift(countAdd,fill = 1)),]

  plotIDTable[, OutputPathId := trimws(gsub("[()]",'',OutputPathId)),]

  plotIDTable[,  PlotID := paste0('P',countPlot),]

  plotIDTable$PlotID <- factor(plotIDTable$PlotID,)


  return(plotIDTable %>% dplyr::select('Scenario','OutputPathId','PlotID'))
}


# Validation ----------------


#' Validation of config table fo time profiles plots
#'
#' @teamplate projectConfig
#' @param configTable plot Configuration table
#' @param observedData observed data as data.table
#'
#' @export
validateConfigTableForTimeProfiles <- function(configTable, observedData, projectConfiguration) {
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
        "DataGroupId",
        "PlotCaptionAddon",
        "ReferenceScenario"
      ),
    numericColumns = c(
      "TimeOffset",
      "TimeOffset_Reference"
    ),
    logicalColumns = c(
      "Plot_TimeProfiles",
      "Plot_PredictedVsObserved",
      "Plot_ResidualsAsHistogram",
      "Plot_ResidualsVsTime",
      "Plot_ResidualsVsObserved"
    ),
    numericRangeColumns = c("ylimit_linear", "ylimit_log"),
    subsetList = list(
      scenario = list(
        cols = c("Scenario", "ReferenceScenario"),
        allowedValues = dtScenarios$Scenario_name
      ),
      dataGroupId = list(
        cols = c("DataGroupId"),
        allowedValues = unique(observedData$group)
      ),
      OutputPathId = list(
        cols = c("OutputPathId"),
        allowedValues = unique(dtOutputPaths$OutputPathId)
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
      "ylimit_linear",
      "ylimit_log",
      "PlotCaptionAddon",
      "FacetType",
      "FacetScale"
    ),
    dtOutputPaths = dtOutputPaths
  )
  return(invisible())
}



#' check if panel columns ar filled consistently
#'
#' @template configTablePlots
#' @param panelcolumns vector of columns which should be consistent
validatePanelConsistency <- function(
    configTablePlots,
    panelColumns,
    dtOutputPaths) {
  # Check for unique values of panel columns for each `PlotName`
    uniquePanelValues <-
    configTable[, lapply(.SD, function(x) {
      length(unique(x))
    }), by = PlotName, .SDcols = panelColumns]
  tmp <- lapply(panelColumns, function(col) {
    if (any(uniquePanelValues[[col]] > 1)) stop(paste("values for", col, "should be the same within each panel"))
  })

  # check if more than two different units are combined in one panel
  configTableList <- split(configTable, by = "PlotName")
  for (configPanel in configTableList) {
    outputs <- gsub("[()]", "", splitInputs(configPanel$OutputPathIds))
    if (dplyr::n_distinct(dtOutputPaths[OutputPathId %in% outputs]$DisplayUnit) > 2) {
      stop("do not combine more than two yUnits in one Panel")
    }
  }

  return(invisible())
}

#' validates timerange columns
#'
#' Timerange columns must be character and mast contain NA
#' 'total','firstApplication','lastApplication'
#' or a string which evaluates in r to a numeric vector length 2  (e.g. 'c(2,3)' or 'c(2,NA)'
#'
#' @template configTablePlots
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


validateOutputPathIdFormat <- function(configTablePlots) {

  if(any(length(stringr::str_extract_all(configTablePlots$OutputPathId, "\\([^)]*$")) > 0) ||
     any(length(stringr::str_extract_all(configTablePlots$OutputPathId, "(?<!\\()\\([^)]+")) > 0))
    stop('Please check the brackets in column OutputPathId')

}



# support usability --------------------

#' Adds a default sheet to the plot configuration table
#'
#' @template projectConfig
#' @param sheetName name of sheet in plot configuration table
#' @param overwrite `boolean` if TRUE existing configurations will be overwritten
#'
#' @return
addDefaultConfigForTimeProfilePlots <- function(projectConfiguration,
                                                sheetName = "TimeProfile_Panel", overwrite = FALSE) {
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)
  if (sheetName %in% wb$sheet_names & !overwrite) {
    stop(paste(sheetName, "already exist"))
  }

  scenarios <- getScenarioDefinitions(projectConfiguration)
  dtOutputPaths <- getOutputPathIds(projectConfiguration)

  dtNewHeader <- data.table(
    Level = 1,
    Header = "Concentration time profiles"
  )

  dtNewConfig <- data.table(
    PlotName = scenarios$Scenario_name,
    Scenario = scenarios$Scenario_name,
    ScenarioCaptionName = scenarios$Scenario_name,
    OutputPathIds = paste(unique(dtOutputPaths$OutputPathId), collapse = ", "),
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
    Plot_ResidualsAsHistogram = FALSE,
    Plot_ResidualsVsTime = FALSE,
    Plot_ResidualsVsObserved = FALSE
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
