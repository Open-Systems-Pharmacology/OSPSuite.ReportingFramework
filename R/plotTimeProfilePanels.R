plotTimeProfilePanels <- function(projectConfiguration,
                                  subfolder,
                                  configTableSheet,
                                  observedData,
                                  prepareElectronicPackage = TRUE){
  checkmate::assert_path_for_output(file.path(projectConfiguration$outputFolder,subfolder),overwrite = TRUE)
  checkmate::assertFlag(prepareElectronicPackage)

  # use data.table format for observedData
  if ("DataCombined" %in% class(observedData) )
    observedData <-   convertDataCombinedToDataTable(observedData)

  # read configuration tables
  dtScenarios <- xlsxReadData(wb = projectConfiguration$scenarioDefinitionFile,sheetName  = 'Scenarios')
  dtOutputPaths <- xlsxReadData(wb = projectConfiguration$scenarioDefinitionFile,sheetName  = 'OutputPaths')
  configTable <- xlsxReadData(wb = projectConfiguration$plotsFile,
                              sheetName  = configTableSheet,
                              skipDescriptionRow = TRUE)
  validateConfigTableForTimeProfiles(configTable,dtScenarios,dtOutputPaths,observedData)


  # initialize Container for RMD generation for .Rmd generation
  rmdContainer <-
    RmdContainer$new(
      rmdfolder = file.path(projectConfiguration$outputFolder),
      subfolder = subfolder)

  # add  section headers
  rmdContainer$addHeader('Concentration time profiles', level = 1)

  for (panelConfig in split(configTable, by = "PanelName")) {

    rmdContainer <-
      plotLoopTimeProfilePanels(
        panelConfig = panelConfig,
        observedData = observedData,
        dtScenarios = dtScenarios,
        dtOutputPaths = dtOutputPaths,
        rmdContainer = rmdContainer
      )

  }


  # add a figure
  plotObject <-
    ospsuite.plots::plotHistogram(
      data = data.frame(x = rnorm(100)),
      mapping = ggplot2::aes(x = x))

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

  rmdContainer$addAndExportTable(dt = dt,
                                 path = resultDirectory,
                                 caption = "My captiontxt",
                                 tablename = "quantiles"
  )

  return(rmdContainer)
}


plotLoopTimeProfilePanels <- function(projectConfiguration,
                                      panelConfig,
                                      observedData,
                                      dtScenarios,
                                      dtOutputPaths,
                                      rmdContainer){
  # collect all data
  simulatedData <-
    loadSimulatedResultsForTimeProfilePanelPlot(projectConfiguration,
                                                panelConfig,
                                                dtScenarios,
                                                dtOutputPaths)
  observedDataPanel <-
    getObservedDataForTimeProfilePanelPlot(
      observedData = observedData,
      panelConfig = panelConfig,
      dtUnit = attr(simulatedData, 'dtUnit')
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
  #       fileNameOfPlot = paste0(paste(panelConfig$PanelName[1],
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


#' load simulated results for all scenarios of this panel
#'
#' @template projectConfig
#' @param panelConfig
#' @param dtScenarios
#' @param dtOutputPaths
#'
#' @return `data.table` with simulated results
#' @export
loadSimulatedResultsForTimeProfilePanelPlot <- function(projectConfiguration,
                                            panelConfig,
                                            dtScenarios,
                                            dtOutputPaths){
  simulatedData = data.table()
  dtUnit = data.table()

  simulatedResults <-
    esqlabsR::loadScenarioResults(
      scenarioNames = c(panelConfig$Scenario,
                        panelConfig[!is.na(ReferenceScenario)]$ReferenceScenario),
      resultsFolder = file.path(projectConfiguration$outputFolder, 'SimulationResults')
    )

  # use index and not scenario_name,
  # it may be necessary to duplicate scenarios, e.g. to plot with different data,or units
  for (scenarioIndex  in seq_len(nrow(panelConfig))){

    configList = as.list(panelConfig[scenarioIndex,])

    timeprofile <-
      getSimulatedTimeprofile(
        simulatedResult = simulatedResults[[configList$Scenario]],
        targetTimeUnit = configList$TimeUnit,
        outputs = gsub("[()]", "", splitInputs(configList$OutputPathIds)),
        dtOutputPaths = dtOutputPaths,
        timeOffset = as.double(configList$TimeOffset_Reference) +
          as.double(configList$TimeShift)
      ) %>%
      dplyr::mutate(scenarioIndex = scenarioIndex)


    simulatedData <- rbind(
      simulatedData,
      timeprofile
    )

    dtUnit <- rbind(dtUnit,
                    attr(timeprofile,'dtUnit')) %>%
      unique()

    if (!is.na(configList$ReferenceScenario) && as.logical(configList$ReferenceScenario)){
      simulatedData = rbind(simulatedData,
        getSimulatedTimeprofile(simulatedResult = simulatedResults[[configList$ReferenceScenario]],
                                targetTimeUnit = configList$TimeUnit,
                                outputs = gsub("[()]", "", splitInputs(configList$OutputPathIds)),
                                dtOutputPaths = dtOutputPaths,
                                timeOffset = as.double(configList$TimeShift)) %>%
                              dplyr::mutate(scenarioIndex = scenarioIndex))
    }
  }


  data.table::setattr(simulatedData,'dtUnit',dtUnit)

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
                                                   dtUnit){

  observedDataPanel = data.table()

  for (scenarioIndex  in seq_len(nrow(panelConfig))){

    configList = as.list(panelConfig[scenarioIndex,])

    dataGroupIds <- splitInputs(configList$DataGroupIds)
    # load observed data
    if (!is.null(dataGroupIds)){
      observedDataTmp <-
      observedData[group %in% dataGroupIds &
                     OutputPathId %in% gsub("[()]", "", splitInputs(configList$OutputPathIds))] %>%
        dplyr::mutate(scenarioIndex = scenarioIndex)

      observedDataTmp[, xValues := toUnit(
        quantityOrDimension  = 'Time',
        sourceUnit = observedDataTmp$xUnit[1],
        values = as.double(xValues),
        targetUnit = configList$TimeUnit,
      )]

      dtUnitObserved <- observedDataTmp %>%
        dplyr::select('OutputPathId', 'yUnit') %>%
        unique() %>%
        merge(dtUnit,
              by = 'OutputPathId')
      dtUnitObserved[,unitFactor:= toUnit(
        quantityOrDimension  = dimension,
        values = 1,
        sourceUnit = yUnit,
        targetUnit = DisplayUnit,
        molWeight = molWeight,molWeightUnit = 'g/mol'
      )]

      observedDataTmp <- observedDataTmp %>%
        merge(dtUnitObserved,
              by = 'OutputPathId')
      observedDataTmp[,yValues := yValues*unitFactor]

      observedDataPanel <-
        rbind(observedDataPanel,observedDataTmp)

    }
  }

  return(observedDataPanel)

}


#' loads simulated result and convert for plotting
#'
#' if population scenario, time profiles are aggregated
#' Units are converted to desired units
#'
#' @param simulatedResult simulation result for one scenario
#' @param outputs `outputPathsIds` selected for scenario
#' @param targetTimeUnit target unit for time display
#' @param timeOffset offset which is subtracted from time to adjust to Reference or data
#'
#' @return `data.table` with converted output
#' @export
getSimulatedTimeprofile <- function(simulatedResult,
                                    outputs,
                                    dtOutputPaths,
                                    targetTimeUnit,
                                    timeOffset) {

  dt = ospsuite::simulationResultsToDataFrame(
    simulationResults = simulatedResult$results,
    quantitiesOrPaths = unique(dtOutputPaths[OutputPathId %in% outputs]$OutputPath)) %>%
    data.table::setDT()

  # unit conversion
  dt[, Time := toUnit(
    quantityOrDimension  = 'Time',
    values = as.double(Time),
    targetUnit = targetTimeUnit,
  )]

  dtUnit <- dt %>%
    dplyr::select('paths', 'dimension', 'unit','molWeight') %>%
    unique() %>%
    merge(dtOutputPaths %>%
            dplyr::select('OutputPathId','DisplayUnit','OutputPath'),
          by.x = 'paths',
          by.y = 'OutputPath')
  dtUnit[,unitFactor:= toUnit(
    quantityOrDimension  = dimension,
    values = 1,
    sourceUnit = unit,
    targetUnit = DisplayUnit,
    molWeight = molWeight,molWeightUnit = 'g/mol'
  )]

  dt <- dt %>%
    merge(dtUnit[,c('paths','OutputPathId','unitFactor')],
          by = 'paths')
  dt[,simulationValues := simulationValues*unitFactor]

  if (dplyr::n_distinct(dt$IndividualId) > 1){
    stop('aggregate')
  } else{
    dt <- dt %>%
      dplyr::select(c('OutputPathId','Time','simulationValues')) %>%
      data.table::setnames(old = c('Time','simulationValues'),
               new = c('xValues','yValues'))
  }

  dt[,xValues:= xValues - timeOffset]


  # add full list of metadata to data set
  data.table::setattr(dt, "dtUnit", dtUnit %>%  dplyr::select(c(
    'OutputPathId', 'dimension', 'DisplayUnit', 'molWeight'
  )))

  return(dt)

}




#' Validation of config table fo time profiles plots
#'
#' @param configTable plot Configuration table
#' @param dtScenarios data.table with scenario definitions
#' @param dtOutputPaths data.table with output definitions
#' @param observedData observed data as data.table
#'
#' @export
validateConfigTableForTimeProfiles <- function(configTable,dtScenarios,dtOutputPaths,observedData){

  validateOutputIdsForPlot(dtOutputPaths)

  checkmate::assertCharacter(configTable$PanelName,any.missing = FALSE)
  checkmate::assertCharacter(configTable$Scenario,any.missing = FALSE)
  checkmate::assertNames(configTable$Scenario,subset.of = dtScenarios$Scenario_name)
  checkmate::assertCharacter(configTable$DataGroupIds)
  checkmate::assertNames(splitInputs(configTable$DataGroupIds),subset.of = unique(observedData$group))
  checkmate::assertCharacter(configTable$OutputPathIds,any.missing = FALSE)
  tmp <- gsub("[()]", "", splitInputs(configTable$OutputPathIds))
  checkmate::assertNames(tmp,subset.of = dtOutputPaths$OutputPathId,.var.name = 'configTable$OutputPathIds')
  if (any(!is.na(configTable$ReferenceScenario)))
    checkmate::assertNames(configTable[!is.na(ReferenceScenario)]$ReferenceScenario,subset.of = dtScenarios$Scenario_name)
  checkmate::assertNames(configTable$TimeUnit,subset.of = ospsuite::getUnitsForDimension('Time'))
  checkmate::assertNumeric(configTable$TimeOffset_Reference)
  checkmate::assertNumeric(configTable$TimeShift)
  checkmate::assertNames(tolower(splitInputs(configTable$yscale)),subset.of =  c('linear','log'))
  checkmate::assertCharacter(configTable$ScenarioCaptionName,any.missing = FALSE)
  checkmate::assertCharacter(configTable$PanelCaptionAddon,any.missing = TRUE)
  checkmate::assertNames(tolower(configTable$panel_facet_scale),subset.of = c('fixed','free','free_x','free_y'))

  # Timerange columns must be character and mast contain NA
  # 'total','firstApplication','lastApplication'
  # or a string which evaluates in r to a numeric vector length 2  (e.g. 'c(2,3)' or 'c(2,NA)'
  TimeRangeColumns <-
    names(configTable)[grepl("^TimeRange_", names(configTable))]
  if (length(TimeRangeColumns) > 0) {
    TimeRangeColumns <-
      TimeRangeColumns[sapply(configTable[, ..TimeRangeColumns], function(x)
        any(!is.na(x)))]
  }
  if (length(TimeRangeColumns)== 0) stop('You need at least one TimeRange Column')

  if (!all(sapply(configTable[, ..TimeRangeColumns], is.character)))
    stop('TimeRange columns must be character columns')

  tryCatch({
    if (!all(sapply(configTable[, ..TimeRangeColumns], function(x){
      valid <- x %in% c(NA, 'total', 'firstApplication', 'lastApplication')
      if (!all(valid)){
        valid <-
          is.numeric(eval(parse(text = x[!valid]) )) &&
          length(eval(parse(text = x[!valid]))) == 2
      }
      return(all(valid))})
    )) {
      stop('invalid inputs in one of the "TimeRange" columns')
    }
  }, error = function(err) {
    stop('invalid inputs in one of the "TimeRange" columns')
  })

  # ylimits
  tryCatch({
    if (!all(sapply(configTable[, c('ylimit_linear','ylimit_log')], function(x){
      valid <- is.na(x)
      if (!all(valid)){
        valid <-
          is.numeric(eval(parse(text = x[!valid]) )) &&
          length(eval(parse(text = x[!valid]))) == 2
      }
      return(all(valid))})
    )) {
      stop('invalid inputs in one of the "ylimit" columns')
    }
  }, error = function(err) {
    stop('invalid inputs in one of the "ylimit" columns')
  })



  # Check for unique values of panel columns for each PanelName
  panelColumns <- c('TimeUnit','ylimit_linear','ylimit_log','PanelCaptionAddon','panel_facet_scale')
  uniquePanelValues <-
    configTable[, lapply(.SD, function(x)
      length(unique(x))), by = PanelName, .SDcols = panelColumns]
  tmp <-lapply(panelColumns,function(col){
    if (any(uniquePanelValues[[col]]>1)) stop(paste('values for',col, 'should be the same within each panel'))
  })

  # check if more than two different units are combined in one panel
  configTableList <- split(configTable, by = "PanelName")
  for (configPanel in configTableList){
    outputs <-  gsub("[()]", "", splitInputs(configPanel$OutputPathIds))
    if (dplyr::n_distinct(dtOutputPaths[OutputPathId %in% outputs]$DisplayUnit) > 2)
      stop('do not combine more than two yUnits in one Panel')
  }

  return(invisible())
}

#' Adds a default sheet to the plot configuration table
#'
#' @template projectConfig
#' @param sheetName name of sheet in plot configuration table
#' @param overwrite `boolean` if TRUE existing configurations will be overwrtitten
#'
#' @return
addDefaultConfigForTimeProfilePlots <- function(projectConfiguration,
                                                sheetName = 'TimeProfiles',overwrite = FALSE){

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)
  if (sheetName %in% wb$sheet_names & ! overwrite)
    stop(paste(sheetName, 'already exist'))

  scenarios <- xlsxReadData(wb = projectConfiguration$scenarioDefinitionFile,sheetName  = 'Scenarios')
  dtOutputPaths <- xlsxReadData(wb = projectConfiguration$scenarioDefinitionFile,sheetName  = 'OutputPaths')


  dtNewConfig = data.table(PanelName = scenarios$Scenario_name,
                           Scenario = scenarios$Scenario_name,
                           OutputPathIds = paste(unique(dtOutputPaths$OutputPathId),collapse = ', '),
                           TimeOffset_Reference = 0,
                           TimeShift = 0,
                           TimeRange_total = 'total',
                           TimeRange_firstApplication = 'firstApplication',
                           TimeRange_lastApplication = 'lastApplication',
                           yScale =  'linear, log',
                           panel_facet_scale = 'fixed')

  wb <- addConfigToTemplate(wb = wb,
                            templateSheet = 'TimeProfiles',
                            sheetName = sheetName,
                            dtNewConfig = dtNewConfig)

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

}

