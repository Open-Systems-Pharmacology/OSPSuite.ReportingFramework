#' function to generate list of time profiles as facet panels
#'
#' @template projectConfig
#' @param subfolder is name of subfolder in the project/to/path/report directory
#' @param configTableSheet name of sheet in plot configuration xlsx, which defines the plot
#' @template observedData
#' @param nFacetColumns maximal number of facet columns (default 2) used in for facet type "by Order"
#'
#' @return object of class `rmdContainer`
#' @export
plotTimeProfilePanels <- function(projectConfiguration,
                                  subfolder,
                                  configTableSheet,
                                  dataObserved,
                                  nFacetColumns = 2,
                                  facetAspectRatio = 0.5) {
  checkmate::assert_path_for_output(file.path(projectConfiguration$outputFolder, subfolder), overwrite = TRUE)
  checkmate::assertIntegerish(nFacetColumns,lower = 1,len = 1)
  checkmate::assertDouble(facetAspectRatio,lower = 0,len = 1)

  # use data.table format for dataObserved
  if ("DataCombined" %in% class(dataObserved)) {
    dataObserved <- convertDataCombinedToDataTable(dataObserved)
  }

  # read configuration tables
  configTable <- xlsxReadData(
    wb = projectConfiguration$plotsFile,
    sheetName = configTableSheet,
    skipDescriptionRow = TRUE
  )
  validateConfigTableForTimeProfiles(
    configTable = configTable,
    dataObserved = dataObserved,
    projectConfiguration = projectConfiguration
  )


  # initialize Container for RMD generation for .Rmd generation
  rmdContainer <-
    RmdContainer$new(
      rmdfolder = file.path(projectConfiguration$outputFolder),
      subfolder = subfolder
    )

  iRow = 1
  levelLines = which(!is.na(configTable$Level))
  while (iRow < nrow(configTable)){
    if (!is.na(configTable$Level[iRow])){
      # add  section headers
      rmdContainer$addHeader(configTable$Header[iRow],
                             level = configTable$Level[iRow])
      iRow = iRow +1
    } else{
      # execute plot section
      iEndX = head(which(levelLines>iRow))
      if (length(iEndX) ==0 ) {
        iEnd <- nrow(configTable)}
      else{
        iEnd <- levelLines[iEndX]-1
      }

      for (onePlotConfig in split(configTable[seq(iRow,iEnd)], by = "PlotName")) {
        rmdContainer <-
          plotOfOnePanel(projectConfiguration = projectConfiguration,
                                    onePlotConfig = onePlotConfig,
                                    dataObserved = dataObserved,
                                    rmdContainer = rmdContainer,
                                    nFacetColumns = nFacetColumns,
                                    facetAspectRatio = facetAspectRatio
          )
      }

      iRow <- iEnd + 1

    }

  }

  return(rmdContainer)
}


#' function to generate one plot as facet panels
#'
#'
#' @template projectConfig
#' @template onePlotConfig
#' @template observedDataDT
#' @param rmdContainer object of class `rmdContainer`
#'
#' @return object of class `rmdContainer` with added figures
#' @export
#'
plotOfOnePanel <- function(projectConfiguration,
                                      onePlotConfig,
                                      dataObserved,
                                      nFacetColumns,
                                      rmdContainer,
                                      facetAspectRatio) {
  plotData <- PlotDataTimeProfile$new(projectConfiguration = projectConfiguration,
                                      onePlotConfig = onePlotConfig)

  plotData$collectData(projectConfiguration = projectConfiguration,
                           dataObserved = dataObserved)

  plotData$setOrderAndFactors(
    identifierColumns = getColumnsForColumnType(dt = dataObserved, 'identifier'))

  # collect all data
  plotDataList <- collectPlotData(
                           )


  # get table with caption information per plot Id
  plotDataList <-
    addPlotIdTable(
      plotDataList= plotDataList,
      onePlotConfig = onePlotConfig %>%  dplyr::select('scenarioIndex', 'OutputPathIds', 'ScenarioCaptionName'),
      projectConfiguration = projectConfiguration
    )

  # setColorIndex
  plotDataList <- setColorIndex(projectConfiguration = projectConfiguration,
                                plotData$List)

  # TODO replicate TimeRanges
  timeRangeTag <- 'total'

  if (as.logical(onePlotConfig$Plot_TimeProfiles[1]))
    rmdContainer <- plotTPPanel(projectConfiguration,
                                onePlotConfig,
                                plotData,
                                dtPlotId,
                                rmdContainer,
                                nFacetColumns = nFacetColumns,
                                facetAspectRatio = facetAspectRatio)

  return(rmdContainer)

}




#' create the TimeProfile panels
#'
#' @template projectConfig
#' @template onePlotConfig
#' @param plotData
#' @param dtPlotId
#' @param rmdContainer
#' @param nFacetColumns
#' @param facetAspectRatio
#'
#' @return
#' @export
#'
#' @examples
plotTPPanel <- function(projectConfiguration,
                            onePlotConfig,
                            plotData,
                            dtPlotId,
                            rmdContainer,
                            nFacetColumns,
                            facetAspectRatio){


  countGroups <-
    plotData[, .(
      nOutput = dplyr::n_distinct(OutputPathId),
      nGroup = dplyr::n_distinct(group, na.rm = TRUE)
    ), by = 'PlotTag'][, .(nOutput = max(nOutput), nGroup = max(nGroup))]

  # One data group more the one output
  if (countGroups$nOutput > 1 & countGroups$nGroup == 1){

    dtOutputPaths = getOutputPathIds(projectConfiguration)
    plotData[,colorIndex := OutputPathId]
    plotData$colorIndex = factor(plotData$colorIndex,
                                 levels = dtOutputPaths$OutputPathId,
                                 labels = dtOutputPaths$DisplayName,
                                 ordered = TRUE)

    mapSimulatedAndObserved = data.frame(
      simulated = levels(plotData$colorIndex),
      observed = levels(plotData$colorIndex),
      color = c(ggsci::pal_d3("category20c")(20)[1],ggsci::pal_d3("category20b")(20)[4]),
      fill = c(ggsci::pal_d3("category20c")(20)[6],ggsci::pal_d3("category20b")(20)[9])
    )

    if (all(is.na(dtOutputPaths$color)) & all(is.na(dtOutputPaths$color))){
      if (countGroups$nOutput <= 10){
        mapSimulatedAndObserved$color <- ggsci::pal_d3("category20c")(20)[seq(1,countGroups$nOutput)]
        mapSimulatedAndObserved$fill <- ggsci::pal_d3("category20c")(20)[seq(1,countGroups$nOutput)+10]
      }
    } else {
      if (any(!is.na(dtOutputPaths$color)))
      mapSimulatedAndObserved$color = setNames(as.list(dtOutputPaths$color),
                                               dtOutputPaths$DisplayName)[levels(plotData$colorIndex)]
      if( all(is.na(dtOutputPaths$fill)))
        mapSimulatedAndObserved[,fill := color]
      if (any(!is.na(dtOutputPaths$fill)))
        mapSimulatedAndObserved$fill = setNames(as.list(dtOutputPaths$fill),
                                                dtOutputPaths$DisplayName)[levels(plotData$colorIndex)]

      if( all(is.na(dtOutputPaths$color)))
        mapSimulatedAndObserved[,color := fill]

    }

    mapping = aes(groupby = colorIndex)
    groupAesthetics = c("colour", "fill")

    labels = list(fill_ggnewscale_1 = 'simulated timeprofile',
                  colour_ggnewscale_1 = 'simulated timeprofile',
                  fill = 'observed data',
                  colour = 'observed data')
  }

  for (yScale in splitInputs(onePlotConfig$yScale[1])){

    plotObject <- do.call(
      what = ospsuite_plotTimeProfile,
      args = c(
        list(
          plotData = plotData,
          yscale = yScale,
          mapping = mapping,
          groupAesthetics = groupAesthetics,
          geomLLOQAttributes = list(linetype = "dashed"),
          mapSimulatedAndObserved = mapSimulatedAndObserved
        ),
        eval(parse(
          text = paste('list(', onePlotConfig$PlotInputs_TP[1], ')')
        ))
      ))


    # add Facet Columns
    plotObject <- addFacets(plotObject = plotObject,
                            onePlotConfig = onePlotConfig,
                            nFacetColumns = nFacetColumns,
                            facetAspectRatio = facetAspectRatio)

    plotObject$labels = utils::modifyList(plotObject$labels,
                                          labels)

    rmdContainer$addAndExportFigure(
      plotObject = plotObject,
      caption = getCaptionTimeProfile(dtPlotId = dtPlotId,
                                      yScale = yScale,
                                      timeRangeTag = 'total',
                                      PlotCaptionAddon = onePlotConfig$PlotCaptionAddon[1]),
      footNoteLines = getFootNoteLines(dataObserved = plotData[plotType == 'observed'],
                                       projectConfiguration = projectConfiguration),
      figureKey = paste(onePlotConfig$PlotName[1],
                        'TP',
                        ifelse(yScale == 'log','Log','Linear'),
                        timeRangeTag,
                        sep = '-')
    )


  }

  return(rmdContainer)
}



#' Collects all observed and simulated data needed for this plots
#'
#' @template projectConfig
#' @template onePlotConfig
#' @template observedDataDT
#'
#' @return `data.table` with plotData and attribute metaData
#' @export
collectPlotData <- function(projectConfiguration,
                            onePlotConfig,
                            dataObserved,
                            facetType) {
  simulatedData <-
    loadSimulatedResultsForTimeProfilePanelPlot(
      projectConfiguration = projectConfiguration,
      onePlotConfig = onePlotConfig
    ) %>%
    dplyr::mutate(dataType = "simulated")

  observedDataPanel <-
    getObservedDataForTimeProfilePanelPlot(
      dataObserved = dataObserved,
      onePlotConfig = onePlotConfig,
      dtUnit = attr(simulatedData, "dtUnit")
    )

  plotData <- rbind(
    simulatedData,
    observedDataPanel,
    fill = TRUE
  )

  # make sure every thing will be plotted in correct order
  plotData <- setOrderAndFactors(
    projectConfiguration = projectConfiguration,
    plotData = plotData,
    identifierColumns = getColumnsForColumnType(dt = dataObserved, 'identifier'))

  return(list(plotData = plotData))
}

#' addFacetColum
#'
#' @param plotObject ggplot object
#' @param onePlotConfig
#' @param nFacetColumns maxinaml number of facet used for facettype byOrder
#'
#' @return ggplot Object with facets
#' @export
addFacets <- function(plotObject,
                      onePlotConfig,
                      nFacetColumns,
                      facetAspectRatio = 0.5) {



  if (onePlotConfig$FacetType[1] == FACETTYPE$byOrder){


    plotObject <- plotObject +
      facet_wrap(facets = vars(PlotTag),
                 scales = onePlotConfig$FacetScale[1],
                 ncol = nFacetColumns)

  } else if(onePlotConfig$FacetType[1] == FACETTYPE$vsOutput){
    stop('not yet implemented')
  } else if(onePlotConfig$FacetType[1] == FACETTYPE$vsTimeRange){
    stop('not yet implemented')
  } else {
    stop(paste('unknown facet type. Pleae use one of those:'),
         paste(FACETTYPE,collapse = ','))
  }

  plotObject <- plotObject +
    theme(aspect.ratio = facetAspectRatio,
          strip.background = element_rect(fill = NA,color = NA),
          strip.text = element_text(hjust = 0,vjust = 1))


  return(plotObject)
}


#' load simulated results for all scenarios of this panel
#'
#' @template projectConfig
#' @template onePlotConfig
#'
#' @return `data.table` with simulated results
#' @export
loadSimulatedResultsForTimeProfilePanelPlot <- function(projectConfiguration,
                                                        onePlotConfig) {
  simulatedData <- data.table()
  dtUnit <- data.table()

  simulatedResults <-
    esqlabsR::loadScenarioResults(
      scenarioNames = c(
        onePlotConfig$Scenario,
        onePlotConfig[!is.na(ReferenceScenario)]$ReferenceScenario
      ),
      resultsFolder = file.path(projectConfiguration$outputFolder, "SimulationResults")
    )

  # use index and not scenario_name,
  # it may be necessary to duplicate scenarios, e.g. to plot with different data,or units
  for (scenarioIndex in seq_len(nrow(onePlotConfig))) {
    configList <- as.list(onePlotConfig[scenarioIndex, ])

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
#' @template onePlotConfig
#' @template observedDataDT
#' @param dtUnit `data.table` with unit information
#'
#' @return `data.table` with observed data used in this plot
#'
#' @export
getObservedDataForTimeProfilePanelPlot <- function(dataObserved,
                                                   onePlotConfig,
                                                   dtUnit) {
  observedDataPanel <- data.table()

  for (scenarioIndex in seq_len(nrow(onePlotConfig))) {
    configList <- as.list(onePlotConfig[scenarioIndex, ])

    dataGroupIds <- splitInputs(configList$DataGroupIds)
    # load observed data
    if (!is.null(dataGroupIds)) {
      observedDataTmp <-
        dataObserved[group %in% dataGroupIds &
          OutputPathId %in% gsub("[()]", "", splitInputs(configList$OutputPathIds))] %>%
        dplyr::select(dplyr::any_of(getColumnsForColumnType(dataObserved,
                                                            columnTypes = c("identifier", "timeprofile")))) %>%
        dplyr::mutate(scenarioIndex = scenarioIndex)

      observedDataTmp[, xValues := toUnit(
        quantityOrDimension = "Time",
        sourceUnit = observedDataTmp$xUnit[1],
        values = as.double(xValues),
        targetUnit = configList$TimeUnit,
      )]
      observedDataTmp[,xUnit := configList$TimeUnit]

      dtUnitObserved <- observedDataTmp %>%
        dplyr::select(c("OutputPathId", "yUnit")) %>%
        unique() %>%
        merge(dtUnit,
          by = c("OutputPathId")
        )
      for (iRow in seq_len(nrow(dtUnitObserved))){
        dtUnitObserved$unitFactor[iRow] = toUnit(
          quantityOrDimension = dtUnitObserved$dimension[iRow],
          values = 1,
          sourceUnit = dtUnitObserved$yUnit[iRow],
          targetUnit = dtUnitObserved$DisplayUnit[iRow],
          molWeight = dtUnitObserved$molWeight[iRow],
          molWeightUnit = "g/mol"
        )
      }

      observedDataTmp <- observedDataTmp %>%
        merge(dtUnitObserved %>%
                dplyr::select(c('OutputPathId','DisplayUnit','unitFactor')),
          by = "OutputPathId"
        )
      observedDataTmp[, yValues := yValues * unitFactor]

      observedDataTmp <- observedDataTmp %>%
        dplyr::select(-c('yUnit','unitFactor')) %>%
        data.table::setnames('DisplayUnit','yUnit')

      observedDataPanel <-
        rbind(observedDataPanel,
              observedDataTmp)
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
  dt[,xUnit:= targetTimeUnit]

  dtUnit <- dt %>%
    dplyr::select("paths", "dimension", "unit", "molWeight") %>%
    unique() %>%
    merge(
      dtOutputPaths %>%
        dplyr::select("OutputPathId", "DisplayUnit", "OutputPath"),
      by.x = "paths",
      by.y = "OutputPath"
    )
  for (iRow in seq_len(nrow(dtUnit))){
    dtUnit$unitFactor[iRow] = toUnit(
      quantityOrDimension = dtUnit$dimension[iRow],
      values = 1,
      sourceUnit = dtUnit$unit[iRow],
      targetUnit = dtUnit$DisplayUnit[iRow],
      molWeight = dtUnit$molWeight[iRow],
      molWeightUnit = "g/mol"
    )
  }

  dt <- dt %>%
    merge(dtUnit[, c("paths", "OutputPathId", "unitFactor",'DisplayUnit')] ,
      by = "paths"
    )
  dt[, simulationValues := simulationValues * unitFactor]

  if (dplyr::n_distinct(dt$IndividualId) > 1) {
    stop("aggregate")
  } else {
    dt <- dt %>%
      dplyr::select(c("OutputPathId", "Time", "xUnit","simulationValues",'DisplayUnit')) %>%
      data.table::setnames(
        old = c("Time", "simulationValues",'DisplayUnit'),
        new = c("xValues", "yValues",'yUnit')
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


#' Creates PlotId for each scenario outputs group
#'
#' @template projectConfig
#' @template plotDataList
#' @param onePlotConfig
#'
#' @return `data.table` with columns 'Scenario','OutputPathId','PlotId'
#' @export
addPlotIdTable <- function(plotDataList,onePlotConfig,projectConfiguration) {

  dtPlotId <-
    tidyr::separate_rows(data.table::setDF(onePlotConfig), OutputPathIds, sep = ",\\s*|(?<=\\)),\\s*|\\s(?=\\()")  %>%
    data.table::setDT() %>%
    data.table::setnames('OutputPathIds','OutputPathId')


  # set Plot ID as cumulated sum over difference off number of open and closed brackets
  dtPlotId[, nBracketOpen := cumsum(grepl("\\(", OutputPathId)),]
  dtPlotId[, nBracketClosed := cumsum(grepl("\\)", OutputPathId)),]
  dtPlotId[, countAdd := nBracketClosed-nBracketOpen+1,]
  dtPlotId[, PlotId := cumsum(data.table::shift(countAdd,fill = 1)),]
  dtPlotId[, PlotTag := toupper(letters[PlotId]),]

  dtPlotId$PlotTag <-
    factor(dtPlotId$PlotTag,
           ordered = TRUE,
           levels = unique(dtPlotId$PlotTag))

  # add display name for caption
  dtPlotId[, OutputPathId := trimws(gsub("[()]",'',OutputPathId)),]

  dtOutputPathIds <- getOutputPathIds(projectConfiguration = projectConfiguration)
  dtPlotId <- dtPlotId %>%
    merge(dtOutputPathIds %>%  dplyr::select(OutputPathId,'DisplayName'),
          by = 'OutputPathId') %>%
    data.table::setnames('DisplayName','outputDisplayName')


  plotDataList$dtPlotId <-
    dtPlotId%>% dplyr::select('scenarioIndex','OutputPathId','PlotTag','ScenarioCaptionName','outputDisplayName')

  # add plot ID
  plotDataList$plotData <- plotDataList$plotData %>%
    merge(plotDataList$dtPlotId %>%  dplyr::select('scenarioIndex','OutputPathId','PlotTag'),
          by = c('scenarioIndex','OutputPathId'))


  return(plotDataList)
}


#' creates a caption text
#'
#' @param dtPlotId data.table with one row per plotID
#' @param yScale scale of Y axis
#' @param timeRangeTag
#' @param PlotCaptionAddon additional text for plot
#'
#' @return  `character` with caption text
#' @export
#'
getCaptionTimeProfile = function(dtPlotId,
                                 yScale,
                                 timeRangeTag,
                                 PlotCaptionAddon){


  .pasteTags = function(dtPlotId,captionColumn){

    if (dplyr::n_distinct(dtPlotId[[captionColumn]]) == 1) {

      v_txt <- unique(dtPlotId[[captionColumn]])

    } else {

      v <- dtPlotId[, .(tags = paste0(get(captionColumn),
                                     ' (', paste(unique(PlotTag), collapse = ', '), ')')),
                   by = captionColumn]$tags

      allTags <- dtPlotId[, .(tags = paste0(' \\(', paste(unique(PlotTag), collapse = ', '), '\\)'))]$tags

      v <- gsub(allTags,'',v)

      v_txt =
        paste(c(paste(v[seq(1, length(v) - 1)],
                      collapse = ', '),
                tail(v, 1)),
              collapse = ' and ')
    }

    return(v_txt)
  }


  # generate captiontext
  captiontext = paste('Concentration-time profiles of',
                      .pasteTags(dtPlotId,captionColumn = 'outputDisplayName'),
                      'for',
                      .pasteTags(dtPlotId,captionColumn = 'ScenarioCaptionName'),
                      'on a',ifelse(yScale == 'linear','linear','logarithmic'),
                      'y-scale.',
                      ifelse (!is.na(PlotCaptionAddon),
                              PlotCaptionAddon,''),
                      timeRangeTag
  )

  return(captiontext)

}

#' constructs footnote lines for aggregated data and data references
#'
#' @template projectConfig
#' @template observedData
#'
#' @return vector of characters, each entry is one footnote line
#' @export
getFootNoteLines = function(dataObserved,
                            projectConfiguration){
  footnoteLines = c()
  if (any(!is.na(dataObserved$yErrorType))) {
    if (length(unique(dataObserved[!is.na(yErrorType)]$yErrorType)) == 1){
      stop('not implemented yet')
    }
  }


  dtDataReference <- dataObserved %>%
    dplyr::select('StudyId','group') %>%
    unique() %>%
    merge(getDataGroups(projectConfiguration = projectConfiguration),
          by = c('StudyId','group'))

  dtDataReference <- dtDataReference[!is.na(Reference)]

  if (nrow(dtDataReference) > 0) {
    footnoteLines = c(footnoteLines,
                      paste0(
                        'Data source: [',
                        paste(dtDataReference$Reference %>%
                                unique(),
                              collapse = ', '),
                        ']   '
                      ))
  }
  return(footnoteLines)
}

#' setfactors to plot
#'
#' @param plotData `data.table`  with all data
#' @param identifierColumns identifier columns which define sort order
#' @template projectConfig
#'
#' @return
setOrderAndFactors <- function(projectConfiguration,
                               plotData,
                               identifierColumns){

  dtOutputPaths <- getOutputPathIds(projectConfiguration)

  plotData$OutputPathId <- factor(plotData$OutputPathId,
                                  levels = levels(dtOutputPaths$OutputPathId),
                                  ordered = TRUE)

  dtDataGroups <- getDataGroups(projectConfiguration)

  plotData$group <- factor(plotData$group,
                           levels = levels(dtDataGroups$group),
                           ordered = TRUE)

  plotData %>% data.table::setorderv(intersect(
    unique('dataType',identifierColumns),
    names(plotData)))

  return(plotData)

}

#' add colorIndex column
#'
#' @template projectConfig
#' @template plotDataList
#'
#' @return list with entries
#'
#'
#' @export
setColorIndex <- function(projectConfiguration,
                          plotDataList){

  countGroups <-
    plotDataList$plotData[, .(
      nOutput = dplyr::n_distinct(OutputPathId),
      nGroup = dplyr::n_distinct(group, na.rm = TRUE)
    ), by = 'PlotTag'][, .(nOutput = max(nOutput), nGroup = max(nGroup))]

  # One data group more the one output
  if (countGroups$nOutput >= 1 & countGroups$nGroup == 1){

    dtOutputPaths = getOutputPathIds(projectConfiguration)
    plotDataList$plotData[,colorIndex := OutputPathId]
    plotDataList$plotData$colorIndex = factor(plotDataList$plotData$colorIndex,
                                 levels = dtOutputPaths$OutputPathId,
                                 labels = dtOutputPaths$DisplayName,
                                 ordered = TRUE)

    scaleVectors <- generateColorScaleVectors(
      dt = dtOutputPaths[DisplayName %in% levels(plotDataList$plotData$colorIndex)],
      index = 'DisplayName')

  }

  plotDataList[[scaleVectors]] <- scaleVectors

  return(plotDataList)

}


# Validation ----------------


#' Validation of config table fo time profiles plots
#'
#' @template projectConfig
#' @param configTable plot Configuration table
#' @template observedDataDT
#'
#' @export
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
        cols = c("DataGroupIds"),
        allowedValues = unique(dataObserved$group)
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
      "yScale",
      "ylimit_linear",
      "ylimit_log",
      "PlotCaptionAddon",
      "FacetType",
      "FacetScale",
      'Plot_TimeProfiles',
      'Plot_PredictedVsObserved',
      'Plot_ResidualsAsHistogram',
      'Plot_ResidualsVsTime',
      'Plot_ResidualsVsObserved'

    ),
    dtOutputPaths = dtOutputPaths
  )

  validatePlotInput(configTablePlots = configTablePlots,
                    plotInputColumns = c('PlotInputs_TP'))

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


#' Validates additional Input columns
#'
#' @template configTablePlots
#' @param plotInputColumns vector with names of additional inputs
#'
validatePlotInput <- function(configTablePlots,plotInputColumns){

  for (col in plotInputColumns){
    tryCatch(
      {

        plotInputs <- unique(configTablePlots[!is.na(getcol)][[col]])
        if (length(plotInputs) > 0)
          invisible(lapply(plotInputs, function(x){
            checkmate::assertList(eval(parse(text = paste('list(',x,')'))),names = 'named')})
          )
      },
      error = function(err) {
        stop(paste('invalid inputs in column',col))
      }
    )
  }

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
    Plot_PredictedVsObserved = FALSE,
    Plot_ResidualsAsHistogram = FALSE,
    Plot_ResidualsVsTime = FALSE,
    Plot_ResidualsVsObserved = FALSE
  )

  dtNewConfig <- dtNewConfig %>%
    merge(dtDataGroups %>%
            dplyr::select(c('group','DefaultScenario')) %>%
            data.table::setnames(old = c('group','DefaultScenario'),
                                 new = c('DataGroupIds','Scenario')),
          by = 'Scenario',
          all.x = TRUE,sort = FALSE)


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
