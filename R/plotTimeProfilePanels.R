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
  configTable <- readTimeprofileConfigTable(sheetName = configTableSheet)

  # initialize Container for RMD generation for .Rmd generation
  rmdContainer <-
    RmdContainer$new(
      rmdfolder = file.path(projectConfiguration$outputFolder),
      subfolder = subfolder
    )

  iRow = 1
  levelLines = which(!is.na(configTable$Level))
  while (iRow <= nrow(configTable)){
    if (!is.na(configTable$Level[iRow])){
      # add  section headers
      rmdContainer$addHeader(configTable$Header[iRow],
                             level = configTable$Level[iRow])
      iRow = iRow +1
    } else{
      # execute plot section
      iEndX = utils::head(which(levelLines>iRow),1)
      if (length(iEndX) ==0 ) {
        iEnd <- nrow(configTable)
      }else{
        iEnd <- levelLines[iEndX]-1
      }

      for (onePlotConfig in split(configTable[seq(iRow,iEnd)], by = "PlotName")) {
        rmdContainer <-
          createPanelPlotsForPlotName(projectConfiguration = projectConfiguration,
                                    onePlotConfig = onePlotConfig,
                                    dataObserved = dataObserved,
                                    rmdContainer = rmdContainer,
                                    nFacetColumns = nFacetColumns,
                                    facetAspectRatio = facetAspectRatio,
                                    timeRangeTable = timeRangeTable
          )
      }

      iRow <- iEnd + 1

    }

  }

  return(rmdContainer)
}


#' function to generate one plot as facet panels
#'
#' @template projectConfig
#' @template onePlotConfig
#' @template observedDataDT
#' @param rmdContainer object of class `RmdContainer`
#' @param `data.table` with Time Range configurations
#'
#' @return object of class `rmdContainer` with added figures
#' @export
#'
createPanelPlotsForPlotName <- function(projectConfiguration,
                                      onePlotConfig,
                                      dataObserved,
                                      nFacetColumns,
                                      rmdContainer,
                                      facetAspectRatio,
                                      timeRangeTable) {
  plotData <- PlotDataTimeProfile$new(projectConfiguration = projectConfiguration,
                                      onePlotConfig = onePlotConfig)
  # read simulated data and filter observed
  plotData$collectData(projectConfiguration = projectConfiguration,
                           dataObserved = dataObserved)

  # replicate data for each TimeRange Tag
  plotData$addTimeRangeTags()

  # make sure everything will be plotted in correct order
  plotData$setOrderAndFactors(
    identifierColumns = getColumnsForColumnType(dt = dataObserved, 'identifier'))

  # get table with caption information per plot Id
  plotData$prepareCaptionDetails(nFacetColumns)

  # setColorIndex
  plotData$prepareLegendDetails()


  if (as.logical(plotData$configTable$Plot_TimeProfiles[1]))
    rmdContainer <- plotTPPanel(plotData,
                                rmdContainer,
                                facetAspectRatio = facetAspectRatio)


  return(rmdContainer)

}




#' create the TimeProfile panels
#' @inheritParams createPanelPlotsForPlotName
#' @param plotData object of class PlotDataTimeprofile which contains data and informations neede for plotting
#'
#' @return RmdContainer object with added lines
#' @export
plotTPPanel <- function(plotData,
                            rmdContainer,
                            facetAspectRatio){


  if(any(plotData$data$dataType == 'observed')){
    mapSimulatedAndObserved = data.table(
      simulated = as.character(plotData$data[dataType == 'simulated']$colorIndex) %>% unique(),
      observed = as.character(plotData$data[dataType == 'observed']$colorIndex) %>% unique(),
      color =  plotData$scaleVectors$color,
      fill =  plotData$scaleVectors$fill)
  } else{
    mapSimulatedAndObserved = NULL
  }


  mapping = eval(parse(text = paste(
    'aes(groupby =', interaction(intersect(
      names(plotData$data), c('colorIndex', 'shapeIndex')
    )), ')'
  )))
  groupAesthetics = intersect(c('colour','fill','shape'),
                              ggplot2::standardise_aes_names(names(plotData$scaleVectors)))


  for (timeRangeFilter in names(plotData$timeRangeTagFilter)){

    for (yScale in splitInputs(plotData$configTable$yScale[1])){

      plotObject <- do.call(what = ospsuite_plotTimeProfile,
                            args = utils::modifyList(
                              list(
                                plotData = plotData$getDataForTimeRange(timeRangeFilter),
                                yscale = yScale,
                                mapping = mapping,
                                groupAesthetics = groupAesthetics,
                                geomLLOQAttributes = list(linetype = "dashed"),
                                mapSimulatedAndObserved = mapSimulatedAndObserved
                              ),
                              eval(parse(
                                text = paste('list(', plotData$configTable$PlotInputs_TP[1], ')')
                              ))
                            ))

      # add Facet Columns
      plotObject <- addFacets(plotObject = plotObject,
                              plotData =  plotData,
                              facetAspectRatio = facetAspectRatio)

      # adjust labes
      plotObject$labels = utils::modifyList(plotObject$labels,
                                            plotData$tpLabels)

      plotObject <- plotObject +
        labs(x = plotData$getTimeLabelForTimeRange(timeRangeFilter))

      # export
      rmdContainer$addAndExportFigure(
        plotObject = plotObject,
        caption = plotData$getCaptionForTimeProfile(yScale = yScale,
                                                    filterName = timeRangeFilter),
        footNoteLines = plotData$getFootNoteLines(),
        figureKey = paste(plotData$configTable$PlotName[1],
                          'TP',
                          ifelse(yScale == 'log','Log','Linear'),
                          timeRangeFilter,
                          sep = '-')
      )


    }
  }

  return(rmdContainer)
}



#' addFacetColum
#'
#' @param plotObject ggplot object to which the facet should be added
#' @inheritParams createPanePlotsForPlotName
#' @param nFacetColumns maximal number of facet used for facettype byOrder
#'
#' @return ggplot Object with facets
#' @export
addFacets <- function(plotObject,
                      plotData,
                      facetAspectRatio = 0.5) {

  if (!is.null(plotData$nFacetColumns)){


    plotObject <- plotObject +
      facet_wrap(facets = vars(PlotTag),
                 scales = plotData$configTable$FacetScale[1],
                 ncol = plotData$nFacetColumns) +
      theme(aspect.ratio = facetAspectRatio)


  }

  return(plotObject)
}


#' Read the configtable for the timeprofile
#'
#' @param sheetName
#'
#' @return `data.table` with plot configurations
#' @export
readTimeprofileConfigTable <- function(sheetName){
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

  for (col in plotInputColumns){
    configTable[is.na(get(col)) ,(col):='']
  }

  return(configTable)

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
    if (dplyr::n_distinct(dtOutputPaths[outputPathId %in% outputs]$DisplayUnit) > 2) {
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
        plotInputs <- unique(configTablePlots[!is.na(get(col))][[col]])
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

  if(any(length(stringr::str_extract_all(configTablePlots$outputPathId, "\\([^)]*$")) > 0) ||
     any(length(stringr::str_extract_all(configTablePlots$outputPathId, "(?<!\\()\\([^)]+")) > 0))
    stop('Please check the brackets in column outputPathId')

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
    Plot_ResidualsVsObserved = FALSE
  )

  dtNewConfig <- dtNewConfig %>%
    merge(dtDataGroups %>%
            dplyr::select(c('Group','DefaultScenario')) %>%
            data.table::setnames(old = c('Group','DefaultScenario'),
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
