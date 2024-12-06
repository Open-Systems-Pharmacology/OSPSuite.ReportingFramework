#' Plot Sensitivity Function
#'
#' This function generates sensitivity plots based on the provided project configuration and scenario list.
#'
#' @param projectConfiguration A list containing project configuration settings, including file paths and parameters.
#' @param subfolder A string specifying the subfolder where the plots will be saved.
#' @param configTableSheet A string indicating the name of the configuration table sheet to be read.
#' @param scenarioList A list of scenarios for which the sensitivity analysis will be performed.
#'
#' @return An RmdContainer object containing the generated sensitivity plots.
#' @export
plotSensitivity <- function(projectConfiguration,
                            subfolder,
                            configTableSheet,
                            scenarioList){


# read configuration tables
configTable <-
  readSensitivityConfigTable(
    sheetName = configTableSheet,
    projectConfiguration = projectConfiguration
  )


# call plot function per plotName
rmdContainer <- generateRmdContainer(
  projectConfiguration = projectConfiguration,
  subfolder = subfolder,
  configTable = configTable,
  plotFunction =
    function(onePlotConfig, rmdContainer, ...) {
      createSensitivityPlotForPlotName(
        onePlotConfig = onePlotConfig,
        rmdContainer = rmdContainer,
        projectConfiguration = projectConfiguration,
        scenarioList = scenarioList)
    }
)


return(rmdContainer)

}

#' Read Sensitivity Configuration Table
#'
#' Reads the sensitivity configuration table from the specified sheet and validates its contents.
#'
#' @param sheetName A string indicating the name of the sheet to read from the project configuration file.
#' @param projectConfiguration A list containing project configuration settings, including file paths and parameters.
#'
#' @return A validated data frame containing the configuration table for sensitivity plots.
#' @keywords internal
readSensitivityConfigTable <- function(sheetName,projectConfiguration){

  # read configuration tables
  configTable <- xlsxReadData(
    wb = projectConfiguration$plotsFile,
    sheetName = sheetName,
    skipDescriptionRow = TRUE
  )

  configTablePlots <- validateHeaders(configTable)

  validateOutputIdsForPlot()
  validateDataGroupIdsForPlot()

  validateConfigTablePlots(
    configTablePlots = configTablePlots,
    charactersWithoutMissing = c(
      "plotName",
      "scenario",
      "scenarioLongName",
      "outputPathIds",
      "pKParameters",
      "sensitivityParameterSheet"
    ),
    charactersWithMissing = NULL,
    numericColumns = c(
      "threshold"),
    logicalColumns = NULL,
    numericRangeColumns = NULL,
    subsetList = list(
      scenario = list(
        cols = c("scenario"),
        allowedValues = configEnv$scenarios$scenarioName
      ),
      outputPathIds = list(
        cols = c("outputPathIds"),
        allowedValues = as.character(unique(configEnv$outputPaths$outputPathId))
      )
    )
  )


  validatePanelConsistency(
    configTablePlots = configTablePlots,
    panelColumns = c(
      "sensitivityParameterSheet",
      "threshold"
    )
  )

  checkmate::assertFileExists( x = file.path(projectConfiguration$outputFolder,EXPORTDIR$sensitivityResults,
  unique(sensitivityAnalyisName(configTablePlots$scenario,configTablePlots$sensitivityParameterSheet))),
  .var.name = 'sensitivityResult file')

  return(configTable)

}


#' Create Sensitivity Plot for a Given Plot Name
#'
#' This function creates a sensitivity plot based on the provided plot configuration and adds it to the Rmd container.
#'
#' @param onePlotConfig A data frame containing the configuration for a single plot.
#' @param rmdContainer An RmdContainer object to which the plot will be added.
#' @param projectConfiguration A list containing project configuration settings.
#' @param scenarioList A list of scenarios for which the sensitivity analysis will be performed.
#'
#' @return The updated RmdContainer object with the added plot.
#'
#' @keywords internal
createSensitivityPlotForPlotName <- function(onePlotConfig,
                                             rmdContainer,
                                             projectConfiguration,
                                             scenarioList){

  plotData <- prepareSensitivityPlotData(onePlotConfig,
                                         projectConfiguration,
                                         scenarioList)

  plotObject <- ggplot(plotData) +
    geom_col(aes(x = parameter, y = sens),
             fill = 'grey',
             position = "dodge") +
    labs(y = 'Sensitivity',
         x = '') +
    scale_x_discrete(limits = rev(unique(plotData$parameter))) +
    theme(legend.position = 'top',
          legend.direction = 'vertical',
          legend.title = element_blank()) +
    scale_fill_grey(start = 0.7, end = 0) +
    coord_flip() +
    layerWatermark()

  plotObject <- addFacets(plotObject,
                          'fixed',
                          facetAspectRatio = length(levels(plotData$parameter))/length(levels(plotData$plotTag))/5,
                          nFacetColumns = length(levels(plotData$plotTag)))


  rmdContainer$addAndExportFigure(
    plotObject = plotObject,
    caption = getCaptionForSensitivityPlot(plotData = plotData,
                                           projectConfiguration = projectConfiguration,
                                           plotCaptionAddon = onePlotConfig$onePlotConfig[1] ),
    figureKey = onePlotConfig$plotName[1],
    width = 20
  )

  return(rmdContainer)
}

#' Prepare Sensitivity Plot Data
#'
#' Prepares the data required for creating a sensitivity plot based on the provided configuration.
#'
#' @param onePlotConfig A data frame containing the configuration for a single plot.
#' @param projectConfiguration A list containing project configuration settings.
#' @param scenarioList A list of scenarios for which the sensitivity analysis will be performed.
#'
#' @return A data table containing the prepared data for the sensitivity plot.
#'
#' @keywords internal
prepareSensitivityPlotData <- function(onePlotConfig,
                                       projectConfiguration,
                                       scenarioList){

  pkDefinitions <- getPKParameterOverview(projectConfiguration)

  plotData <- list()

  onePlotConfig <- onePlotConfig %>%
    separateAndTrim( 'pKParameters') %>%
    separateAndTrim('outputPathIds') %>%
    merge(pkDefinitions[,c('scenarioName','pKParameter','displayNamePKParameter','displayUnitPKParameter')] %>%
            data.table::setnames(old = c('scenarioName'),
                                 new = c('scenario')),
          by = c('scenario','pKParameter'))

  configList <- split(onePlotConfig,by = 'scenario')

  for (scenarioName in names(configList)){

    configLine <- configList[[scenarioName]] %>%
      data.table::setDT() %>%
      merge(dtOutputPaths,by = 'outputPathId')

    sensitivityResults <- ospsuite::importSensitivityAnalysisResultsFromCSV(
      simulation = scenarioList[[scenarioName]]$simulation,
      filePaths = file.path(
        projectConfiguration$outputFolder,
        EXPORTDIR$sensitivityResults,
        sensitivityAnalyisName(scenarioName,configLine$sensitivityParameterSheet[1])
      ))


    plotData <- append(plotData,
                       lapply(split(configLine,seq(1,nrow(configLine))), function(cL){
                         data.table(
                           sens = lapply(
                             sensitivityResults$allPKParameterSensitivitiesFor(
                               pkParameterName = cL$pKParameter,
                               outputPath = cL$outputPath,
                               totalSensitivityThreshold = cL$threshold),getElement,'value') %>%
                             unlist(),
                           parameter = lapply(
                             sensitivityResults$allPKParameterSensitivitiesFor(
                               pkParameterName = cL$pKParameter,
                               outputPath = cL$outputPath,
                               totalSensitivityThreshold = cL$threshold),getElement,'parameterName') %>%
                             unlist(),
                           outputPathId = cL$outputPathId,
                           pKParameter = cL$pkDisplayName,
                           scenarioLongName = cL$scenarioLongName)
                       }))


  }

  plotData <- data.table::rbindlist(plotData)

  plotData$outputPathId = factor(plotData$outputPathId, levels = unique(plotData$outputPathId), ordered = TRUE)
  plotData$pKParameter = factor(plotData$pKParameter, levels = rev(unique(plotData$pKParameter)), ordered = TRUE)
  plotData[,plotTag := generatePlotTag((as.numeric(outputPathId)-1)*length(levels(plotData$pKParameter))+
                                         as.numeric(pKParameter))]
  plotData$plotTag = factor(plotData$plotTag, ordered = TRUE,levels = sort(unique(plotData$plotTag)))

  plotData <- plotData[order(-abs(sens))]
  plotData$parameter = factor(plotData$parameter, levels = rev(unique(plotData$parameter)), ordered = TRUE)

  return(plotData)

}

#' Get Caption for Sensitivity Plot
#'
#' Generates a caption for the sensitivity plot based on the plot data and project configuration.
#'
#' @param plotData A data table containing the data for the sensitivity plot.
#' @param projectConfiguration A list containing project configuration settings.
#' @param plotCaptionAddon An optional string to be added to the caption.
#'
#' @return A string containing the generated caption for the sensitivity plot.

#' @keywords internal
getCaptionForSensitivityPlot <- function(plotData,projectConfiguration,plotCaptionAddon){

  dtCaption <- plotData %>%
    dplyr::select(c('plotTag','outputPathId','pKParameter','scenarioLongName')) %>%
    unique() %>%
    merge(configEnv$outputPaths[,c('outputPathId','displayNameOutputs')],
          by = 'outputPathId')


  captiontext <- paste(
    "Sensitivity of",
    pasteFigureTags(dtCaption, captionColumn = "pKParameter"),
    "for",
    pasteFigureTags(dtCaption, captionColumn = "displayNameOutputs"),
    "for",
    pasteFigureTags(dtCaption, captionColumn = "scenarioLongName"),
    ".",
    ifelse(!is.na(plotCaptionAddon[1]),
           plotCaptionAddon[1],
           '')
  )
  return(captiontext)


}

#' Get PK Parameter Overview
#'
#' This function retrieves and compiles pharmacokinetic (PK) parameter data from specified scenario definitions
#' and a PK parameter Excel file. It processes the data by reading the relevant sheets and merging them with
#' scenario information.
#'
#' @param projectConfiguration An object of class `ProjectConfiguration` containing paths to Excel files.
#'
#' @return A `data.table` containing the merged PK parameter data along with associated scenario names.
#' @export
getPKParameterOverview <- function(projectConfiguration) {
  pkParameterSheets <- unique(splitInputs(configEnv$scenarios$pKParameter))

  pkSheets <- stats::setNames(
    lapply(pkParameterSheets, function(sheet) {
      xlsxReadData(
        wb = projectConfiguration$addOns$pKParameterFile,
        sheetName = sheet,
        skipDescriptionRow = TRUE
      )
    }),
    pkParameterSheets
  ) %>%
    rbindlist(idcol = "pKParameter") %>%
    .[, descriptions := NULL] %>%
    merge(configEnv$scenarios[, c("scenarioName", "pKParameter")],
          by = "pKParameter", allow.cartesian = TRUE
    ) %>%
    .[, pKParameter := NULL] %>%
    setnames(old = "name", new = "pKParameter")

  return(pkSheets)
}
