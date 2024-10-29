plotPKBoxPlot <- function(projectConfiguration,
                        subfolder,
                        configTableSheet,
                        pkParameterDT,
                        customFunction = NULL,
                        asRatio = FALSE) {

  checkmate::assertClass(projectConfiguration, classes = 'ProjectConfiguration')
  checkmate::assertString(subfolder)
  checkmate::assertDataTable(pkParameterDT)
  checkmate::assertNames(names(pkParameterDT), must.include = c("scenarioName", "parameter", "individualId", "value", "outputPathId", "displayName", "displayUnit"))
  checkmate::assertFlag(asRatio)

  # Read configuration tables
  configTable <- readPKRatioConfigTable(
    sheetName = configTableSheet,
    projectConfiguration = projectConfiguration,
    pkParameterDT = pkParameterDT
  )

  rmdContainer <- generateRmdContainer(
    projectConfiguration,
    subfolder,
    configTable,
    function(onePlotConfig, rmdContainer, ...) {
      createPKBoxPlotForPlotName(onePlotConfig = onePlotConfig,
                                  rmdContainer = rmdContainer,
                                  pkParameterDT = pkParameterDT)
    }
  )

  return(rmdContainer)
}


createPKBoxPlotForPlotName <- function(onePlotConfig,
                           rmdContainer,
                           pkParameterDT){


  for (outputPathIdLoop in unique(pkParameterDT$outputPathId)) {


    pkParameterDTForOutput = pkParameterDT[outputPathIdLoop == outputPathId]


    plotObject <- ospsuite_plotForest(plotData,
                                      vlineIntercept = vlineIntercept,
                                      digitsToRound = digitsToRound,
                                      digitsToShow = digitsToShow,
                                      tableLabels = c('Ratio',
                                                      paste0(confLevel * 100, '%\nCI lower'),
                                                      paste0(confLevel * 100, '%\nCI upper')),
                                      xlabel = xlabel)

    # Add color and shape scales
    plotObject <- plotObject +
      scale_color_manual(values = unlist(lapply(scaleVactors, getElement, 'color'))) +
      scale_fill_manual(values = unlist(lapply(scaleVactors, getElement, 'fill'))) +
      scale_shape_manual(values = unlist(lapply(scaleVactors, getElement, 'shape')))

    # Export
    rmdContainer$addAndExportFigure(
      plotObject = plotObject,
      caption = getCaptionForDDIRatioPlot(outputPathIdLoop = outputPathIdLoop,
                                          plotData = plotData,
                                          pkParameterDT = pkParameterDT),
      figureKey = paste(onePlotConfig$plotName[1], outputPathIdLoop, sep = "-"),
      width = 30
    )
  }

}


prepareDataForPKBoxplot <- function(onePlotConfig){


  plotData <- data.table::copy(onePlotConfig) %>%
    tidyr::separate_rows(pkParameter, sep = ",") %>%
    data.table::setDT() %>%
    .[,pkParameter := trimws(pkParameter)] %>%
    dplyr::select(!c('level', 'header','plotName','dataGroupId')) %>%
    merge(pkParameterDTForOutput %>%
            unique() %>%
            data.table::setnames(old = c('parameter','displayName','scenarioName'),
                                 new = c('pkParameter','parameterDisplayName','scenario')),
          by = c('scenario','pkParameter')
    )



# Ensure order by creating factors
plotData$displayName <- factor(plotData$displayName,
                               levels = unique(onePlotConfig$displayName),
                               ordered = TRUE)

plotData$type <- factor(plotData$type,
                        levels = unique(plotData$type), ordered = TRUE)

plotData$displayGroup <- factor(plotData$displayGroup,
                                levels = unique(plotData$displayGroup), ordered = TRUE)

return(plotData)
}



# auxiliaries used in differen PK Plots  ------------

#' Read PK Ratio Configuration Table
#'
#' Reads and validates the PK ratio configuration table from the specified sheet.
#'
#' @param projectConfiguration A ProjectConfiguration object.
#' @param sheetName Name of the sheet to read from.
#' @param pkParameterDT A data.table with PK parameters.
#'
#' @return A validated configuration table as a data.table.
#'
#' @keywords internal
readPKConfigTable <- function(projectConfiguration, sheetName, pkParameterDT) {
  # Initialize variable used in data.tables
  level <- NULL

  # Read configuration tables
  configTable <- xlsxReadData(
    wb = projectConfiguration$plotsFile,
    sheetName = sheetName,
    skipDescriptionRow = TRUE
  )

  configTablePlots <- validateHeaders(configTable)

  validateConfigTablePlots(
    configTablePlots = configTablePlots,
    charactersWithoutMissing = c("displayName"),
    charactersWithMissing = c("displayGroup",'dataGroupId'),
    logicalColumns = NULL,
    numericRangeColumns = NULL,
    subsetList = list(
      scenario = list(
        cols = c("scenario", "referenceScenario"),
        allowedValues = unique(pkParameterDT$scenarioName)
      ),
      pkParameter = list(
        cols = c("pkParameter"),
        allowedValues = unique(pkParameterDT$parameter)
      )
    )
  )

  return(configTable)
}



