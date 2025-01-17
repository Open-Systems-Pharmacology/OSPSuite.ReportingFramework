plotPKBoxwhisker <- function(projectConfiguration,
                        subfolder,
                        configTableSheet,
                        pkParameterDT,

                        customFunction = NULL,
                        asRatio = FALSE) {

  checkmate::assertClass(projectConfiguration, classes = 'ProjectConfiguration')
  checkmate::assertString(subfolder)
  checkmate::assertDataTable(pkParameterDT)
  checkmate::assertNames(names(pkParameterDT), must.include = c("scenarioName", "parameter", "individualId", "value", "outputPathId", "displayNamePKParameter", "displayUnitPKParameter"))
  checkmate::assertFlag(asRatio)
  # Read configuration tables
  configTable <- readPKBoxwhiskerConfigTable(
    sheetName = configTableSheet,
    projectConfiguration = projectConfiguration,
    pkParameterDT = pkParameterDT
  )

  rmdPlotManager <- generateRmdContainer(
    projectConfiguration,
    subfolder,
    configTable,
    function(onePlotConfig, rmdPlotManager, ...) {
      createPKBoxPlotForPlotName(onePlotConfig = onePlotConfig,
                                  rmdPlotManager = rmdPlotManager,
                                  pkParameterDT = pkParameterDT)
    }
  )

  return(rmdPlotManager)
}


createPKBoxPlotForPlotName <- function(onePlotConfig,
                           rmdPlotManager,
                           pkParameterDT,
                           xAxisTextAngle,
                           facetAspectRatio){

  plotData <- prepareDataForPKBoxplot(onePlotConfig,pkParameterDT)

  if(dplyr::n_distinct(plotData$plotTag)>1) {
    nFacetColumns <- 1
  }else {
    nFacetColumns <- NULL
  }


  for (plotNameLoop in unique(plotData$plotName)){

    plotDataPk <- plotData[plotName == plotNameLoop]

    ylabelUnit <- plotDataPk$displayUnit[1]
    if(ylabelUnit != '') ylabelUnit <- paste0('[',ylabelUnit,']')

    for (yScale in splitInputs(onePlotConfig$yScale[1])){

      plotObject <-
        ospsuite.plots::plotBoxWhisker(data =   plotData,
                                       mapping = aes(x = scenarioLongName,
                                                     y = value,
                                                     fill = colorIndex),
                                       yscale = yScale,
                                       percentiles = percentiles,
                                       statFun = statFunDefault(percentiles),
                                       outliers = outliers) +
        labs(x = '',
             y = paste(plotData$parameterDisplayName[1],ylabelUnit)) +
        theme(legend.position =  'none')

      plotObject <-
        addFacets(plotObject = plotObject,
                  facetScale = onePlotConfig$facetScale[1],
                  facetAspectRatio = facetAspectRatio,
                  nFacetColumns = nFacetColumns)

      if (xAxisTextAngle > 0)
        plotObject <- plotObject +
        theme(axis.text.x = element_text(angle = 45,hjust = 1))

      rmdPlotManager$addAndExportFigure(
        plotObject = plotObject,
        caption = getCaptionForBoxwhiskerPlot(plotDataPk = plotDataPk,
                                              yScale = yScale,
                                              percentiles = percentiles,
                                              plotCaptionAddon = onePlotConfig$plotCaptionAddon[1]),
        figureKey = plotNameLoop
      )
    }

    dtExport <-  plotDataPk %>%
      data.table::setDT() %>%
      .[, as.list(plotObject$statFun(value)),
        by = c("displayNameOutput","scenarioLongName")
      ] %>%
      setnames( old =  c("ymin", "lower", "middle", "upper", "ymax"),
                new = paste(percentiles))




  }

}


prepareDataForPKBoxplot <- function(onePlotConfig,pkParameterDT){


  plotData <- data.table::copy(onePlotConfig)

  plotData <- separateAndTrim(plotData, "outputPathIds")
  plotData <- separateAndTrim(plotData, "pkParameters")

  plotData <- plotData %>%
    dplyr::select(!c('level', 'header')) %>%
    merge(pkParameterDT %>%
            unique() %>%
            data.table::setnames(old = c('parameter', 'scenarioName'),
                                 new = c('pkParameter','scenario')),
          by = c('scenario', 'pkParameter', 'outputPathId')
    )

  if (nrow(plotData) > 0){

    if (dplyr::n_distinct(plotData$pkParameter) > 1){
      plotData[,plotName := paste(plotName,pkParameter,sep = '_')]
    }

    plotData[,isReference := scenario %in% referenceScenario, by = c('plotName')]
    plotData[,colorIndex := ifelse(isReference == TRUE,'reference','default')]

    # Ensure order by creating factors
    plotData$displayNameOutput <- factor(plotData$displayNameOutput,
                                         levels = unique(plotData$displayNameOutput),
                                         ordered = TRUE)

    plotData$scenarioLongName <- factor(plotData$scenarioLongName,
                                           levels = unique(onePlotConfig$scenarioLongName),
                                           ordered = TRUE)
    # add Tag for faceting
    plotData[,plotTag := toupper(letters[as.numeric(displayNameOutput)])]
  }

return(plotData)
}

statFunDefault <- function(percentiles){

  statFun <- function(y) {
    y = y[!is.na(y)]
    rQuantiles <- stats::quantile(y, probs = percentiles, names = FALSE, na.rm = TRUE)
    names(rQuantiles) <- c("ymin", "lower", "middle", "upper", "ymax")


    r <- c(N = length(y),
           rQuantiles,
           "arith mean" = mean(y),
           "arith standard deviation" = sd(y),
           "arith CV" = sd(y)/mean(y),
           "geo mean"	= exp(mean(log(y))),
           "geo standard deviation"	= exp(sd(log(y))),
           "geo CV" = sqrt(exp((log(sd(y)))^2)-1)
)


    return(r)
  }
}

# auxiliaries  ------------

getCaptionForBoxwhiskerPlot <- function(plotDataPk,percentiles, yScale,plotCaptionAddon ) {

  # 1087
  # #For case1 it should be clear that the ratios presented are ratios of the summary statistics e.g.:
  # "Ratio of population summary statistics of cmax of midazolam plasma conc for midazolam treatment in comparison to midazolam control" (for the plot additionally: "shown as box-whisker plot, which indicates ratios of the 5th, 25th, 50th, 75th, 95th percentiles" as you suggested)
  #
  # ok
  #
  # For case 2 I would suggest:
  #   "Population summary statistics of cmax ratios of midazolam plasma conc for midazolam treatment in comparison to midazolam control"
  # Ist there no boxplot for case 2 or did you just leave out the caption? If there are no corresponding boxplots, yet, I would propose to add them to the report
  #
  # Suggest:
  #   "Cmax ratio population summary statistics of midazolam plasma conc for midazolam treatment in comparison to midazolam control"
  #

  dtCaption <-
    plotDataPk[, c(
      "scenarioLongName",
      'displayNameOutput',
      'plotTag',
      "plotCaptionAddon",
      "parameterDisplayName",
      "displayNameOutput"
    )] %>%  unique()

  captiontext <- paste("Population summary statistics of",
                       dtCaption$parameterDisplayName[1],
                       "of",
                       pasteFigureTags(dtCaption, captionColumn = "displayNameOutput"),
                       "for",
                       pasteFigureTags(dtCaption, captionColumn = "scenarioLongName"))

  if (!is.null(percentiles)){
    captiontext <- paste(captiontext,
                         "shown as box-whisker plot, which indicates the",
                         paste(formatPercentiles(percentiles,suffix = '',allAsPercentiles = TRUE),collapse = ', '),'percentiles',
                         "on a", ifelse(yScale == "linear", "linear", "logarithmic"),
                         "y-scale")
  }
  captiontext <- paste(paste0(captiontext,'.'),
                       ifelse(!is.na(plotCaptionAddon),
                              plotCaptionAddon,
                              ''))


  captiontext <- paste(
    plotTypeTxt,
    "for",
    pasteFigureTags(dtCaption, captionColumn = "displayNameOutputs"),
    "for",
    pasteFigureTags(dtCaption, captionColumn = "scenarioLongName"),
    individualtext,

  )
  return(captiontext)
}


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
readPKBoxwhiskerConfigTable <- function(projectConfiguration, sheetName, pkParameterDT) {
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
    charactersWithoutMissing = c("plotName","scenario","scenarioLongName",'pkParameters','outputPathIds'),
    charactersWithMissing = c("plotCaptionAddon"),
    logicalColumns = NULL,
    numericRangeColumns = c("ylimit_linear", "ylimit_log"),
    subsetList = list(
      scenario = list(
        cols = c("scenario", "referenceScenario"),
        allowedValues = unique(pkParameterDT$scenarioName)
      ),
      pkParameter = list(
        cols = c("pkParameters"),
        allowedValues = unique(pkParameterDT$parameter)
      ),
      outputPathId = list(
        cols = c("outputPathIds"),
        allowedValues = unique(pkParameterDT$outputPathId)
      ),
      yscale = list(
        cols = c("yscale"),
        allowedValues = c("linear", "log")
      ),
      facetScale = list(
        cols = c("facetScale"),
        allowedValues = c("fixed", "free", "free_x", "free_y")
      )
    )
  )

  validateGroupConsistency(
    dt = configTablePlots,
    valueColumns = c(
      'pkParameters',
      "yScale",
      "plotCaptionAddon",
      "ylimit_linear",
      "ylimit_log",
      "facetScale"
    ))


  return(configTable)
}

# support usability --------------------
addDefaultConfigForPKBoxwhsikerPlots <- function(projectConfiguration,
                                                 pkParameterDT,
                                                sheetName = "PKParameter_Boxplot",
                                                overwrite = FALSE) {

  # avoid warnings for global variables during check
  scenarioName <- NULL

  # this function stops in valid runs
  stopHelperFunction()
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  scenarios <- getScenarioDefinitions(projectConfiguration$scenariosFile)

  if (sheetName %in% wb$sheet_names & !overwrite) {
    dtNewHeader <- xlsxReadData(wb, sheetName = sheetName, skipDescriptionRow = TRUE)
    scenarios <- scenarios[!(scenarioName %in% unique(dtNewHeader$scenario))]
  } else {
    dtNewHeader <- data.table(
      level = 1,
      header = "PK-parameter"
    )
  }

  # Create a unique combination of parameters and outputPathId
  dt <- pkParameterDT[, .(pkParameters = paste(unique(parameter), collapse = ', ')), by = outputPathId] %>%
    .[, .(outputPathIds = paste(unique(outputPathId), collapse = ', ')), by = pkParameters]

  # Create a new data.table with all combinations of pkParameters and scenario names
  dtNewConfig <- dt[, .(scenario = scenarios$scenarioName,
                        yScale = 'linear, log',
                        facetScale = 'fixed'),
                    by = .(outputPathIds, pkParameters)]


  wb <- addDataAsTemplateToXlsx(
    wb = wb,
    templateSheet = "PKParameter_Boxplot",
    sheetName = sheetName,
    dtNewData = rbind(dtNewHeader,
                        dtNewConfig, # nolint indentation_linter
                        fill = TRUE
    )
  )

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

}
