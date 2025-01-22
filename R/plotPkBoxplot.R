#' Plot PK Box-and-Whisker
#'
#' Generates box-and-whisker plots for pharmacokinetic (PK) parameters based on
#' a provided project configuration and plot configuration. This function creates
#' visual representations of the distribution of PK parameters across different
#' scenarios, allowing for comparison of absolute values and ratios.
#'
#' The function supports customization options such as specifying the angle of the
#' x-axis text, color schemes, and facet aspect ratios.
#'
#' @param projectConfiguration A ProjectConfiguration object that contains
#'   settings and paths relevant to the project.
#' @param onePlotConfig A data.table containing configuration settings for a single plot,
#'   including details about which plots to generate and their formatting.
#' @param pkParameterDT A data.table containing PK parameter data.
#' @param percentiles A numeric vector specifying the percentiles to calculate
#'   for the box-and-whisker plots. Default is retrieved from project options.
#' @param xAxisTextAngle An integer specifying the angle (in degrees) for
#'   rotating the x-axis text labels. Default is 0 (no rotation).
#' @param colorVector A named vector specifying colors for the plots. It should
#'   include at least two names: `default` for general use and `reference` for
#'   reference scenarios. If not provided, default colors will be used.
#' @param facetAspectRatio A numeric value specifying the aspect ratio for
#'   faceting the plots. Default is 0.5, which may need adjustment based on the
#'   number of facets and plot dimensions.
#' @param nBootstrap An integer specifying the number of bootstrap samples to
#'   use when calculating ratios. Default is 1000.
#' @param seed An integer value to set the random seed for reproducibility of
#'   bootstrap sampling. Default is 12345.
#' @param ... Additional arguments passed to plotting functions for further
#'   customization.
#' @return A list of generated plots. Each plot is an object that can be rendered
#'   using ggplot2 or similar plotting systems. The list may include both absolute
#'   and ratio plots depending on the configuration.
#' @details
#' The function first validates the input parameters and sets the random seed for
#' reproducibility. It checks if the color vector is provided; if not, it generates
#' default colors. The function then iterates over the specified plot types (absolute
#' and ratio) and generates the corresponding plots based on the provided data and
#' configuration.
#'
#' When calculating ratios, the function distinguishes between two cases:
#'
#' - **Case 1**: If the scenarios being compared are based on the **same populations**,
#'   (same `PopulationId` in scenario configuration) the plots will show summary statistics of individual ratios.
#'
#' - **Case 2**: If the scenarios are based on **different populations**, the plots
#'   will display the ratio of summary statistics, using either bootstrapping methods
#'   or analytical solutions for "geo mean","geo standard deviation","geo CV".
#'
#' This ensures that the analysis is valid and meaningful, based on the population
#' structure of the scenarios being compared.
#'
#' The resulting plots can be further customized or exported as needed. It is
#' recommended to review the generated plots to ensure they meet the desired
#' presentation standards.
#'
#' @examples
#' # Example usage of plotPKBoxwhisker
#' \dontrun{
#' plotList <- plotPKBoxwhisker(projectConfiguration = myProjectConfig,
#'                                onePlotConfig = myPlotConfig,
#'                                pkParameterDT = myPKData,
#'                                percentiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
#'                                xAxisTextAngle = 45,
#'                                colorVector = c(default = "blue", reference = "red"),
#'                                facetAspectRatio = 1,
#'                                nBootstrap = 1000,
#'                                seed = 42)
#' }
#' @export
plotPKBoxwhisker <- function(projectConfiguration,
                             onePlotConfig,
                        pkParameterDT,
                        percentiles = getOspsuite.plots.option(optionKey = OptionKeys$Percentiles),
                        xAxisTextAngle = 0,
                        colorVector = c(default = NA, reference = NA),
                        facetAspectRatio = 0.5,
                        nBootstrap = 1000,
                        seed = 12345,
                        ...){

  checkmate::assertClass(projectConfiguration, classes = 'ProjectConfiguration')
  checkmate::assertDataTable(pkParameterDT)
  checkmate::assertNames(names(pkParameterDT), must.include = c("scenarioName", "parameter", "individualId", "value", "outputPathId", "displayNamePKParameter", "displayUnitPKParameter"))
  checkmate::assertNumeric(xAxisTextAngle,any.missing = FALSE)
  checkmate::assertNumeric(facetAspectRatio,any.missing = FALSE)
  validateColorVector(colorVector)

  set.seed(seed)

  if (any(is.na(colorVector))){
    namesOfScaleVector = names(colorVector)
    colorVector <-  getDefaultColorsForScaleVector( n = length(namesOfScaleVector))
    names(colorVector) <- namesOfScaleVector
  }

  plotList <- list()
  for (plotType in c("Absolute", "Ratio")) {
    if (as.logical(onePlotConfig[[paste0('plot_',plotType)]][1])){
      plotList <- c(plotList,
                    generateBoxwhiskerPlotForPlotType(onePlotConfig = onePlotConfig,
                                            pkParameterDT = pkParameterDT,
                                            percentiles = percentiles,
                                            xAxisTextAngle =xAxisTextAngle,
                                            colorVector = colorVector,
                                            facetAspectRatio = facetAspectRatio,
                                            nBootstrap = nBootstrap,
                                            asRatio = plotType == 'Ratio',
                                            ...)
      )
    }
  }
  return(plotList)
}
#' Generate Box-and-Whisker Plot for a Specific Plot Type
#'
#' Creates box-and-whisker plots for either absolute values or ratios of PK parameters.
#'
#' @param onePlotConfig Configuration for a single plot.
#' @param pkParameterDT A data.table containing PK parameter data.
#' @param percentiles A vector of percentiles to calculate.
#' @param xAxisTextAngle Angle for x-axis text rotation.
#' @param colorVector A named vector for colors.
#' @param facetAspectRatio Aspect ratio for facets.
#' @param nBootstrap Number of bootstrap samples.
#' @param asRatio Logical indicating if the plot is for ratios.
#' @param ... Additional arguments passed to plotting functions.
#' @return A list of generated plots for the specified plot type.
#' @keywords internal
generateBoxwhiskerPlotForPlotType <- function(onePlotConfig,
                          pkParameterDT,
                          percentiles,
                          xAxisTextAngle,
                          colorVector,
                          facetAspectRatio,
                          nBootstrap,
                          asRatio,
                          ...){


  plotData <- prepareDataForPKBoxplot(onePlotConfig = onePlotConfig,
                                      pkParameterDT = pkParameterDT,
                                      colorVector = colorVector,
                                      asRatio = asRatio,
                                      nBootstrap = nBootstrap,
                                      percentiles = percentiles)

  if(dplyr::n_distinct(plotData$plotTag)>1) {
    nFacetColumns <- 1
  }else {
    nFacetColumns <- NULL
  }


  plotList <- list()

  # one plot per PK Parameter
  for (plotNameLoop in unique(plotData$plotName)){

    plotDataPk <- plotData[plotName == plotNameLoop]

    ylabelUnit <- plotDataPk$displayUnit[1]
    if(ylabelUnit != '') ylabelUnit <- paste0('[',ylabelUnit,']')

    for (yScale in splitInputs(onePlotConfig$yScale[1])){

      if (asRatio){
        if ('value' %in% names(plotDataPk)){
          ratioMode <- 'individualRatios'
        plotObject <-
          ospsuite.plots::plotBoxWhisker(data =   plotDataPk,
                                         mapping = aes(x = scenarioShortName,
                                                       y = value),
                                         yscale = yScale,
                                         yscale.args = getYlimits(onePlotConfig,yScale,...),
                                         ...) +
          labs(x = '',
               y = paste(plotDataPk$displayNamePKParameter[1],'ratio'))
        } else {
          ratioMode <- 'ratioOfPopulation'
          plotObject <- ospsuite.plots::initializePlot() +
            geom_boxplot(data = plotDataPk,
                         mapping = aes(x = scenarioShortName,lower = lower, upper = upper,
                                       middle = middle, ymin = ymin, ymax = ymax ),
                         stat="identity")
            plotObject <- ospsuite.plots::addYscale(plotObject = plotObject,
                                                    yscale = yScale,
                                      yscale.args = getYlimits(onePlotConfig,yScale,...)) +
              labs(x = '',
                   y = paste(plotDataPk$displayNamePKParameter[1],'ratio'))

        }

      } else {
        ratioMode <- 'none'

        plotObject <-
          ospsuite.plots::plotBoxWhisker(data =   plotDataPk,
                                         mapping = aes(x = scenarioShortName,
                                                       y = value,
                                                       fill = colorIndex),
                                         yscale = yScale,
                                         yscale.args = getYlimits(onePlotConfig,yScale,...),
                                         ...) +
          labs(x = '',
               y = paste(plotDataPk$displayNamePKParameter[1],ylabelUnit)) +
          scale_fill_manual(values = colorVector) +
          theme(legend.title = element_blank())

      }

      plotObject <-
        addFacets(plotObject = plotObject,
                  facetScale = onePlotConfig$facetScale[1],
                  facetAspectRatio = facetAspectRatio,
                  nFacetColumns = nFacetColumns)

      if (xAxisTextAngle > 0)
        plotObject <- plotObject +
        theme(axis.text.x = element_text(angle = xAxisTextAngle,hjust = 1))


      # prepare for export
      setattr(plotObject,'caption',
              getCaptionForBoxwhiskerPlot(plotDataPk = plotDataPk,
                                          yScale = yScale,
                                          percentiles = percentiles,
                                          plotCaptionAddon = onePlotConfig$plotCaptionAddon[1],
                                          ratioMode = ratioMode
                                          )
      )
      figureKey = paste(plotNameLoop,
              ifelse(yScale == "log", "log", "linear"),
              ifelse(asRatio,'ratio','abs'),
              sep = "-"
        )
      plotList[[figureKey]] <- plotObject

    }
    # generate table
    tableKey = paste(plotNameLoop,
                      ifelse(asRatio,'ratio','abs'),
                      sep = "-"
    )
    plotList[[tableKey]] <- addAsTable(plotDataPk = plotDataPk,
                                           plotObject = plotObject,
                                           onePlotConfig = onePlotConfig,
                                           ratioMode = ratioMode,
                                           percentiles = percentiles)

  }

  return(plotList)
}

#' Prepare Data for PK Boxplot
#'
#' Prepares and cleans data for box-and-whisker plotting.
#'
#' @param onePlotConfig Configuration for a single plot.
#' @param pkParameterDT A data.table containing PK parameter data.
#' @param colorVector A named vector for colors.
#' @param asRatio Logical indicating if the plot is for ratios.
#' @param nBootstrap Number of bootstrap samples.
#' @param percentiles A vector of percentiles to calculate.
#' @return A data.table prepared for plotting.
#' @keywords internal
prepareDataForPKBoxplot <- function(onePlotConfig,pkParameterDT,colorVector,asRatio,nBootstrap,percentiles){

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
    ) %>%  merge(configEnv$outputPaths[, c('outputPathId', 'displayNameOutputs')],
                         by = 'outputPathId')


  if (nrow(plotData) > 0){

    if (dplyr::n_distinct(plotData$pkParameter) > 1){
      plotData[,plotName := paste(plotName,pkParameter,sep = '_')]
    }

    if (asRatio){
      plotData <- plotData[!is.na(referenceScenario)]
      # filter relevant reference values
      dtPopID <- plotData[,c('scenario','referenceScenario','populationId')] %>%
                    unique() %>%
                    merge(pkParameterDT[,c('scenarioName','populationId')] %>%
                            unique() ,
                          by.x = 'referenceScenario',
                          by.y  = c('scenarioName'),
                          suffixes = c('','.reference')
      )
      if (all(dtPopID$populationId == dtPopID$populationId.reference)){
        plotData <- merge(plotData,
                          pkParameterDT[,c('scenarioName',"parameter","individualId","outputPathId","value","populationId")] %>%
                            setnames(old = c('scenarioName','parameter'),
                                     new = c('referenceScenario','pkParameter')),
                          by = c('referenceScenario',"pkParameter","individualId","outputPathId"),
                          suffixes = c('','.reference'))
        plotData[,ratio := value/value.reference]
      } else  if (all(dtPopID$populationId != dtPopID$populationId.reference)){
        plotData <- getPopulationRatioByBootstrapping(plotData = plotData,
                                                      pkParameterDT = pkParameterDT,
                                                      nBootstrap = nBootstrap,
                                                      percentiles = percentiles)
      } else {
        print(dtPopID)
        stop('Within one plot you must either compare always scenarios with the same base population or
             always scenarios with different base populations')
      }
    } else {
      plotData[,isReference := scenario %in% referenceScenario, by = c('plotName')]
      plotData[,colorIndex := ifelse(isReference == TRUE,names(colorVector)[2],names(colorVector)[1])]
    }

    # Ensure order by creating factors
    plotData$displayNameOutput <- factor(plotData$displayNameOutput,
                                         levels = unique(plotData$displayNameOutput),
                                         ordered = TRUE)

    plotData$scenarioShortName <- factor(plotData$scenarioShortName,
                                           levels = unique(onePlotConfig$scenarioShortName),
                                           ordered = TRUE)
    # add Tag for faceting
    plotData[,plotTag := generatePlotTag(as.numeric(displayNameOutput))]
  }

return(plotData)
}
#' Add Summary Table from Plot Data
#'
#' Generates a summary table from the box-and-whisker plot data.
#'
#' @param plotDataPk A data.table containing plot data.
#' @param plotObject The plot object to extract statistics from.
#' @param ratioMode Mode indicating if the plot is for ratios.
#' @param onePlotConfig Configuration for a single plot.
#' @param percentiles A vector of percentiles to calculate.
#' @return A data.table summarizing the plot data.
#' @keywords internal
addAsTable <- function(plotDataPk,plotObject,ratioMode,onePlotConfig,percentiles){

  if (ratioMode == 'ratioOfPopulation'){
    dtExport <- copy(plotDataPk)
    setnames(dtExport, old = c('ymin','lower','middle','upper','ymax'),
             new = paste(scales::label_ordinal()(x = percentiles * 100),'percentile'))
  } else {
    dtExport <- plotDataPk %>%
      data.table::setDT() %>%
      .[, as.list(plotObject$statFun(value)),
        by = c("plotTag","scenarioShortName")
      ]
    dtExport$scenarioShortName <- factor(dtExport$scenarioShortName,
                                         levels = levels(plotDataPk$scenarioShortName),
                                         ordered = TRUE)
  }

  setorderv(dtExport,'scenarioShortName')

  setattr(dtExport,'caption',
          getCaptionForBoxwhiskerPlot(plotDataPk = plotDataPk,
                                      plotCaptionAddon = onePlotConfig$plotCaptionAddon[1],
                                      isPlotCaption = FALSE,
                                      ratioMode = ratioMode
          )
  )

  if (dplyr::n_distinct(dtExport$plotTag) == 1){
    dtExport[,plotTag := NULL]
  }

  return(dtExport)
}

# auxiliaries  ------------
#' Calculate Population Ratio by Bootstrapping
#'
#' Calculates population ratios using bootstrapping techniques.
#'
#' @param plotData A data.table containing plot data.
#' @param pkParameterDT A data.table containing PK parameter data.
#' @param nBootstrap Number of bootstrap samples.
#' @param percentiles A vector of percentiles to calculate.
#' @return A data.table containing bootstrapped ratio statistics.
#' @keywords internal
getPopulationRatioByBootstrapping <- function(plotData,pkParameterDT,nBootstrap,percentiles){


  # Create a function to calculate statistics for each PKParameter and outputPathId
  calculateStatistics <- function(value, valueReference,nBootstrap,percentiles) {
    n1 <- length(value)
    n2 <- length(valueReference)
    sampleSize <- min(n1, n2)

    # Pre-allocate results
    ratios <- matrix(0, nrow = nBootstrap, ncol = sampleSize)

    # Vectorized sampling
    for (ii in 1: nBootstrap) {
      s1 <- sample(value, size = sampleSize, replace = FALSE)
      s2 <- sample(valueReference, size = sampleSize, replace = FALSE)
      ratios[ii, ] <- s2 / s1
    }

    # Calculate statistics on the entire matrix
    arithMean <- rowMeans(ratios)
    arithSd <- apply(ratios, 1, sd)
    arithCV <- arithSd / arithMean
    rQuantiles <- apply(ratios, 1, quantile, probs = percentiles)
    rownames(rQuantiles) <- c('ymin','lower','middle','upper','ymax')

    # Aggregate results
    tmp <- as.data.table(cbind(t(rQuantiles), arithMean, arithSd, arithCV))
    result <- tmp[, lapply(.SD, median, na.rm = TRUE)]

    # add quantities which are calculated analytical
    result[,geoMean := exp(mean(log(value)) - mean(log(valueReference)))]
    result[,geoSd := exp(sqrt((sd(log(value)))^2 + (sd(log(valueReference)))^2))]
    result[,geoCV := sqrt(exp((sd(log(value)))^2 + (sd(log(valueReference)))^2) - 1)]

    setnames(result,
             old = c('arithMean','arithSd','arithCV','geoMean','geoSd','geoCV'),
             new = c("arith mean","arith standard deviation","arith CV","geo mean","geo standard deviation","geo CV")
    )
    return(result)
  }

  # Apply the function to each unique combination of PKParameter and outputPathId
  resultList <- lapply(split(plotData,by = c('outputPathId','pkParameter','scenario')), function(dDefault) {
    dtDefault <- setDT(dDefault)

    dtReference <- merge(dtDefault %>%
                           dplyr::select('outputPathId','pkParameter','referenceScenario') %>%
                           unique() %>%
                           setnames(old = 'referenceScenario', new = 'scenario'),
                         pkParameterDT[,c('scenarioName',"parameter","individualId","outputPathId","value","populationId")] %>%
                           setnames(old = c('scenarioName','parameter'),
                                    new = c('scenario','pkParameter')),
                         by = c('scenario',"pkParameter","outputPathId"))

    # Call the statistics calculation function
    dt <- calculateStatistics(value = dtDefault[!is.na(value)]$value,
                              valueReference = dtReference[!is.na(value)]$value,
                              nBootstrap = nBootstrap,
                              percentiles = percentiles
    )

    dt <- cbind(dtDefault[1,] %>% dplyr::select(!c('individualId','value')),dt)
  })

  # Combine results into a single data.table
  return(rbindlist(resultList))

}
#' Get Caption for Box-and-Whisker Plot
#'
#' Generates a caption for the box-and-whisker plot based on the data and configuration.
#'
#' @param plotDataPk A data.table containing plot data.
#' @param percentiles A vector of percentiles to calculate.
#' @param yScale Scale type (linear or log).
#' @param plotCaptionAddon Additional text for the caption.
#' @param isPlotCaption Logical indicating if the caption is for the plot.
#' @param ratioMode Mode indicating if the plot is for ratios.
#' @return A character string containing the caption.
#' @keywords interal
getCaptionForBoxwhiskerPlot <-
  function(plotDataPk,
           percentiles = NULL,
           yScale = NULL,
           plotCaptionAddon,
           isPlotCaption = TRUE,
           ratioMode) {
  dtCaption <-
    plotDataPk[, c(
      "scenarioLongName",
      'displayNameOutput',
      'plotTag',
      "plotCaptionAddon",
      "displayNamePKParameter",
      "displayNameOutput"
    )] %>%  unique()


  captiontext <- switch(ratioMode,
                        'none' = paste("Population summary statistics of",
                                  dtCaption$displayNamePKParameter[1]),
                        'individualRatios' = paste('Population summary statistics of',
                                   dtCaption$displayNamePKParameter[1], 'ratios'),
                        'ratioOfPopulation' = paste('Ratio of population summary statistics of',
                                     dtCaption$displayNamePKParameter[1]))

  captiontext <- paste(captiontext,
                       "of",
                       pasteFigureTags(dtCaption, captionColumn = "displayNameOutput"))

  captiontext <- paste(paste0(captiontext,'.'),
                       ifelse(!is.na(plotCaptionAddon),
                              plotCaptionAddon,
                              ''))
  if (isPlotCaption){
    captiontext <- paste(captiontext,
                         "Shown as box-whisker plot, which indicates the",
                         paste(formatPercentiles(percentiles,suffix = '',allAsPercentiles = TRUE),collapse = ', '),'percentiles',
                         "on a", ifelse(yScale == "linear", "linear", "logarithmic"),
                         "y-scale")
  }
  return(captiontext)
}

#' Validate PK Box-and-Whisker Configuration Table
#'
#' Validates the configuration table for PK box-and-whisker plots.
#'
#' @param configTable A data.table containing the configuration table.
#' @param pkParameterDT A data.table containing PK parameter data.
#' @param ... Additional arguments for validation.
#' @return NULL (invisible).
#'
#' @keywords internal
validatePKBoxwhiskerConfigTable <- function(configTable, pkParameterDT,...) {

  configTablePlots <- validateHeaders(configTable)
  validateOutputIdsForPlot()
  validateDataGroupIdsForPlot()

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
      "facetScale",
      "plot_Absolute",
      "plot_Ratio"

    ))


  return(invisible())
}

# support usability --------------------
#' Add Default Configuration for PK Box-and-Whisker Plots
#'
#' Adds default configurations for box-and-whisker plots to the `Plots.xlsx` configuration file.
#'
#' @param projectConfiguration A ProjectConfiguration object.
#' @param pkParameterDT A data.table containing PK parameter data.
#' @param sheetName Name of the sheet to create.
#' @param overwrite Logical indicating if existing data should be overwritten.
#' @return NULL (invisible).
#' @export
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
                        facetScale = 'fixed',
                        plot_Absolute = 1,
                        plot_Ratio = 1),
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
