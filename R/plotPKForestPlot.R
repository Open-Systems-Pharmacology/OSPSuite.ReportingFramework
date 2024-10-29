' Common PK Ratio Plot Function
#'
#' This internal function performs common validation and configuration setup for PK ratio plots.
#'
#' @param projectConfiguration A ProjectConfiguration object.
#' @param subfolder A string specifying the subfolder for output.
#' @param configTableSheet Name of the sheet containing configuration tables.
#' @param pkParameterDT A data.table with PK parameters.
#' @param pkParameterObserved A data.table with observed PK parameters (optional).
#' @param ratioCalculationInputs A list of inputs specific to the ratio calculation method.
#' @param ratioCalculationMethod A string specifying the method for ratio calculation.
#' @param vlineIntercept Intercept for vertical line on plots.
#' @param scaleVectors A list of aesthetic parameters for plots.
#' @param digitsToRound Number of digits to round in output.
#' @param digitsToShow Number of digits to display.
#' @param xlabel Label for x-axis.
#'
#' @return An RmdContainer object with generated plots.
#'
#' @keywords internal
commonPKRatioPlot <- function(projectConfiguration,
                              subfolder,
                              configTableSheet,
                              pkParameterDT,
                              pkParameterObserved,
                              ratioCalculationInputs,
                              ratioCalculationMethod,
                              vlineIntercept,
                              scaleVectors,
                              digitsToRound,
                              digitsToShow,
                              xlabel,
                              tableLabels) {

  checkmate::assertClass(projectConfiguration, classes = 'ProjectConfiguration')
  checkmate::assertString(subfolder)
  checkmate::assertString(xlabel)
  checkmate::assertDataTable(pkParameterDT)
  checkmate::assertNames(names(pkParameterDT),
                         must.include = c("scenarioName", "parameter", "individualId", "value", "outputPathId", "displayName", "displayUnit"))
  checkmate::assertNumeric(vlineIntercept, any.missing = FALSE, unique = TRUE, null.ok = TRUE)
  checkmate::assertList(scaleVectors, types = 'list')
  checkmate::assertIntegerish(digitsToRound, lower = 0, len = 1)
  checkmate::assertIntegerish(digitsToShow, lower = 0, len = 1)

  # Read configuration tables
  configTable <- readPKForestConfigTable(
    sheetName = configTableSheet,
    projectConfiguration = projectConfiguration,
    pkParameterDT = pkParameterDT
  )

  rmdContainer <- generateRmdContainer(
    projectConfiguration,
    subfolder,
    configTable,
    function(onePlotConfig, rmdContainer, ...) {
      createPkPlotForPlotName(onePlotConfig = onePlotConfig,
                              rmdContainer = rmdContainer,
                              pkParameterDT = pkParameterDT,
                              pkParameterObserved = pkParameterObserved,
                              ratioCalculationMethod = ratioCalculationMethod,
                              ratioCalculationInputs = ratioCalculationInputs,
                              vlineIntercept = vlineIntercept,
                              scaleVectors = scaleVectors,
                              digitsToRound = digitsToRound,
                              digitsToShow = digitsToShow,
                              xlabel = xlabel,
                              tableLabels = tableLabels)
    }
  )

  return(rmdContainer)
}

#' Plot PK Ratio by Ratio Aggregation
#'
#' This function generates plots for pharmacokinetic (PK) ratios using various aggregation methods.
#'
#' The function allows for different aggregation techniques, including:
#' - Geometric and Arithmetic Standard Deviations
#' - Percentiles
#' - A user-defined custom function for aggregation
#'
#' @param projectConfiguration A ProjectConfiguration object containing project settings.
#' @param subfolder A string specifying the output directory.
#' @param configTableSheet Name of the sheet containing configuration tables.
#' @param pkParameterDT A data.table with PK parameters.
#' @param pkParameterObserved A data.table with observed PK parameters (optional).
#' @param aggregationFlag A character string indicating the aggregation method. Options include:
#' - "GeometricStdDev"
#' - "ArithmeticStdDev"
#' - "Percentiles"
#' - "Custom"
#' @param percentiles A numeric vector of percentiles to calculate if aggregationFlag is "Percentiles". Default is c(5, 50, 95).
#' @param customFunction A custom function for aggregation if aggregationFlag is "Custom". Default is NULL.
#'  The custom function should take a numeric vector `y` as input and return a list containing:
#' - `yValues`: The aggregated value (e.g., mean).
#' - `yMin`: The lower value of the aggregated data, (e.g. mean - sd).
#' - `yMax`: The upper value of the aggregated data, (e.g. mean + sd).
#' @param vlineIntercept Intercept for vertical line on plots.
#' @param scaleVectors A list of aesthetic parameters for plots.
#' @param digitsToRound Number of digits to round in output.
#' @param digitsToShow Number of digits to display.
#' @param xlabel Label for the x-axis.
#' @param tableLabels Labels for tables generated in the plot.
#'
#' @return An RmdContainer object containing the generated plots and related information.
#'
#' @export
plotPKRatioForestPlotByRatioAggregation <- function(projectConfiguration,
                                                    subfolder,
                                                    configTableSheet,
                                                    pkParameterDT,
                                                    pkParameterObserved = NULL,
                                                    aggregationFlag = c("GeometricStdDev", "ArithmeticStdDev", "Percentiles", "Custom"),
                                                    percentiles = c(5, 50, 95),
                                                    customFunction = NULL,
                                                    vlineIntercept = 1,
                                                    scaleVectors = list(
                                                      simulated = list(color = 'darkgrey', fill = 'darkgrey', shape = 'circle'),
                                                      observed = list(color = 'darkgrey', fill = 'lightgrey', shape = 'triangle filled')),
                                                    digitsToRound = 2,
                                                    digitsToShow = 2,
                                                    xlabel = 'Ratio',
                                                    tableLabels = NULL) {

  aggregationFlag <- match.arg(aggregationFlag)
  aggregationFun <- getAggregationFunction(aggregationFlag, percentiles, customFunction)

  if (is.null(tableLabels)){
    if (aggregationFlag == ospsuite::DataErrorType$GeometricStdDev){
      tableLabels <- getErrorLabels(aggregationFlag) %>%
        gsub(pattern = 'geometric ',replacement = 'geometric\n')
    } else if (aggregationFlag == "Percentiles"){

      tableLabels <- paste0(scales::label_ordinal()(x = percentiles[c(2,1,3)] * 100), "\npercentile") %>%
        gsub(pattern = "50th\npercentile",replacement = 'median') %>%
        gsub(pattern = "0th\npercentile",replacement = 'min') %>%
        gsub(pattern = "100th\npercentile",replacement = 'max')

    } else if (aggregationFlag == "Custom"){
      checkmate::assertCharacter(tableLabels,len = 3,null.ok = FALSE)
    }
  }

  ratioCalculationInputs = list(aggregationFun = aggregationFun)

  return(commonPKRatioPlot(projectConfiguration,
                           subfolder,
                           configTableSheet,
                           pkParameterDT,
                           pkParameterObserved,
                           ratioCalculationInputs,
                           ratioCalculationMethod = 'byRatioAggregation',
                           vlineIntercept = vlineIntercept,
                           scaleVectors = scaleVectors,
                           digitsToRound = digitsToRound,
                           digitsToShow = digitsToShow,
                           xlabel = xlabel,
                           tableLabels = tableLabels))
}

#' Plot PK Ratio by Bootstrapping
#'
#' This function generates plots for PK ratios using bootstrapping methods.
#'
#' @param projectConfiguration A ProjectConfiguration object.
#' @param subfolder A string specifying the subfolder for output.
#' @param configTableSheet Name of the sheet containing configuration tables.
#' @param pkParameterDT A data.table with PK parameters.
#' @param pkParameterObserved A data.table with observed PK parameters (optional).
#' @param coefficientOfVariation Coefficient of variation for simulations.
#' @param nObservationDefault Default number of observations.
#' @param nBootstrap Number of bootstrap samples.
#' @param statFun Function to compute statistics.
#' @param confLevel Confidence level for intervals.
#' @param seed Random seed for reproducibility.
#' @param vlineIntercept Intercept for vertical line on plots.
#' @param scaleVectors A list of aesthetic parameters for plots.
#' @param digitsToRound Number of digits to round in output.
#' @param digitsToShow Number of digits to display.
#' @param xlabel Label for x-axis.
#'
#' @return An RmdContainer object with generated plots.
#'
#' @export
plotPKRatioForestPlotByBoostrapping <- function(projectConfiguration,
                                                subfolder,
                                                configTableSheet,
                                                pkParameterDT,
                                                pkParameterObserved = NULL,
                                                coefficientOfVariation,
                                                nObservationDefault,
                                                nBootstrap = 1000,
                                                statFun = function(x) exp(mean(log(x))),
                                                confLevel = 0.9,
                                                seed = 123,
                                                vlineIntercept = 1,
                                                scaleVectors = list(
                                                  simulated = list(color = 'darkgrey', fill = 'darkgrey', shape = 'circle'),
                                                  observed = list(color = 'darkgrey', fill = 'lightgrey', shape = 'triangle filled')),
                                                digitsToRound = 2,
                                                digitsToShow = 2,
                                                xlabel = 'DDI Ratio',
                                                tableLabels =  c('Ratio',
                                                                 paste0(confLevel * 100, '%\nCI lower'),
                                                                 paste0(confLevel * 100, '%\nCI upper'))) {

  checkmate::assertDouble(coefficientOfVariation, lower = 0, len = 1, finite = TRUE)
  checkmate::assertIntegerish(nObservationDefault, lower = 0, len = 1)
  checkmate::assertIntegerish(nBootstrap, lower = 0, len = 1)
  checkmate::assertIntegerish(seed, len = 1)
  checkmate::assertDouble(confLevel, lower = 0, upper = 1, len = 1, finite = TRUE)
  checkmate::assertFunction(statFun)
  checkmate::assertCharacter(tableLabels,any.missing = FALSE,len = 3)

  bootsTrappingInputs = list(
    coefficientOfVariation = coefficientOfVariation,
    nObservationDefault = nObservationDefault,
    nBootstrap = nBootstrap,
    confLevel = confLevel,
    statFun = statFun,
    seed = seed
  )


  return(commonPKRatioPlot(projectConfiguration,
                           subfolder,
                           configTableSheet,
                           pkParameterDT,
                           pkParameterObserved,
                           bootsTrappingInputs,
                           ratioCalculationMethod = 'byBootsTrapping',
                           vlineIntercept = vlineIntercept,
                           scaleVectors = scaleVectors,
                           digitsToRound = digitsToRound,
                           digitsToShow = digitsToShow,
                           xlabel = xlabel,
                           tableLabels))
}

# plotCreation  ---------------


#' Create PK Ratio Plot
#'
#' Creates a plot for the PK ratio based on the provided configuration.
#'
#' @param onePlotConfig A data.table containing the plot configuration.
#' @param rmdContainer An RmdContainer object.
#' @param pkParameterDT A data.table with PK parameters.
#' @param coefficientOfVariation Coefficient of variation for simulations.
#' @param nObservationDefault Default number of observations.
#' @param nBootstrap Number of bootstrap samples.
#' @param confLevel Confidence level for intervals.
#' @param statFun Function to compute statistics.
#' @param seed Random seed for reproducibility.
#' @param vlineIntercept Intercept for vertical line on plots.
#' @param scaleVectors A list of aesthetic parameters for plots.
#' @param digitsToRound Number of digits to round in output.
#' @param digitsToShow Number of digits to display.
#' @param xlabel Label for x-axis.
#'
#' @return An updated RmdContainer object with the added plot.
createPkPlotForPlotName <- function(onePlotConfig,
                                    rmdContainer,
                                    pkParameterDT,
                                    pkParameterObserved,
                                    ratioCalculationMethod,
                                    ratioCalculationInputs,
                                    vlineIntercept,
                                    scaleVectors,
                                    digitsToRound,
                                    digitsToShow,
                                    xlabel,
                                    tableLabels) {
  for (outputPathIdLoop in splitInputs(onePlotConfig$outputPathId[1])) {

    pkParameterDTForOutput <- pkParameterDT[outputPathId == outputPathIdLoop]

    if(!is.null(pkParameterObserved)){
      pkParameterObservedForOutput <- pkParameterObserved[outputPathId == outputPathIdLoop]
    } else {
      pkParameterObservedForOutput <- NULL
    }

    plotData <- prepareDataForPKRatioPlot(
      onePlotConfig = onePlotConfig,
      pkParameterDT = pkParameterDTForOutput,
      pkParameterObserved = pkParameterObservedForOutput,
      ratioCalculationMethod = ratioCalculationMethod,
      ratioCalculationInputs = ratioCalculationInputs)

    # adjust names for function call
    data.table::setnames(
      plotData,
      old = c('scenarioGroup','scenarioCaptionName','parameterDisplayName','xMin','xMax','xErrorValues'),
      new = c('displayGroup', 'displayName', 'parameter','xmin','xmax','xrange')
    )

    plotObject <- ospsuite_plotForest(plotData,
                                      vlineIntercept = vlineIntercept,
                                      digitsToRound = digitsToRound,
                                      digitsToShow = digitsToShow,
                                      tableLabels = tableLabels,
                                      xlabel = xlabel)

    # Add color and shape scales
    plotObject <- plotObject +
      scale_color_manual(values = unlist(lapply(scaleVectors, getElement, 'color'))) +
      scale_fill_manual(values = unlist(lapply(scaleVectors, getElement, 'fill'))) +
      scale_shape_manual(values = unlist(lapply(scaleVectors, getElement, 'shape')))


    browser()
    # Export
    rmdContainer$addAndExportFigure(
      plotObject = plotObject,
      caption = getCaptionForRatioPlot(outputPathIdLoop = outputPathIdLoop,
                                          plotData = plotData,
                                          pkParameterDT = pkParameterDT),
      figureKey = paste(onePlotConfig$plotName[1], outputPathIdLoop, sep = "-"),
      width = 30
    )
  }

  return(rmdContainer)
}

#' Prepare Data for DDI Ratio Plot
#'
#' Prepares the data necessary for generating a DDI ratio plot.
#'
#' @param onePlotConfig A data.table containing the plot configuration.
#' @param pkParameterDT A data.table with PK parameters.
#' @param coefficientOfVariation Coefficient of variation for simulations.
#' @param nBootstrap Number of bootstrap samples.
#' @param nObservationDefault Default number of observations.
#' @param confLevel Confidence level for intervals.
#' @param statFun Function to compute statistics.
#' @param seed Random seed for reproducibility.
#'
#' @return A data.table containing the prepared plot data.
#'
#' @keywords internal
prepareDataForPKRatioPlot <- function(onePlotConfig,
                                      pkParameterDT,
                                      pkParameterObserved,
                                      ratioCalculationMethod,
                                      ratioCalculationInputs) {
  plotData <- data.table()

  for (iRow in seq_len(nrow(onePlotConfig))) {
    configList <- onePlotConfig[iRow,]

    dataForConfig <-
      mergePkData(
        configList,
        pkParameterObserved,
        columns = c('x', 'xMin', 'xMax', 'xErrorValues', 'numberOfObservations')
      )
    if (ratioCalculationMethod == 'byBootsTrapping')
      dataForConfig[is.na(numberOfObservations),numberOfObservations := ratioCalculationInputs$nObservationDefault]

    dataForConfig <- dataForConfig %>%
      merge(pkParameterDT[,c('parameter','displayName')] %>%
              unique() %>%
              data.table::setnames(old = c('parameter','displayName'),
                                   new = c('pkParameter','parameterDisplayName')),
            by = 'pkParameter')

    # get rows for simulated PK
    plotDataSim <-
      switch(ratioCalculationMethod,
             byBootsTrapping =
               data.table::rbindlist(
                 lapply(dataForConfig$parameterDisplayName,
                        function(pkIdentifier){
                          calculateRatiosByBoostrapping(
                            configList = configList,
                            pkParameterDT = pkParameterDT[displayName == pkIdentifier],
                            coefficientOfVariation = ratioCalculationInputs$coefficientOfVariation,
                            nObservations = dataForConfig[parameterDisplayName == pkIdentifier]$numberOfObservations,
                            nBootstrap = ratioCalculationInputs$nBootstrap,
                            confLevel = ratioCalculationInputs$confLevel,
                            statFun = ratioCalculationInputs$statFun,
                            seed = ratioCalculationInputs$seed
                          ) %>%
                            dplyr::mutate(parameterDisplayName = pkIdentifier)
                        })),
             byRatioAggregation =
               data.table::rbindlist(
                 lapply(dataForConfig$parameterDisplayName,
                        function(pkIdentifier){
                          calculateRatiosByAggregation(
                            configList = configList,
                            pkParameterDT = pkParameterDT[displayName == pkIdentifier],
                            aggregationFun = ratioCalculationInputs$aggregationFun) %>%
                            dplyr::mutate(parameterDisplayName = pkIdentifier)
                        }))

      )
    # merge with observed data
    plotData <- rbind(
      plotData,
      rbind(
        cbind(configList[,c('scenarioCaptionName',
                            'scenarioGroup')],
              plotDataSim
        ) %>%
          dplyr::mutate(type = 'simulated'),
        dataForConfig[!is.na(x),c('scenarioCaptionName',
                                  'scenarioGroup',
                                  'parameterDisplayName',
                                  'x',
                                  'xMin',
                                  'xMax',
                                  'xErrorValues')] %>%
          dplyr::mutate(type = 'observed'),
        fill = TRUE
      )
    )
  }

  if (any(is.na(plotData$x))) {
    warning('PKParameter for  Ratio contains NAs, rows will be omitted')
    plotData <- plotData[!is.na(x)]
  }

  # Ensure order by creating factors
  plotData$scenarioCaptionName <- factor(plotData$scenarioCaptionName,
                                         levels = unique(onePlotConfig$scenarioCaptionName),
                                         ordered = TRUE)

  plotData$type <- factor(plotData$type,
                          levels = unique(plotData$type), ordered = TRUE)

  plotData$scenarioGroup <- factor(plotData$scenarioGroup,
                                   levels = unique(plotData$scenarioGroup), ordered = TRUE)

  return(plotData)
}

#' Calculate Ratios for Workflow
#'
#' Calculates ratios for the specified workflow based on the provided configuration.
#'
#' @param configList A data.table containing the configuration for the current workflow.
#' @param pkParameterDT A data.table with PK parameters.
#' @param coefficientOfVariation Coefficient of variation for simulations.
#' @param nObservations Number of observations to simulate.
#' @param nBootstrap Number of bootstrap samples.
#' @param confLevel Confidence level for intervals.
#' @param statFun Function to compute statistics.
#' @param seed Random seed for reproducibility.
#'
#' @return A data.table containing the bootstrapped statistics.
#'
#' @keywords internal
calculateRatiosByBoostrapping <- function(configList,
                                          pkParameterDT,
                                          coefficientOfVariation,
                                          nObservations,
                                          nBootstrap,
                                          confLevel,
                                          statFun,
                                          seed) {
  # Add inter-occasional variability
  set.seed(seed)

  sigma <- sqrt(log(coefficientOfVariation^2 + 1))
  pkParameterDT[, valueIOV := value * exp(rnorm(n = .N, mean = 0, sd = sigma))]

  # Calculate ratio
  dt <- merge(pkParameterDT[scenarioName == configList$referenceScenario],
              pkParameterDT[scenarioName == configList$scenario],
              by = 'individualId',
              all = TRUE,
              suffixes = c('.reference', '')) %>%
    .[, ratio := valueIOV / valueIOV.reference]

  # Adjust maximal number of observations
  if (nrow(dt) <= nObservations * 1.1) {
    nObservations <- floor(nrow(dt) * 0.9)
    warning(paste('Reduce nObservations as simulated data set is too small:',
                  'nObservations =', nObservations,
                  'n simulated ratios', nrow(dt)))
  }

  # Start bootstrapping
  set.seed(seed)

  getStatSample <- function(y, statFun) {
    return(statFun(sample(x = y, size = nObservations, replace = FALSE)))
  }

  myBootstrap <- function(y, statFun) {
    y <- y[!is.na(y) & y > 0]
    bstrap <- do.call(rbind, replicate(nBootstrap, getStatSample(y, statFun), simplify = FALSE))

    return(stats::setNames(c(quantile(bstrap, probs = c((1 - confLevel) / 2, 0.5, 1 - ((1 - confLevel) / 2)))),
                           c('xMin', 'x', 'xMax')))
  }

  dtBootstrapped <- dt[, as.list(myBootstrap(ratio, statFun))]

  return(dtBootstrapped)
}

#' Calculate Ratios for Workflow
#'
#' Calculates ratios for the specified workflow based on the provided configuration.
#'
#' @param configList A data.table containing the configuration for the current workflow.
#' @param pkParameterDT A data.table with PK parameters.
#' @param coefficientOfVariation Coefficient of variation for simulations.
#' @param nObservations Number of observations to simulate.
#' @param nBootstrap Number of bootstrap samples.
#' @param confLevel Confidence level for intervals.
#' @param statFun Function to compute statistics.
#' @param seed Random seed for reproducibility.
#'
#' @return A data.table containing the bootstrapped statistics.
#'
#' @keywords internal
calculateRatiosByAggregation <- function(configList,
                                         pkParameterDT,
                                         aggregationFun) {
  # Calculate ratio
  dt <- merge(pkParameterDT[scenarioName == configList$referenceScenario],
              pkParameterDT[scenarioName == configList$scenario],
              by = 'individualId',
              all = TRUE,
              suffixes = c('.reference', '')) %>%
    .[, ratio := value / value.reference]

  dtAggregated <- dt[, as.list(aggregationFun(ratio))]

  if (!is.null(dtAggregated$yErrorValues)){
    if (dtAggregated$yErrorType[1] == ospsuite::DataErrorType$ArithmeticStdDev) {
      dtAggregated[,yMin := yValues - yErrorValues]
      dtAggregated[,yMax := yValues + yErrorValues]
    }
    if (dtAggregated$yErrorType[1] == ospsuite::DataErrorType$GeometricStdDev) {
      dtAggregated[,yMin := yValues/yErrorValues]
      dtAggregated[,yMax := yValues*yErrorValues]
    }
  }

  data.table::setnames(dtAggregated,old = c('yMin','yValues','yMax','yErrorValues'),
                       new = c('xMin', 'x', 'xMax','xErrorValues'),
                       skip_absent = TRUE)



  return(dtAggregated)
}


# plotHelper --------------
#' Get Caption for DDI Ratio Plot
#'
#' Generates a caption for the DDI ratio plot based on the output path ID and plot data.
#'
#' @param outputPathIdLoop The output path ID for the current plot.
#' @param plotData A data.table containing the plot data.
#' @param pkParameterDT A data.table with PK parameters.
#'
#' @return A string containing the caption for the plot.
#'
#' @keywords internal
getCaptionForRatioPlot <- function(outputPathIdLoop,
                                      plotData,
                                      pkParameterDT) {
  paste0('Simulated ',
         ifelse('observed' %in% unique(plotData$type), ' and observed ', ''),
         concatWithAnd(paste(unique(plotData$parameter), 'ratios')),
         ' of ', pkParameterDT[outputPathId == outputPathIdLoop,]$displayNameOutput[1])
}



# auxiliaries ---------

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
readPKForestConfigTable <- function(projectConfiguration, sheetName, pkParameterDT) {
  # Initialize variable used in data.tables
  level <- NULL

  dtOutputPaths <- getOutputPathIds(projectConfiguration)

  # Read configuration tables
  configTable <- xlsxReadData(
    wb = projectConfiguration$plotsFile,
    sheetName = sheetName,
    skipDescriptionRow = TRUE
  )

  configTablePlots <- validateHeaders(configTable)

  validateConfigTablePlots(
    configTablePlots = configTablePlots,
    charactersWithoutMissing = c("plotName","scenarioCaptionName","scenario"),
    charactersWithMissing = c("scenarioGroup","plotCaptionAddon",'dataGroupId'),
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
      ),
      outputPathId = list(
        cols = c("outputPathId"),
        allowedValues = unique(pkParameterDT$outputPathId)
      )
    )
  )

  validatePanelConsistency(
    configTablePlots = configTablePlots,
    panelColumns = c(
      "plotCaptionAddon",
      "outputPathId"
    ),
    dtOutputPaths = dtOutputPaths
  )


  return(configTable)
}



#' Merge Pharmacokinetic PK Parameter to config Table
#'
#' This function merges a configuration table with observed pharmacokinetic parameters.
#'
#' @param configTable A data.table containing the configuration data.
#' @param pkParameterObserved A data.table containing observed pharmacokinetic parameters.
#' @param columns A character vector of additional columns to include in the merge.
#' @return A data.table resulting from the merge operation with missing columns added as NA.
#'
#' @keywords internal
mergePkData <- function(configTable, pkParameterObserved, columns = c('x', 'xMin', 'xMax', 'numberOfObservations')) {

  dataForConfig <- data.table::copy(configTable) %>%
    tidyr::separate_rows(pkParameter, sep = ",") %>%
    data.table::setDT() %>%
    .[,pkParameter := trimws(pkParameter)]


  if (!is.null(pkParameterObserved)) {
    # Select relevant columns including the keys and the specified columns
    relevantColumns <- unique(c('pkParameter', 'group', columns))

    dataForConfig <- merge(
      data.table::setnames(configTable, 'dataGroupId', 'group'),
      pkParameterObserved[, ..relevantColumns, with = FALSE] %>%
        unique(),
      by = c('pkParameter', 'group')
    )
  }

  # Add missing columns with NA values without a loop
  missingCols <- setdiff(columns, names(dataForConfig))
  dataForConfig[, (missingCols) := lapply(missingCols, function(x) NA_real_)]

  return(dataForConfig)
}
