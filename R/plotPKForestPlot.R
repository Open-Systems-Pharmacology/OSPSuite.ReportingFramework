#' Common PK Ratio Plot Function
#'
#' This internal function performs common validation and configuration setup for PK ratio plots.
#' It ensures that the input data is valid and prepares the necessary configuration for generating
#' pharmacokinetic (PK) ratio plots. The function also handles the reading of configuration tables
#' and the setup for observed PK parameters if provided.
#'
#' @param projectConfiguration A ProjectConfiguration object that contains settings and parameters
#'        for the project, including file paths and other configurations.
#' @param subfolder A string specifying the subfolder within the project directory where output files
#'        will be saved.
#' @param configTableSheet A string representing the name of the sheet that contains configuration
#'        tables relevant for generating the plots.
#' @param pkParameterDT A data.table containing PK parameters, which includes columns such as
#'        scenario names, parameter types, individual IDs, values, output path IDs, display names,
#'        and units.
#' @param pkParameterObserved A data.table with observed PK parameters. This parameter is optional
#'        and can be NULL if only simulated data is to be used.
#' @param ratioCalculationInputs A list containing inputs specific to the method used for calculating
#'        ratios, such as aggregation functions or simulation parameters.
#' @param ratioCalculationMethod A string that specifies the method to be used for calculating ratios.
#'        Possible values include 'byRatioAggregation' or 'byBootsTrapping'.
#' @param vlineIntercept A numeric value that specifies the intercept for any vertical lines to be drawn
#'        on the plots, typically used to indicate a reference line.
#' @param scaleVectors A list containing aesthetic parameters for the plots, such as colors, fills, and shapes
#'        for different data types (e.g., simulated vs. observed).
#' @param digitsToRound An integer specifying the number of digits to round numerical outputs to.
#' @param digitsToShow An integer specifying the number of digits to display in plot annotations.
#' @param xlabel A string representing the label to be used for the x-axis in the plots.
#' @param tableLabels A list of labels for tables generated in the plot. This should correspond to the
#'        various statistics displayed in the plot.
#'
#' @return An RmdContainer object that contains the generated plots and any related information for
#'         further processing or exporting.
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

  # switch to naming convention of forrestplot
  if (!is.null(pkParameterObserved))
  data.table::setnames(x = pkParameterObserved,
                       old = c('values','minValue','maxValue','numberOfIndividuals','parameter','errorValues'),
                       new = c('x', 'xMin', 'xMax', 'numberOfObservations','pkParameter', 'xErrorValues'),
                       skip_absent = TRUE)

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
#' It allows users to visualize the ratios based on different statistical techniques, making it easier
#' to interpret the pharmacokinetic data.
#'
#' The function supports multiple aggregation methods, including:
#' - Geometric Standard Deviations
#' - Arithmetic Standard Deviations
#' - Percentiles
#' - A user-defined custom function for aggregation
#'
#' @param projectConfiguration A ProjectConfiguration object containing project settings and parameters.
#' @param subfolder A string specifying the output directory where the plots will be saved.
#' @param configTableSheet A string representing the name of the sheet that contains configuration tables.
#' @param pkParameterDT A data.table with PK parameters, including required columns for analysis.
#' @param pkParameterObserved A data.table with observed PK parameters. This parameter is optional.
#' @param aggregationFlag A character string indicating the aggregation method. Options include:
#' - "GeometricStdDev"
#' - "ArithmeticStdDev"
#' - "Percentiles"
#' - "Custom"
#' @param percentiles A numeric vector of percentiles to calculate if aggregationFlag is "Percentiles".
#'        Default is c(5, 50, 95).
#' @param customFunction A custom function for aggregation if aggregationFlag is "Custom". Default is NULL.
#'        The function should take a numeric vector `y` as input and return a list containing:
#' - `yValues`: The aggregated value (e.g., mean).
#' - `yMin`: The lower value of the aggregated data (e.g., mean - sd).
#' - `yMax`: The upper value of the aggregated data (e.g., mean + sd).
#' @param vlineIntercept A numeric value for the intercept of the vertical line on plots.
#' @param scaleVectors A list of aesthetic parameters for the plots, including colors and shapes.
#' @param digitsToRound An integer specifying the number of digits to round in output.
#' @param digitsToShow An integer specifying the number of digits to display in plot annotations.
#' @param xlabel A string representing the label for the x-axis in the plots.
#' @param tableLabels A list of labels for tables generated in the plot.
#'
#' @return An RmdContainer object containing the generated plots and related information for further
#'         processing or exporting.
#'
#' @export
plotPKRatioForestPlotByRatioAggregation <- function(projectConfiguration,
                                                    subfolder,
                                                    configTableSheet,
                                                    pkParameterDT,
                                                    pkParameterObserved = NULL,
                                                    aggregationFlag = c("GeometricStdDev", "ArithmeticStdDev", "Percentiles", "Custom"),
                                                    percentiles = c(0.05, 0.50, 0.95),
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

      tableLabels <- formatPercentiles(percentiles[c(2,1,3)], "\npercentile")

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
#' This function generates plots for PK ratios using bootstrapping methods. It allows users to visualize
#' the variability and uncertainty in PK ratios based on simulated data.
#'
#'
#' @param description projectConfiguration A ProjectConfiguration object containing project settings and parameters.
#' @param subfolder A string specifying the output directory for the generated plots.
#' @param configTableSheet A string representing the name of the sheet that contains configuration tables.
#' @param pkParameterDT A data.table with PK parameters, including required columns for analysis.
#' @param pkParameterObserved A data.table with observed PK parameters. This parameter is optional.
#' @param coefficientOfVariation A numeric value representing the coefficient of variation for simulations.
#' @param nObservationDefault An integer representing the default number of observations for simulations.
#' @param nBootstrap An integer specifying the number of bootstrap samples to generate.
#' @param statFun A function to compute statistics from the bootstrap samples. Default is the geometric mean.
#' @param confLevel A numeric value representing the confidence level for intervals (e.g., 0.9 for 90%).
#' @param seed An integer for setting the random seed to ensure reproducibility of results.
#' @param vlineIntercept A numeric value for the intercept of the vertical line on plots.
#' @param scaleVectors A list of aesthetic parameters for the plots, including colors and shapes.
#' @param digitsToRound An integer specifying the number of digits to round in output.
#' @param digitsToShow An integer specifying the number of digits to display in plot annotations.
#' @param xlabel A string representing the label for the x-axis in the plots.
#' @param tableLabels A character vector of labels for tables generated in the plot.
#'
#' @return An RmdContainer object containing the generated plots and related information for further
#'         processing or exporting.
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
#' Creates a plot for the PK ratio based on the provided configuration. This function handles the
#' preparation of data, plotting, and exporting of the generated plot.
#'
#' @param onePlotConfig A data.table containing the plot configuration, including details such as
#'        scenario names and parameters to be plotted.
#' @param rmdContainer An RmdContainer object that will hold the generated plot and related information.
#' @param pkParameterDT A data.table with PK parameters, including the necessary columns for analysis.
#' @param pkParameterObserved A data.table with observed PK parameters. This parameter is optional.
#' @param ratioCalculationMethod A string specifying the method used for calculating ratios (e.g., 'byBootsTrapping').
#' @param ratioCalculationInputs A list containing inputs specific to the method used for calculating ratios.
#' @param vlineIntercept A numeric value for the intercept of the vertical line on plots.
#' @param scaleVectors A list of aesthetic parameters for the plots, including colors and shapes.
#' @param digitsToRound An integer specifying the number of digits to round in output.
#' @param digitsToShow An integer specifying the number of digits to display in plot annotations.
#' @param xlabel A string representing the label for the x-axis in the plots.
#' @param tableLabels A list of labels for tables generated in the plot.
#'
#' @return An updated RmdContainer object with the added plot and related information.
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
      old = c('scenarioGroup','scenarioLongName','parameterDisplayName','xMin','xMax','xErrorValues'),
      new = c('displayGroup', 'displayName', 'parameter','xmin','xmax','xrange'),
      skip_absent = TRUE
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


    # Export
    rmdContainer$addAndExportFigure(
      plotObject = plotObject,
      caption = getCaptionForRatioPlot(outputPathIdLoop = outputPathIdLoop,
                                       plotData = plotData,
                                       pkParameterDT = pkParameterDT,
                                       plotCaptionAddon = onePlotConfig$plotCaptionAddon[1]),
      figureKey = paste(onePlotConfig$plotName[1], outputPathIdLoop, sep = "-"),
      width = 30
    )
  }

  return(rmdContainer)
}

#' Prepare Data for DDI Ratio Plot
#'
#' Prepares the necessary data for generating a DDI ratio plot. This function handles the merging
#' of configuration data with observed PK parameters and calculates the ratios based on the specified
#' method.
#'
#' @param onePlotConfig A data.table containing the plot configuration for the current plot.
#' @param pkParameterDT A data.table with PK parameters that will be used for ratio calculations.
#' @param pkParameterObserved A data.table containing observed PK parameters. This parameter is optional.
#' @param ratioCalculationInputs A list containing inputs specific to the method used for calculating ratios.
#' @param ratioCalculationMethod A string specifying the method used for calculating ratios (e.g., 'byBootsTrapping').
#'
#' @return A data.table containing the prepared plot data, including calculated ratios and relevant
#'         statistics for plotting.
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
      merge(pkParameterDT[,c('parameter','displayNamePKParameter')] %>%
              unique() %>%
              data.table::setnames(old = c('parameter'),
                                   new = c('pkParameter')),
            by = 'pkParameter')

    # get rows for simulated PK
    plotDataSim <-
      switch(ratioCalculationMethod,
             byBootsTrapping =
               data.table::rbindlist(
                 lapply(dataForConfig$displayNamePKParameter,
                        function(pkIdentifier){
                          calculateRatiosByBoostrapping(
                            configList = configList,
                            pkParameterDT = pkParameterDT[displayNamePKParameter == pkIdentifier],
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
                 lapply(dataForConfig$displayNamePKParameter,
                        function(pkIdentifier){
                          calculateRatiosByAggregation(
                            configList = configList,
                            pkParameterDT = pkParameterDT[displayNamePKParameter == pkIdentifier],
                            aggregationFun = ratioCalculationInputs$aggregationFun) %>%
                            dplyr::mutate(parameterDisplayName = pkIdentifier)
                        }))

      )
    # merge with observed data
    plotData <- rbind(
      plotData,
      rbind(
        cbind(configList[,c('scenarioLongName',
                            'scenarioGroup')],
              plotDataSim
        ) %>%
          dplyr::mutate(type = 'simulated'),
        dataForConfig[!is.na(x),c('scenarioLongName',
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
  plotData$scenarioLongName <- factor(plotData$scenarioLongName,
                                         levels = unique(onePlotConfig$scenarioLongName),
                                         ordered = TRUE)

  plotData$type <- factor(plotData$type,
                          levels = unique(plotData$type), ordered = TRUE)

  plotData$scenarioGroup <- factor(plotData$scenarioGroup,
                                   levels = unique(plotData$scenarioGroup), ordered = TRUE)

  return(plotData)
}

#' Calculate Ratios for Workflow
#'
#' Calculates ratios for the specified workflow based on the provided configuration. This function
#' handles the merging of reference and scenario data and computes the ratios using the specified
#' statistical method.
#'
#' @param configList A data.table containing the configuration for the current workflow, including
#'        reference and scenario details.
#' @param pkParameterDT A data.table with PK parameters that will be used for ratio calculations.
#' @param coefficientOfVariation A numeric value representing the coefficient of variation for simulations.
#' @param nObservations An integer representing the number of observations to simulate.
#' @param nBootstrap An integer specifying the number of bootstrap samples to generate.
#' @param confLevel A numeric value representing the confidence level for intervals.
#' @param statFun A function to compute statistics from the bootstrap samples.
#' @param seed An integer for setting the random seed to ensure reproducibility of results.
#'
#' @return A data.table containing the calculated bootstrapped statistics, including the ratio and
#'         confidence intervals.
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
#' Calculates ratios for the specified workflow based on the provided configuration, using an aggregation
#' function. This function merges reference and scenario data and computes the ratios accordingly.
#'
#' @param configList A data.table containing the configuration for the current workflow, including
#'        reference and scenario details.
#' @param pkParameterDT A data.table with PK parameters that will be used for ratio calculations.
#' @param aggregationFun A function used for aggregating the ratios calculated from the data.
#'
#' @return A data.table containing the aggregated statistics, including the calculated ratio and relevant
#'         confidence intervals.
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
#' Generates a caption for the DDI ratio plot based on the output path ID and plot data. This function
#' constructs a descriptive caption that summarizes the content of the plot, including information about
#' the scenarios and parameters being visualized.
#'
#' @param outputPathIdLoop The output path ID for the current plot, used to retrieve relevant information.
#' @param plotData A data.table containing the plot data, which includes calculated ratios and parameters.
#' @param pkParameterDT A data.table with PK parameters that will be used for generating the caption.
#' @param plotCaptionAddon An optional string to append additional information to the caption.
#'
#' @return A string containing the caption for the plot, summarizing the key elements of the analysis.
#'
#' @keywords internal
getCaptionForRatioPlot <- function(outputPathIdLoop,
                                   plotData,
                                   pkParameterDT,
                                   plotCaptionAddon) {
  captiontxt = paste0('Simulated ',
                      ifelse('observed' %in% unique(plotData$type), ' and observed ', ''),
                      concatWithAnd(paste(unique(plotData$parameter), 'ratios')),
                      ' of ', pkParameterDT[outputPathId == outputPathIdLoop,]$displayNameOutput[1])

  if (!is.na(plotCaptionAddon) & plotCaptionAddon !=''){
    captiontxt = paste(captiontxt,plotCaptionAddon)
  }
  return(captiontxt)
}



# auxiliaries ---------

#' Read PK Ratio Configuration Table
#'
#' Reads and validates the PK ratio configuration table from the specified sheet in the project configuration.
#' This function ensures that the configuration data is structured correctly and contains all necessary
#' information for generating plots.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project settings and parameters.
#' @param sheetName A string representing the name of the sheet to read from the project configuration file.
#' @param pkParameterDT A data.table with PK parameters that will be used for validation against the config.
#'
#' @return A validated configuration table as a data.table, ready for use in generating plots.
#'
#' @keywords internal
readPKForestConfigTable <- function(projectConfiguration, sheetName, pkParameterDT) {
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
    charactersWithoutMissing = c("plotName","scenarioLongName","scenario"),
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
    ))


  return(configTable)
}



#' Merge Pharmacokinetic PK Parameter to config Table
#'
#' This function merges a configuration table with observed pharmacokinetic parameters. It ensures that the
#' configuration data includes relevant PK parameter information for further analysis and plotting.
#'
#' @param configTable A data.table containing the configuration data that needs to be enriched with PK parameters.
#' @param pkParameterObserved A data.table containing observed pharmacokinetic parameters. This parameter is optional.
#' @param columns A character vector of additional columns to include in the merge, such as calculated ratios.
#'
#' @return A data.table resulting from the merge operation, with missing columns added as NA where necessary.
#'
#' @keywords internal
mergePkData <- function(configTable, pkParameterObserved, columns = c('x', 'xMin', 'xMax', 'numberOfObservations')) {

  dataForConfig <- data.table::copy(configTable) %>%
    tidyr::separate_rows(pkParameter, sep = ",") %>%
    data.table::setDT() %>%
    .[,pkParameter := trimws(pkParameter)]

  if (!is.null(pkParameterObserved)) {
    # Select relevant columns including the keys and the specified columns
    relevantColumns <- unique(c('pkParameter', 'group', intersect(columns,names(pkParameterObserved))))

    dataForConfig <- merge(dataForConfig %>%
      data.table::setnames('dataGroupId', 'group'),
      pkParameterObserved[, ..relevantColumns, with = FALSE] %>%
        unique(),
      by = c('pkParameter', 'group'),
      all.x = TRUE
    )
  }

  # Add missing columns with NA values without a loop
  missingCols <- setdiff(columns, names(dataForConfig))
  dataForConfig[, (missingCols) := lapply(missingCols, function(x) NA_real_)]

  return(dataForConfig)
}
