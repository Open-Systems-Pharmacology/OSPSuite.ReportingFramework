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
#' @param colorVector A named vector specifying colors for the plots. Names should correspond to the characters
#'  defined in the configtable column colorLegend. If your color legend is for example 'DDI|control',
#'  your color vector could be colorVector = c(DDI = 'red',control = 'blue'), if no color is defined default values are used.
#' @param facetAspectRatio A numeric value specifying the aspect ratio for
#'   faceting the plots. Default is 0.5, which may need adjustment based on the
#'   number of facets and plot dimensions.
#' @param ... Additional arguments passed to plotting functions for further
#'   customization.
#' @return A list of generated plots. Each plot is an object that can be rendered
#'   using ggplot2 or similar plotting systems. The list may include both absolute
#'   and ratio plots depending on the configuration.
#'
#' @examples
#' # Example usage of plotPKBoxwhisker
#' \dontrun{
#' plotList <- plotPKBoxwhisker(
#'   projectConfiguration = myProjectConfig,
#'   onePlotConfig = myPlotConfig,
#'   pkParameterDT = myPKData,
#'   percentiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
#'   xAxisTextAngle = 45,
#'   colorVector = c(DDI = "red", control = "blue"),
#'   facetAspectRatio = 1,
#' )
#' }
#' @export
plotPKBoxwhisker <- function(projectConfiguration,
                             onePlotConfig,
                             pkParameterDT,
                             percentiles = getOspsuite.plots.option(optionKey = OptionKeys$Percentiles),
                             xAxisTextAngle = 0,
                             colorVector = c(scenario = NA, referenceScenario = NA),
                             facetAspectRatio = 0.5,
                             ...) {
  checkmate::assertNumeric(xAxisTextAngle, any.missing = FALSE)
  checkmate::assertNumeric(facetAspectRatio, any.missing = FALSE)

  colorVector <- getColorVectorForLegend(
    colorVector = colorVector,
    colorLegend = onePlotConfig[["colorLegend"]][1]
  )

  plotList <- list()
  for (plotType in c("Absolute", "Ratio")) {
    if (any(as.logical(onePlotConfig[[paste0("plot_", plotType)]]))) {
      plotList <- c(
        plotList,
        generateBoxwhiskerPlotForPlotType(
          onePlotConfig = onePlotConfig[as.logical(get(paste0("plot_", plotType))) == TRUE],
          pkParameterDT = pkParameterDT,
          percentiles = percentiles,
          xAxisTextAngle = xAxisTextAngle,
          colorVector = colorVector,
          facetAspectRatio = facetAspectRatio,
          asRatio = plotType == "Ratio",
          ...
        )
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
                                              asRatio,
                                              ...) {
  # initialize to avoid linter messages
  plotName <- plotDataPkTag <- NULL

  # Prepare data for plotting
  plotData <- prepareDataForPKBoxplot(
    onePlotConfig = onePlotConfig,
    pkParameterDT = pkParameterDT,
    colorVector = colorVector,
    asRatio = asRatio
  )
  if (nrow(plotData) == 0) {
    warning(paste("No data for", onePlotConfig$plotName[1]))
    return(list())
  }

  # Determine facet columns
  nFacetColumns <- NULL
  if (dplyr::n_distinct(plotData$plotTag) > 1) nFacetColumns <- 1

  # Initialize plot list
  plotList <- list()

  # Loop through unique plot names
  for (plotNameLoop in unique(plotData$plotName)) {
    plotDataPk <- plotData[plotName == plotNameLoop]

    plotDataSummary <- getSummaryTable(
      plotDataPk = plotDataPk,
      onePlotConfig = onePlotConfig,
      percentiles = percentiles
    )

    # Loop through yScale values
    for (yScale in splitInputs(onePlotConfig$yScale[1])) {
      plotObject <- createBaseBoxWhisker(
        plotDataPk = plotDataPk,
        yScale = yScale,
        asRatio = asRatio,
        colorVector = colorVector,
        onePlotConfig = onePlotConfig,
        ...
      )

      # Add facets
      plotObject <- addFacets(
        plotObject = plotObject,
        facetScale = onePlotConfig$facetScale[1],
        facetAspectRatio = facetAspectRatio,
        nFacetColumns = nFacetColumns
      )

      # Adjust x-axis text angle
      if (xAxisTextAngle > 0) {
        plotObject <- plotObject +
          theme(axis.text.x = element_text(angle = xAxisTextAngle, hjust = 1))
      }

      # Prepare for export
      plotObject <- setExportAttributes(
        object = plotObject,
        caption = getCaptionForBoxwhiskerPlot(
          plotDataPk = plotDataPk,
          yScale = yScale,
          percentiles = percentiles,
          plotCaptionAddon = onePlotConfig$plotCaptionAddon[1],
          asRatio = asRatio
        )
      )
      # Create figure key and store plot
      figureKey <- paste(plotNameLoop,
        ifelse(yScale == "log", "log", "linear"),
        ifelse(asRatio, "ratio", "abs"),
        sep = "-"
      )
      plotList[[figureKey]] <- plotObject
    }

    # Export table
    for (plotDataSummaryTag in split(plotDataSummary, by = "plotTag")) {
      tableKey <- paste(plotNameLoop, ifelse(asRatio, "ratio", "abs"), sep = "-")
      if (uniqueN(plotDataSummary$plotTag) > 1) {
        tableKey <- paste(tableKey, plotDataSummaryTag$plotTag[1], sep = "-")
      }

      plotList[[tableKey]] <- prepareTableForExport(
        dtExport = plotDataSummaryTag,
        plotCaptionAddon = onePlotConfig$plotCaptionAddon[1],
        asRatio = asRatio,
        plotDataPk = plotDataPk
      )
    }
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
#' @param ratioMode Mode indicating if the plot is for ratios.
#' @return A data.table prepared for plotting.
#' @keywords internal
prepareDataForPKBoxplot <- function(onePlotConfig, pkParameterDT, colorVector, asRatio) {
  # initialize to avoid linter messages
  displayNameOutput <- plotTag <- NULL

  plotData <- mergePKParameterWithConfigTable(
    onePlotConfig = onePlotConfig,
    pkParameterDT = pkParameterDT,
    colorVector = colorVector,
    asRatio = asRatio
  )

  if (dplyr::n_distinct(plotData$pkParameter) > 1) {
    plotData[, plotName := paste(plotName, pkParameter, sep = "_")]
  }

  # add Tag for faceting
  plotData[, plotTag := generatePlotTag(as.numeric(displayNameOutput))]

  return(plotData)
}
# auxiliaries  ------------

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
getSummaryTable <- function(plotDataPk, asRatio, onePlotConfig, percentiles) {
  # initialize to avoid linter messages
  value <- NULL

  # add names of box whisker limits to ggplot for use in function getBoxWhiskerLimits
  statFun <- function(y) {
    y <- y[!is.na(y)]
    rQuantiles <- stats::quantile(y, probs = percentiles, names = FALSE, na.rm = TRUE)
    names(rQuantiles) <- formatPercentiles(percentiles, suffix = " percentile")

    r <- c(
      N = length(y),
      rQuantiles,
      "arith mean" = mean(y),
      "arith standard deviation" = sd(y),
      "arith CV" = sd(y) / mean(y),
      "geo mean" = exp(mean(log(y))),
      "geo standard deviation" = exp(sd(log(y))),
      "geo CV" = sqrt(exp((log(sd(y)))^2) - 1)
    )

    return(r)
  }

  dtExport <- plotDataPk %>%
    .[, as.list(statFun(value)),
      by = intersect(c("scenarioShortName", "colorIndex", "plotTag"), names(plotDataPk))
    ]

  return(dtExport)
}

prepareTableForExport <- function(dtExport, asRatio, plotCaptionAddon, plotDataPk) {
  # initialize to avoid linter messages
  colorIndex <- value <- scenarioShortName <- scenario <- NULL

  # reorder
  setorderv(dtExport, "scenarioShortName")

  if ("colorIndex" %in% names(dtExport)) {
    dtExport[, scenario := paste(scenarioShortName, colorIndex)]
    dtExport[, scenarioShortName := NULL]
    dtExport[, colorIndex := NULL]
    dtExport$scenario <- factor(dtExport$scenario,
      levels = unique(dtExport$scenario),
      ordered = TRUE
    )
  } else {
    setnames(dtExport, old = "scenarioShortName", new = "scenario")
    dtExport$scenario <- factor(dtExport$scenario,
      levels = levels(plotDataPk$scenarioShortName),
      ordered = TRUE
    )
  }
  setcolorder(dtExport, c("scenario"))

  dtExport <- setExportAttributes(
    object = dtExport,
    caption = getCaptionForBoxwhiskerPlot(
      plotDataPk = plotDataPk,
      plotCaptionAddon = plotCaptionAddon,
      isPlotCaption = FALSE,
      asRatio = asRatio
    )
  )

  return(dtExport)
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
getCaptionForBoxwhiskerPlot <- function(plotDataPk,
                                        percentiles = NULL,
                                        yScale = NULL,
                                        plotCaptionAddon,
                                        isPlotCaption = TRUE,
                                        asRatio) {
  dtCaption <-
    plotDataPk[, c(
      "scenarioLongName",
      "displayNameOutput",
      "plotTag",
      "displayNamePKParameter",
      "displayNameOutput"
    )] %>% unique()


  captiontext <- paste(
    paste0("Population summary statistics of", ifelse(asRatio, " ratios", "")),
    "of",
    pasteFigureTags(dtCaption, captionColumn = "displayNameOutput")
  )

  captiontext <- addCaptionTextAddon(captiontext, plotCaptionAddon)

  if (isPlotCaption) {
    captiontext <- paste(
      captiontext,
      "Shown as box-whisker plot, which indicates the",
      paste(formatPercentiles(percentiles, suffix = "", allAsPercentiles = TRUE), collapse = ", "), "percentiles",
      "on a", ifelse(yScale == "linear", "linear", "logarithmic"),
      "y-scale"
    )
  }
  return(captiontext)
}

#' Create a Box and Whisker Plot Object
#'
#' This helper function generates a box and whisker plot object based on the provided
#' plot data and configuration. It supports different ratio modes and applies the
#' appropriate aesthetics and scales to the plot.
#'
#' @param plotDataPk A data frame containing the plot data for a specific PK parameter.
#' @param yScale A character string indicating the scale for the y-axis (e.g., "linear", "log").
#' @param ratioMode A character string indicating the mode of ratio to be used for plotting
#'                  (e.g., "individualRatios", "ratioOfPopulation", "none").
#' @param colorVector A vector of colors to be used for filling the plot.
#' @param onePlotConfig A list containing configuration settings for the plot.
#' @param ... Additional arguments to be passed to the plotting functions.
#'
#' @return A ggplot object representing the box and whisker plot.
#' @keywords internal
createBaseBoxWhisker <- function(plotDataPk, yScale, asRatio, colorVector, onePlotConfig, ...) {
  # initialize to avoid linter messages
  colorIndex <- value <- scenarioShortName <- NULL

  # Define the mapping for aes() based on asRatio
  aesMapping <- aes(x = scenarioShortName, y = value)
  if (!asRatio) {
    aesMapping <- structure(utils::modifyList(aesMapping, aes(fill = colorIndex)), class = "uneval")
  }

  # Create the plot object
  plotObject <- ospsuite.plots::plotBoxWhisker(
    data = plotDataPk,
    mapping = aesMapping,
    yscale = yScale,
    yscale.args = getXorYlimits(onePlotConfig, yScale, ...)
  )

  if (uniqueN(plotDataPk$colorIndex) == 1) {
    plotObject <- plotObject +
      theme(legend.position = "none")
  }

  if (!asRatio) {
    plotObject <- plotObject +
      scale_fill_manual(values = colorVector) +
      theme(legend.title = element_blank())
  }

  # Set ylabelUnit based on asRatio condition
  ylabelUnit <- if (asRatio) {
    "ratio"
  } else {
    ifelse(plotDataPk$displayUnit[1] != "",
      paste0("[", plotDataPk$displayUnit[1], "]"), ""
    )
  }
  plotObject <- plotObject +
    labs(x = "", y = paste(plotDataPk$displayNamePKParameter[1], ylabelUnit))

  return(plotObject)
}


# validation ------------
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
validatePKBoxwhiskerConfig <- function(configTable, pkParameterDT, ...) {
  # initialize to avoid linter messages
  plot_Ratio <- plot_Absolute <- referenceScenario <- value.reference <- value.base <- value <- NULL # nolint

  configTablePlots <- validateHeaders(configTable)
  validateOutputIdsForPlot()
  validateDataGroupIdsForPlot()
  validatePKParameterDT(pkParameterDT)

  validateConfigTablePlots(
    configTablePlots = configTablePlots,
    charactersWithoutMissing = c("plotName", "scenario", "scenarioShortName", "pkParameters", "outputPathIds"),
    charactersWithMissing = c("plotCaptionAddon", "colorLegend"),
    logicalColumns = c("plot_Absolute", "plot_Ratio"),
    numericRangeColumns = c("ylimit_linear", "ylimit_log"),
    subsetList = list(
      scenario = list(
        cols = c("scenario", "referenceScenario"),
        allowedValues = unique(pkParameterDT$scenario)
      ),
      pkParameter = list(
        cols = c("pkParameters"),
        allowedValues = unique(pkParameterDT$pkParameter)
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
      "pkParameters",
      "colorLegend",
      "yScale",
      "plotCaptionAddon",
      "ylimit_linear",
      "ylimit_log",
      "facetScale"
    )
  )

  tmp <- separateAndTrim(configTablePlots, "outputPathIds")
  tmp <- separateAndTrim(tmp, "pkParameters")
  tmp <- tmp[, c("plotName", "scenario", "outputPathId", "pkParameter")]
  if (any(duplicated(tmp))) {
    tmp <- duplicated(tmp)
    stop(paste(
      "Per plot only one combination of scenario, outputPathId and pkParameter is allowed. Please check plot",
      paste(tmp$plotName %>% unique(), collapse = ", ")
    ))
  }

  validateColorLegend(dt = configTablePlots[!is.na(referenceScenario)])


  tmp <- configTablePlots[as.logical(plot_Ratio) == FALSE & as.logical(plot_Absolute) == FALSE] %>% unique()
  if (nrow(tmp) > 0) {
    print(tmp)
    stop("Please select either Plot_Ratio or Plot_Absolute!")
  }


  # check if reference Scenarios are there
  validateExistenceOfReferenceForRatio(configTablePlots = configTablePlots[as.logical(plot_Ratio) == TRUE], pkParameterDT)

  # check if populations are consistent for ratio plots
  tmp <- configTablePlots[as.logical(plot_Ratio) == TRUE & !is.na(referenceScenario), c("plotName", "scenario", "referenceScenario")] %>% unique()
  validateIsCrossOverStudy(configTablePlots = tmp, pkParameterDT = pkParameterDT)

  return(invisible())
}

#' Validate Cross-Over Study
#'
#' This function checks if the provided `configTablePlots` and `pkParameterDT`
#' data frames represent a valid cross-over study by ensuring that the scenario
#' and reference scenario are based on the same population.
#'
#' @param configTablePlots A data.table containing configuration for plots,
#'                         including scenario and referenceScenario columns.
#' @param pkParameterDT A data.table containing pharmacokinetic parameters
#'                      including scenario names and population IDs.
#'
#' @return None. The function will print any invalid configurations and stop
#'         execution if the validation fails.
#' @keywords internals
validateIsCrossOverStudy <- function(configTablePlots, pkParameterDT) {
  configTablePlots <- configTablePlots %>%
    merge(
      pkParameterDT[, c("scenario", "populationId")] %>%
        unique(),
      by = "scenario",
    ) %>%
    merge(
      pkParameterDT[, c("scenario", "populationId")] %>%
        unique(),
      by.x = "referenceScenario",
      by.y = "scenario",
      suffixes = c("", ".reference")
    )

  configTablePlots <- configTablePlots[populationId != populationId.reference]
  if (nrow(configTablePlots) > 0) {
    print(configTablePlots)
    stop("Ratio plots are only available if scenario and referenceScenario is based on the same population")
  }
}
#' Validate Existence of Reference for Ratio Plots
#'
#' This function checks if there are valid reference scenarios for the plots
#' specified in `configTablePlots`. It ensures that at least one reference
#' scenario is selected for each plot name.
#'
#' @param configTablePlots A data.table containing configuration for plots,
#'                         including referenceScenario and plotName columns.
#' @param pkParameterDT A data.table containing pharmacokinetic parameters
#'                      including scenario names and population IDs (not used
#'                      in this function, but may be relevant for context).
#'
#' @return None. The function will stop execution if validation fails,
#'         otherwise returns invisibly.
#' @keywords internal
validateExistenceOfReferenceForRatio <- function(configTablePlots, pkParameterDT) {
  # initialize to avoid linter messages
  isValid <- plotName <- referenceScenario <- populationId.reference <- populationId <- NULL

  if (nrow(configTablePlots) == 0) {
    return(invisible())
  }

  # check if reference Scenarios are there
  tmp <- configTablePlots[, .(isValid = any(!is.na(referenceScenario))), by = plotName]
  if (any(tmp$isValid == FALSE)) {
    stop(paste(
      "For ratio plots at lease one reference scenario has to be selected. Check PlotName",
      paste(tmp[isValid == FALSE, ]$plotName, collapse = ", ")
    ))
  }

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
  # initialize to avoid linter messages
  pkParameters <- outputPathIds <- outputPathId <- parameter <- scenarioName <- NULL

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
  dt <- pkParameterDT[, .(pkParameters = paste(unique(pkParameter), collapse = ", ")), by = outputPathId] %>%
    .[, .(outputPathIds = paste(unique(outputPathId), collapse = ", ")), by = pkParameters]

  # Create a new data.table with all combinations of pkParameters and scenario names
  dtNewConfig <- dt[, .(
    plotName = paste(splitInputs(pkParameters), collapse = "_"),
    scenario = scenarios$scenarioName,
    yScale = "linear, log",
    facetScale = "fixed",
    plot_Absolute = 1,
    plot_Ratio = 0
  ),
  by = .(outputPathIds, pkParameters)
  ]


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
