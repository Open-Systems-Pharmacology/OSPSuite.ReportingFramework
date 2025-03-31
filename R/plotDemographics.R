#' Plot Demographics
#'
#' This function generates demographic plots based on the provided configuration and data.
#'
#' @param projectConfiguration A configuration object for the project.
#' @param onePlotConfig A configuration for the specific plot.
#' @param pkParameterDT A data.table containing PK parameter data (optional).
#' @param scenarioList A list of scenarios to consider (optional).
#' @param asStepPlot Logical indicating if the plot should be a step plot (default is TRUE).
#' @param aggregationFlag A character vector indicating the type of aggregation function to use
#' for simulated data. Options include "GeometricStdDev" (Default), "ArithmeticStdDev", "Percentiles",
#' and "Custom".
#'
#' @param percentiles A numeric vector of percentiles to consider if the aggregation method is set
#' to "Percentiles" (default is c(5, 50, 95)).
#'
#' @param customFunction An optional custom function for aggregation.
#' A custom function should take a numeric vector `y` as input and return a list containing:
#'
#' - `yValues`: The aggregated value (e.g., mean).
#'
#' - `yMin`: The lower value of the aggregated data, (e.g. mean - sd).
#'
#' - `yMax`: The upper value of the aggregated data, (e.g. mean + sd).
#'
#' - `yErrorType`: A string indicating the type of error associated with the aggregation,
#' it is used in plot legends and captions. It must be a concatenation of the descriptor of yValues and the descriptor of yMin - yMax range
#' separated by "|" (e.g., "mean | standard deviation" or "median | 5th - 95th percentile").
#' @param facetAspectRatio A numeric value for the aspect ratio of the facets (default is 0.5).
#' @param colorVector A named vector for colors corresponding to scenarios (default is c(scenario = NA, referenceScenario = NA)).
#' @param nMaxFacetRows Maximum number of facet rows (default is 2).
#' @param ... Additional arguments passed to plotting function ospsuite.plots::plotHistogram or for rangePlots plotRangeDistribution.
#' @return A list of plot objects generated based on the input configuration.
#' @export
plotDemographics <- function(projectConfiguration,
                             onePlotConfig,
                             pkParameterDT = NULL,
                             scenarioList = NULL,
                             asStepPlot = TRUE,
                             aggregationFlag = c(
                               "Percentiles",
                               "GeometricStdDev",
                               "ArithmeticStdDev",
                               "Custom"
                             ),
                             customFunction = NULL,
                             percentiles = opsuite.plots::getOspsuite.plots.option(optionKey = OptionKeys$Percentiles)[c(1, 3, 5)],
                             facetAspectRatio = 0.5,
                             colorVector = c(scenario = NA, referenceScenario = NA),
                             nMaxFacetRows = 2,
                             ...) {
  # initialize data.table variables
  dataType <- .Id <- value <- categoricValue <- NULL # nolint

  checkmate::assertFlag(asStepPlot)
  checkmate::assertNumeric(facetAspectRatio, lower = 0, finite = TRUE, len = 1)
  colorVector <- getColorVectorForLegend(
    colorVector = colorVector,
    colorLegend = onePlotConfig[["colorLegend"]][1]
  )

  asRangePlot <- any(!is.na(onePlotConfig$parameterId_Bin))
  usePKParameter <- !any(splitInputs(onePlotConfig$parameterIds)
  %in% configEnv$modelParameter$parameterId)

  checkmate::assertList(scenarioList, types = "Scenario", null.ok = usePKParameter & !asRangePlot)
  if (usePKParameter) validatePKParameterDT(pkParameterDT)

  plotData <- prepareDemographicPlotData(
    onePlotConfig = onePlotConfig,
    pkParameterDT = pkParameterDT,
    scenarioList = scenarioList,
    usePKParameter = usePKParameter,
    asRangePlot = asRangePlot,
    colorVector = colorVector
  )

  if (asRangePlot) {
    # read aggregation function
    aggregationFlag <- match.arg(aggregationFlag)
    aggregationFun <- getAggregationFunction(aggregationFlag, percentiles, customFunction, legendsize = 3)

    plotList <- generateRangePlots(
      onePlotConfig = onePlotConfig,
      plotData = plotData[!is.na(value)],
      colorVector = colorVector,
      facetAspectRatio = facetAspectRatio,
      asStepPlot = asStepPlot,
      aggregationFun = aggregationFun
    )

    if ("categoricValue" %in% names(plotData)) {
      warning(paste(
        "Categoric parameter are not suited for range plots and will be ignored:",
        concatWithAnd(unique(plotData[!is.na(categoricValue), ]$parameterId))
      ))
    }
  } else {
    plotList <- generateHistograms(
      onePlotConfig = onePlotConfig,
      plotData = plotData,
      colorVector = colorVector,
      facetAspectRatio = facetAspectRatio,
      nMaxFacetRows = nMaxFacetRows,
      ...
    )
  }

  return(plotList)
}
#' Prepare Demographic Plot Data
#'
#' Prepares the data for demographic plotting based on the provided configuration and parameters.
#'
#' @param onePlotConfig A configuration for the specific plot.
#' @param pkParameterDT A data.table containing PK parameter data (optional).
#' @param scenarioList A list of scenarios to consider (optional).
#' @param usePKParameter Logical indicating if PK parameters are to be used.
#' @param asRangePlot Logical indicating if the plot should be a range plot.
#' @param colorVector A named vector for colors corresponding to scenarios.
#' @return A data.table containing the prepared data for plotting.
#' @keywords internal
prepareDemographicPlotData <- function(onePlotConfig,
                                       pkParameterDT,
                                       scenarioList,
                                       usePKParameter,
                                       asRangePlot,
                                       colorVector) {
  # initialize to avoid linter messages
  scenarioType <- referenceScenario <- scenarioType <- scenario <- NULL

  if (usePKParameter) {
    onePlotConfigIdentifier <- copy(onePlotConfig) %>%
      setnames("parameterIds", "pkParameters")
    onePlotConfigIdentifier <- rbind(
      onePlotConfigIdentifier[, !("referenceScenario"), with = FALSE],
      onePlotConfigIdentifier[!is.na(referenceScenario), !("scenario"), with = FALSE] %>%
        setnames("referenceScenario", "scenario")
    ) %>%
      unique()

    plotData <- mergePKParameterWithConfigTable(
      onePlotConfig = onePlotConfigIdentifier,
      pkParameterDT = pkParameterDT,
      colorVector = colorVector,
      asRatio = FALSE
    )

    # make names consistent
    setnames(plotData,
      old = c("pkParameter", "displayNamePKParameter", "displayUnitPKParameter"),
      new = c("parameterId", "displayNameParameter", "displayUnit"),
      skip_absent = TRUE
    )
  } else {
    plotData <- loadDemographicParameters(onePlotConfig, scenarioList)
  }


  if (asRangePlot) {
    plotData <- addDemographicsToBin(
      plotData = plotData,
      onePlotConfig = onePlotConfig,
      scenarioList = scenarioList
    )

    plotData[, scenarioType := factor(ifelse(scenario == onePlotConfig$referenceScenario,
      names(colorVector)[2],
      names(colorVector)[1]
    ), levels = names(colorVector))]
  } else {
    if (any(!is.na(onePlotConfig$referenceScenario))) {
      # for the facet plots reference scenario has to be duplicated
      plotData <-
        rbind(
          plotData[!(scenario %in% onePlotConfig$referenceScenario)] %>%
            .[, scenarioType := names(colorVector)[1]],
          merge(onePlotConfig[, c("scenario", "referenceScenario")],
            plotData[scenario %in% onePlotConfig$referenceScenario],
            by.x = "referenceScenario",
            by.y = "scenario", allow.cartesian = TRUE
          ) %>%
            .[, referenceScenario := NULL] %>%
            .[, scenarioType := names(colorVector)[2]]
        )
    } else {
      plotData[, scenarioType := names(colorVector)[1]]
    }
  }

  plotData <- setPlotTag(
    plotData = plotData, asRangePlot = asRangePlot,
    usePKParameter = usePKParameter
  )

  return(plotData) # Return the prepared data
}
#' Generate Histograms
#'
#' Generates histogram plots based on the provided configuration and data.
#'
#' @param onePlotConfig A configuration for the specific plot.
#' @param plotData A data.table containing the plot data.
#' @param colorVector A named vector for colors corresponding to scenarios.
#' @param facetAspectRatio A numeric value for the aspect ratio of the facets.
#' @param nMaxFacetRows Maximum number of facet rows.
#' @param ... Additional arguments passed to the histogram plotting function.
#' @return A list of histogram plot objects.
#' @keywords internal
generateHistograms <- function(onePlotConfig,
                               plotData,
                               colorVector,
                               facetAspectRatio,
                               nMaxFacetRows,
                               ...) {
  # initialize to avoid linter messages
  plotTag <- scenarioType <- value <- categoricValue <- parameterId <- NULL

  plotList <- list()
  for (pid in splitInputs(onePlotConfig$parameterIds)) {
    idData <- plotData[parameterId == pid]
    xlabel <- idData$displayNameParameter[1]
    xlabelUnit <- idData$displayUnit[1]

    if ("categoricValue" %in% names(idData) &&
      any(!is.na(idData$categoricValue))) {
      mapping <- aes(x = categoricValue, groupby = scenarioType)
    } else {
      mapping <- aes(x = value, groupby = scenarioType)
    }

    for (xscale in splitInputs(onePlotConfig$scale[1])) {
      plotObject <-
        ospsuite.plots::plotHistogram(
          data = idData,
          mapping = mapping,
          xscale = xscale,
          ...
        ) +
        facet_wrap(vars(plotTag)) +
        labs(x = constructLabelWithUnit(xlabel, xlabelUnit)) +
        scale_fill_manual(values = colorVector) +
        theme(legend.title = element_blank())

      if (idData[, uniqueN(scenarioType)] == 1) {
        plotObject <- plotObject + theme(legend.position = "none")
      }

      nFacetColumns <- getNFacetsForDemographics(
        idData = idData,
        isRangePlot = FALSE,
        nMaxFacetRows = nMaxFacetRows
      )

      plotObject <- addFacets(plotObject,
        onePlotConfig$facetScale[1],
        facetAspectRatio = facetAspectRatio,
        nFacetColumns = nFacetColumns
      )

      # Prepare for export
      plotObject <- setExportAttributes(
        object = plotObject,
        caption = getCaptionForDemographicPlot(
          idData = idData,
          valueLabel = xlabel,
          binLabel = NULL,
          valueScale = xscale,
          plotCaptionAddon = onePlotConfig$plotCaptionAddon[1]
        )
      )

      # prepare export
      figureKey <- paste(onePlotConfig$plotName[1], pid, xscale, sep = "_")
      plotList[[figureKey]] <- plotObject
    }
  }

  return(plotList)
}
#' Generate Range Plots
#'
#' Generates range plots based on the provided configuration and data.
#'
#' @param onePlotConfig A configuration for the specific plot.
#' @param plotData A data.table containing the plot data.
#' @param colorVector A named vector for colors corresponding to scenarios.
#' @param facetAspectRatio A numeric value for the aspect ratio of the facets.
#' @param asStepPlot Logical indicating if the plot should be a step plot.
#' @param aggregationFun A function for aggregating the data for plotting.
#' @param ... Additional arguments passed to the range plotting function.
#' @return A list of range plot objects.
#' @keywords internal
generateRangePlots <- function(onePlotConfig,
                               plotData,
                               colorVector,
                               facetAspectRatio,
                               asStepPlot,
                               aggregationFun,
                               ...) {
  # initialize to avoid linter messages
  plotTag <- scenarioType <- value <- value.bin <- parameterId <- NULL #nolint

  # convert the aggreagtion function so that it suited as input for plotRange
  statFun <- function(y) {
    l <- aggregationFun(y)
    if (l$yErrorType == ospsuite::DataErrorType$ArithmeticStdDev) {
      l$yMin <- l$yValues - l$yErrorValues
      l$yMax <- l$yValues + l$yErrorValues
    } else if (l$yErrorType == ospsuite::DataErrorType$GeometricStdDev) {
      l$yMin <- l$yValues / l$yErrorValues
      l$yMax <- l$yValues * l$yErrorValues
    }
    return(c(
      y = l$yValues,
      ymin = l$yMin,
      ymax = l$yMax
    ))
  }
  if (onePlotConfig$modeOfBinning[1] == BINNINGMODE$breaks) {
    breaks <- eval(parse(text = onePlotConfig$numberOfBins[1]))
    numberOfBins <- NA
  } else {
    breaks <- NA
    numberOfBins <- onePlotConfig$numberOfBins[1]
  }

  plotList <- list()
  for (pid in splitInputs(onePlotConfig$parameterIds)) {
    idData <- plotData[parameterId == pid]
    xlabel <- idData[!is.na(value.bin)]$displayNameParameter.bin[1]
    xlabelUnit <- idData[!is.na(value.bin)]$displayUnit.bin[1]
    ylabel <- idData$displayNameParameter[1]
    ylabelUnit <- idData$displayUnit[1]


    # initialize dtExport (will be the set only once per pid)
    dtExport <- NULL

    for (yscale in splitInputs(onePlotConfig$scale[1])) {
      plotObject <-
        plotRangeDistribution(
          data = idData,
          mapping = aes(x = value.bin, y = value, groupby = scenarioType),
          modeOfBinning = onePlotConfig$modeOfBinning[1],
          numberOfBins = numberOfBins,
          breaks = breaks,
          yscale = yscale,
          asStepPlot = asStepPlot,
          statFun = statFun,
          identifier = "individualId",
          ...
        ) +
        labs(
          x = constructLabelWithUnit(xlabel, xlabelUnit),
          y = constructLabelWithUnit(ylabel, ylabelUnit)
        ) +
        scale_fill_manual(values = colorVector) +
        scale_color_manual(values = colorVector) +
        theme(legend.title = element_blank())

      if (idData[, uniqueN(scenarioType)] == 1) {
        plotObject <- plotObject + theme(legend.position = "none")
      }

      nFacetColumns <- getNFacetsForDemographics(idData = idData, isRangePlot = TRUE)
      plotObject <- addFacets(plotObject,
        onePlotConfig$facetScale[1],
        facetAspectRatio = facetAspectRatio,
        nFacetColumns = nFacetColumns
      )

      if (is.null(dtExport)) {
        dtExport <- getExportTableForRanges(
          plotObject = plotObject,
          aggregationFun = aggregationFun,
          xLabel = constructLabelWithUnit(xlabel, xlabelUnit)
        )
      }

      # Prepare for export
      plotObject <- setExportAttributes(
        object = plotObject,
        caption = getCaptionForDemographicPlot(
          idData = idData,
          valueLabel = ylabel,
          binLabel = xlabel,
          valueScale = yscale,
          plotCaptionAddon = onePlotConfig$plotCaptionAddon[1]
        ),
        footNoteLines = getFootnoteLinesForRangePlots(attr(dtExport, "errorLabels"))
      )

      # prepare export
      figureKey <- paste(onePlotConfig$plotName[1], pid, yscale, sep = "_")
      plotList[[figureKey]] <- plotObject
    }
    # Export table
    for (dtExportTag in split(dtExport, by = "plotTag")) {
      tableKey <- paste(onePlotConfig$plotName[1], pid, sep = "_")
      if (uniqueN(dtExport$plotTag) > 1) tableKey <- paste(tableKey, dtExportTag$plotTag[1], sep = "_")

      dtExportTag <- setExportAttributes(
        object = dtExportTag[, !c("plotTag", "scenarioType"), with = FALSE],
        caption = getCaptionForDemographicPlot(
          idData = idData[plotTag == dtExportTag$plotTag[1]],
          valueLabel = ylabel,
          binLabel = xlabel,
          valueScale = NULL,
          plotCaptionAddon = onePlotConfig$plotCaptionAddon[1]
        )
      )

      plotList[[tableKey]] <- dtExportTag
    }
  }

  return(plotList)
}

# auxiliaries ---------------
#' Load Demographic Parameters
#'
#' Loads demographic parameters from the configuration and scenario list.
#'
#' @param onePlotConfig A configuration for the specific plot.
#' @param scenarioList A list of scenarios to consider.
#' @return A data.table containing the loaded demographic parameters.
#' @keywords internal
loadDemographicParameters <- function(onePlotConfig, scenarioList) {
  # initialize to avoid linter messages
  referenceScenario <- NULL

  onePlotConfigIdentifier <- onePlotConfig[, c("scenario", "referenceScenario", "parameterIds")] %>%
    separateAndTrim(columnName = "parameterIds") %>%
    merge(configEnv$modelParameter, by = "parameterId")

  # load parameter_id values
  scenarioNames <- unique(c(
    onePlotConfigIdentifier$scenario,
    onePlotConfigIdentifier[!is.na(referenceScenario)]$referenceScenario
  ))

  # Combine the results into a single data.table
  plotData <- rbindlist(
    lapply(scenarioNames, function(scName) {
      loadPopulationParameterForScenario(
        scenarioName = scName,
        scenarioList = scenarioList,
        onePlotConfigIdentifier = onePlotConfigIdentifier
      )
    }),
    use.names = TRUE, fill = TRUE
  )

  plotData$parameterId <- factor(plotData$parameterId,
    levels = unique(splitInputs(onePlotConfig$parameterIds)),
    ordered = TRUE
  )
  plotData$scenario <- factor(plotData$scenario,
    levels = scenarioNames,
    ordered = TRUE
  )

  # make names consistent
  setnames(plotData,
    old = c("displayNameModelParameter"),
    new = c("displayNameParameter"),
    skip_absent = TRUE
  )

  return(plotData)
}
#' Add Demographics to Bin
#'
#' Adds demographic data to bins for plotting.
#'
#' @param plotData A data.table containing the plot data.
#' @param onePlotConfig A configuration for the specific plot.
#' @param scenarioList A list of scenarios to consider.
#' @return A data.table containing the updated plot data with demographic bins.
#' @keywords internal
addDemographicsToBin <- function(plotData = plotData,
                                 onePlotConfig = onePlotConfig,
                                 scenarioList = scenarioList) {
  onePlotConfigIdentifier <- onePlotConfig[, c("scenario", "parameterId_Bin")] %>%
    setnames(old = "parameterId_Bin", new = "parameterId") %>%
    merge(configEnv$modelParameter, by = "parameterId")

  scenarioNames <- unique(onePlotConfigIdentifier$scenario)
  plotDataBin <- rbindlist(
    lapply(scenarioNames, function(scName) {
      loadPopulationParameterForScenario(
        scenarioName = scName,
        scenarioList = scenarioList,
        onePlotConfigIdentifier = onePlotConfigIdentifier
      )
    }),
    use.names = TRUE, fill = TRUE
  ) %>%
    setnames(old = "parameterId", new = "parameterId_Bin")

  if (all(is.na(plotData$value)) & "categoricValue" %in% names(plotData)) {
    stop(paste("Categoric Values are not allowed for x-axis on rangeplots. Check plotName", onePlotConfig$plotName[1]))
  }

  plotData <- plotData %>%
    merge(plotDataBin,
      by = c("scenario", "individualId"),
      suffixes = c("", ".bin"),
      all.x = TRUE
    )

  setnames(plotData,
    old = c("displayNameModelParameter"),
    new = c("displayNameParameter.bin"),
    skip_absent = TRUE
  )

  return(plotData)
}
#' Set Plot Tag
#'
#' Assigns plot tags to the data for plotting.
#'
#' @param plotData A data.table containing the plot data.
#' @param asRangePlot Logical indicating if the plot is a range plot.
#' @param usePKParameter Logical indicating if PK parameters are to be used.
#' @return A data.table containing the updated plot data with assigned plot tags.
#' @keywords internal
setPlotTag <- function(plotData, asRangePlot, usePKParameter) {
  # initialize to avoid linter messages
  plotTag <- NULL

  plotTagIdentifier <- c()
  if (usePKParameter) {
    plotTagIdentifier <- "displayNameOutput"
  }
  if (!asRangePlot) {
    plotTagIdentifier <- c("scenario", plotTagIdentifier)
  }
  if (length(plotTagIdentifier) > 0) {
    dtPlotTag <- plotData[, ..plotTagIdentifier] %>% #nolint
      unique() %>%
      setorderv(plotTagIdentifier)
    dtPlotTag[, plotTag := generatePlotTag(.I)]
    plotData <- merge(plotData, dtPlotTag, by = plotTagIdentifier)
  } else {
    plotData[, plotTag := generatePlotTag(1)]
  }

  return(plotData)
}
#' Get Export Table for Ranges
#'
#' Generates an export table for range plots.
#'
#' @param plotObject A plot object generated from the plotting functions.
#' @param aggregationFun A function for aggregating the data for exporting.
#' @param xLabel A label for the x-axis.
#' @return A data.table containing the export data for range plots.
#' @keywords internal
getExportTableForRanges <- function(plotObject, aggregationFun, xLabel) {
  # initialize to avoid linter messages
  bin <- .bin <- scenarioType <- yErrorType <- value <- breaks <- NULL

  dtExport <- unique(setDT(plotObject$data[, c(".bin", "plotTag", "scenarioType", "value", "individualId")]))[, as.list(c(
    list(N = length(!is.na(value))),
    aggregationFun(value)
  )), by = c(".bin", "plotTag", "scenarioType")]

  errorLabels <- getErrorLabels(dtExport$yErrorType[1])
  setnames(dtExport,
    old = intersect(
      c("yValues", "yErrorValues", "yMin", "yMax"),
      names(dtExport)
    ),
    new = errorLabels,
    skip_absent = TRUE
  )

  dtExport[, yErrorType := NULL]

  border <- plotObject$border

  border[, bin := paste(breaks, "-", shift(breaks, type = "lead"))]
  border <- border[!duplicated(.bin)]

  dtExport <- merge(dtExport,
    border[, c("bin", ".bin", "medianX")],
    by = ".bin",
    all.x = TRUE
  )

  dtExport[is.na(bin), bin := scenarioType]

  dtExport$bin <- factor(dtExport$bin,
    levels = unique(dtExport[order(.bin)]$bin),
    ordered = TRUE
  )

  dtExport[, .bin := NULL]
  setcolorder(dtExport, c("bin", "medianX"))
  setnames(dtExport,
    old = c("bin", "medianX"),
    new = c(paste(xLabel, "range"), paste(xLabel, "median"))
  )

  setattr(dtExport, "errorLabels", errorLabels)

  return(dtExport = dtExport)
}
#' Load Population Parameter for Scenario
#'
#' Loads population parameters for a specific scenario.
#'
#' @param scenarioName The name of the scenario.
#' @param scenarioList A list of scenarios to consider.
#' @param onePlotConfigIdentifier A configuration identifier for the plot.
#' @return A data.table containing the loaded population parameters.
#' @keywords internal
loadPopulationParameterForScenario <- function(scenarioName, scenarioList, onePlotConfigIdentifier) {
  # initialize to avoid linter messages
  modelPath <- value <- categoricValue <- scenario <- NULL

  dtPop <- ospsuite::populationToDataFrame(scenarioList[[scenarioName]]$population) %>% setDT()
  modelPaths <- unique(onePlotConfigIdentifier$modelPath)

  if (!all(modelPaths %in% names(dtPop))) {
    stop(paste(
      "Parameter path(s)", paste(setdiff(modelPaths, names(dtPop)), collapse = ", "),
      "is not available for", scenarioName
    ))
  }

  dtPop <- dtPop[, c("IndividualId", ..modelPaths)][ # nolint
    , scenario := scenarioName
  ] %>%
    setnames("IndividualId", "individualId")

  numericColumns <- setdiff(names(dtPop)[sapply(dtPop, is.numeric)], "individualId")
  characterColumns <- setdiff(names(dtPop)[sapply(dtPop, Negate(is.numeric))], "scenario")

  if (length(characterColumns) > 0) {
    dtPopc <- dtPop[, melt(.SD,
      measure.vars = characterColumns,
      variable.name = "modelPath",
      value.name = "categoricValue"
    )][
      configEnv$modelParameter,
      on = .(modelPath), nomatch = 0
    ]
    dtPopc[, categoricValue := factor(categoricValue)]
    dtPop <- dtPop[, !(characterColumns), with = FALSE]
    dtPopc <- dtPopc[, !(numericColumns), with = FALSE]
  } else {
    dtPopc <- data.table()
  }

  dtPop <- dtPop[, melt(.SD, measure.vars = numericColumns, variable.name = "modelPath", value.name = "value")][
    configEnv$modelParameter,
    on = .(modelPath), nomatch = 0
  ]

  for (d in split(unique(dtPop[, c("modelPath", "displayUnit")]), by = "modelPath")) {
    par <- ospsuite::getParameter(d$modelPath[1], scenarioList[[scenarioName]]$simulation)

    if (par$unit != d$displayUnit[1]) {
      unitFactor <- ospsuite::toUnit(
        quantityOrDimension = par$dimension,
        values = 1,
        targetUnit = d$displayUnit[1],
        sourceUnit = par$unit
      )
      dtPop[modelPath == d$modelPath[1], value := value * unitFactor]
    }
  }

  # add character columns
  dtPop <- rbind(dtPop, dtPopc, fill = TRUE)

  dtPop[, modelPath := NULL]
  return(dtPop)
}
#' Get Caption for Demographic Plot
#'
#' Generates a caption for demographic plots based on the provided data.
#'
#' @param idData A data.table containing the plot data.
#' @param valueLabel A label for the value axis.
#' @param binLabel A label for the bin axis (optional).
#' @param valueScale A scale type for the value axis (optional).
#' @param plotCaptionAddon Additional text to append to the caption (optional).
#' @return A string containing the generated caption for the plot.
#' @keywords internal
getCaptionForDemographicPlot <- function(
    idData,
    valueLabel,
    binLabel,
    valueScale,
    plotCaptionAddon) {
  if ("displayNameOutput" %in% names(idData)) {
    dtCaption <-
      idData[, c(
        "displayNameOutputs",
        "plotTag"
      )] %>% unique()

    outputText <- paste(" of", pasteFigureTags(dtCaption, captionColumn = "displayNameOutputs"))
  } else {
    outputText <- ""
  }

  scaletxt <- ""
  if (!is.null(valueScale)) {
    scaletxt <- paste0(
      " on a ", ifelse(valueScale == "linear", "linear", "logarithmic"),
      ifelse(!is.null(binLabel), " y", " x"), "-scale."
    )
  }

  captiontxt <- paste0(
    "Simulated ", valueLabel, ifelse(!is.null(binLabel), " dependency", ""),
    outputText, ifelse(!is.null(binLabel), paste(" vs", binLabel), ""),
    scaletxt
  )

  captiontxt <- addCaptionTextAddon(captiontxt, plotCaptionAddon)
  return(captiontxt)
}
#' Get Footnote Lines for Range Plots
#'
#' Generates footnote lines for range plots based on error labels.
#'
#' @param errorLabels A vector of error labels to include in the footnotes.
#' @return A string containing the footnote lines for the plot.
#' @keywords internal
getFootnoteLinesForRangePlots <- function(errorLabels) {
  footnoteLines <-
    paste0(
      "Data is displayed as ",
      concatWithAnd(errorLabels),
      "."
    )

  return(footnoteLines)
}
#' Get Number of Facets for Demographics
#'
#' Determines the number of facets to use for demographic plots.
#'
#' @param idData A data.table containing the plot data.
#' @param isRangePlot Logical indicating if the plot is a range plot.
#' @param nMaxFacetRows Maximum number of facet rows (default is 2).
#' @return The number of facet columns to use.
#' @keywords internal
getNFacetsForDemographics <- function(idData, isRangePlot, nMaxFacetRows = 2) {
  # initialize to avoid linter messages
  scenario <- displayNameOutput <- plotTag <- NULL

  nFacetColumns <- NULL

  if (idData[, uniqueN(plotTag)] > 1) {
    if (isRangePlot) {
      nFacetColumns <- 1
    } else if ("displayNameOutput" %in% names(idData) &&
      idData[, uniqueN(scenario)] > 1 &&
      idData[, uniqueN(displayNameOutput)] > 1) {
      nFacetColumns <- idData[, uniqueN(displayNameOutput)]
    } else {
      nFacetColumns <- ceiling(idData[, uniqueN(scenario)] / nMaxFacetRows)
    }
  }
  return(nFacetColumns)
}
# validation ----------
#' Validate Demographics Configuration Table
#'
#' Validates the configuration table for PK box-and-whisker plots.
#'
#' @param configTable A data.table containing the configuration table.
#' @param pkParameterDT A data.table containing PK parameter data.
#' @param ... Additional arguments for validation.
#' @return NULL (invisible).
#' @export
validateDemographicsConfig <- function(configTable, ...) {
  # initialize to avoid linter messages
  referenceScenario <- NULL

  configTablePlots <- validateHeaders(configTable)
  validateOutputIdsForPlot()

  validateConfigTablePlots(
    configTablePlots = configTablePlots,
    charactersWithoutMissing = c("plotName", "scenario", "parameterIds"),
    charactersWithMissing = c("plotCaptionAddon", "colorLegend", "referenceScenario"),
    numericRangeColumns = c("limit_linear", "limit_log"),
    subsetList = list(
      scenario = list(
        cols = c("scenario"),
        allowedValues = configEnv$scenarios$scenarioName
      ),
      referenceScenario = list(
        cols = c("referenceScenario"),
        allowedValues = configEnv$scenarios$scenarioName,
        splitAllowed = FALSE
      ),
      parameterId_Bin = list(
        cols = c("ParameterId_Bin"),
        allowedValues = configEnv$modelParameter$parameterId,
        splitAllowed = FALSE
      ),
      modeOfBinning = list(
        cols = c("modeOfBinning"),
        allowedValues = unlist(unname(BINNINGMODE))
      ),
      yscale = list(
        cols = c("scale"),
        allowedValues = c("linear", "log")
      ),
      facetScale = list(
        cols = c("facetScale"),
        allowedValues = c("fixed", "free", "free_x", "free_y")
      )
    )
  )


  # check for rangePlots
  validateRangePlotsColumns(configTablePlots)

  # check for ParameterID
  validateParameterID(configTablePlots, ...)

  # check if columns are consistent for plotName
  validateGroupConsistency(
    dt = configTablePlots,
    valueColumns = c(
      "plotCaptionAddon",
      "outputPathIds",
      "colorLegend",
      "parameterIds",
      "outputPathIds",
      "parameterId_Bin",
      "modeOfBinning",
      "numberOfBins",
      "plotCaptionAddon",
      "scale",
      "limit_linear",
      "limit_log",
      "facetScale"
    )
  )

  validateColorLegend(dt = configTablePlots[!is.na(referenceScenario)])

  return(invisible())
}
#' Validate Range Plots Columns
#'
#' Validates the columns used for range plots in the configuration table.
#'
#' @param configTablePlots A data.table containing the configuration table for plots.
#' @return NULL (invisible).
#' @keywords internal
validateRangePlotsColumns <- function(configTablePlots) {
  # initialize to avoid linter messages
  modeOfBinning <- NULL

  if (any(!is.na(configTablePlots$parameterId_Bin))) {
    if (any(is.na(configTablePlots$parameterId_Bin))) {
      stop(paste("Please do not mix range plots and histogram in oneplot:", configTablePlots$plotName[1]))
    }

    checkmate::assertNumeric(unique(configTablePlots[modeOfBinning != BINNINGMODE$breaks]$numberOfBins),
      lower = 2, finite = TRUE, len = 1,
      .var.name = paste("Plot configuration column NumberfBins")
    )
    validateNumericVectorColumns("numberOfBins", configTablePlots[modeOfBinning == BINNINGMODE$breaks])
  } else {
    validateConfigTablePlots(
      configTablePlots = configTablePlots,
    )
  }

  return(invisible())
}
#' Validate Parameter ID
#'
#' Validates the parameter IDs in the configuration table for plots.
#'
#' @param configTablePlots A data.table containing the configuration table for plots.
#' @param ... Additional arguments for validation.
#' @return NULL (invisible).
#' @keywords internal
validateParameterID <- function(configTablePlots, ...) {
  if (any(splitInputs(configTablePlots$parameterIds) %in% configEnv$modelParameter$parameterId)) {
    validateConfigTablePlots(
      configTablePlots = configTablePlots,
      subsetList = list(
        ParameterIds = list(
          cols = c("ParameterIds"),
          allowedValues = configEnv$modelParameter$parameterId
        )
      )
    )
  } else {
    dotarg <- list(...)
    if (!("pkParameterDT") %in% names(dotarg)) {
      stop(paste(
        "The ParameterIds are no valid modelparameters!
                 Are they PK-Parameter? But pkParameterDT is missing as input.",
        configTablePlots$plotName[1]
      ))
    }

    validatePKParameterDT(dotarg$pkParameterDT)

    validateConfigTablePlots(
      configTablePlots = configTablePlots,
      charactersWithoutMissing = c("outputPathIds"),
      subsetList = list(
        ParameterIds = list(
          cols = c("ParameterIds"),
          allowedValues = unique(dotarg$pkParameterDT$parameter)
        ),
        outputPathId = list(
          cols = c("outputPathIds"),
          allowedValues = unique(dotarg$pkParameterDT$outputPathId)
        )
      )
    )
  }
  return(invisible())
}
# support usability --------------------
#' Add Default Configuration for Pediatric Demography Plots
#'
#' Adds default configurations for Pediatric demography plots to the `Plots.xlsx` configuration file.
#'
#' @param projectConfiguration A ProjectConfiguration object.
#' @param sheetName Name of the sheet to create.
#' @param overwrite Logical indicating if existing data should be overwritten.
#' @return NULL (invisible).
#' @export
addDefaultDemographicPlots <- function(projectConfiguration,
                                       pkParameterDT = NULL,
                                       sheetName = "DemographicPlots",
                                       overwrite = FALSE) {
  # initialize to avoid linter messages
  scenarioName <- outputPathIds <- outputPathId <- plotName <- parameterIds <- pkParameter <- populationId <- NULL

  # this function stops in valid runs
  stopHelperFunction()
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  scenarios <- getScenarioDefinitions(projectConfiguration$scenariosFile)

  dtNewHeader <- data.table()
  if (sheetName %in% wb$sheet_names & !overwrite) {
    dtNewHeader <- xlsxReadData(wb, sheetName = sheetName, skipDescriptionRow = TRUE)
    scenarios <- scenarios[!(scenarioName %in% unique(dtNewHeader$scenario))]
  }
  if (nrow(dtNewHeader) == 0) {
    dtNewHeader <- data.table(
      level = 1,
      header = "Demographics"
    )
  }

  dtNewConfig <- data.table(
    plotName = "demographics",
    # get list of scenarios with unique populations
    scenarios = paste(scenarios[!duplicated(populationId)]$scenarioName, collapse = ", "),
    parameterIds = "weight, height, BMI",
    parameterId_Bin = "age",
    modeOfBinning = BINNINGMODE$number,
    numberOfBins = 20,
    scale = "linear",
    facetScale = "fixed"
  )

  if (!is.null(pkParameterDT)) {
    dtNewHeaderPK <- data.table(
      level = 1,
      header = "PK Parameter"
    )

    # Create a unique combination of parameters and outputPathId
    dt <- pkParameterDT[, .(parameterIds = paste(unique(pkParameter), collapse = ", ")), by = outputPathId] %>%
      .[, .(outputPathIds = paste(unique(outputPathId), collapse = ", ")), by = parameterIds]

    # Create a new data.table with all combinations of pkParameters and scenario names
    dtNewConfigPK <- dt[, .(
      plotName = "pkparameter",
      scenarios = paste(scenarios$scenarioName, collapse = ", "),
      scale = "linear, log",
      facetScale = "fixed",
      parameterId_Bin = "age",
      modeOfBinning = BINNINGMODE$number,
      numberOfBins = 20
    ),
    by = .(outputPathIds, parameterIds)
    ]
    if (nrow(dtNewConfigPK) > 1) {
      dtNewConfigPK[, plotName := paste0(plotName, .I)]
    }

    dtNewConfig <- rbind(dtNewConfig,
      dtNewHeaderPK,
      dtNewConfigPK,
      fill = TRUE
    )
  }

  wb <- addDataAsTemplateToXlsx(
    wb = wb,
    templateSheet = "DemographicPlots",
    sheetName = sheetName,
    dtNewData = rbind(dtNewHeader,
      dtNewConfig, # nolint indentation_linter
      fill = TRUE
    )
  )

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)
}
