#' Plot Sensitivity Function
#'
#' Generates sensitivity plots based on the provided project configuration and scenario list.
#'
#' @param projectConfiguration A list containing project configuration settings, including file paths and parameters.
#' @param onePlotConfig A data frame containing the configuration for a single plot.
#' @param scenarioList A list of scenarios for which the sensitivity analysis will be performed.
#' @param significantDigits Number of significant digits used for sensitivity values
#'   in exported tables.
#'
#' @return A list of plots and tables, where each entry corresponds to a generated sensitivity plot or its data.
#' @export
#' @family plot functions
plotSensitivity <- function(
  projectConfiguration,
  onePlotConfig,
  scenarioList,
  significantDigits = 3
) {
  checkmate::assertNumber(significantDigits, lower = 1, finite = TRUE)

  # initialize variable to avoid messages
  sens <- parameterName <- NULL
  representativeStratum <- scenarioShortName <- NULL
  resultFileConfig <- unique(data.table::as.data.table(onePlotConfig)[, c(
    "scenario",
    "sensitivityParameterSheet"
  )])
  resultFiles <- mapply(
    FUN = function(scenarioName, sensitivitySheet) {
      .getSensitivityResultFilePath(
        projectConfiguration = projectConfiguration,
        scenarioName = scenarioName,
        sensitivitysheet = sensitivitySheet
      )
    },
    scenarioName = resultFileConfig$scenario,
    sensitivitySheet = resultFileConfig$sensitivityParameterSheet,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
  checkmate::assertFileExists(
    x = resultFiles,
    .var.name = "sensitivityResult file"
  )

  plotData <- .prepareSensitivityPlotData(
    onePlotConfig,
    projectConfiguration,
    scenarioList
  )

  hasRepresentativeStrata <-
    any(!is.na(plotData$RepresentativeQuantile))

  plotObject <- ggplotWithWatermark(plotData) +
    geom_hline(yintercept = 0, color = "grey70")

  if (hasRepresentativeStrata) {
    plotObject <- plotObject +
      geom_point(
        aes(
          x = sensitivityParameter,
          y = sens,
          color = scenarioShortName,
          shape = RepresentativeQuantile
        ),
        position = position_dodge(width = 0.7),
        size = 2.5
      )
  } else {
    plotObject <- plotObject +
      geom_col(
        aes(x = sensitivityParameter, y = sens),
        fill = "grey",
        position = "dodge"
      ) +
      scale_fill_grey(start = 0.7, end = 0)
  }
  plotObject <- plotObject +
    labs(
      y = "Sensitivity",
      x = "",
      color = "Scenario",
      shape = "Representative"
    ) +
    scale_x_discrete(limits = rev(unique(plotData$parameterName))) +
    ospsuite.plots::theme_osp() +
    theme(
      legend.position = "top",
      legend.direction = "vertical"
    ) +
    coord_flip()

  nFacetColumns <- .getFacetColumns(
    data = plotData,
    strategy = "allInOneRow",
    facetColumn = "plotTag"
  )
  nFacets <- ifelse(is.null(nFacetColumns), 1, nFacetColumns)
  facetAspectRatio <- length(levels(plotData$parameterName)) /
    max(nFacets, 1) /
    5 ## value by experience

  plotObject <- addFacets(
    plotObject = plotObject,
    facetScale = "fixed",
    facetAspectRatio = facetAspectRatio,
    nFacetColumns = nFacetColumns
  )

  # Prepare for export
  plotObject <- setExportAttributes(
    object = plotObject,
    caption = .getCaptionForSensitivityPlot(
      plotData = plotData,
      projectConfiguration = projectConfiguration,
      plotCaptionAddon = onePlotConfig$plotCaptionAddon[1]
    ),
    exportArguments = list(width = 20)
  )

  plotList <- list(plotObject)
  names(plotList) <- onePlotConfig$plotName[1]

  # Export table
  for (plotDataTag in split(plotData, by = "plotTag")) {
    tableKey <- paste(
      onePlotConfig$plotName[1],
      plotDataTag$plotTag[1],
      sep = "-"
    )

    plotDataTag <- setExportAttributes(
      object = .formatSensitivityExportTable(
        plotDataTag,
        significantDigits = significantDigits
      ),
      caption = .getCaptionForSensitivityPlot(
        plotData = plotDataTag,
        projectConfiguration = projectConfiguration,
        plotCaptionAddon = onePlotConfig$plotCaptionAddon[1]
      )
    )

    plotList[[tableKey]] <- plotDataTag
  }

  return(plotList)
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
.prepareSensitivityPlotData <- function(
  onePlotConfig,
  projectConfiguration,
  scenarioList
) {
  # initialize variable to avoid messages
  sens <- plotTag <- NULL
  PKParameter <- QuantityPath <- NULL
  representativeStratum <- plotFacetLabel <- NULL
  scenarioName <- longName <- NULL

  pkDefinitions <- .getPKParameterOverview(projectConfiguration) %>%
    .[, c("pKParameter", "displayNamePKParameter", "displayUnitPKParameter")]

  onePlotConfig <- onePlotConfig %>%
    .separateAndTrimColumn("pKParameters") %>%
    .separateAndTrimColumn("outputPathIds") %>%
    merge(
      pkDefinitions,
      by = c("pKParameter")
    ) %>%
    merge(configEnv$outputPaths, by = "outputPathId")

  scenarioShortNameLevels <- unique(onePlotConfig$scenarioShortName)

  sensitivityParameterDt <- xlsxReadData(
    wb = file.path(projectConfiguration$addOns$sensitivityFile),
    sheetName = onePlotConfig$sensitivityParameterSheet[1]
  )

  plotData <- data.table()

  for (configLine in split(onePlotConfig, by = "scenario")) {
    sensitivityResultFile <- .getSensitivityResultFilePath(
      projectConfiguration = projectConfiguration,
      scenarioName = configLine$scenario[1],
      sensitivitysheet = configLine$sensitivityParameterSheet[1]
    )

    rawSensitivityData <- fread(file = sensitivityResultFile)

    if (!"RepresentativeQuantile" %chin% names(rawSensitivityData)) {
      rawSensitivityData[, RepresentativeQuantile := NA_character_]
    }

    plotDataLine <- lapply(
      split(configLine, seq(1, nrow(configLine))),
      function(cL) {
        filteredData <- merge(
          cL,
          rawSensitivityData,
          by.x = c('pKParameter', 'outputPath'),
          by.y = c('PKParameter', 'QuantityPath')
        )[
          abs(sens) >= threshold,
          .(
            sens,
            SensitivityParameter,
            threshold,
            outputPathId,
            pKParameter,
            displayNamePKParameter,
            scenarioShortName,
            scenarioLongName,
            RepresentativeQuantile
          )
        ] |>
          setnames('SensitivityParameter', 'sensitivityParameter')

        return(filteredData)
      }
    )
    plotDataLine <-
      rbindlist(plotDataLine)

    plotData <- rbind(
      plotData,
      plotDataLine
    )
  }

  plotData$outputPathId <- factor(
    plotData$outputPathId,
    levels = unique(plotData$outputPathId),
    ordered = TRUE
  )
  plotData$pKParameter <- factor(
    plotData$pKParameter,
    levels = rev(unique(plotData$pKParameter)),
    ordered = TRUE
  )
  plotData$scenarioShortName <- factor(
    plotData$scenarioShortName,
    levels = scenarioShortNameLevels,
    ordered = TRUE
  )

  dtPlotTag <- unique(plotData[, c(
    "outputPathId",
    "pKParameter"
  )])
  dtPlotTag[, plotTag := generatePlotTag(.I)]
  plotData <- merge(
    plotData,
    dtPlotTag,
    by = c("outputPathId", "pKParameter"),
    all.x = TRUE,
    sort = FALSE
  )
  plotData$plotTag <- factor(
    plotData$plotTag,
    ordered = TRUE,
    levels = unique(dtPlotTag$plotTag)
  )

  plotData <- plotData[order(-abs(sens))]
  plotData$sensitivityParameter <- factor(
    plotData$sensitivityParameter,
    levels = rev(unique(plotData$sensitivityParameter)),
    ordered = TRUE
  )

  return(plotData)
}

#' Get Caption for Sensitivity Plot
#'
#' Generates a caption for the sensitivity plot based on the plot data and project configuration.
#' If the configured sensitivity threshold is greater than 0, the caption
#' includes the applied absolute sensitivity filter.
#'
#' @param plotData A data table containing the data for the sensitivity plot.
#' @param projectConfiguration A list containing project configuration settings.
#' @param plotCaptionAddon An optional string to be added to the caption.
#'
#' @return A string containing the generated caption for the sensitivity plot.
#' @keywords internal
.getCaptionForSensitivityPlot <- function(
  plotData,
  projectConfiguration,
  plotCaptionAddon
) {
  referenceScenario <- referenceScenarioLongName <- scenarioLongName <- NULL
  ratioPair <- referenceLabel <- NULL

  dtCaption <- plotData %>%
    dplyr::select(c(
      "plotTag",
      "outputPathId",
      "pKParameter",
      "scenarioLongName"
    )) %>%
    unique() %>%
    merge(
      configEnv$outputPaths[, c("outputPathId", "displayNameOutput")],
      by = "outputPathId"
    )

  captiontext <- paste(
    captionPrefixSimulated(),
    "Sensitivity of",
    pasteFigureTags(dtCaption, captionColumn = "pKParameter"),
    "for",
    pasteFigureTags(dtCaption, captionColumn = "displayNameOutput"),
    "for",
    pasteFigureTags(dtCaption, captionColumn = "scenarioLongName"),
    "sorted by absolute sensitivity."
  )

  thresholdValues <- unique(plotData$threshold)
  thresholdValues <- thresholdValues[!is.na(thresholdValues)]
  thresholdValues <- thresholdValues[thresholdValues > 0]
  if (length(thresholdValues) > 0) {
    thresholdText <- concatWithAnd(format(thresholdValues, trim = TRUE))
    captiontext <- addCaptionTextAddon(
      captiontext,
      paste(
        "Filtering threshold (absolute sensitivity): >=",
        thresholdText
      )
    )
  }

  if (
    "referenceScenario" %in%
      names(plotData) &&
      any(
        !is.na(plotData$referenceScenario) & nzchar(plotData$referenceScenario)
      )
  ) {
    ratioCaptionData <- unique(plotData[
      !is.na(referenceScenario) & nzchar(referenceScenario),
      c(
        "plotTag",
        "scenarioLongName",
        "referenceScenario",
        "referenceScenarioLongName"
      )
    ])
    ratioCaptionData[, referenceLabel := referenceScenarioLongName]
    ratioCaptionData[
      is.na(referenceLabel) | !nzchar(referenceLabel),
      referenceLabel := referenceScenario
    ]
    ratioCaptionData[,
      ratioPair := paste0(scenarioLongName, " / ", referenceLabel)
    ]

    captiontext <- addCaptionTextAddon(
      captiontext,
      paste(
        "Ratio mode (scenario/reference):",
        pasteFigureTags(ratioCaptionData, captionColumn = "ratioPair")
      )
    )
  }

  if (
    "representativeStratum" %in%
      names(plotData) &&
      any(
        !is.na(plotData$representativeStratum) &
          nzchar(plotData$representativeStratum)
      )
  ) {
    representativeCaption <- unique(
      plotData$representativeStratum[
        !is.na(plotData$representativeStratum) &
          nzchar(plotData$representativeStratum)
      ]
    )
    captiontext <- addCaptionTextAddon(
      captiontext,
      paste(
        "Representative strata:",
        concatWithAnd(representativeCaption)
      )
    )
  }

  captiontext <- addCaptionTextAddon(captiontext, plotCaptionAddon[1])

  return(captiontext)
}

.getSensitivityResultFilePath <- function(
  projectConfiguration,
  scenarioName,
  sensitivitysheet
) {
  representativeFile <- file.path(
    projectConfiguration$outputFolder,
    EXPORTDIR$sensitivityResults,
    .representativeSensitivityAnalysisName(scenarioName, sensitivitysheet)
  )
  if (file.exists(representativeFile)) {
    return(representativeFile)
  }

  file.path(
    projectConfiguration$outputFolder,
    EXPORTDIR$sensitivityResults,
    .sensitivityAnalysisName(scenarioName, sensitivitysheet)
  )
}


#' Format Sensitivity Export Table
#'
#' Formats a sensitivity table for export and rounds sensitivity values to a fixed number
#' of significant digits.
#'
#' @param plotDataTag A `data.table` containing sensitivity plot data for one plot tag.
#' @param significantDigits Number of significant digits for exported sensitivity
#'   values.
#'
#' @return A formatted `data.table` with export-ready column names.
#' @keywords internal
#' @noRd
.formatSensitivityExportTable <- function(plotDataTag, significantDigits = 3) {
  Sensitivity <- NULL
  checkmate::assertNumber(significantDigits, lower = 1, finite = TRUE)

  if (any(!is.na(plotDataTag$RepresentativeQuantile))) {
    exportTable <- (plotDataTag[, c(
      "RepresentativeQuantile",
      "sensitivityParameter",
      "sens"
    )] %>%
      setnames(
        old = c("RepresentativeQuantile", "sensitivityParameter", "sens"),
        new = c("Representative", "Parameter", "Sensitivity")
      ))
  } else {
    exportTable <- plotDataTag[, c("sensitivityParameter", "sens")] %>%
      setnames(
        old = c("sensitivityParameter", "sens"),
        new = c("Parameter", "Sensitivity")
      )
  }

  exportTable[,
    Sensitivity := base::signif(Sensitivity, digits = significantDigits)
  ]

  return(exportTable)
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
#' @keywords internal
.getPKParameterOverview <- function(projectConfiguration) {
  pkParameterSheets <- unique(splitInputs(configEnv$scenarios$pKParameter))
  pkSheets <- .readPKDefinitionsFromSheets(
    projectConfiguration = projectConfiguration,
    pkParameterSheets = pkParameterSheets
  ) %>%
    .normalizedPKDefinitionsForSensitivity()

  return(pkSheets)
}
#' Validate Sensitivity Configuration Table
#'
#' This function checks the headers of the configuration table, validates output IDs and data group IDs for plotting,
#' and ensures the configuration adheres to specified criteria. It also checks for file existence in the output folder.
#'
#' @param configTable A data.table containing the configuration for sensitivity analysis.
#' @param ... Additional arguments passed to other functions.
#'
#' @return A validated data frame containing the configuration table for sensitivity plots.
#' @export
#' @family plot configuration validation function
#' @family functions to generate sensitivity plots
validateSensitivityConfig <- function(configTable, scenarioList, ...) {
  scenario <- sensitivityParameterSheet <- isRepresentativeMode <- N <- NULL

  configTablePlots <- validateHeaders(configTable)

  validateOutputIdsForPlot()

  validateConfigTablePlots(
    configTablePlots = configTablePlots,
    charactersWithoutMissing = c(
      "plotName",
      "scenario",
      "scenarioLongName",
      "scenarioShortName",
      "outputPathIds",
      "pKParameters",
      "sensitivityParameterSheet"
    ),
    charactersWithMissing = NULL,
    numericColumns = NULL,
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

  checkmate::assertNumeric(
    configTablePlots$threshold,
    lower = 0,
    finite = TRUE,
    any.missing = FALSE,
    .var.name = "Plot configuration column threshold"
  )

  validateGroupConsistency(
    dt = configTablePlots,
    valueColumns = c(
      "sensitivityParameterSheet",
      "threshold"
    )
  )

  popScenarios <- names(scenarioList)[unlist(lapply(
    scenarioList,
    function(scenarioList) {
      (!is.null(scenarioList$population) &&
        "Population" %in% class(scenarioList$population))
    }
  ))]

  configTablePlots[, isRepresentativeMode := scenario %chin% popScenarios]

  nonRepresentativeDuplicates <-
    configTablePlots[,
      .N,
      by = c("plotName", "scenario", "isRepresentativeMode")
    ][isRepresentativeMode == FALSE & N > 1]

  if (nrow(nonRepresentativeDuplicates) > 0) {
    stop(messages$errorplotSensitivityMultipleRowsPerScenario())
  }

  return(configTable)
}
