#' Plot Sensitivity Function
#'
#' Generates sensitivity plots based on the provided project configuration and scenario list.
#'
#' @param projectConfiguration A list containing project configuration settings, including file paths and parameters.
#' @param onePlotConfig A data frame containing the configuration for a single plot.
#' @param scenarioList A list of scenarios for which the sensitivity analysis will be performed.
#'
#' @return A list of plots and tables, where each entry corresponds to a generated sensitivity plot or its data.
#' @export
#' @family plot functions
plotSensitivity <- function(projectConfiguration, onePlotConfig, scenarioList) {
  # initialize variable to avoid messages
  sens <- parameterName <- plotFacetLabel <- plotTag <- NULL
  representativeStratum <- scenarioLongName <- NULL

  checkmate::assertFileExists(
    x = .getSensitivityResultFilePath(
      projectConfiguration = projectConfiguration,
      scenarioName = unique(onePlotConfig$scenario),
      sensitivitysheet = unique(onePlotConfig$sensitivityParameterSheet)
    ),
    .var.name = "sensitivityResult file"
  )

  plotData <- prepareSensitivityPlotData(
    onePlotConfig,
    projectConfiguration,
    scenarioList
  )

  hasRepresentativeStrata <-
    "representativeStratum" %in% names(plotData) &&
    any(!is.na(plotData$representativeStratum) & nzchar(plotData$representativeStratum))

  if (hasRepresentativeStrata) {
    plotObject <- ggplotWithWatermark(plotData) +
      geom_hline(yintercept = 0, color = "grey70") +
      geom_point(
        aes(
          x = parameterName,
          y = sens,
          color = scenarioLongName,
          shape = representativeStratum
        ),
        position = position_dodge(width = 0.7),
        size = 2.5
      ) +
      labs(
        y = "Sensitivity",
        x = "",
        color = "Scenario",
        shape = "Representative"
      ) +
      scale_x_discrete(limits = rev(unique(plotData$parameterName))) +
      theme(
        legend.position = "top",
        legend.direction = "vertical"
      ) +
      coord_flip()
  } else {
    plotObject <- ggplotWithWatermark(plotData) +
      geom_col(
        aes(x = parameterName, y = sens),
        fill = "grey",
        position = "dodge"
      ) +
      labs(
        y = "Sensitivity",
        x = ""
      ) +
      scale_x_discrete(limits = rev(unique(plotData$parameterName))) +
      theme(
        legend.position = "top",
        legend.direction = "vertical",
        legend.title = element_blank()
      ) +
      scale_fill_grey(start = 0.7, end = 0) +
      coord_flip()
  }

  facetValues <- if ("plotFacetLabel" %in% names(plotData)) {
    plotData$plotFacetLabel
  } else {
    plotData$plotTag
  }
  facetValues <- facetValues[!is.na(facetValues) & facetValues != ""]
  nFacets <- length(unique(facetValues))
  facetAspectRatio <- length(levels(plotData$parameterName)) /
    max(nFacets, 1) /
    5

  plotObject <- plotObject +
    ggplot2::theme(aspect.ratio = facetAspectRatio)

  if (nFacets > 1) {
    facetColumn <- if ("plotFacetLabel" %in% names(plotData)) {
      ggplot2::vars(plotFacetLabel)
    } else {
      ggplot2::vars(plotTag)
    }

    plotObject <- plotObject +
      ggplot2::facet_wrap(
        facets = facetColumn,
        scales = "fixed",
        ncol = nFacets
      ) +
      ggplot2::theme(
        strip.background = ggplot2::element_rect(fill = NA, color = NA),
        strip.text = ggplot2::element_text(hjust = 0, vjust = 1)
      )
  }

  # Prepare for export
  plotObject <- setExportAttributes(
    object = plotObject,
    caption = getCaptionForSensitivityPlot(
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
      object = .formatSensitivityExportTable(plotDataTag),
      caption = getCaptionForSensitivityPlot(
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
prepareSensitivityPlotData <- function(
  onePlotConfig,
  projectConfiguration,
  scenarioList
) {
  # initialize variable to avoid messages
  sens <- plotTag <- NULL
  PKParameter <- QuantityPath <- NULL
  representativeStratum <- plotFacetLabel <- NULL

  scenarioName <- longName <- NULL

  referenceScenarioLookup <- data.table::data.table(
    scenarioName = character(0),
    referenceScenarioLongName = character(0)
  )
  if (
    "scenarios" %in% ls(envir = configEnv) &&
      all(c("scenarioName", "longName") %in% names(configEnv$scenarios))
  ) {
    referenceScenarioLookup <- unique(configEnv$scenarios[, .(
      scenarioName,
      referenceScenarioLongName = longName
    )])
  }

  pkDefinitions <- getPKParameterOverview(projectConfiguration) %>%
    .[, c("pKParameter", "displayNamePKParameter", "displayUnitPKParameter")]

  onePlotConfig <- onePlotConfig %>%
    .separateAndTrimColumn("pKParameters") %>%
    .separateAndTrimColumn("outputPathIds") %>%
    merge(
      pkDefinitions,
      by = c("pKParameter")
    ) %>%
    merge(configEnv$outputPaths, by = "outputPathId")

  plotData <- data.table()

  for (configLine in split(onePlotConfig, by = "scenario")) {
    sensitivityResultFile <- .getSensitivityResultFilePath(
      projectConfiguration = projectConfiguration,
      scenarioName = configLine$scenario[1],
      sensitivitysheet = configLine$sensitivityParameterSheet[1]
    )

    rawSensitivityData <- fread(file = sensitivityResultFile)
    usesEnhancedFormat <- all(
      c(
        "SensitivityParameter",
        "QuantityPath",
        "PKParameter",
        "sens"
      ) %in%
        names(rawSensitivityData)
    )

    sensitivityParameterDt <- xlsxReadData(
      wb = file.path(projectConfiguration$addOns$sensitivityFile),
      sheetName = configLine$sensitivityParameterSheet[1]
    )

    if (usesEnhancedFormat) {
      hasRepresentativeStrata <- all(
        c(
          "RepresentativeOutputPath",
          "RepresentativePKParameter",
          "RepresentativeQuantile",
          "RepresentativeIndividualId"
        ) %in%
          names(rawSensitivityData)
      )

      plotDataLine <- lapply(
        split(configLine, seq(1, nrow(configLine))),
        function(cL) {
          referenceScenarioName <- if ("referenceScenario" %in% names(cL)) {
            cL$referenceScenario
          } else {
            NA_character_
          }

          referenceScenarioLongName <- NA_character_
          if (!is.na(referenceScenarioName) && nzchar(referenceScenarioName)) {
            if (nrow(referenceScenarioLookup) > 0) {
              matchedLongName <- referenceScenarioLookup[
                scenarioName == referenceScenarioName
              ]$referenceScenarioLongName
              if (length(matchedLongName) > 0 &&
                !is.na(matchedLongName[1]) &&
                nzchar(matchedLongName[1])) {
                referenceScenarioLongName <- matchedLongName[1]
              } else {
                referenceScenarioLongName <- referenceScenarioName
              }
            } else {
              referenceScenarioLongName <- referenceScenarioName
            }
          }

          filteredData <- rawSensitivityData[
            PKParameter == cL$pKParameter &
              QuantityPath == cL$outputPath &
              abs(sens) >= cL$threshold
          ]

          data.table(
            sens = filteredData$sens,
            parameterInternal = filteredData$SensitivityParameter,
            threshold = cL$threshold,
            outputPathId = cL$outputPathId,
            pKParameter = cL$pKParameter,
            displayNamePKParameter = cL$displayNamePKParameter,
            scenarioLongName = cL$scenarioLongName,
            referenceScenario = referenceScenarioName,
            referenceScenarioLongName = referenceScenarioLongName,
            RepresentativeOutputPath = if (hasRepresentativeStrata) {
              filteredData$RepresentativeOutputPath
            } else {
              NA_character_
            },
            RepresentativePKParameter = if (hasRepresentativeStrata) {
              filteredData$RepresentativePKParameter
            } else {
              NA_character_
            },
            RepresentativeQuantile = if (hasRepresentativeStrata) {
              filteredData$RepresentativeQuantile
            } else {
              NA_character_
            },
            RepresentativeIndividualId = if (hasRepresentativeStrata) {
              as.character(filteredData$RepresentativeIndividualId)
            } else {
              NA_character_
            }
          )
        }
      )

      nameDictionary <- .sensitivityParameterNameDictionary(
        sensitivityParameterDt
      )
    } else {
      sensitivityResults <- ospsuite::importSensitivityAnalysisResultsFromCSV(
        simulation = scenarioList[[configLine$scenario[1]]]$simulation,
        filePaths = sensitivityResultFile
      )

      plotDataLine <-
        lapply(split(configLine, seq(1, nrow(configLine))), function(cL) {
          referenceScenarioName <- if ("referenceScenario" %in% names(cL)) {
            cL$referenceScenario
          } else {
            NA_character_
          }

          referenceScenarioLongName <- NA_character_
          if (!is.na(referenceScenarioName) && nzchar(referenceScenarioName)) {
            if (nrow(referenceScenarioLookup) > 0) {
              matchedLongName <- referenceScenarioLookup[
                scenarioName == referenceScenarioName
              ]$referenceScenarioLongName
              if (length(matchedLongName) > 0 &&
                !is.na(matchedLongName[1]) &&
                nzchar(matchedLongName[1])) {
                referenceScenarioLongName <- matchedLongName[1]
              } else {
                referenceScenarioLongName <- referenceScenarioName
              }
            } else {
              referenceScenarioLongName <- referenceScenarioName
            }
          }

          sensitivities <- sensitivityResults$allPKParameterSensitivitiesFor(
            pkParameterName = cL$pKParameter,
            outputPath = cL$outputPath,
            totalSensitivityThreshold = cL$threshold
          )
          data.table(
            sens = unlist(lapply(sensitivities, getElement, "value")),
            parameterInternal = unlist(lapply(
              sensitivities,
              getElement,
              "parameterName"
            )),
            threshold = cL$threshold,
            outputPathId = cL$outputPathId,
            pKParameter = cL$pKParameter,
            displayNamePKParameter = cL$displayNamePKParameter,
            scenarioLongName = cL$scenarioLongName,
            referenceScenario = referenceScenarioName,
            referenceScenarioLongName = referenceScenarioLongName
          )
        })

      nameDictionary <-
        merge(
          rawSensitivityData[, c("ParameterPath", "Parameter")] %>%
            unique() %>%
            setnames(
              old = c("ParameterPath", "Parameter"),
              new = c("parameterPath", "parameter")
            ),
          sensitivityParameterDt,
          by = "parameterPath",
          suffixes = c("Internal", "Name")
        )
    }

    plotDataLine <- if (usesEnhancedFormat) {
      merge(
        rbindlist(plotDataLine),
        nameDictionary,
        by = "parameterInternal"
      )
    } else {
      merge(rbindlist(plotDataLine), nameDictionary, by = "parameterInternal")
    }

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

  if (
    all(
      c(
        "RepresentativeOutputPath",
        "RepresentativePKParameter",
        "RepresentativeQuantile",
        "RepresentativeIndividualId"
      ) %in%
        names(plotData)
    )
  ) {
    representativeOutputs <- configEnv$outputPaths[, c(
      "outputPath",
      "displayNameOutput"
    )]
    plotData <- merge(
      plotData,
      representativeOutputs,
      by.x = "RepresentativeOutputPath",
      by.y = "outputPath",
      all.x = TRUE,
      sort = FALSE,
      suffixes = c("", ".representative")
    )
    representativeOutputDisplay <- plotData[[
      "displayNameOutput.representative"
    ]]
    representativeOutputDisplay[is.na(representativeOutputDisplay)] <-
      plotData[["RepresentativeOutputPath"]]
    representativePKParameter <- plotData[["RepresentativePKParameter"]]
    representativeQuantile <- plotData[["RepresentativeQuantile"]]
    representativeIndividualId <- plotData[["RepresentativeIndividualId"]]
    plotData[,
      representativeStratum := .formatRepresentativeSensitivityStratum(
        representativeOutputDisplay = representativeOutputDisplay,
        representativePKParameter = representativePKParameter,
        representativeQuantile = representativeQuantile,
        representativeIndividualId = representativeIndividualId
      )
    ]
  } else {
    plotData[, representativeStratum := ""]
  }

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

  plotData[, plotFacetLabel := as.character(plotTag)]
  plotData$plotFacetLabel <- factor(
    plotData$plotFacetLabel,
    ordered = TRUE,
    levels = unique(plotData$plotFacetLabel)
  )

  plotData <- plotData[order(-abs(sens))]
  plotData$parameterName <- factor(
    plotData$parameterName,
    levels = rev(unique(plotData$parameterName)),
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
getCaptionForSensitivityPlot <- function(
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
    "referenceScenario" %in% names(plotData) &&
      any(!is.na(plotData$referenceScenario) & nzchar(plotData$referenceScenario))
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
    ratioCaptionData[, ratioPair := paste0(scenarioLongName, " / ", referenceLabel)]

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

.formatRepresentativeSensitivityStratum <- function(
  representativeOutputDisplay,
  representativePKParameter,
  representativeQuantile,
  representativeIndividualId
) {
  hasRepresentativeInfo <-
    !is.na(representativeQuantile) & nzchar(representativeQuantile)

  stratum <- rep("", length(representativeQuantile))
  stratum[hasRepresentativeInfo] <- paste0(
    representativePKParameter[hasRepresentativeInfo],
    " in ",
    representativeOutputDisplay[hasRepresentativeInfo],
    " (",
    representativeQuantile[hasRepresentativeInfo],
    ", ID ",
    representativeIndividualId[hasRepresentativeInfo],
    ")"
  )

  stratum
}

.formatSensitivityExportTable <- function(plotDataTag) {
  if (
    "representativeStratum" %in%
      names(plotDataTag) &&
      any(
        !is.na(plotDataTag$representativeStratum) &
          nzchar(plotDataTag$representativeStratum)
      )
  ) {
    return(
      plotDataTag[, c("representativeStratum", "parameterName", "sens")] %>%
        setnames(
          old = c("representativeStratum", "parameterName", "sens"),
          new = c("Representative", "Parameter", "Sensitivity")
        )
    )
  }

  plotDataTag[, c("parameterName", "sens")] %>%
    setnames(
      old = c("parameterName", "sens"),
      new = c("Parameter", "Sensitivity")
    )
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
getPKParameterOverview <- function(projectConfiguration) {
  # initialize variable to avoid messages
  pKParameter <- descriptions <- NULL

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
    .[, pKParameter := NULL] %>%
    setnames(
      old = c("name", "displayName", "displayUnit"),
      new = c("pKParameter", "displayNamePKParameter", "displayUnitPKParameter")
    ) %>%
    .separateAndTrimColumn(columnName = "outputPathIds")

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
validateSensitivityConfig <- function(configTable, ...) {
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
  if (any(configTablePlots$threshold < 0)) {
    stop("Plot configuration column threshold must be >= 0")
  }

  validateGroupConsistency(
    dt = configTablePlots,
    valueColumns = c(
      "sensitivityParameterSheet",
      "threshold"
    )
  )

  return(configTable)
}
