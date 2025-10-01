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
plotSensitivity <- function(projectConfiguration,
                            onePlotConfig,
                            scenarioList) {
  # initialize variable to avoid messages
  sens <- parameterName <- NULL

  checkmate::assertFileExists(
    x = file.path(
      projectConfiguration$outputFolder, EXPORTDIR$sensitivityResults,
      unique(sensitivityAnalyisName(
        onePlotConfig$scenario,
        onePlotConfig$sensitivityParameterSheet
      ))
    ),
    .var.name = "sensitivityResult file"
  )


  plotData <- prepareSensitivityPlotData(
    onePlotConfig,
    projectConfiguration,
    scenarioList
  )

  plotObject <- ggplotWithWatermark(plotData) +
    geom_col(aes(x = parameterName, y = sens),
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

  plotObject <- addFacets(plotObject,
    "fixed",
    facetAspectRatio = length(levels(plotData$parameterName)) / length(levels(plotData$plotTag)) / 5,
    nFacetColumns = length(levels(plotData$plotTag))
  )

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
    tableKey <- paste(onePlotConfig$plotName[1], plotDataTag$plotTag[1], sep = "-")


    plotDataTag <- setExportAttributes(
      object = plotDataTag[, c("parameterName", "sens")] %>%
        setnames(
          old = c("parameterName", "sens"),
          new = c("Parameter", "Sensitivity")
        ),
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
prepareSensitivityPlotData <- function(onePlotConfig,
                                       projectConfiguration,
                                       scenarioList) {
  # initialize variable to avoid messages
  sens <- pKParameter <- outputPathId <- plotTag <- parameterPath <- NULL

  pkDefinitions <- getPKParameterOverview(projectConfiguration) %>%
    .[, c("pKParameter", "displayNamePKParameter", "displayUnitPKParameter")]

  onePlotConfig <- onePlotConfig %>%
    separateAndTrimColumn("pKParameters") %>%
    separateAndTrimColumn("outputPathIds") %>%
    merge(
      pkDefinitions,
      by = c("pKParameter")
    ) %>%
    merge(configEnv$outputPaths, by = "outputPathId")

  plotData <- data.table()

  for (configLine in split(onePlotConfig, by = "scenario")) {
    sensitivityResults <- ospsuite::importSensitivityAnalysisResultsFromCSV(
      simulation = scenarioList[[configLine$scenario[1]]]$simulation,
      filePaths = file.path(
        projectConfiguration$outputFolder,
        EXPORTDIR$sensitivityResults,
        sensitivityAnalyisName(configLine$scenario[1], configLine$sensitivityParameterSheet[1])
      )
    )

    plotDataLine <-
      lapply(split(configLine, seq(1, nrow(configLine))), function(cL) {
        data.table(
          sens = lapply(
            sensitivityResults$allPKParameterSensitivitiesFor(
              pkParameterName = cL$pKParameter,
              outputPath = cL$outputPath,
              totalSensitivityThreshold = cL$threshold
            ), getElement, "value"
          ) %>%
            unlist(),
          parameterInternal = lapply(
            sensitivityResults$allPKParameterSensitivitiesFor(
              pkParameterName = cL$pKParameter,
              outputPath = cL$outputPath,
              totalSensitivityThreshold = cL$threshold
            ), getElement, "parameterName"
          ) %>%
            unlist(),
          outputPathId = cL$outputPathId,
          pKParameter = cL$pKParameter,
          displayNamePKParameter = cL$displayNamePKParameter,
          scenarioLongName = cL$scenarioLongName
        )
      })

    nameDictionary <-
      merge(
        fread(file = file.path(
          projectConfiguration$outputFolder,
          EXPORTDIR$sensitivityResults,
          sensitivityAnalyisName(configLine$scenario[1], configLine$sensitivityParameterSheet[1])
        ))[, c("ParameterPath", "Parameter")] %>%
          unique() %>%
          setnames(
            old = c("ParameterPath", "Parameter"),
            new = c("parameterPath", "parameter")
          ),
        xlsxReadData(
          wb = file.path(projectConfiguration$addOns$sensitivityFile),
          sheetName = configLine$sensitivityParameterSheet[1]
        ),
        by = "parameterPath",
        suffixes = c("Internal", "Name")
      ) %>%
      .[, parameterPath := NULL]

    plotDataLine <-
      merge(rbindlist(plotDataLine),
        nameDictionary,
        by = "parameterInternal"
      )

    plotData <- rbind(
      plotData,
      plotDataLine
    )
  }

  plotData$outputPathId <- factor(plotData$outputPathId, levels = unique(plotData$outputPathId), ordered = TRUE)
  plotData$pKParameter <- factor(plotData$pKParameter, levels = rev(unique(plotData$pKParameter)), ordered = TRUE)
  plotData[, plotTag := generatePlotTag((as.numeric(outputPathId) - 1) * length(levels(plotData$pKParameter)) +
    as.numeric(pKParameter))]
  plotData$plotTag <- factor(plotData$plotTag, ordered = TRUE, levels = sort(unique(plotData$plotTag)))

  plotData <- plotData[order(-abs(sens))]
  plotData$parameterName <- factor(plotData$parameterName, levels = rev(unique(plotData$parameterName)), ordered = TRUE)

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
getCaptionForSensitivityPlot <- function(plotData, projectConfiguration, plotCaptionAddon) {
  dtCaption <- plotData %>%
    dplyr::select(c("plotTag", "outputPathId", "pKParameter", "scenarioLongName")) %>%
    unique() %>%
    merge(configEnv$outputPaths[, c("outputPathId", "displayNameOutput")],
      by = "outputPathId"
    )


  captiontext <- paste0(
    "Sensitivity of",
    pasteFigureTags(dtCaption, captionColumn = "pKParameter"),
    "for",
    pasteFigureTags(dtCaption, captionColumn = "displayNameOutput"),
    "for",
    pasteFigureTags(dtCaption, captionColumn = "scenarioLongName"),
    "sorted by absolute sensitivity."
  )

  captiontext <- addCaptionTextAddon(captiontext, plotCaptionAddon[1])

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
    separateAndTrimColumn(columnName = "outputPathIds")

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
    numericColumns = c(
      "threshold"
    ),
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


  validateGroupConsistency(
    dt = configTablePlots,
    valueColumns = c(
      "sensitivityParameterSheet",
      "threshold"
    )
  )

  return(configTable)
}
