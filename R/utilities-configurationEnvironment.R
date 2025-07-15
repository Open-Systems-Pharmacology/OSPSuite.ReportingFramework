#' Load Configuration Tables
#'
#' This function loads various configuration tables from specified Excel files into a global environment.
#'
#' @param projectConfiguration An object of class `ProjectConfiguration` containing paths to Excel files.
#'
#' @return An invisible value indicating that the loading was successful.
#' @export
loadConfigTableEnvironment <- function(projectConfiguration) {
  # Initialize the global configuration environment
  if (!exists("configEnv", envir = .GlobalEnv)) {
    configEnv <<- new.env() # Assign to the global environment
  }

  wbPlots <- projectConfiguration$plotsFile
  wbScenarios <- projectConfiguration$scenariosFile

  # Load data into the environment
  configEnv$outputPaths <- getOutputPathIds(wbPlots)
  configEnv$dataGroupIds <- getDataGroups(wbPlots)
  configEnv$timeTags <- getTimeRangeTags(wbPlots)
  configEnv$scenarios <- getScenarioDefinitions(wbScenarios = wbScenarios, wbPlots = wbPlots)
  configEnv$modelParameter <- getModelParameterDefinitions(wbPlots)

  return(invisible())
}
#' Load the Properties for Data Groups
#'
#' This function loads the properties for data groups from the specified workbook.
#'
#' @param wbPlots The path to the workbook containing the data groups.
#'
#' @return A `data.table` with data group IDs.
#' @export
getDataGroups <- function(wbPlots) {
  dtDataGroups <- xlsxReadData(
    wb = wbPlots,
    sheetName = "DataGroups",
    skipDescriptionRow = TRUE
  ) %>%
    setnames(old = "displayName", new = "displayNameData")

  dtDataGroups$group <- factor(dtDataGroups$group,
    levels = unique(dtDataGroups$group),
    ordered = TRUE
  )

  return(dtDataGroups)
}

#' Load the Output Configurations
#'
#' This function loads the output configurations from the specified workbook.
#'
#' @param wbPlots The path to the workbook containing the output configurations.
#'
#' @return A `data.table` with output configurations.
#' @export
getOutputPathIds <- function(wbPlots) {
  dtOutputPaths <- xlsxReadData(
    wb = wbPlots,
    sheetName = "Outputs",
    skipDescriptionRow = TRUE
  ) %>%
    setnames(old = "displayName", new = "displayNameOutput")

  dtOutputPaths[, displayUnit := gsub("Âµ", "\u00B5", as.character(displayUnit))]
  dtOutputPaths[is.na(displayUnit), displayUnit := ""]

  dtOutputPaths$outputPathId <- factor(dtOutputPaths$outputPathId,
    levels = unique(dtOutputPaths$outputPathId),
    ordered = TRUE
  )

  return(dtOutputPaths)
}
#' Load the Time Range Tags
#'
#' This function loads the time range tags from the specified workbook.
#'
#' @param wbPlots The path to the workbook containing the time range tags.
#'
#' @return A `data.table` with time range tags.
#' @export
getTimeRangeTags <- function(wbPlots) {
  dtTimeRange <- xlsxReadData(
    wb = wbPlots,
    sheetName = "TimeRange",
    skipDescriptionRow = TRUE,
    emptyAsNA = FALSE
  )

  dtTimeRange$tag <- factor(dtTimeRange$tag,
    levels = unique(dtTimeRange$tag),
    ordered = TRUE
  )

  return(dtTimeRange)
}
#' Load the Model Parameter Definitions
#'
#' This function loads the model parameter definitions from the specified workbook.
#'
#' @param wbPlots The path to the workbook containing the model parameter definitions.
#'
#' @return A `data.table` with model parameter definitions.
#' @export
getModelParameterDefinitions <- function(wbPlots) {
  dtParameter <- xlsxReadData(
    wb = wbPlots,
    sheetName = "ModelParameter",
    skipDescriptionRow = TRUE,
  ) %>%
    setnames(old = "displayName", new = "displayNameModelParameter")

  dtParameter[, displayUnit := gsub("Âµ", "\u00B5", as.character(displayUnit))]
  dtParameter[is.na(displayUnit), displayUnit := ""]

  dtParameter$parameterId <- factor(dtParameter$parameterId,
    levels = unique(dtParameter$parameterId),
    ordered = TRUE
  )

  return(dtParameter)
}
#' Load the Scenario Definitions
#'
#' This function loads the scenario definitions from the specified workbooks.
#'
#' @param wbPlots The path to the workbook containing the scenario definitions.
#' @param wbScenarios The path to the workbook containing additional scenario definitions.
#'
#' @return A `data.table` with scenario definitions.
#' @export
getScenarioDefinitions <- function(wbScenarios, wbPlots = NULL) {
  scenariosSc <- xlsxReadData(
    wb = wbScenarios,
    sheetName = "Scenarios",
    skipDescriptionRow = FALSE
  )

  pkParameter <- xlsxReadData(
    wb = wbScenarios,
    sheetName = "PKParameter",
    skipDescriptionRow = FALSE
  )

  if (nrow(pkParameter) > 0) {
    scenarios <- merge(scenariosSc,
      pkParameter,
      by = "scenario_name",
      all.x = TRUE,
      sort = FALSE
    )
  } else {
    scenarios <- copy(scenariosSc)
    scenarios[, pKParameter := NA]
  }

  setnames(scenarios, old = "scenario_name", new = "scenarioName")

  if (!is.null(wbPlots)) {
    scenariosPl <- xlsxReadData(
      wb = wbPlots,
      sheetName = "Scenarios",
      skipDescriptionRow = FALSE
    ) %>%
      setnames("scenario", "scenarioName")

    scenarios <- merge(scenariosPl,
      scenarios,
      by = "scenarioName",
      all.y = TRUE,
      sort = FALSE
    )
  }

  return(scenarios)
}
