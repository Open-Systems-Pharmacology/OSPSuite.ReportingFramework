# internal helpers ----
#' Resolve project configuration workbook path
#' @param configurationDirectory Path to the project configuration directory.
#' @return Absolute path to `ProjectConfiguration.xlsx`.
#' @keywords internal
#' @noRd
.projectConfigurationPath <- function(configurationDirectory) {
  configXlsxDest <- file.path(
    configurationDirectory,
    "ProjectConfiguration.xlsx"
  )
  if (!file.exists(configXlsxDest)) {
    stop(
      "No configuration workbook found: ProjectConfiguration.xlsx",
      call. = FALSE
    )
  }
  configXlsxDest
}

#' Check if project is already a ReportingFramework project
#' @param configWb Workbook object for project configuration.
#' @return `TRUE` if RF version is present in `addons`, otherwise `FALSE`.
#' @keywords internal
#' @noRd
.isReportingFrameworkProject <- function(configWb) {
  if (!("addons" %in% configWb$sheet_names)) {
    return(FALSE)
  }

  dtAddOnsExisting <- xlsxReadData(
    wb = configWb,
    sheetName = "addons",
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )
  if (!all(c("Property", "Value") %in% names(dtAddOnsExisting))) {
    return(FALSE)
  }

  addOnProperties <- trimws(as.character(dtAddOnsExisting$Property))
  any(
    addOnProperties == REPORTING_FRAMEWORK_VERSION_PROPERTY & # nolint: object_usage_linter.
      !is.na(dtAddOnsExisting$Value) &
      nzchar(as.character(dtAddOnsExisting$Value)),
    na.rm = TRUE
  )
}

#' Get single non-addons configuration sheet name
#' @param configWb Workbook object for project configuration.
#' @return Name of the single non-`addons` sheet.
#' @keywords internal
#' @noRd
.singleConfigSheetName <- function(configWb) {
  configSheetName <- setdiff(configWb$sheet_names, "addons")
  if (length(configSheetName) != 1) {
    stop(
      "Expected exactly one non-addons sheet in configuration workbook, found: ",
      paste(configSheetName, collapse = ", "),
      call. = FALSE
    )
  }
  configSheetName[[1]]
}

#' Ensure configuration sheet is named esqlabsR
#' @param configWb Workbook object for project configuration.
#' @param configSheetName Current configuration sheet name.
#' @return Invisible `NULL`.
#' @keywords internal
#' @noRd
.ensureEsqlabsRSheet <- function(configWb, configSheetName) {
  if (configSheetName != "esqlabsR") {
    openxlsx::cloneWorksheet(
      wb = configWb,
      clonedSheet = configSheetName,
      sheetName = "esqlabsR"
    )
    openxlsx::removeWorksheet(wb = configWb, sheet = configSheetName)
  }
  invisible(NULL)
}

#' Resolve template esqlabsR sheet name
#' @param templateConfigurationWb Workbook object for template configuration.
#' @return Template sheet name used as esqlabsR source.
#' @keywords internal
#' @noRd
.templateEsqlabsSheetName <- function(templateConfigurationWb) {
  if ("esqlabsR" %in% templateConfigurationWb$sheet_names) {
    return("esqlabsR")
  }
  setdiff(templateConfigurationWb$sheet_names, "addons")[[1]]
}

#' Read configuration sheet without header conversion
#' @param wb Workbook object or path to workbook.
#' @param sheetName Sheet name to read.
#' @return A data frame with configuration sheet content.
#' @keywords internal
#' @noRd
.readConfigSheet <- function(wb, sheetName) {
  xlsxReadData(
    wb = wb,
    sheetName = sheetName,
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )
}

#' Normalize configuration value
#' @param x A scalar configuration value.
#' @return Trimmed character scalar or empty string.
#' @keywords internal
#' @noRd
.normalizeConfigValue <- function(x) {
  if (length(x) == 0 || is.na(x[[1]])) {
    return("")
  }
  trimws(as.character(x[[1]]))
}

#' Resolve file path from configuration row
#' @param dtConfig Configuration table with `Property` and `Value` columns.
#' @param configurationDirectory Base directory of the configuration workbook.
#' @param property Property name containing a file path.
#' @return Absolute file path or `NA_character_`.
#' @keywords internal
#' @noRd
.resolveConfiguredFilePath <- function(
  dtConfig,
  configurationDirectory,
  property
) {
  fileValue <- .normalizeConfigValue(dtConfig$Value[
    dtConfig$Property == property
  ])
  if (!nzchar(fileValue)) {
    return(NA_character_)
  }

  configFolder <- .resolveConfigurationFolder(dtConfig, configurationDirectory)
  fs::path_abs(fileValue, start = configFolder)
}

#' Copy a single filesystem entry to its target path
#' @param srcPath Absolute source path.
#' @param dstPath Absolute destination path.
#' @param overwrite Logical flag controlling overwrite behavior.
#' @return Invisible `NULL`.
#' @keywords internal
#' @noRd
.copyDirectoryEntry <- function(srcPath, dstPath, overwrite) {
  if (dir.exists(srcPath)) {
    if (!dir.exists(dstPath)) {
      dir.create(dstPath, recursive = TRUE, showWarnings = FALSE)
    }
    return(invisible(NULL))
  }
  dstDir <- dirname(dstPath)
  if (!dir.exists(dstDir)) {
    dir.create(dstDir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!file.exists(dstPath) || overwrite) {
    file.copy(from = srcPath, to = dstPath, overwrite = overwrite)
  }
  return(invisible(NULL))
}

#' Copy directory content recursively
#' @param sourceDir Source directory.
#' @param targetDir Target directory.
#' @param overwrite Logical flag controlling overwrite behavior.
#' @return Invisible `NULL`.
#' @keywords internal
#' @noRd
.copyDirectoryContents <- function(sourceDir, targetDir, overwrite) {
  if (!dir.exists(sourceDir)) {
    return(invisible(NULL))
  }
  entries <- list.files(
    sourceDir,
    recursive = TRUE,
    all.files = TRUE,
    include.dirs = TRUE,
    no.. = TRUE
  )
  if (length(entries) == 0) {
    return(invisible(NULL))
  }
  for (entry in entries) {
    .copyDirectoryEntry(
      srcPath = file.path(sourceDir, entry),
      dstPath = file.path(targetDir, entry),
      overwrite = overwrite
    )
  }
  return(invisible(NULL))
}

#' Copy individual configuration files listed under *File properties
#' @param dtConfigOld Original esqlabsR configuration table.
#' @param dtConfigNew Updated RF configuration table.
#' @param configurationDirectory Base directory of the configuration workbook.
#' @param overwrite Logical flag controlling overwrite behavior.
#' @return Invisible `NULL`.
#' @keywords internal
#' @noRd
.copyConfigurationFile <- function(oldFile, newFile, overwrite) {
  if (is.na(oldFile) || is.na(newFile)) {
    return(invisible(NULL))
  }
  if (identical(oldFile, newFile) || !file.exists(oldFile)) {
    return(invisible(NULL))
  }
  newDir <- dirname(newFile)
  if (!dir.exists(newDir)) {
    dir.create(newDir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!file.exists(newFile) || overwrite) {
    file.copy(from = oldFile, to = newFile, overwrite = overwrite)
  }
  return(invisible(NULL))
}

.transferConfigurationFilesByProperty <- function(
  dtConfigOld,
  dtConfigNew,
  configurationDirectory,
  overwrite
) {
  fileProperties <- grep(
    "File$",
    intersect(dtConfigOld$Property, dtConfigNew$Property),
    value = TRUE
  )
  for (prop in unique(fileProperties)) {
    .copyConfigurationFile(
      oldFile = .resolveConfiguredFilePath(
        dtConfigOld,
        configurationDirectory,
        prop
      ),
      newFile = .resolveConfiguredFilePath(
        dtConfigNew,
        configurationDirectory,
        prop
      ),
      overwrite = overwrite
    )
  }
  return(invisible(NULL))
}

#' Transfer initialized esqlabsR files into RF target layout
#' @param dtConfigOld Original esqlabsR configuration table.
#' @param dtConfigNew Updated RF configuration table.
#' @param configurationDirectory Base directory of the configuration workbook.
#' @param overwrite Logical flag controlling overwrite behavior.
#' @return Invisible `NULL`.
#' @keywords internal
#' @noRd
.transferInitializedFilesToNewStructure <- function(
  dtConfigOld,
  dtConfigNew,
  configurationDirectory,
  overwrite
) {
  oldFolder <- .normalizeConfigValue(dtConfigOld$Value[
    dtConfigOld$Property == "configurationsFolder"
  ])
  newFolder <- .normalizeConfigValue(dtConfigNew$Value[
    dtConfigNew$Property == "configurationsFolder"
  ])
  if (!nzchar(oldFolder) || !nzchar(newFolder)) {
    return(invisible(NULL))
  }
  oldFolderAbs <- fs::path_abs(oldFolder, start = configurationDirectory)
  newFolderAbs <- fs::path_abs(newFolder, start = configurationDirectory)
  if (identical(oldFolderAbs, newFolderAbs)) {
    return(invisible(NULL))
  }
  if (!dir.exists(newFolderAbs)) {
    dir.create(newFolderAbs, recursive = TRUE, showWarnings = FALSE)
  }
  .copyDirectoryContents(oldFolderAbs, newFolderAbs, overwrite)
  .transferConfigurationFilesByProperty(
    dtConfigOld,
    dtConfigNew,
    configurationDirectory,
    overwrite
  )
  return(invisible(NULL))
}

#' Delete all subdirectories in legacy configurations folder
#' @param dtConfigOld Original esqlabsR configuration table.
#' @param dtConfigNew Updated RF configuration table.
#' @param configurationDirectory Base directory of the configuration workbook.
#' @return Invisible `NULL`.
#' @keywords internal
#' @noRd
.cleanupLegacyConfigurationDirectories <- function(
  dtConfigOld,
  dtConfigNew,
  configurationDirectory
) {
  oldFolderAbs <- .resolveConfigurationFolder(
    dtConfigOld,
    configurationDirectory
  )
  newFolderAbs <- .resolveConfigurationFolder(
    dtConfigNew,
    configurationDirectory
  )

  if (!dir.exists(oldFolderAbs) || identical(oldFolderAbs, newFolderAbs)) {
    return(invisible(NULL))
  }
  dirsToDelete <- setdiff(
    list.dirs(newFolderAbs, recursive = FALSE, full.names = TRUE),
    newFolderAbs
  )

  if (length(dirsToDelete) > 0) {
    unlink(dirsToDelete, recursive = TRUE, force = TRUE)
  }

  invisible(NULL)
}

#' Apply template values to esqlabsR sheet
#' @param configWb Workbook object for project configuration.
#' @param templateConfigurationWb Workbook object for template configuration.
#' @return Updated `esqlabsR` configuration table.
#' @keywords internal
#' @noRd
.applyTemplateToConfigSheet <- function(
  configWb,
  templateConfigurationWb,
  configurationDirectory,
  overwrite,
  transferInitializedFiles = FALSE
) {
  dtConfiguration <- .readConfigSheet(configWb, "esqlabsR")
  dtConfigurationOriginal <- dtConfiguration
  dtTemplateConfiguration <- .readConfigSheet(
    templateConfigurationWb,
    .templateEsqlabsSheetName(templateConfigurationWb)
  )

  if (!all(c("Property", "Value") %in% names(dtConfiguration))) {
    stop(
      "Configuration sheet must contain columns 'Property' and 'Value'.",
      call. = FALSE
    )
  }
  if (!all(c("Property", "Value") %in% names(dtTemplateConfiguration))) {
    stop(
      "Template configuration sheet must contain columns 'Property' and 'Value'.",
      call. = FALSE
    )
  }

  templateRows <- dtTemplateConfiguration[
    dtTemplateConfiguration$Property %in% dtConfiguration$Property,
    ,
    drop = FALSE
  ]
  if (nrow(templateRows) > 0) {
    dtConfiguration[
      match(templateRows$Property, dtConfiguration$Property),
      "Value"
    ] <- templateRows$Value

    if (
      "Description" %in%
        names(dtConfiguration) &&
        "Description" %in% names(templateRows)
    ) {
      dtConfiguration[
        match(templateRows$Property, dtConfiguration$Property),
        "Description"
      ] <- templateRows$Description
    }
  }

  if ("esqlabsRVersion" %in% dtConfiguration$Property) {
    dtConfiguration[
      dtConfiguration$Property == "esqlabsRVersion",
      "Value"
    ] <- as.character(utils::packageVersion("esqlabsR"))
  }

  if (isTRUE(transferInitializedFiles)) {
    .transferInitializedFilesToNewStructure(
      dtConfigOld = dtConfigurationOriginal,
      dtConfigNew = dtConfiguration,
      configurationDirectory = configurationDirectory,
      overwrite = overwrite
    )
    .cleanupLegacyConfigurationDirectories(
      dtConfigOld = dtConfigurationOriginal,
      dtConfigNew = dtConfiguration,
      configurationDirectory = configurationDirectory
    )
  }

  xlsxWriteData(wb = configWb, sheetName = "esqlabsR", dt = dtConfiguration)
  dtConfiguration
}

#' Apply template addons sheet
#' @param configWb Workbook object for project configuration.
#' @param templateConfigurationWb Workbook object for template configuration.
#' @return Invisible `NULL`.
#' @keywords internal
#' @noRd
.applyTemplateAddonsSheet <- function(configWb, templateConfigurationWb) {
  if ("addons" %in% configWb$sheet_names) {
    openxlsx::removeWorksheet(wb = configWb, sheet = "addons")
  }
  openxlsx::addWorksheet(wb = configWb, sheetName = "addons")

  dtTemplateAddOns <- .readConfigSheet(templateConfigurationWb, "addons")
  if (
    !all(c("Property", "Value", "Description") %in% names(dtTemplateAddOns))
  ) {
    stop(
      "Template addons sheet must contain 'Property', 'Value', and 'Description'.",
      call. = FALSE
    )
  }

  addOnProperties <- trimws(as.character(dtTemplateAddOns$Property))
  if (
    any(addOnProperties == REPORTING_FRAMEWORK_VERSION_PROPERTY, na.rm = TRUE) # nolint: object_usage_linter.
  ) {
    versionIndex <- which(
      addOnProperties == REPORTING_FRAMEWORK_VERSION_PROPERTY # nolint: object_usage_linter.
    )[[1]]
    dtTemplateAddOns[
      versionIndex,
      c("Value", "Description")
    ] <- list(
      .currentReportingFrameworkVersion(), # nolint: object_usage_linter.
      .reportingFrameworkVersionDescription() # nolint: object_usage_linter.
    )
  } else {
    dtTemplateAddOns <- rbind(
      dtTemplateAddOns,
      data.frame(
        Property = REPORTING_FRAMEWORK_VERSION_PROPERTY, # nolint: object_usage_linter.
        Value = .currentReportingFrameworkVersion(), # nolint: object_usage_linter.
        Description = .reportingFrameworkVersionDescription(), # nolint: object_usage_linter.
        stringsAsFactors = FALSE
      )
    )
  }

  openxlsx::writeData(wb = configWb, sheet = "addons", x = dtTemplateAddOns)
  invisible(NULL)
}

#' Resolve configuration folder from configuration table
#' @param dtConfig Configuration table with `Property` and `Value` columns.
#' @param configurationDirectory Base directory of the configuration workbook.
#' @return Absolute path to the configuration folder.
#' @keywords internal
#' @noRd
.resolveConfigurationFolder <- function(dtConfig, configurationDirectory) {
  configFolderValue <- dtConfig$Value[
    dtConfig$Property == "configurationsFolder"
  ]
  configFolderValue <- .normalizeConfigValue(configFolderValue)
  if (!nzchar(configFolderValue)) {
    return(configurationDirectory)
  }
  fs::path_abs(configFolderValue, start = configurationDirectory)
}

#' Resolve scenarios workbook path from configuration table
#' @param dtConfig Configuration table with `Property` and `Value` columns.
#' @param configFolderPath Absolute path to the configuration folder.
#' @return Absolute path to the scenarios workbook or `NA_character_`.
#' @keywords internal
#' @noRd
.resolveScenarioPath <- function(dtConfig, configFolderPath) {
  scenarioFileValue <- dtConfig$Value[dtConfig$Property == "scenariosFile"]
  scenarioFileValue <- .normalizeConfigValue(scenarioFileValue)
  if (nzchar(scenarioFileValue)) {
    return(fs::path_abs(scenarioFileValue, start = configFolderPath))
  }
  NA_character_
}

#' Create project directories defined in the configuration
#' @param dtConfiguration Configuration table with `Property` and `Value` columns.
#' @param configurationDirectory Base directory of the configuration workbook.
#' @return Invisible `NULL`.
#' @keywords internal
#' @noRd
.createProjectDirectories <- function(dtConfiguration, configurationDirectory) {
  folderProperties <- c(
    "modelFolder",
    "configurationsFolder",
    "populationsFolder",
    "outputFolder",
    "dataFolder",
    "electronicPackageFolder"
  )

  for (prop in folderProperties) {
    val <- .normalizeConfigValue(dtConfiguration$Value[
      dtConfiguration$Property == prop
    ])
    if (!nzchar(val)) {
      next
    }
    dirPath <- fs::path_abs(val, start = configurationDirectory)
    if (!dir.exists(dirPath)) {
      dir.create(dirPath, recursive = TRUE, showWarnings = FALSE)
    }
  }

  invisible(NULL)
}

#' Copy RF configuration workbooks into destination folder
#' @param templatePath Path to RF template directory.
#' @param destinationFolder Destination configuration folder.
#' @param overwrite Logical flag controlling overwrite behavior.
#' @return Invisible `NULL`.
#' @keywords internal
#' @noRd
.copyReportingFrameworkConfigFiles <- function(
  templatePath,
  destinationFolder,
  overwrite
) {
  filesToCopy <- c(
    "DataImportConfiguration.xlsx",
    "Reports.xlsx",
    "PKParameter.xlsx",
    "SensitivityParameter.xlsx"
  )

  for (f in filesToCopy) {
    file.copy(
      from = file.path(templatePath, f),
      to = file.path(destinationFolder, f),
      overwrite = overwrite || !file.exists(file.path(destinationFolder, f))
    )
  }

  invisible(NULL)
}

#' Find existing scenarios workbook candidate
#' @param configurationDirectory Base configuration directory.
#' @param oldConfigurationFolder Previously resolved configuration folder.
#' @param scenariosXlsxNew Preferred scenarios workbook path.
#' @param scenariosXlsxOld Fallback scenarios workbook path.
#' @return Existing scenarios workbook path or `NA_character_`.
#' @keywords internal
#' @noRd
.findScenariosWorkbook <- function(
  configurationDirectory,
  oldConfigurationFolder,
  scenariosXlsxNew,
  scenariosXlsxOld
) {
  scenarioCandidates <- unique(c(
    scenariosXlsxNew,
    scenariosXlsxOld,
    file.path(configurationDirectory, "Scenarios.xlsx"),
    file.path(configurationDirectory, "Scenario.xlsx"),
    file.path(oldConfigurationFolder, "Scenarios.xlsx"),
    file.path(oldConfigurationFolder, "Scenario.xlsx")
  ))
  scenarioCandidates <- scenarioCandidates[!is.na(scenarioCandidates)]
  existingScenarioCandidates <- scenarioCandidates[file.exists(
    scenarioCandidates
  )]
  if (length(existingScenarioCandidates) > 0) {
    return(existingScenarioCandidates[[1]])
  }
  NA_character_
}

#' Copy scenarios workbook to target location if needed
#' @param scenariosXlsxNew Preferred scenarios workbook path.
#' @param scenariosXlsx Existing scenarios workbook path.
#' @param overwrite Logical flag controlling overwrite behavior.
#' @return Path to scenarios workbook after copy attempt.
#' @keywords internal
#' @noRd
.copyScenariosWorkbookIfNeeded <- function(
  scenariosXlsxNew,
  scenariosXlsx,
  overwrite
) {
  if (
    !is.na(scenariosXlsxNew) &&
      !file.exists(scenariosXlsxNew) &&
      !is.na(scenariosXlsx) &&
      file.exists(scenariosXlsx)
  ) {
    file.copy(
      from = scenariosXlsx,
      to = scenariosXlsxNew,
      overwrite = overwrite
    )
    if (file.exists(scenariosXlsxNew)) {
      return(scenariosXlsxNew)
    }
  }
  scenariosXlsx
}

#' Add PKParameter sheet to scenarios workbook
#' @param templatePath Path to RF template directory.
#' @param scenariosXlsx Path to scenarios workbook.
#' @param overwrite Logical flag controlling overwrite behavior.
#' @return Invisible `NULL`.
#' @keywords internal
#' @noRd
.addPkParameterSheetToScenarios <- function(
  templatePath,
  scenariosXlsx,
  overwrite
) {
  if (is.na(scenariosXlsx) || !file.exists(scenariosXlsx)) {
    return(invisible(NULL))
  }

  templateScenarios <- file.path(templatePath, "Scenarios.xlsx")
  checkmate::assertFileExists(templateScenarios)
  dtTemplatePkParameter <- xlsxReadData(
    wb = templateScenarios,
    sheetName = "PKParameter",
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )

  wbScenarios <- openxlsx::loadWorkbook(scenariosXlsx)
  if ("PKParameter" %in% wbScenarios$sheet_names) {
    if (overwrite) {
      openxlsx::removeWorksheet(wbScenarios, "PKParameter")
    } else {
      openxlsx::saveWorkbook(wbScenarios, scenariosXlsx, overwrite = TRUE)
      return(invisible(NULL))
    }
  }

  openxlsx::addWorksheet(wbScenarios, "PKParameter")
  openxlsx::writeData(
    wb = wbScenarios,
    sheet = "PKParameter",
    x = dtTemplatePkParameter
  )
  openxlsx::saveWorkbook(wbScenarios, scenariosXlsx, overwrite = TRUE)
  invisible(NULL)
}

#' Upgrade an esqlabsR Project to a ReportingFramework Project
#'
#' @description Adds reporting-framework-specific configuration and template files
#'   to a project directory. This is the second step in the two-step project
#'   initialization workflow:
#'   \enumerate{
#'     \item Call \code{esqlabsR::initProject()} to set up the base esqlabsR project.
#'     \item Call \code{upgradeToReportingFramework()} to add RF-specific components.
#'   }
#'
#' @param configurationDirectory A character string specifying the path to the RF
#'   configuration directory to be set up. Defaults to the current working directory.
#' @param templatePath Path to the RF template files. Defaults to the package templates.
#' @param overwrite A logical value indicating whether to overwrite existing files.
#'   Defaults to \code{FALSE}.
#' @param keepEsqlabsRStructure If `TRUE` (default), keep the existing esqlabsR
#'   directory/layout values and only add reporting-framework-specific components.
#'   If `FALSE`, apply reporting framework default configuration values and transfer
#'   files generated by esqlabsR initialization into the new structure.
#'
#' @return Invisible NULL. Used for its side effects of creating directories and
#'   copying files.
#' @export
#' @family project initialization
upgradeToReportingFramework <- function(
  configurationDirectory = ".",
  templatePath = system.file(
    "templates",
    package = "ospsuite.reportingframework"
  ),
  overwrite = FALSE,
  keepEsqlabsRStructure = TRUE
) {
  configurationDirectory <- fs::path_abs(configurationDirectory)
  checkmate::assertDirectoryExists(templatePath)
  checkmate::assertFlag(keepEsqlabsRStructure)

  if (!dir.exists(configurationDirectory)) {
    dir.create(configurationDirectory, recursive = TRUE, showWarnings = FALSE)
  }

  configXlsxDest <- .projectConfigurationPath(configurationDirectory)

  templateConfigurationXlsx <- file.path(
    templatePath,
    "ProjectConfiguration.xlsx"
  )
  checkmate::assertFileExists(templateConfigurationXlsx)
  templateConfigurationWb <- openxlsx::loadWorkbook(templateConfigurationXlsx)

  configWb <- openxlsx::loadWorkbook(configXlsxDest)
  if (.isReportingFrameworkProject(configWb)) {
    return(invisible())
  }

  configSheetName <- .singleConfigSheetName(configWb)
  .ensureEsqlabsRSheet(configWb, configSheetName)

  dtConfiguration <- .readConfigSheet(configWb, "esqlabsR")
  dtConfigurationOriginal <- dtConfiguration
  if (!keepEsqlabsRStructure) {
    dtConfiguration <- .applyTemplateToConfigSheet(
      configWb = configWb,
      templateConfigurationWb = templateConfigurationWb,
      configurationDirectory = configurationDirectory,
      overwrite = overwrite,
      transferInitializedFiles = TRUE
    )
  }
  .applyTemplateAddonsSheet(configWb, templateConfigurationWb)
  openxlsx::saveWorkbook(configWb, configXlsxDest, overwrite = TRUE)
  stampReportingFrameworkVersion(path = configXlsxDest) # nolint: object_usage_linter.

  oldConfigurationFolder <- .resolveConfigurationFolder(
    dtConfigurationOriginal,
    configurationDirectory
  )
  newConfigurationFolder <- .resolveConfigurationFolder(
    dtConfiguration,
    configurationDirectory
  )
  if (!dir.exists(newConfigurationFolder)) {
    dir.create(newConfigurationFolder, recursive = TRUE, showWarnings = FALSE)
  }

  .copyReportingFrameworkConfigFiles(
    templatePath,
    newConfigurationFolder,
    overwrite
  )

  scenariosXlsxNew <- .resolveScenarioPath(
    dtConfiguration,
    newConfigurationFolder
  )
  scenariosXlsxOld <- .resolveScenarioPath(
    dtConfigurationOriginal,
    oldConfigurationFolder
  )
  scenariosXlsx <- .findScenariosWorkbook(
    configurationDirectory = configurationDirectory,
    oldConfigurationFolder = oldConfigurationFolder,
    scenariosXlsxNew = scenariosXlsxNew,
    scenariosXlsxOld = scenariosXlsxOld
  )
  scenariosXlsx <- .copyScenariosWorkbookIfNeeded(
    scenariosXlsxNew,
    scenariosXlsx,
    overwrite
  )
  .addPkParameterSheetToScenarios(templatePath, scenariosXlsx, overwrite)
  .createProjectDirectories(dtConfiguration, configurationDirectory)

  return(invisible())
}

#' Initialize a ReportingFramework Project Directory
#'
#' @description Convenience wrapper that initializes a complete ReportingFramework
#'   project by first calling \code{esqlabsR::initProject()} for the base esqlabsR
#'   structure and then \code{upgradeToReportingFramework()} for the RF-specific
#'   configuration.
#'
#'   For more control, call the two steps separately:
#'   \enumerate{
#'     \item \code{esqlabsR::initProject(destination)}
#'     \item \code{upgradeToReportingFramework(configurationDirectory)}
#'   }
#'
#' @param projectDirectory A character string specifying the root project directory
#'   passed to \code{esqlabsR::initProject()}. Defaults to the current working directory.
#' @param configurationDirectory A character string specifying the path to the RF
#'   configuration subdirectory. Defaults to \code{"Scripts/ReportingFramework"} relative
#'   to \code{projectDirectory}.
#' @param templatePath Path to the RF template files. Defaults to the package templates.
#' @param overwrite A logical value indicating whether to overwrite existing files.
#'   Defaults to \code{FALSE}.
#'
#' @return Invisible NULL.
#' @export
#' @family project initialization
initProject <- function(
  projectDirectory = ".",
  configurationDirectory = file.path(
    projectDirectory,
    "Scripts",
    "ReportingFramework"
  ),
  templatePath = system.file(
    "templates",
    package = "ospsuite.reportingframework"
  ),
  overwrite = FALSE
) {
  configurationDirectory <- fs::path_abs(configurationDirectory)
  if (!dir.exists(configurationDirectory)) {
    dir.create(configurationDirectory, recursive = TRUE, showWarnings = FALSE)
  }
  esqlabsR::initProject(
    destination = configurationDirectory,
    overwrite = overwrite
  )
  upgradeToReportingFramework(
    configurationDirectory = configurationDirectory,
    templatePath = templatePath,
    overwrite = overwrite,
    keepEsqlabsRStructure = FALSE
  )

  # Create initial JSON snapshot of the project configuration
  pc <- createProjectConfiguration(
    path = file.path(configurationDirectory, "ProjectConfiguration.xlsx"),
    ignoreVersionCheck = FALSE
  )
  snapshotProjectConfigurationRF(pc, outputDir = configurationDirectory) # nolint: object_usage_linter.

  return(invisible())
}

#' #' Create a `ProjectConfiguration`
#'
#' @description  Create a `ProjectConfigurationRF` based on the `"ProjectConfiguration.xlsx"`
#'
#' based on esqlabsR::ProjectConfiguration but with additional file information for PK Parameter definitions
#'
#' @param path path to the `ProjectConfiguration.xlsx` file. default to the `ProjectConfiguration.xlsx` file located in the working directory.
#' @param ignoreVersionCheck If `TRUE`, skip version mismatch checks for esqlabsR and the
#'   reporting framework when loading the configuration file. Defaults to `FALSE`.
#'
#' @return Object of type `ProjectConfigurationRF`
#' @export
#' @family project initialization
createProjectConfiguration <- function(
  path = file.path("ProjectConfiguration.xlsx"),
  ignoreVersionCheck = FALSE
) {
  projectConfiguration <- ProjectConfigurationRF$new(
    projectConfigurationFilePath = path,
    ignoreVersionCheck = ignoreVersionCheck
  )

  return(projectConfiguration)
}
#' Create Scenario objects from `ScenarioConfiguration` objects
#'
#' wrap of `esqlabsR::createDefaultProjectConfiguration()` with `esqlabsR::createScenarios()` as input
#'
#' @param projectConfiguration Object of class `ProjectConfiguration` containing information on paths and file names
#' @param scenarioNames Names of the scenarios that are defined in the excel file.
#' If NULL (default), all scenarios specified in the excel file will be created.
#'
#' @return  Named list of Scenario objects.
#' @export
#' @family scenario management
createScenariosWrapped <- function(
  projectConfiguration,
  scenarioNames = NULL
) {
  baseProjectConfiguration <- projectConfiguration$baseProjectconfiguration
  scenarioList <-
    esqlabsR::createScenarios(
      scenarioConfigurations = esqlabsR::readScenarioConfigurationFromExcel(
        scenarioNames = scenarioNames,
        projectConfiguration = baseProjectConfiguration
      )
    )

  synchronizeScenariosWithPlots(projectConfiguration)
  synchronizeScenariosOutputsWithPlots(projectConfiguration)

  return(scenarioList)
}
#' Load existing scenario results
#'
#' This function loads the results of specified scenarios. If the results do not exist,
#' it returns an error.
#'
#' @param projectConfiguration Configuration for the project, containing paths and settings necessary
#' to load the results.
#' @param scenarioNames Character vector of the names of the scenarios whose results are to be loaded.
#'
#' @return A list containing the loaded scenario results, including population data if available.
#' throws Error if the scenario results do not exist.
#'
#' @export
#' @family scenario management
loadScenarioResultsToFramework <- function(
  projectConfiguration,
  scenarioNames
) {
  outputFolder <- file.path(
    projectConfiguration$outputFolder,
    EXPORTDIR$simulationResult
  )
  resultFiles <- file.path(outputFolder, paste0(scenarioNames, ".csv"))

  if (!all(file.exists(resultFiles))) {
    stop(paste(
      "Error: Simulation results for scenario(s)",
      paste(scenarioNames[!file.exists(resultFiles)], collapse = ", "),
      "do not exist."
    ))
  }

  scenarioResults <- list()

  for (sc in scenarioNames) {
    writeToLog(type = "Info", msg = paste("Load simulation result of", sc))

    scenarioResult <- esqlabsR::loadScenarioResults(
      scenarioNames = sc,
      resultsFolder = outputFolder
    )[[1]]

    # Load population if it exists
    popFile <- file.path(outputFolder, paste0(sc, "_population.csv"))
    if (file.exists(popFile)) {
      scenarioResult[["population"]] <- ospsuite::loadPopulation(popFile)
    }

    scenarioResults[[sc]] <- scenarioResult
  }

  return(scenarioResults)
}
#' Run and save scenarios
#'
#' This function simulates a list of scenarios and saves the results.
#' If results already exist for a scenario, it will overwrite them based on the provided options.
#'
#' @param projectConfiguration Configuration for the project, containing paths and settings necessary
#' to run the simulations and save the results.
#' @param scenarioList Named list of Scenario objects to be simulated.
#' @param simulationRunOptions Object of type `SimulationRunOptions` that will be passed to simulation runs.
#' If `NULL`, default options are used.
#' @param ... Additional arguments passed to `esqlabsR::saveScenarioResults`.
#'
#' @return A list containing the simulation results for each scenario that was run.
#'
#' @examples
#' \dontrun{
#' runAndSaveScenarios(
#'   projectConfiguration = myProjectConfig,
#'   scenarioList = myScenarioList,
#'   simulationRunOptions = myRunOptions
#' )
#' }
#'
#' @export
#' @family scenario management
runAndSaveScenarios <- function(
  projectConfiguration,
  scenarioList,
  simulationRunOptions = NULL,
  ...
) {
  outputFolder <- file.path(
    projectConfiguration$outputFolder,
    EXPORTDIR$simulationResult
  )

  scenarioResults <- list()

  for (sc in names(scenarioList)) {
    writeToLog(type = "Info", msg = paste("Start simulation of", sc))

    # Make sure custom params are not again overwritten by population
    scenarioList[[sc]] <- setCustomParamsToPopulation(scenarioList[[sc]])

    scenarioResults[sc] <- esqlabsR::runScenarios(
      scenarios = scenarioList[sc],
      simulationRunOptions = simulationRunOptions
    )

    # Set scenario name as simulation name
    scenarioResults[[sc]]$simulation$set("Name", sc)

    esqlabsR::saveScenarioResults(
      simulatedScenariosResults = scenarioResults[sc],
      projectConfiguration = projectConfiguration$baseProjectconfiguration,
      outputFolder = outputFolder,
      ...
    )
  }
  calculatePKParameterForScenarios(projectConfiguration, scenarioResults)

  return(invisible(scenarioResults))
}

#' Run or load scenarios
#'
#' This function checks if the simulation results for scenarios already exist.
#' If they do, it loads them; otherwise, it runs the scenarios and saves the results.
#'
#' @param projectConfiguration Configuration for the project, containing paths and settings necessary
#' to run the simulations and load the results.
#' @param scenarioList Named list of Scenario objects to be managed.
#' @param simulationRunOptions Object of type `SimulationRunOptions` that will be passed to simulation runs.
#' If `NULL`, default options are used.
#' @param ... Additional arguments passed to `runAndSaveScenarios`.
#'
#' @return A list containing the simulation results for each scenario that was loaded or run.
#'
#' @export
#' @family scenario management
runOrLoadScenarios <- function(
  projectConfiguration,
  scenarioList,
  simulationRunOptions = NULL,
  ...
) {
  scenarioResults <- list()

  for (sc in names(scenarioList)) {
    if (
      file.exists(file.path(
        projectConfiguration$outputFolder,
        EXPORTDIR$simulationResult,
        paste0(sc, ".csv")
      ))
    ) {
      scenarioResults[sc] <- loadScenarioResultsToFramework(
        projectConfiguration,
        sc
      )
    } else {
      scenarioResults[sc] <- runAndSaveScenarios(
        projectConfiguration,
        scenarioList[sc],
        simulationRunOptions,
        ...
      )
    }
  }

  return(invisible(scenarioResults))
}
#' Read Ontogenies from Data
#'
#' based on esqlabsR:::.readOntongeniesFromXLS
#'
#' This function extracts protein ontogeny mappings from the provided data.
#' It splits the mappings into individual protein-ontogeny pairs and validates
#' the structure of each pair. Each valid pair is then converted into a
#' `MoleculeOntogeny` object.
#'
#' @param data A data frame containing a column named "Protein Ontogenies".
#'
#' @return A list of `MoleculeOntogeny` objects, each representing a protein
#' and its corresponding ontogeny. Returns NULL if the "Protein Ontogenies"
#' field is NA.
#'
#' @keywords internal
readOntongenies <- function(data) {
  proteinOntogenyMappings <- data[["protein Ontogenies"]]
  if (is.na(proteinOntogenyMappings)) {
    return(NULL)
  }
  proteinOntogenyMappings <- as.character(proteinOntogenyMappings)
  proteinOntogenyMappings <- unlist(strsplit(
    x = proteinOntogenyMappings,
    split = ",",
    fixed = TRUE
  ))
  proteinOntogenyMappings <- trimws(proteinOntogenyMappings)
  moleculeOntogenies <- vector("list", length(proteinOntogenyMappings))
  for (i in seq_along(proteinOntogenyMappings)) {
    ontogeny <- proteinOntogenyMappings[[i]]
    ontogenyMapping <- unlist(strsplit(
      x = ontogeny,
      split = ":",
      fixed = TRUE
    ))
    if (length(ontogenyMapping) != 2) {
      stop(paste("The ontogeny has the wrong structure:", ontogeny))
    }
    protein <- ontogenyMapping[[1]]
    ontogeny <- ontogenyMapping[[2]]
    ospsuite.utils::validateEnumValue(
      value = ontogeny,
      enum = ospsuite::StandardOntogeny
    )
    moleculeOntogenies[[i]] <- ospsuite::MoleculeOntogeny$new(
      molecule = protein,
      ontogeny = ospsuite::StandardOntogeny[[ontogeny]]
    )
  }
  return(moleculeOntogenies)
}
#' Add user defined variability on parameters to a population from an excel file.
#'
#' @param population Object of type `Population`
#' @param XLSpath Path to the excel file that stores the information of
#'   parameters. The file must have the columns "Container.Path",
#'   "Parameter.Name", "Mean", "SD", "Units", and "Distribution". Mean and SD
#'   values must be in the base units of the parameters.
#' @param sheet Name or the index of the sheet in the excel file.
#' If `NULL`, the first sheet in the file is used.
#'
#' @details The method reads the information from the specified excel sheet(s)
#'   and calls `extendPopulationByUserDefinedParams`
#'   copy of esqlabsR::extendPopulationFromXLS but columnNames always withdot
#'
#' @keywords internal
extendPopulationFromXLS_RF <- function(population, XLSpath, sheet = NULL) {
  # nolint: object_name_linter.
  ospsuite.utils::validateIsOfType(population, "Population")
  ospsuite.utils::validateIsString(XLSpath)
  ospsuite.utils::validateIsString(sheet, nullAllowed = TRUE)
  if (is.null(sheet)) {
    sheet <- 1
  }

  columnNames <- c(
    "Container.Path",
    "Parameter.Name",
    "Mean",
    "SD",
    "Distribution"
  )

  data <- readExcel(path = XLSpath, sheet = sheet)
  names(data) <- gsub(" ", "\\.", names(data))
  if (!all(columnNames %in% names(data))) {
    stop("errorWrongXLSStructure")
    # stop(messages$errorWrongXLSStructure(filePath = XLSpath, expectedColNames = columnNames)) # nolint
  }

  paramPaths <- c(dim(data)[[1]])
  meanVals <- c(dim(data)[[1]])
  sdVals <- c(dim(data)[[1]])
  distributions <- c(dim(data)[[1]])

  for (i in seq_along(data$Container.Path)) {
    paramPath <- paste(
      data[["Container.Path"]][[i]],
      data[["Parameter.Name"]][[i]],
      sep = "|"
    )
    paramPaths[[i]] <- paramPath
    meanVals[[i]] <- as.numeric(data[["Mean"]][[i]])
    sdVals[[i]] <- as.numeric(data[["SD"]][[i]])
    distributions[[i]] <- data[["Distribution"]][[i]]
  }

  extendPopulationByUserDefinedParams_RF(
    population = population,
    parameterPaths = paramPaths,
    meanValues = meanVals,
    sdValues = sdVals,
    distributions = distributions
  )
}


#' Add user defined variability on parameters to a population.
#'
#' @param population Object of type `Population`
#' @param parameterPaths Vector of parameter path for which the variability is to be added.
#' @param meanValues Vector of mean values of the parameters. Must have the same
#'   length as `parameterPaths`. The type of mean (arithmetic, geometric)
#'   depends on the selected `distribution`. The values must be in the base
#'   units of the parameters.
#' @param sdValues Vector of standard deviation values of the parameters. Must
#'   have the same length as `parameterPaths`. The type of standard deviation
#'   depends on the selected `distribution`.
#' @param distributions Type of distribution from which the random values will
#'   be sampled. Must have the same length as `parameterPaths`.
#' A list of supported distributions is defined in `Distributions`. Default is `"Normal"`.
#' @keywords internal
extendPopulationByUserDefinedParams_RF <- function(
  # nolint: object_name_linter.
  population,
  parameterPaths,
  meanValues,
  sdValues,
  distributions = Distributions$Normal
) {
  ospsuite.utils::validateIsOfType(population, "Population")
  ospsuite.utils::validateIsString(parameterPaths)
  ospsuite.utils::validateIsNumeric(sdValues)
  ospsuite.utils::validateIsNumeric(meanValues)
  distributions <- distributions %||%
    rep(Distributions$Normal, length(parameterPaths))
  ospsuite.utils::validateIsSameLength(
    parameterPaths,
    meanValues,
    sdValues,
    distributions
  )

  # Iterate through all parameters and sample a parameter values vector
  for (i in seq_along(parameterPaths)) {
    path <- parameterPaths[[i]]
    mean <- meanValues[[i]]
    sd <- sdValues[[i]]

    # Sample values
    vals <- sampleRandomValue(
      distribution = distributions[[i]],
      mean = mean,
      sd = sd,
      n = population$count
    )

    population$setParameterValues(parameterOrPath = path, values = vals)
  }
}
