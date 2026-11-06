# Shared test fixtures used across multiple test files.

.aciclovirSimulation <- function() {
  ospsuite::loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite"),
    loadFromCache = FALSE
  )
}


.simpleSimulation <- function() {
  ospsuite::loadSimulation(
    system.file("extdata", "simple.pkml", package = "ospsuite"),
    loadFromCache = FALSE
  )
}

.populationPath <- function() {
  system.file("extdata", "pop.csv", package = "ospsuite")
}


.pkAnalysesCsvPath <- function() {
  system.file("extdata", "PKAnalyses.csv", package = "ospsuite")
}

.makeTutorialDataProjectConfiguration <- function(
  projectDirPrefix,
  includeIndividualsFile = FALSE,
  includeScenariosFile = FALSE
) {
  projectDir <- tempfile(pattern = projectDirPrefix)
  dir.create(projectDir, recursive = TRUE, showWarnings = FALSE)
  projectConfigurationDirPath <- projectDir
  tutorialDir <- system.file(
    "extdata",
    "Tutorial",
    package = "ospsuite.reportingframework"
  )

  filesToCopy <- c(
    "DataImportMap.xlsx",
    "Individuals.xlsx",
    "Reports.xlsx",
    "Scenarios.xlsx"
  )

  file.copy(
    from = file.path(tutorialDir, filesToCopy),
    to = file.path(projectDir, filesToCopy),
    overwrite = TRUE
  )

  dataImportMap <- openxlsx::read.xlsx(
    xlsxFile = file.path(projectDir, "DataImportMap.xlsx"),
    sheet = "DataFiles"
  )
  dataImportMap <- dataImportMap[-1, , drop = FALSE]
  dataFileColumn <- if ("dataFile" %in% names(dataImportMap)) {
    "dataFile"
  } else {
    "DataFile"
  }
  dataImportMap[[dataFileColumn]] <- file.path(
    "Data",
    basename(as.character(dataImportMap[[dataFileColumn]]))
  )

  wb <- openxlsx::loadWorkbook(file.path(projectDir, "DataImportMap.xlsx"))
  openxlsx::writeData(
    wb = wb,
    sheet = "DataFiles",
    x = dataImportMap,
    startRow = 3,
    colNames = FALSE
  )
  openxlsx::saveWorkbook(
    wb = wb,
    file = file.path(projectDir, "DataImportMap.xlsx"),
    overwrite = TRUE
  )

  dataDir <- file.path(projectDir, "Data")
  dir.create(dataDir, recursive = TRUE, showWarnings = FALSE)
  dataFiles <- c(
    "timeprofiles_study1234_iv.csv",
    "timeprofiles_study1234_po.csv"
  )
  file.copy(
    from = file.path(tutorialDir, dataFiles),
    to = file.path(dataDir, dataFiles),
    overwrite = TRUE
  )

  projectConfiguration <- list(
    projectConfigurationDirPath = projectConfigurationDirPath,
    addOns = list(
      dataImportMapFile = file.path(projectDir, "DataImportMap.xlsx"),
      reportsFile = file.path(projectDir, "Reports.xlsx")
    )
  )

  if (includeIndividualsFile) {
    projectConfiguration$individualsFile <- file.path(
      projectDir,
      "Individuals.xlsx"
    )
  }

  if (includeScenariosFile) {
    projectConfiguration$scenariosFile <- file.path(
      projectDir,
      "Scenarios.xlsx"
    )
  }

  projectConfiguration
}
