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
  projectConfigurationDirPath <- tempdir()
  projectDir <- tempfile(pattern = projectDirPrefix)
  dir.create(projectDir, recursive = TRUE, showWarnings = FALSE)
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

  # Stage tutorial CSVs where DataFiles paths resolve from projectConfigurationDirPath.
  configuredDataFiles <- trimws(as.character(stats::na.omit(dataImportMap$dataFile)))
  configuredDataFiles <- configuredDataFiles[nzchar(configuredDataFiles)]

  sourceByName <- stats::setNames(file.path(tutorialDir, dataFiles), dataFiles)
  configuredBasenames <- basename(configuredDataFiles)
  missingSources <- setdiff(unique(configuredBasenames), names(sourceByName))
  if (length(missingSources) > 0) {
    stop(paste("Missing tutorial source files:", paste(missingSources, collapse = ", ")))
  }

  targetDataFiles <- normalizePath(
    file.path(projectConfigurationDirPath, configuredDataFiles),
    winslash = "/",
    mustWork = FALSE
  )

  targetDataDirs <- unique(dirname(targetDataFiles))
  for (targetDataDir in targetDataDirs) {
    dir.create(targetDataDir, recursive = TRUE, showWarnings = FALSE)
  }

  file.copy(
    from = unname(sourceByName[configuredBasenames]),
    to = targetDataFiles,
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
