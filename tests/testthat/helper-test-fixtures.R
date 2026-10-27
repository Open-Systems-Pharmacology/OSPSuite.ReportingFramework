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

  tutorialDir <- system.file(
    "extdata",
    "Tutorial",
    package = "ospsuite.reportingframework"
  )

  filesToCopy <- c(
    "DataImportConfiguration.xlsx",
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

  # Data import workbooks reference files via Data/...; stage files in common temp roots.
  for (rootDir in unique(c(
    tempdir(),
    dirname(tempdir()),
    dirname(dirname(tempdir())),
    projectDir
  ))) {
    tempDataDir <- file.path(rootDir, "Data")
    dir.create(tempDataDir, recursive = TRUE, showWarnings = FALSE)
    srcFiles <- file.path(dataDir, dataFiles)
    dstFiles <- file.path(tempDataDir, dataFiles)
    if (
      !all(
        normalizePath(srcFiles, winslash = "/") ==
          normalizePath(dstFiles, winslash = "/", mustWork = FALSE)
      )
    ) {
      file.copy(
        from = srcFiles,
        to = dstFiles,
        overwrite = TRUE
      )
    }
  }

  projectConfiguration <- list(
    dataImporterConfigurationFile = file.path(
      projectDir,
      "DataImportConfiguration.xlsx"
    ),
    projectConfigurationDirPath = tempdir(),
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
