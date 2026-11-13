test_that("upgradeToReportingFramework keeps esqlabsR structure by default", {
  rootDirectory <- withr::local_tempdir()
  configurationDirectory <- file.path(rootDirectory, "Root")
  dir.create(configurationDirectory, recursive = TRUE)

  esqlabsR::initProject(
    destination = configurationDirectory,
    overwrite = TRUE
  )
  upgradeToReportingFramework(
    configurationDirectory = configurationDirectory,
    overwrite = TRUE
  )

  configPath <- file.path(configurationDirectory, "ProjectConfiguration.xlsx")
  expect_true(file.exists(configPath))

  sheetNames <- openxlsx::getSheetNames(configPath)
  expect_true("esqlabsR" %in% sheetNames)
  expect_true("addons" %in% sheetNames)

  dtAddons <- xlsxReadData(
    wb = configPath,
    sheetName = "addons",
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )
  expect_true(REPORTING_FRAMEWORK_VERSION_PROPERTY %in% dtAddons$Property)
  expect_true(any(
    dtAddons$Property == REPORTING_FRAMEWORK_VERSION_PROPERTY &
      nzchar(as.character(dtAddons$Value)),
    na.rm = TRUE
  ))

  dtConfig <- xlsxReadData(
    wb = configPath,
    sheetName = "esqlabsR",
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )
  expect_equal(
    trimws(dtConfig$Value[dtConfig$Property == "configurationsFolder"][[
      1
    ]]),
    "Configurations/"
  )

  configFolder <- dtConfig$Value[
    dtConfig$Property == "configurationsFolder"
  ][[1]]
  configFolder <- if (is.na(configFolder) || !nzchar(configFolder)) {
    configurationDirectory
  } else {
    fs::path_abs(configFolder, start = configurationDirectory)
  }

  expectedFiles <- c(
    "DataImportMap.xlsx",
    "Reports.xlsx",
    "PKParameter.xlsx",
    "SensitivityParameter.xlsx"
  )
  for (f in expectedFiles) {
    expect_true(file.exists(file.path(configFolder, f)))
  }

  scenariosData <- xlsxReadData(
    wb = file.path(configFolder, "Scenarios.xlsx"),
    sheetName = "Scenarios",
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )
  expect_gt(nrow(scenariosData), 0)

  rootGitignore <- file.path(configurationDirectory, ".gitignore")
  expect_true(file.exists(rootGitignore))
  expect_true(any(trimws(readLines(rootGitignore, warn = FALSE)) == "*.xlsx"))

  configGitignore <- file.path(configFolder, ".gitignore")
  expect_true(file.exists(configGitignore))
  expect_true(any(trimws(readLines(configGitignore, warn = FALSE)) == "*.xlsx"))
})


test_that("upgradeToReportingFramework applies RF defaults when requested", {
  rootDirectory <- withr::local_tempdir()
  configurationDirectory <- file.path(
    rootDirectory,
    "Root",
    "Scripts",
    "ReportingFramework"
  )
  dir.create(configurationDirectory, recursive = TRUE)

  esqlabsR::initProject(
    destination = configurationDirectory,
    overwrite = TRUE
  )

  oldScenarios <- file.path(
    configurationDirectory,
    "Configurations",
    "Scenarios.xlsx"
  )
  expect_true(file.exists(oldScenarios))

  customDataDir <- file.path(configurationDirectory, "MyCustomData")
  dir.create(customDataDir, recursive = TRUE)
  customDataFile <- file.path(customDataDir, "source.csv")
  writeLines("id,value\n1,42", customDataFile)

  upgradeToReportingFramework(
    configurationDirectory = configurationDirectory,
    overwrite = TRUE,
    keepEsqlabsRStructure = FALSE
  )

  expect_false(file.exists(oldScenarios))

  configPath <- file.path(configurationDirectory, "ProjectConfiguration.xlsx")
  dtConfig <- xlsxReadData(
    wb = configPath,
    sheetName = "esqlabsR",
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )

  expect_equal(
    trimws(dtConfig$Value[dtConfig$Property == "configurationsFolder"][[
      1
    ]]),
    "."
  )
  expect_equal(
    trimws(dtConfig$Value[
      dtConfig$Property == "dataImporterConfigurationFile"
    ][[1]]),
    "DataImportConfiguration.xlsx"
  )

  dtAddons <- xlsxReadData(
    wb = configPath,
    sheetName = "addons",
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )
  expect_equal(
    trimws(dtAddons$Value[dtAddons$Property == "dataImportMapFile"][[1]]),
    "DataImportMap.xlsx"
  )

  newScenarios <- file.path(configurationDirectory, "Scenarios.xlsx")
  expect_true(file.exists(newScenarios))
  expect_true("PKParameter" %in% openxlsx::getSheetNames(newScenarios))

  cleanedScenarios <- xlsxReadData(
    wb = newScenarios,
    sheetName = "Scenarios",
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )
  cleanedOutputPaths <- xlsxReadData(
    wb = newScenarios,
    sheetName = "OutputPaths",
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )
  expect_gt(nrow(cleanedScenarios), 0)
  expect_gt(nrow(cleanedOutputPaths), 0)

  expect_true(file.exists(file.path(
    configurationDirectory,
    "PKParameter.xlsx"
  )))
  expect_true(file.exists(file.path(configurationDirectory, ".gitignore")))
  expect_true(any(
    trimws(readLines(
      file.path(configurationDirectory, ".gitignore"),
      warn = FALSE
    )) ==
      "*.xlsx"
  ))

  expect_true(file.exists(customDataFile))
  expect_equal(
    readLines(customDataFile, warn = FALSE),
    c("id,value", "1,42")
  )
  expect_false(dir.exists(file.path(configurationDirectory, "Configurations")))

  remainingLegacyDirs <- list.dirs(
    file.path(configurationDirectory, "Configurations"),
    recursive = FALSE,
    full.names = TRUE
  )
  remainingLegacyDirs <- remainingLegacyDirs[
    remainingLegacyDirs != file.path(configurationDirectory, "Configurations")
  ]
  expect_length(remainingLegacyDirs, 0)
})

test_that("upgradeToReportingFramework keeps legacy folder when transfer is incomplete", {
  rootDirectory <- withr::local_tempdir()
  configurationDirectory <- file.path(
    rootDirectory,
    "Root",
    "Scripts",
    "ReportingFramework"
  )
  dir.create(configurationDirectory, recursive = TRUE)

  esqlabsR::initProject(
    destination = configurationDirectory,
    overwrite = TRUE
  )

  legacyFile <- file.path(
    configurationDirectory,
    "Configurations",
    "LegacyOnly.csv"
  )
  writeLines("legacy", legacyFile)
  destinationFile <- file.path(configurationDirectory, "LegacyOnly.csv")
  writeLines("different", destinationFile)

  upgradeToReportingFramework(
    configurationDirectory = configurationDirectory,
    overwrite = FALSE,
    keepEsqlabsRStructure = FALSE
  )

  expect_true(dir.exists(file.path(configurationDirectory, "Configurations")))
  expect_true(file.exists(legacyFile))
  expect_equal(readLines(destinationFile, warn = FALSE), "different")
})

test_that("upgradeToReportingFramework is idempotent", {
  rootDirectory <- withr::local_tempdir()
  configurationDirectory <- file.path(
    rootDirectory,
    "Root",
    "Scripts",
    "ReportingFramework"
  )
  dir.create(configurationDirectory, recursive = TRUE)

  esqlabsR::initProject(
    destination = configurationDirectory,
    overwrite = TRUE
  )
  upgradeToReportingFramework(
    configurationDirectory = configurationDirectory,
    overwrite = TRUE
  )

  expect_no_error(
    upgradeToReportingFramework(
      configurationDirectory = configurationDirectory
    )
  )
})


test_that("upgradeToReportingFramework updates existing gitignore", {
  rootDirectory <- withr::local_tempdir()
  configurationDirectory <- file.path(
    rootDirectory,
    "Root",
    "Scripts",
    "ReportingFramework"
  )
  dir.create(configurationDirectory, recursive = TRUE)

  writeLines("/Outputs", file.path(configurationDirectory, ".gitignore"))

  esqlabsR::initProject(
    destination = configurationDirectory,
    overwrite = TRUE
  )
  upgradeToReportingFramework(
    configurationDirectory = configurationDirectory,
    overwrite = TRUE,
    keepEsqlabsRStructure = FALSE
  )

  gitignoreLines <- readLines(
    file.path(configurationDirectory, ".gitignore"),
    warn = FALSE
  )
  expect_true("/Outputs" %in% gitignoreLines)
  expect_true(any(trimws(gitignoreLines) == "*.xlsx"))
})


test_that("initProject initializes and upgrades project in one call", {
  rootDirectory <- withr::local_tempdir()
  projectDirectory <- file.path(rootDirectory, "Root")

  initProject(
    projectDirectory = projectDirectory,
    overwrite = TRUE
  )

  configurationDirectory <- file.path(
    projectDirectory,
    "Scripts",
    "ReportingFramework"
  )
  configPath <- file.path(configurationDirectory, "ProjectConfiguration.xlsx")
  expect_true(file.exists(configPath))
  sheetNames <- openxlsx::getSheetNames(configPath)
  expect_true("esqlabsR" %in% sheetNames)
  expect_true("addons" %in% sheetNames)

  dtConfig <- xlsxReadData(
    wb = configPath,
    sheetName = "esqlabsR",
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )
  expect_equal(
    trimws(dtConfig$Value[dtConfig$Property == "configurationsFolder"][[
      1
    ]]),
    "."
  )

  expect_true(dir.exists(file.path(rootDirectory, "Root", "Models")))
  expect_true(dir.exists(file.path(
    rootDirectory,
    "Root",
    "Models",
    "Populations"
  )))
  expect_true(dir.exists(file.path(
    rootDirectory,
    "Root",
    "Outputs",
    "ReportingFramework"
  )))

  modelFiles <- list.files(
    file.path(rootDirectory, "Root", "Models"),
    recursive = TRUE,
    all.files = TRUE,
    no.. = TRUE,
    include.dirs = FALSE
  )
  populationFiles <- list.files(
    file.path(rootDirectory, "Root", "Models", "Populations"),
    recursive = TRUE,
    all.files = TRUE,
    no.. = TRUE,
    include.dirs = FALSE
  )
  outputFiles <- list.files(
    file.path(rootDirectory, "Root", "Outputs", "ReportingFramework"),
    recursive = TRUE,
    all.files = TRUE,
    no.. = TRUE,
    include.dirs = FALSE
  )
  dataFiles <- list.files(
    file.path(rootDirectory, "Root", "Data"),
    recursive = TRUE,
    all.files = TRUE,
    no.. = TRUE,
    include.dirs = FALSE
  )

  expect_length(modelFiles, 0)
  expect_length(populationFiles, 0)
  expect_length(outputFiles, 0)
  expect_length(dataFiles, 0)

  gitignoreFile <- file.path(configurationDirectory, ".gitignore")
  expect_true(file.exists(gitignoreFile))
  expect_true(any(trimws(readLines(gitignoreFile, warn = FALSE)) == "*.xlsx"))

  # initProject deletes all esqlabsR directories
  expect_false(dir.exists(file.path(
    rootDirectory,
    "Root",
    "Scripts",
    "ReportingFramework",
    "PopulationsCSV"
  )))
})


test_that("initProject supports a custom analysisSubfolderName", {
  rootDirectory <- withr::local_tempdir()
  projectDirectory <- file.path(rootDirectory, "Root")
  analysisSubfolderName <- "RF_Analysis"

  initProject(
    projectDirectory = projectDirectory,
    analysisSubfolderName = analysisSubfolderName,
    overwrite = TRUE
  )

  configurationDirectory <- file.path(
    projectDirectory,
    "Scripts",
    analysisSubfolderName
  )
  configPath <- file.path(configurationDirectory, "ProjectConfiguration.xlsx")
  expect_true(file.exists(configPath))

  dtConfig <- xlsxReadData(
    wb = configPath,
    sheetName = "esqlabsR",
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )
  outputFolderValue <- trimws(dtConfig$Value[dtConfig$Property == "outputFolder"][[1]])
  expect_true(grepl("RF_Analysis$", outputFolderValue))

  expect_true(dir.exists(file.path(
    rootDirectory,
    "Root",
    "Outputs",
    analysisSubfolderName
  )))
  expect_true(dir.exists(file.path(rootDirectory, "Root", "Models")))
})


test_that("initProject forwards keepEsqlabsRStructure to upgradeToReportingFramework", {
  rootDirectory <- withr::local_tempdir()
  projectDirectory <- file.path(rootDirectory, "Root")

  initProject(
    projectDirectory = projectDirectory,
    overwrite = TRUE,
    keepEsqlabsRStructure = TRUE
  )

  configurationDirectory <- file.path(
    projectDirectory,
    "Scripts",
    "ReportingFramework"
  )
  configPath <- file.path(configurationDirectory, "ProjectConfiguration.xlsx")
  dtConfig <- xlsxReadData(
    wb = configPath,
    sheetName = "esqlabsR",
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )

  expect_equal(
    trimws(dtConfig$Value[dtConfig$Property == "configurationsFolder"][[1]]),
    "Configurations/"
  )

  scenariosData <- xlsxReadData(
    wb = file.path(configurationDirectory, "Configurations", "Scenarios.xlsx"),
    sheetName = "Scenarios",
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )
  expect_gt(nrow(scenariosData), 0)
})


test_that("initProject does not reinitialize existing RF project when overwrite is TRUE", {
  rootDirectory <- withr::local_tempdir()
  projectDirectory <- file.path(rootDirectory, "Root")

  initProject(
    projectDirectory = projectDirectory,
    overwrite = TRUE
  )

  configurationDirectory <- file.path(
    projectDirectory,
    "Scripts",
    "ReportingFramework"
  )
  configPath <- file.path(configurationDirectory, "ProjectConfiguration.xlsx")

  configWb <- openxlsx::loadWorkbook(configPath)
  dtConfig <- xlsxReadData(
    wb = configWb,
    sheetName = "esqlabsR",
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )
  dtConfig[dtConfig$Property == "outputFolder", "Value"] <-
    "Outputs/DoNotOverwrite"
  xlsxWriteData(wb = configWb, sheetName = "esqlabsR", dt = dtConfig)
  openxlsx::saveWorkbook(configWb, configPath, overwrite = TRUE)

  initProject(
    projectDirectory = projectDirectory,
    overwrite = TRUE
  )

  dtAfter <- xlsxReadData(
    wb = configPath,
    sheetName = "esqlabsR",
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )
  expect_equal(
    trimws(dtAfter$Value[dtAfter$Property == "outputFolder"][[1]]),
    "Outputs/DoNotOverwrite"
  )
})


test_that("createProjectConfiguration returns the expected classes", {
  configurationDirectory <- withr::local_tempdir()
  sourceConfiguration <- system.file(
    "templates",
    "ProjectConfiguration.xlsx",
    package = "ospsuite.reportingframework"
  )
  targetConfiguration <- file.path(
    configurationDirectory,
    "ProjectConfiguration.xlsx"
  )

  file.copy(sourceConfiguration, targetConfiguration)
  .stampReportingFrameworkVersion(targetConfiguration)

  projectConfiguration <- createProjectConfiguration(
    path = targetConfiguration,
    ignoreVersionCheck = TRUE
  )

  expect_s3_class(projectConfiguration, "ProjectConfigurationRF")
  expect_s3_class(
    projectConfiguration$baseProjectconfiguration,
    "ProjectConfiguration"
  )
})
