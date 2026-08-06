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
    "DataImportConfiguration.xlsx",
    "Reports.xlsx",
    "PKParameter.xlsx",
    "SensitivityParameter.xlsx"
  )
  for (f in expectedFiles) {
    expect_true(file.exists(file.path(configFolder, f)))
  }
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

  newScenarios <- file.path(configurationDirectory, "Scenarios.xlsx")
  expect_true(file.exists(newScenarios))
  expect_true(file.exists(file.path(
    configurationDirectory,
    "PKParameter.xlsx"
  )))

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
  stampReportingFrameworkVersion(targetConfiguration)

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


test_that("loadScenarioResultsToFramework throws an error for missing scenarios", {
  projectConfiguration <- list(outputFolder = withr::local_tempdir())

  expect_error(
    loadScenarioResultsToFramework(
      projectConfiguration,
      "nonexistentScenario"
    ),
    "do not exist"
  )
})


test_that("runOrLoadScenarios loads existing results and runs missing ones", {
  outputFolder <- withr::local_tempdir()
  resultFolder <- file.path(outputFolder, EXPORTDIR$simulationResult)
  dir.create(resultFolder, recursive = TRUE)

  existingScenario <- "existingScenario"
  missingScenario <- "missingScenario"
  file.create(file.path(resultFolder, paste0(existingScenario, ".csv")))

  projectConfiguration <- list(outputFolder = outputFolder)
  scenarioList <- setNames(
    list("dummyA", "dummyB"),
    c(existingScenario, missingScenario)
  )

  loadedScenarios <- character()
  runScenarios <- character()

  local_mocked_bindings(
    loadScenarioResultsToFramework = function(
      projectConfiguration,
      scenarioNames
    ) {
      loadedScenarios <<- c(loadedScenarios, scenarioNames)
      list(list(scenario = scenarioNames, source = "loaded"))
    },
    runAndSaveScenarios = function(
      projectConfiguration,
      scenarioList,
      simulationRunOptions = NULL,
      ...
    ) {
      scenarioName <- names(scenarioList)
      runScenarios <<- c(runScenarios, scenarioName)
      list(list(scenario = scenarioName, source = "simulated"))
    },
    .package = "ospsuite.reportingframework"
  )

  result <- runOrLoadScenarios(projectConfiguration, scenarioList)

  expect_equal(loadedScenarios, existingScenario)
  expect_equal(runScenarios, missingScenario)
  expect_named(result, c(existingScenario, missingScenario))
})
