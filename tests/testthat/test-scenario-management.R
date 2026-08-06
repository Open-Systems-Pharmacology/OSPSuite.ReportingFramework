# Helper: RF project with two minimal scenarios backed by real pkml files
.makeScenarioProject <- function(.env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = .env)
  initProject(
    projectDirectory = d,
    configurationDirectory = d,
    overwrite = TRUE
  )
  pc <- createProjectConfiguration(
    file.path(d, "ProjectConfiguration.xlsx"),
    ignoreVersionCheck = TRUE
  )
  modelSrcDir <- system.file(
    "extdata",
    "Models",
    package = "ospsuite.reportingframework"
  )
  modelDir <- pc$modelFolder
  if (!dir.exists(modelDir)) {
    dir.create(modelDir, recursive = TRUE)
  }
  file.copy(list.files(modelSrcDir, full.names = TRUE), modelDir)
  wb <- openxlsx::loadWorkbook(pc$scenariosFile)
  template <- xlsxReadData(
    wb,
    "Scenarios",
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )
  minimal <- template[0L, ]
  makeRow <- function(name, model) {
    list(
      Scenario_name = name,
      IndividualId = NA_character_,
      PopulationId = NA_character_,
      ReadPopulationFromCSV = NA_integer_,
      ModelParameterSheets = NA_character_,
      ApplicationProtocol = NA_character_,
      SimulationTime = "0, 60, 10",
      SimulationTimeUnit = "min",
      SteadyState = NA_integer_,
      SteadyStateTime = NA_integer_,
      SteadyStateTimeUnit = NA_character_,
      ModelFile = model,
      OutputPathsIds = NA_character_
    )
  }
  minimal <- rbind(minimal, makeRow("ScenarioA", "iv_1_mg_5_min.pkml"))
  minimal <- rbind(minimal, makeRow("ScenarioB", "po_3_mg_solution.pkml"))
  xlsxWriteData(wb, "Scenarios", minimal)
  openxlsx::saveWorkbook(wb, pc$scenariosFile, overwrite = TRUE)
  return(pc)
}

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

test_that("runAndSaveScenarios runs scenarios and writes result CSV files", {
  pc <- .makeScenarioProject(.env = parent.frame())

  scenarioList <- createScenariosWrapped(pc, scenarioNames = "ScenarioA")

  # writeToLog warns when no log is initialized; suppress in test context
  suppressWarnings(
    result <- runAndSaveScenarios(
      projectConfiguration = pc,
      scenarioList = scenarioList,
      simulationRunOptions = ospsuite::SimulationRunOptions$new(
        showProgress = FALSE
      )
    )
  )

  outputFolder <- file.path(pc$outputFolder, EXPORTDIR$simulationResult)
  expect_true(file.exists(file.path(outputFolder, "ScenarioA.csv")))
  expect_named(result, "ScenarioA")
})

test_that("createScenariosWrapped returns a named list of Scenario objects", {
  pc <- .makeScenarioProject(.env = parent.frame())

  result <- createScenariosWrapped(pc, scenarioNames = "ScenarioA")

  expect_type(result, "list")
  expect_named(result, "ScenarioA")
})

test_that("createScenariosWrapped filters by scenarioNames", {
  pc <- .makeScenarioProject(.env = parent.frame())

  result <- createScenariosWrapped(pc, scenarioNames = "ScenarioB")

  expect_length(result, 1L)
  expect_named(result, "ScenarioB")
})

test_that("createScenariosWrapped returns all scenarios when scenarioNames is NULL", {
  pc <- .makeScenarioProject(.env = parent.frame())

  result <- createScenariosWrapped(pc, scenarioNames = NULL)

  expect_length(result, 2L)
  expect_setequal(names(result), c("ScenarioA", "ScenarioB"))
})

test_that("createScenariosWrapped errors on non-existent scenario name", {
  pc <- .makeScenarioProject(.env = parent.frame())

  expect_error(createScenariosWrapped(
    pc,
    scenarioNames = "NonExistentScenario"
  ))
})

test_that("createScenariosWrapped syncs scenario names into Reports.xlsx", {
  pc <- .makeScenarioProject(.env = parent.frame())

  createScenariosWrapped(pc, scenarioNames = "ScenarioA")

  wb <- openxlsx::loadWorkbook(pc$addOns$reportsFile)
  repSc <- xlsxReadData(
    wb,
    "Scenarios",
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )
  expect_true("ScenarioA" %in% repSc$Scenario)
})

test_that("createScenariosWrapped syncs all scenario names when called with NULL", {
  pc <- .makeScenarioProject(.env = parent.frame())

  createScenariosWrapped(pc, scenarioNames = NULL)

  wb <- openxlsx::loadWorkbook(pc$addOns$reportsFile)
  repSc <- xlsxReadData(
    wb,
    "Scenarios",
    convertHeaders = FALSE,
    emptyAsNA = FALSE
  )
  expect_true(all(c("ScenarioA", "ScenarioB") %in% repSc$Scenario))
})
