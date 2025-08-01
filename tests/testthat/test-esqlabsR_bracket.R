# testProject was set up by setup.R  which involved most of the esqlabsR_bracket scripts,
# it also provides a projectConfiguration, scenrioList and scenarioListInd

test_that("initProject copies files from sourceFolder to destination", {
  configurationDirectory <- projectConfiguration$configurationsFolder

  # Check if the files were copied to the destination folder
  fileList <- list.files(configurationDirectory)
  expect_gte(length(fileList), expected = 9)
  expect_true("Plots.xlsx" %in% fileList)
})


test_that("TestProject has correct format", {
  expect_s3_class(projectConfiguration, "ProjectConfiguration")

  # Perform assertions
  expect_true(length(scenarioList) > 0)

  expect_true(length(list.files(file.path(projectConfiguration$outputFolder, EXPORTDIR$simulationResult))) > 0)
})


test_that("loadScenarioResults loads existing scenarios", {
  scenarioNames <- names(scenarioList)[c(1, 2)]

  result <- loadScenarioResultsToFramework(
    projectConfiguration = projectConfiguration,
    scenarioNames = scenarioNames
  )

  expect_type(result, "list")
  expect_equal(length(result), length(scenarioNames))
  expect_true(all(scenarioNames %in% names(result)))
})

test_that("loadScenarioResults throws an error for missing scenarios", {
  scenarioNames <- c("nonexistentScenario")

  expect_error(loadScenarioResults(projectConfiguration, scenarioNames))
})

test_that("runAndSaveScenarios runs and saves scenarios", {
  scenarioNames <- names(scenarioListInd)[c(2, 3)]

  outfolderOld <- projectConfiguration$outputFolder
  projectConfiguration$outputFolder <-
    file.path("..", "..", "outputTestSimulation")
  if (!dir.exists(projectConfiguration$outputFolder)) dir.create(projectConfiguration$outputFolder)

  result <- runAndSaveScenarios(projectConfiguration, scenarioListInd[scenarioNames])

  expect_type(result, "list")
  expect_true(all(scenarioNames %in% names(result)))

  # Verify saved results
  for (sc in scenarioNames) {
    resultFile <- file.path(projectConfiguration$outputFolder, EXPORTDIR$simulationResult, paste0(sc, ".csv"))
    expect_true(file.exists(resultFile))
  }

  # simulate one additional and load the ones before
  scenarioNames <- names(scenarioListInd)[c(1, 2, 3)]
  result <- runOrLoadScenarios(projectConfiguration, scenarioListInd[scenarioNames])

  expect_type(result, "list")
  expect_true(all(scenarioNames %in% names(result)))

  # Verify saved results
  for (sc in scenarioNames) {
    resultFile <- file.path(projectConfiguration$outputFolder, EXPORTDIR$simulationResult, paste0(sc, ".csv"))
    expect_true(file.exists(resultFile))
  }

  # Clean up
  projectConfiguration$outputFolder <-
    fs::path_rel(outfolderOld,
      start = projectConfiguration$configurationsFolder
    )
})
