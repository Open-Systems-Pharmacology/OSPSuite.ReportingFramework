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

  projectConfigurationTest <- projectConfiguration$clone()

  outfolderOld <- projectConfigurationTest$outputFolder
  projectConfigurationTest$outputFolder <-
    file.path("..", "..", "outputTestSimulation")
  if (!dir.exists(projectConfigurationTest$outputFolder)) dir.create(projectConfigurationTest$outputFolder)

  result <- runAndSaveScenarios(projectConfigurationTest, scenarioListInd[scenarioNames])

  expect_type(result, "list")
  expect_true(all(scenarioNames %in% names(result)))

  # Verify saved results
  for (sc in scenarioNames) {
    resultFile <- file.path(projectConfigurationTest$outputFolder, EXPORTDIR$simulationResult, paste0(sc, ".csv"))
    expect_true(file.exists(resultFile))
  }

  # simulate one additional and load the ones before
  scenarioNames <- names(scenarioListInd)[c(1, 2, 3)]
  result <- runOrLoadScenarios(projectConfigurationTest, scenarioListInd[scenarioNames])

  expect_type(result, "list")
  expect_true(all(scenarioNames %in% names(result)))

  # Verify saved results
  for (sc in scenarioNames) {
    resultFile <- file.path(projectConfigurationTest$outputFolder, EXPORTDIR$simulationResult, paste0(sc, ".csv"))
    expect_true(file.exists(resultFile))
  }

  # Clean up
  rm(projectConfigurationTest)
})

test_that("fixFilePathsInScenarioConfigurations handles hyphen/dash variants", {
  # Skip if model files don't exist
  skip_if_not(dir.exists(projectConfiguration$modelFolder))

  # Get existing model files
  modelFiles <- list.files(projectConfiguration$modelFolder, pattern = "\\.pkml$")
  skip_if(length(modelFiles) == 0, "No model files found")

  # Create a test scenario configuration with EN DASH in filename
  testModelFile <- modelFiles[1]
  testModelFileWithEnDash <- gsub("-", "\u2013", testModelFile) # Replace dash with EN DASH

  # Create mock scenario configuration
  mockScenarioConfig <- list(
    list(
      scenarioName = "test_scenario",
      modelFile = testModelFileWithEnDash
    )
  )

  # Test that the function corrects the file path
  correctedConfigs <- fixFilePathsInScenarioConfigurations(
    scenarioConfigurations = mockScenarioConfig,
    projectConfiguration = projectConfiguration
  )

  # Check that the file path was corrected
  expect_equal(correctedConfigs[[1]]$modelFile, testModelFile)
})

test_that("fixFilePathsInScenarioConfigurations throws error for nonexistent files", {
  # Create a mock scenario configuration with a nonexistent file
  mockScenarioConfig <- list(
    list(
      scenarioName = "test_scenario",
      modelFile = "nonexistent\u2013file.pkml"
    )
  )

  # Test that the function throws an error
  expect_error(
    fixFilePathsInScenarioConfigurations(
      scenarioConfigurations = mockScenarioConfig,
      projectConfiguration = projectConfiguration
    ),
    regexp = "Model file not found"
  )
})

test_that("fixFilePathsInScenarioConfigurations handles NULL or empty modelFile", {
  # Create mock scenario configurations with NULL and empty modelFile
  mockScenarioConfigs <- list(
    list(
      scenarioName = "test_scenario_null",
      modelFile = NULL
    ),
    list(
      scenarioName = "test_scenario_empty",
      modelFile = ""
    )
  )

  # Test that the function handles NULL and empty modelFile without error
  expect_silent(
    fixFilePathsInScenarioConfigurations(
      scenarioConfigurations = mockScenarioConfigs,
      projectConfiguration = projectConfiguration
    )
  )
})

