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

test_that(".fixFilePathsInScenarioConfigurations handles hyphen/dash variants", {
  # Skip if model files don't exist
  skip_if_not(dir.exists(projectConfiguration$modelFolder))

  # Get existing model files
  modelFiles <- list.files(projectConfiguration$modelFolder, pattern = "\\.pkml$")
  skip_if(length(modelFiles) == 0, "No model files found")

  # create a model file with a dash in the name
  modelFileWithDash <- 'file-with-dashes.pkml'
  invisible(file.copy(from = file.path(projectConfiguration$modelFolder,modelFiles[1]),
            to = file.path(projectConfiguration$modelFolder,modelFileWithDash),overwrite = TRUE))

  # Create a test scenario configuration with EN DASH in filename
  testModelFileWithEnDash <- gsub("-", "\u2013", modelFileWithDash) # Replace dash with EN DASH

  # Create mock scenario configuration
  mockScenarioConfig <- list(
    list(
      scenarioName = "test_scenario",
      modelFile = testModelFileWithEnDash
    )
  )

  # Test that the function corrects the file path
  correctedConfigs <- .fixFilePathsInScenarioConfigurations(
    scenarioConfigurations = mockScenarioConfig,
    projectConfiguration = projectConfiguration
  )

  # Check that the file path was corrected
  expect_equal(correctedConfigs[[1]]$modelFile, modelFileWithDash)
  expect_false(correctedConfigs[[1]]$modelFile == testModelFileWithEnDash)
})

test_that(".fixFilePathsInScenarioConfigurations throws error for nonexistent files", {
  # Create a mock scenario configuration with a nonexistent file
  mockScenarioConfig <- list(
    list(
      scenarioName = "test_scenario",
      modelFile = "nonexistent\u2013file.pkml"
    )
  )

  # Test that the function throws an error
  expect_error(
    .fixFilePathsInScenarioConfigurations(
      scenarioConfigurations = mockScenarioConfig,
      projectConfiguration = projectConfiguration
    ),
    regexp = "Model file not found"
  )
})

test_that(".fixFilePathsInScenarioConfigurations throws error for NULL modelFile", {
  # Create a mock scenario configuration with NULL modelFile
  mockScenarioConfig <- list(
    list(
      scenarioName = "test_scenario_null",
      modelFile = NULL
    )
  )

  # Test that the function throws an error
  expect_error(
    .fixFilePathsInScenarioConfigurations(
      scenarioConfigurations = mockScenarioConfig,
      projectConfiguration = projectConfiguration
    ),
    regexp = "Invalid scenario configuration.*modelFile is NULL"
  )
})

test_that(".fixFilePathsInScenarioConfigurations throws error for empty modelFile", {
  # Create a mock scenario configuration with empty modelFile
  mockScenarioConfig <- list(
    list(
      scenarioName = "test_scenario_empty",
      modelFile = ""
    )
  )

  # Test that the function throws an error
  expect_error(
    .fixFilePathsInScenarioConfigurations(
      scenarioConfigurations = mockScenarioConfig,
      projectConfiguration = projectConfiguration
    ),
    regexp = "Invalid scenario configuration.*modelFile is empty"
  )
})


# Tests for .extendPopulationFromXLS_RF
test_that(".extendPopulationFromXLS_RF validates inputs", {
  skip_if(length(scenarioList) == 0, "No scenarios available")
  
  # Get a population from scenario
  scenarioName <- names(scenarioList)[1]
  population <- scenarioList[[scenarioName]]$population
  
  # Test with invalid population
  expect_error(
    ospsuite.reportingframework:::.extendPopulationFromXLS_RF(
      population = "not a population",
      XLSpath = "dummy.xlsx"
    )
  )
  
  # Test with invalid XLSpath
  expect_error(
    ospsuite.reportingframework:::.extendPopulationFromXLS_RF(
      population = population,
      XLSpath = 123
    )
  )
})

test_that(".extendPopulationFromXLS_RF handles file with correct structure", {
  skip_if(length(scenarioList) == 0, "No scenarios available")
  
  # Create a test XLS file with the expected structure
  tmpdir <- tempdir()
  testFile <- file.path(tmpdir, "test_population_params.xlsx")
  
  # Create data with expected column names
  test_data <- data.table(
    "Container Path" = c("Organism", "Organism"),
    "Parameter Name" = c("Weight", "Height"),
    "Mean" = c(70, 175),
    "SD" = c(10, 15),
    "Distribution" = c("Normal", "Normal")
  )
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  openxlsx::writeData(wb, "Sheet1", test_data)
  openxlsx::saveWorkbook(wb, testFile, overwrite = TRUE)
  
  scenarioName <- names(scenarioList)[1]
  population <- scenarioList[[scenarioName]]$population
  
  # Should not error with correct structure
  result <- ospsuite.reportingframework:::.extendPopulationFromXLS_RF(
    population = population,
    XLSpath = testFile,
    sheet = 1
  )
  
  expect_s3_class(result, "Population")
  
  # Clean up
  unlink(testFile)
})

test_that(".extendPopulationFromXLS_RF errors with wrong structure", {
  skip_if(length(scenarioList) == 0, "No scenarios available")
  
  # Create a test XLS file with wrong column names
  tmpdir <- tempdir()
  testFile <- file.path(tmpdir, "test_wrong_structure.xlsx")
  
  test_data <- data.table(
    "WrongColumn1" = c(1, 2),
    "WrongColumn2" = c(3, 4)
  )
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  openxlsx::writeData(wb, "Sheet1", test_data)
  openxlsx::saveWorkbook(wb, testFile, overwrite = TRUE)
  
  scenarioName <- names(scenarioList)[1]
  population <- scenarioList[[scenarioName]]$population
  
  expect_error(
    ospsuite.reportingframework:::.extendPopulationFromXLS_RF(
      population = population,
      XLSpath = testFile,
      sheet = 1
    )
  )
  
  # Clean up
  unlink(testFile)
})
