# initialize logging. Is always needed
projectPath <- iniLogFileForTest()

test_that("initProject creates project folder structure", {

  # Call the function being tested
  myProjectPath <- initProject(projectPath = file.path(projectPath,'TestProject'),
              sourceFolder = templateDirectory(),
              overwrite = FALSE)

  # Perform assertions
  expect_true(dir.exists(myProjectPath ))

  projectConfiguration <-
    createDefaultProjectConfiguration.wrapped(
      path = file.path(myProjectPath, 'ProjectConfiguration.xlsx'))

  expect_s3_class(projectConfiguration, "ProjectConfiguration")

  scenarioList <-
    createScenarios.wrapped(projectConfiguration = projectConfiguration)

  # Perform assertions
  expect_true(length(scenarioList) > 0)

  resultList <- runScenarios.wrapped(scenarioList = scenarioList)

  # Perform assertions
  expect_true(length(resultList) > 0)

  saveScenarioResults.wrapped(simulatedScenariosResults = resultList,
                              projectConfiguration = projectConfiguration)

  expect_true(length(list.files(file.path(projectConfiguration$outputFolder,'SimulationResults'))) > 0)
})

cleanupLogFileForTest(projectPath)

