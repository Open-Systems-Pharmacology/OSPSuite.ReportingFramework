# Tests for utilities-sensitivity.R

test_that("addSensitivityTable adds file to project configuration", {
  # Get a fresh project configuration for this test
  tempConfig <- projectConfiguration
  
  # Remove any existing sensitivity file
  if ("sensitivityFile" %in% names(tempConfig$addOns)) {
    tempConfig$addOns$sensitivityFile <- NULL
  }
  
  result <- addSensitivityTable(
    projectConfiguration = tempConfig,
    scenarioName = "test_scenario"
  )
  
  expect_true("sensitivityFile" %in% names(result$addOns))
  expect_true(file.exists(result$addOns$sensitivityFile))
})

test_that("addSensitivityTable with scenarioList validates scenario name", {
  expect_error(
    addSensitivityTable(
      projectConfiguration = projectConfiguration,
      scenarioList = scenarioList,
      scenarioName = "nonexistent_scenario"
    )
  )
})

test_that("addSensitivityTable with scenarioList creates sheet", {
  # Skip if scenarioList is empty
  skip_if(length(scenarioList) == 0, "No scenarios available for testing")
  
  tempConfig <- projectConfiguration
  scenarioName <- names(scenarioList)[1]
  
  result <- addSensitivityTable(
    projectConfiguration = tempConfig,
    scenarioList = scenarioList,
    scenarioName = scenarioName,
    sheetName = "test_sheet"
  )
  
  expect_true("sensitivityFile" %in% names(result$addOns))
  expect_true(file.exists(result$addOns$sensitivityFile))
  
  # Check that the sheet was created
  wb <- openxlsx::loadWorkbook(result$addOns$sensitivityFile)
  expect_true("test_sheet" %in% names(wb))
})

test_that("runSensitivityAnalysisForScenarios requires sensitivity file", {
  # Create a config without sensitivity file
  tempConfig <- projectConfiguration
  if ("sensitivityFile" %in% names(tempConfig$addOns)) {
    tempConfig$addOns$sensitivityFile <- NULL
  }
  
  expect_error(
    runSensitivityAnalysisForScenarios(
      projectConfiguration = tempConfig,
      scenarioList = scenarioList,
      scenarioNames = character(0),
      sensitivitysheet = "test"
    ),
    regexp = "sensitivity"
  )
})
