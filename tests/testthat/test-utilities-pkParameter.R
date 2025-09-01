# Assuming projectConfiguration, scenarioList, and scenarioResults are already defined in your test setup

test_that("calculatePKParameterForScenarios works correctly", {
  expect_silent(calculatePKParameterForScenarios(projectConfiguration, scenarioResults))
  # Check if output files are created as expected (you may need to adjust this based on your implementation)
  outputFolder <- file.path(projectConfiguration$outputFolder, EXPORTDIR$pKAnalysisResults)
  expect_true(dir.exists(outputFolder))
})

test_that("initializeParametersOfSheets updates parameters", {
  pkParameterSheets <- c("PK_Plasma", "PK_Fraction") # Example sheet names
  expect_silent(initializeParametersOfSheets(projectConfiguration, pkParameterSheets))
})

test_that("readUserDefinedPKParameters reads data correctly", {
  file <- projectConfiguration$addOns$pKParameterFile
  userDefinedParams <- readUserDefinedPKParameters(file)
  expect_s3_class(userDefinedParams, "data.table")
  expect_true(nrow(userDefinedParams) > 0)
})

test_that("addUserDefinedParameters adds parameters correctly", {
  userdefinedParameters <- c("F_tEnd", "F_max") # Example parameters
  dtUserdefPKParameter <- readUserDefinedPKParameters(projectConfiguration$addOns$pKParameterFile)
  expect_silent(addUserDefinedParameters(userdefinedParameters, dtUserdefPKParameter))
  expect_contains(ospsuite::allPKParameterNames(), userdefinedParameters)

  userdefinedParameters <- c("DoesNotExist") # Example parameters
  dtUserdefPKParameter <- readUserDefinedPKParameters(projectConfiguration$addOns$pKParameterFile)
  expect_error(addUserDefinedParameters(userdefinedParameters, dtUserdefPKParameter))
})

test_that("loadPKParameter loads parameters correctly", {
  result <- loadPKParameter(projectConfiguration, scenarioList[c(1, 2)])
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) == 800)
})

test_that("loadPKAnalysisPerScenario processes scenario correctly", {
  scenarioName <- names(scenarioResults)[1]
  scenarioSimulation <- scenarioResults[[scenarioName]]$simulation
  pkParameterSheets <- c("PK_Plasma") # Example sheet name
  result <- loadPKAnalysisPerScenario(scenarioName, scenarioSimulation, pkParameterSheets, projectConfiguration)
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) == 200)
})

test_that("loadPkAnalysisRawData loads data correctly", {
  scenarioName <- names(scenarioResults)[1]
  scenarioSimulation <- scenarioResults[[scenarioName]]$simulation
  result <- loadPkAnalysisRawData(projectConfiguration, scenarioName, scenarioSimulation)
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})
