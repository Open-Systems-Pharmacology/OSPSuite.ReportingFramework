# Assuming projectConfiguration, scenarioList, and scenarioResults are already defined in your test setup

test_that("calculatePKParameterForScenarios works correctly", {
  calculatePKParameterForScenarios(projectConfiguration, scenarioResults)
  # Check if output files are created as expected (you may need to adjust this based on your implementation)
  outputFolder <- file.path(projectConfiguration$outputFolder, EXPORTDIR$pKAnalysisResults)
  expect_true(dir.exists(outputFolder))
})

test_that("initializeParametersOfSheets updates parameters", {
  pkParameterSheets <- c("PK_Plasma", "PK_Fraction") # Example sheet names
  expect_silent(.initializeParametersOfSheets(projectConfiguration, pkParameterSheets))
})

test_that("readUserDefinedPKParameters reads data correctly", {
  file <- projectConfiguration$addOns$pKParameterFile
  userDefinedParams <- .readUserDefinedPKParameters(file)
  expect_s3_class(userDefinedParams, "data.table")
  expect_true(nrow(userDefinedParams) > 0)
})

test_that("readUserDefinedPKParameters strips unit suffixes from column names", {
  file <- projectConfiguration$addOns$pKParameterFile
  dtUserdefPKParameter <- .readUserDefinedPKParameters(file)

  # Unit suffixes like [min] and [µmol/l] must be stripped from column names
  expect_true("startTime" %in% names(dtUserdefPKParameter))
  expect_true("endTime" %in% names(dtUserdefPKParameter))
  expect_true("startTimeOffset" %in% names(dtUserdefPKParameter))
  expect_true("endTimeOffset" %in% names(dtUserdefPKParameter))
  expect_true("concentrationThreshold" %in% names(dtUserdefPKParameter))

  # Columns without unit suffixes must be preserved as-is
  expect_true("startApplicationIndex" %in% names(dtUserdefPKParameter))
  expect_true("endApplicationIndex" %in% names(dtUserdefPKParameter))
  expect_true("normalizationFactor" %in% names(dtUserdefPKParameter))
})

test_that("readUserDefinedPKParameters reads correct values for all optional columns", {
  file <- projectConfiguration$addOns$pKParameterFile
  dtUserdefPKParameter <- .readUserDefinedPKParameters(file)

  # StartTime and EndTime columns (C_max_t1t2 parameter)
  expect_equal(dtUserdefPKParameter[name == "C_max_t1t2", startTime], 60)
  expect_equal(dtUserdefPKParameter[name == "C_max_t1t2", endTime], 480)

  # StartApplicationIndex and EndApplicationIndex columns (C_max_appIdx parameter)
  expect_equal(dtUserdefPKParameter[name == "C_max_appIdx", startApplicationIndex], 1)
  expect_equal(dtUserdefPKParameter[name == "C_max_appIdx", endApplicationIndex], 2)

  # NormalizationFactor column (AUC_norm parameter)
  expect_equal(dtUserdefPKParameter[name == "AUC_norm", normalizationFactor], 0.5)

  # ConcentrationThreshold column (C_threshold parameter)
  expect_equal(dtUserdefPKParameter[name == "C_threshold", concentrationThreshold], 0.1)

  # StartTimeOffset and EndTimeOffset columns (AUC_offset parameter)
  expect_equal(dtUserdefPKParameter[name == "AUC_offset", startTimeOffset], 30)
  expect_equal(dtUserdefPKParameter[name == "AUC_offset", endTimeOffset], 60)
})

test_that("addUserDefinedParameters adds parameters correctly", {
  userdefinedParameters <- c("F_tEnd", "F_max") # Example parameters
  dtUserdefPKParameter <- .readUserDefinedPKParameters(projectConfiguration$addOns$pKParameterFile)
  expect_silent(.addUserDefinedParameters(userdefinedParameters, dtUserdefPKParameter))
  expect_contains(ospsuite::allPKParameterNames(), userdefinedParameters)

  userdefinedParameters <- c("DoesNotExist") # Example parameters
  dtUserdefPKParameter <- .readUserDefinedPKParameters(projectConfiguration$addOns$pKParameterFile)
  expect_error(.addUserDefinedParameters(userdefinedParameters, dtUserdefPKParameter), messages$errorPKParameterNotDefined("DoesNotExist"))
})

test_that("addUserDefinedParameters applies all optional columns to the PK parameter object", {
  ospsuite::removeAllUserDefinedPKParameters()
  dtUserdefPKParameter <- .readUserDefinedPKParameters(projectConfiguration$addOns$pKParameterFile)

  # Add all parameters that use optional columns
  allOptionalColumnParams <- c("C_max_t1t2", "C_max_appIdx", "AUC_norm", "C_threshold", "AUC_offset")
  expect_silent(.addUserDefinedParameters(allOptionalColumnParams, dtUserdefPKParameter))
  expect_contains(ospsuite::allPKParameterNames(), allOptionalColumnParams)

  # Verify the properties on the C_max_t1t2 parameter (uses startTime and endTime)
  pkParam <- ospsuite::addUserDefinedPKParameter(
    name = "C_max_t1t2_verify",
    standardPKParameter = ospsuite::StandardPKParameter$C_max,
    displayUnit = "µg/l"
  )
  pkParam$startTime <- dtUserdefPKParameter[name == "C_max_t1t2", startTime]
  pkParam$endTime <- dtUserdefPKParameter[name == "C_max_t1t2", endTime]
  expect_equal(pkParam$startTime, 60)
  expect_equal(pkParam$endTime, 480)

  # Verify the properties on the C_max_appIdx parameter (uses startApplicationIndex and endApplicationIndex)
  pkParam <- ospsuite::addUserDefinedPKParameter(
    name = "C_max_appIdx_verify",
    standardPKParameter = ospsuite::StandardPKParameter$C_max,
    displayUnit = "µg/l"
  )
  pkParam$startApplicationIndex <- dtUserdefPKParameter[name == "C_max_appIdx", startApplicationIndex]
  pkParam$endApplicationIndex <- dtUserdefPKParameter[name == "C_max_appIdx", endApplicationIndex]
  expect_equal(pkParam$startApplicationIndex, 1)
  expect_equal(pkParam$endApplicationIndex, 2)

  # Verify the properties on the AUC_norm parameter (uses normalizationFactor)
  pkParam <- ospsuite::addUserDefinedPKParameter(
    name = "AUC_norm_verify",
    standardPKParameter = ospsuite::StandardPKParameter$AUC_tEnd,
    displayUnit = "µg*h/l"
  )
  pkParam$normalizationFactor <- dtUserdefPKParameter[name == "AUC_norm", normalizationFactor]
  expect_equal(pkParam$normalizationFactor, 0.5)

  # Verify the properties on the C_threshold parameter (uses concentrationThreshold)
  pkParam <- ospsuite::addUserDefinedPKParameter(
    name = "C_threshold_verify",
    standardPKParameter = ospsuite::StandardPKParameter$Tthreshold,
    displayUnit = "h"
  )
  pkParam$concentrationThreshold <- dtUserdefPKParameter[name == "C_threshold", concentrationThreshold]
  expect_equal(pkParam$concentrationThreshold, 0.1)

  # Verify the properties on the AUC_offset parameter (uses startTimeOffset and endTimeOffset)
  pkParam <- ospsuite::addUserDefinedPKParameter(
    name = "AUC_offset_verify",
    standardPKParameter = ospsuite::StandardPKParameter$AUC_tEnd,
    displayUnit = "µg*h/l"
  )
  pkParam$startTimeOffset <- dtUserdefPKParameter[name == "AUC_offset", startTimeOffset]
  pkParam$endTimeOffset <- dtUserdefPKParameter[name == "AUC_offset", endTimeOffset]
  expect_equal(pkParam$startTimeOffset, 30)
  expect_equal(pkParam$endTimeOffset, 60)
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
  result <- .loadPKAnalysisPerScenario(scenarioName, scenarioSimulation, pkParameterSheets, projectConfiguration)
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) == 200)
})

test_that("loadPkAnalysisRawData loads data correctly", {
  scenarioName <- names(scenarioResults)[1]
  scenarioSimulation <- scenarioResults[[scenarioName]]$simulation
  result <- .loadPkAnalysisRawData(projectConfiguration, scenarioName, scenarioSimulation)
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})
