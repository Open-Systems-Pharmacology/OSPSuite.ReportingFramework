# Tests for utilities-configurationEnvironment.R

test_that("getDataGroups returns a data.table with expected structure", {
  dtDataGroups <- getDataGroups(projectConfiguration$plotsFile)
  
  expect_s3_class(dtDataGroups, "data.table")
  expect_true("displayNameData" %in% names(dtDataGroups))
  expect_true("group" %in% names(dtDataGroups))
  expect_true(is.factor(dtDataGroups$group))
  expect_true(is.ordered(dtDataGroups$group))
})

test_that("getOutputPathIds returns a data.table with expected structure", {
  dtOutputPaths <- getOutputPathIds(projectConfiguration$plotsFile)
  
  expect_s3_class(dtOutputPaths, "data.table")
  expect_true("displayNameOutput" %in% names(dtOutputPaths))
  expect_true("outputPathId" %in% names(dtOutputPaths))
  expect_true("displayUnit" %in% names(dtOutputPaths))
  expect_true(is.factor(dtOutputPaths$outputPathId))
  expect_true(is.ordered(dtOutputPaths$outputPathId))
  expect_true(is.character(dtOutputPaths$displayUnit))
})

test_that("getTimeRangeTags returns a data.table with expected structure", {
  dtTimeRange <- getTimeRangeTags(projectConfiguration$plotsFile)
  
  expect_s3_class(dtTimeRange, "data.table")
  expect_true("tag" %in% names(dtTimeRange))
  expect_true(is.factor(dtTimeRange$tag))
  expect_true(is.ordered(dtTimeRange$tag))
})

test_that("getModelParameterDefinitions returns a data.table with expected structure", {
  dtParameter <- getModelParameterDefinitions(projectConfiguration$plotsFile)
  
  expect_s3_class(dtParameter, "data.table")
  expect_true("displayNameModelParameter" %in% names(dtParameter))
  expect_true("parameterId" %in% names(dtParameter))
  expect_true("displayUnit" %in% names(dtParameter))
  expect_true(is.factor(dtParameter$parameterId))
  expect_true(is.ordered(dtParameter$parameterId))
  expect_true(is.character(dtParameter$displayUnit))
})

test_that("getScenarioDefinitions returns a data.table with expected structure", {
  scenarios <- getScenarioDefinitions(
    wbScenarios = projectConfiguration$scenariosFile,
    wbPlots = projectConfiguration$plotsFile
  )
  
  expect_s3_class(scenarios, "data.table")
  expect_true("scenarioName" %in% names(scenarios))
})

test_that("getScenarioDefinitions works without wbPlots", {
  scenarios <- getScenarioDefinitions(wbScenarios = projectConfiguration$scenariosFile)
  
  expect_s3_class(scenarios, "data.table")
  expect_true("scenarioName" %in% names(scenarios))
})

test_that("loadConfigTableEnvironment creates configEnv", {
  # Clean up any existing configEnv
  if (exists("configEnv", envir = .GlobalEnv)) {
    rm(configEnv, envir = .GlobalEnv)
  }
  
  loadConfigTableEnvironment(projectConfiguration)
  
  expect_true(exists("configEnv", envir = .GlobalEnv))
  expect_true(is.environment(configEnv))
  expect_true(exists("outputPaths", envir = configEnv))
  expect_true(exists("dataGroupIds", envir = configEnv))
  expect_true(exists("timeTags", envir = configEnv))
  expect_true(exists("scenarios", envir = configEnv))
  expect_true(exists("modelParameter", envir = configEnv))
})

test_that("loadConfigTableEnvironment can be called multiple times", {
  loadConfigTableEnvironment(projectConfiguration)
  
  # Should not error when called again
  expect_silent(loadConfigTableEnvironment(projectConfiguration))
  
  expect_true(exists("configEnv", envir = .GlobalEnv))
})
