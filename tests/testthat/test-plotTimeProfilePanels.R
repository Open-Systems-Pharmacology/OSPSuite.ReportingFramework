library(testthat)

# Mock data for testing
mockConfigTable <- function(){
  data.table(
    PanelName = c("Panel1", "Panel1", "Panel2"),
    Scenario = c("Scenario1", "Scenario2", "Scenario1"),
    DataGroupIds = c("group1,group2", "group2,group3", "group1,group3"),
    OutputPathIds = c("(path1,path2)", "path2,path3", "path3"),
    ReferenceScenario = c("Scenario1", NA, "Scenario2"),
    TimeUnit = c('h','h','h'),
    TimeOffset_Reference = c(1, 2, 3),
    TimeShift = c(0.5, 1.5, 2.5),
    yscale = c("linear", "log", "linear"),
    ylimit_linear = c('c(0,10)', 'c(0,10)', 'c(0,NA)'),
    ylimit_log = c('c(1,NA)', 'c(1,NA)', NA),
    ScenarioCaptionName = c("Caption1", "Caption2", "Caption3"),
    PanelCaptionAddon = c("Add1", "Add1", NA),
    panel_facet_scale = c("fixed", "fixed", "free_x"),
    TimeRange_1 = c("total", "firstApplication", "lastApplication"),
    TimeRange_2 = c("c(2,3)", NA, "c(2,NA)")
  )
}
dtScenarios <- data.table(Scenario_name = c("Scenario1", "Scenario2"))
dtOutputPaths <- data.table(OutputPathId = c("path1", "path2", "path3"),
                            OutputPaths = c("path1", "path2", "path3"),
                            DisplayName = c("path1", "path2", "path3"),
                            DisplayUnit = c("mg/L","mg/L","mg/L"))
observedData <- data.table(group = c("group1", "group2", "group3"))


# Unit tests
test_that("validateConfigTableForTimeProfiles function", {
  configTable <- mockConfigTable()
  expect_no_error(validateConfigTableForTimeProfiles(configTable, dtScenarios, dtOutputPaths, observedData))

  # Test for invalid Scenario name
  configTable$Scenario <-
    c("Scenario1", "InvalidScenario", "Scenario2")
  expect_error(
    validateConfigTableForTimeProfiles(configTable, dtScenarios, dtOutputPaths, observedData)
  )

  # Test for invalid DataGroupIds
  configTable <- mockConfigTable()
  configTable$DataGroupIds <- c("group1,group2", "group4,group5", "group1,group3")
  expect_error(validateConfigTableForTimeProfiles(configTable, dtScenarios, dtOutputPaths, observedData))

  # Test for invalid OutputPathIds
  configTable <- mockConfigTable()
  configTable$OutputPathIds <- c("path1,path4","path2",'(path1,path2')
  expect_error(validateConfigTableForTimeProfiles(configTable, dtScenarios, dtOutputPaths, observedData))

  # Test for invalid ReferenceScenario
  configTable <- mockConfigTable()
  configTable$ReferenceScenario <- c(NA, "InvalidScenario", "Scenario2")
  expect_error(validateConfigTableForTimeProfiles(configTable, dtScenarios, dtOutputPaths, observedData))

  # Test for invalid yscale
  configTable <- mockConfigTable()
  configTable$yscale <- c("linear", "invalid", "log")
  expect_error(
    validateConfigTableForTimeProfiles(configTable, dtScenarios, dtOutputPaths, observedData))

# Test for invalid TimeRange columns
  configTable <- mockConfigTable()
  configTable$TimeRange_1 <- c("total", "firstApplication", "invalid")
  configTable$TimeRange_2 <- c("c(2,3)", "NA", "c(2,NA)")
  expect_error(validateConfigTableForTimeProfiles(configTable, dtScenarios, dtOutputPaths, observedData))

  # Test for invalid ylimit columns
  configTable <- mockConfigTable()
  configTable$ylimit_linear <- c("c(2,3)", "NA", "c(2,NA)")
  expect_error(validateConfigTableForTimeProfiles(configTable, dtScenarios, dtOutputPaths, observedData))


  # Test for panel consistency
  configTable <- mockConfigTable()
  configTable$PanelName <- c("Panel1", "Panel1", "Panel2")
  configTable$ylimit_linear <- c(10, 20, 20)  # Setting ylimit_linear values to be different within the same panel
  expect_error(validateConfigTableForTimeProfiles(configTable, dtScenarios, dtOutputPaths, observedData))

})
