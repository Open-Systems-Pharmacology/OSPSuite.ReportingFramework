# Tests for utilities-sensitivity.R

test_that("addSensitivityTable with scenarioList validates scenario name", {
  expect_error(
    addSensitivityTable(
      projectConfiguration = projectConfiguration,
      scenarioList = scenarioList,
      scenarioName = "nonexistent_scenario"
    )
  )
})
