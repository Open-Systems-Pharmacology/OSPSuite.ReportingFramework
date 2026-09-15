# BASIC UNIT TESTS
# This test file contains only basic unit tests for the WorkflowScriptExporter class.
# Integration tests that require external setup (projectConfiguration, scenarioList, full PKML models)
# have been moved to a separate integration test package.

# Create a mock projectConfiguration for unit tests
projectConfiguration <- list(
  configurationsFolder = file.path(tempdir(), "configurations"),
  outputFolder = file.path(tempdir(), "output"),
  workspacePath = tempdir(),
  addOns = list(
    electronicPackageFolder = file.path(tempdir(), "ePackage")
  )
)

# Ensure directories exist
dir.create(
  projectConfiguration$configurationsFolder,
  showWarnings = FALSE,
  recursive = TRUE
)
dir.create(
  projectConfiguration$outputFolder,
  showWarnings = FALSE,
  recursive = TRUE
)

# Test WorkflowScriptExporter constructor and validation
test_that("WorkflowScriptExporter constructor validates wfIdentifier", {
  expect_error(
    WorkflowScriptExporter$new(
      projectConfiguration = projectConfiguration,
      wfIdentifier = NULL,
      scenarioNames = c("scenario1")
    )
  )
})

test_that("WorkflowScriptExporter constructor requires either scenarioNames or workflowRmd", {
  expect_error(
    WorkflowScriptExporter$new(
      projectConfiguration = projectConfiguration,
      wfIdentifier = 1,
      scenarioNames = NULL,
      workflowRmd = NULL
    )
  )
})

test_that("WorkflowScriptExporter constructor rejects both scenarioNames and workflowRmd", {
  expect_error(
    WorkflowScriptExporter$new(
      projectConfiguration = projectConfiguration,
      wfIdentifier = 1,
      scenarioNames = c("scenario1"),
      workflowRmd = "file.Rmd"
    )
  )
})

test_that("WorkflowScriptExporter constructor accepts valid scenarioNames", {
  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    scenarioNames = c("scenario1", "scenario2")
  )

  expect_equal(exporter$wfIdentifier, 1)
  expect_equal(exporter$scenarioNames, c("scenario1", "scenario2"))
  expect_null(exporter$workflowRmd)
})

test_that("WorkflowScriptExporter constructor accepts fileNameReplacements", {
  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 2,
    scenarioNames = c("scenario1"),
    fileNameReplacements = c("old.csv", "new.csv")
  )

  expect_equal(exporter$fileNameReplacements, c("old.csv", "new.csv"))
})

test_that("WorkflowScriptExporter initializes codeChunks", {
  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 3,
    scenarioNames = c("scenario1")
  )

  expect_type(exporter$codeChunks, "list")
})

test_that("WorkflowScriptExporter initializes inputFiles as data.table", {
  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 4,
    scenarioNames = c("scenario1")
  )

  expect_s3_class(exporter$inputFiles, "data.table")
})

test_that("WorkflowScriptExporter properties are accessible", {
  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 5,
    scenarioNames = c("test_scenario")
  )

  expect_equal(exporter$wfIdentifier, 5)
  expect_equal(exporter$scenarioNames, c("test_scenario"))
})
