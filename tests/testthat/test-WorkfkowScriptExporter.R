# BASIC UNIT TESTS
# This test file contains only basic unit tests for the WorkflowScriptExporter class.
# Integration tests that require external setup (projectConfiguration, scenarioList, full PKML models)
# have been moved to a separate integration test package.

# Create a mock projectConfiguration for unit tests
projectConfiguration <- list(
  configurationsFolder = file.path(tempdir(), "configurations"),
  projectConfigurationFilePath = system.file(
    "templates",
    "ProjectConfiguration.xlsx",
    package = "ospsuite.reportingframework"
  ),
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

test_that("WorkflowScriptExporter renames packaged population and script files", {
  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 6,
    scenarioNames = c("scenario1")
  )

  exporter$inputFiles <- data.table(
    source = c("study_population.csv", "workflow_script.R"),
    fileType = c("population", "script")
  )

  packagedFiles <- exporter$inputFiles[, .(fileName, fileType)]
  expect_equal(
    packagedFiles[fileType == "population"]$fileName,
    "study_population_csv.txt"
  )
  expect_equal(
    packagedFiles[fileType == "script"]$fileName,
    "workflow_script_r.txt"
  )
  expect_equal(
    changedInputReferenceFileName(
      fileName = "study_population_csv.txt",
      fileType = "population",
      source = "study_population.csv"
    ),
    "study_population.csv"
  )
  expect_equal(
    changedInputReferenceFileName(
      fileName = "workflow_script_r.txt",
      fileType = "script",
      source = "workflow_script.R"
    ),
    "workflow_script.R"
  )
})

test_that("WorkflowScriptExporter writes manifest with _csv.txt suffix", {
  ePackageDir <- withr::local_tempdir()
  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = modifyList(
      projectConfiguration,
      list(addOns = list(electronicPackageFolder = ePackageDir))
    ),
    wfIdentifier = 7,
    scenarioNames = c("scenario1")
  )

  modelFile <- withr::local_tempfile(fileext = ".pkml")
  writeLines("model", modelFile)

  exporter$inputFiles <- data.table(
    source = modelFile,
    fileType = "model"
  )

  exporter$exportInputFiles()

  expect_true(file.exists(file.path(ePackageDir, "w7_input_files_csv.txt")))
})

test_that("WorkflowScriptExporter removes indented leftover chunk placeholders", {
  projectDir <- withr::local_tempdir(pattern = "workflow_chunk_cleanup_")

  initProject(
    projectDirectory = projectDir,
    configurationDirectory = projectDir,
    overwrite = TRUE
  )

  pc <- createProjectConfiguration(
    path = file.path(projectDir, "ProjectConfiguration.xlsx"),
    ignoreVersionCheck = TRUE
  )

  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = pc,
    wfIdentifier = 70,
    scenarioNames = c("scenario1")
  )

  workflowTextLines <- c(
    "before",
    "    XXCHUNKstart - dataObservedXXX",
    "    XXCHUNKend - dataObservedXXX   ",
    "after"
  )

  cleanedLines <- exporter$.__enclos_env__$private$replaceChunkPlaceholderChunks(
    workflowTextLines
  )

  expect_equal(cleanedLines, c("before", "after"))
})

test_that("importWorkflowFiles restores project-compatible filenames", {
  ePackageDir <- withr::local_tempdir()
  populationsDir <- withr::local_tempdir()
  modelDir <- withr::local_tempdir()
  dataDir <- withr::local_tempdir()

  writeLines("population", file.path(ePackageDir, "study_population_csv.txt"))
  writeLines("script", file.path(ePackageDir, "workflow_script_r.txt"))
  writeLines(
    "fileName,fileType\nstudy_population_csv.txt,population\nworkflow_script_r.txt,script",
    file.path(ePackageDir, "w8_input_files_csv.txt")
  )

  projectConfigurationNew <- list(
    populationsFolder = populationsDir,
    modelFolder = modelDir,
    dataFolder = dataDir,
    addOns = list(dataFileFolder = dataDir)
  )

  importWorkflowFiles(projectConfigurationNew, ePackageDir, 8)

  expect_true(file.exists(file.path(populationsDir, "study_population.csv")))
  expect_true(file.exists(file.path(dataDir, "workflow_script.R")))
})

test_that("importWorkflowFiles supports legacy manifest name", {
  ePackageDir <- withr::local_tempdir()
  populationsDir <- withr::local_tempdir()
  modelDir <- withr::local_tempdir()
  dataDir <- withr::local_tempdir()

  writeLines("population", file.path(ePackageDir, "legacy_population.csv"))
  writeLines(
    "fileName,fileType\nlegacy_population.csv,population",
    file.path(ePackageDir, "w9_input_files.csv")
  )

  projectConfigurationNew <- list(
    populationsFolder = populationsDir,
    modelFolder = modelDir,
    dataFolder = dataDir,
    addOns = list(dataFileFolder = dataDir)
  )

  importWorkflowFiles(projectConfigurationNew, ePackageDir, 9)

  expect_true(file.exists(file.path(populationsDir, "legacy_population.csv")))
})
