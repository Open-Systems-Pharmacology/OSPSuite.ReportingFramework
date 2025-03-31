test_that("initProject copies files from sourceFolder to destination", {
  # Set up a temporary directory for testing
  projectDir <- file.path(tempdir(),'testInit')

  configurationDirectory <- file.path(projectDir, "Scripts", "ReportingFramework")
  if (!dir.exists(configurationDirectory)) dir.create(configurationDirectory,recursive = TRUE)


  # Call the initProject function
  invisible(initProject(configurationDirectory = configurationDirectory, overwrite = FALSE))

  # Check if the files were copied to the destination folder
  fileList <- list.files(configurationDirectory)
  expect_gte(length(fileList ),expected = 9)
  expect_true("Plots.xlsx" %in% fileList )

  # Clean up: delete the temporary directories and files
  unlink(projectDir, recursive = TRUE)
})


test_that("TestProject has corect format", {
  l <- buildTestProjectory()

  expect_s3_class(projectConfiguration, "ProjectConfiguration")

  scenarioList <-
    suppressWarnings(createScenarios.wrapped(projectConfiguration = projectConfiguration))

  # Perform assertions
  expect_true(length(scenarioList) > 0)

  suppressMessages(runAndSaveScenarios(
    projectConfiguration = projectConfiguration,
    scenarioList = scenarioList,
    simulationRunOptions = SimulationRunOptions$new(
      numberOfCores = NULL,
      checkForNegativeValues = NULL,
      showProgress = TRUE
    )
  ))



  expect_true(length(list.files(file.path(projectConfiguration$outputFolder, EXPORTDIR$simulationResult))) > 0)
})

cleanupLogFileForTest(projectConfiguration)
