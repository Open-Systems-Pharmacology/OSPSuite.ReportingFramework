test_that("initProject copies files from sourceFolder to destination", {
  # Set up a temporary directory for testing
  tempDir <- tempdir()
  sourceFolder <- file.path(tempDir, "source")
  destinationFolder <- file.path(tempDir, "destination")

  # Create some files in the source folder
  invisible(dir.create(sourceFolder))
  invisible(file.create(file.path(sourceFolder, "file1.txt")))
  invisible(dir.create(file.path(sourceFolder, "folder")))
  invisible(file.create(file.path(sourceFolder, "folder", "file2.txt")))

  # Call the initProject function
  invisible(initProject(rootDirectory = destinationFolder, sourceFolder = sourceFolder, overwrite = FALSE))

  # Check if the files were copied to the destination folder
  expect_true(file.exists(file.path(destinationFolder, "file1.txt")))
  expect_true(file.exists(file.path(destinationFolder, "folder", "file2.txt")))

  # Clean up: delete the temporary directories and files
  unlink(tempDir, recursive = TRUE)
})

test_that("initProject does not overwrite existing files when overwrite = FALSE", {
  # Set up a temporary directory for testing
  tempDir <- tempdir()
  sourceFolder <- file.path(tempDir, "source")
  destinationFolder <- file.path(tempDir, "destination")

  # Create some files in the source folder
  invisible(dir.create(sourceFolder, recursive = TRUE))
  invisible(file.create(file.path(sourceFolder, "file1.txt")))
  writeLines("This is source file1 content", file.path(sourceFolder, "file1.txt"))

  # Create the same files in the destination folder
  invisible(dir.create(destinationFolder, recursive = TRUE))
  invisible(file.create(file.path(destinationFolder, "file1.txt")))
  writeLines("This is destination file1 content", file.path(destinationFolder, "file1.txt"))

  # Call the initProject function with overwrite = FALSE
  invisible(initProject(rootDirectory = destinationFolder, sourceFolder = sourceFolder, overwrite = FALSE))

  # Check if the existing files in the destination folder were not overwritten
  expect_equal(readLines(file.path(destinationFolder, "file1.txt")), "This is destination file1 content")


  invisible(initProject(rootDirectory = destinationFolder, sourceFolder = sourceFolder, overwrite = TRUE))

  # Check if the existing files in the destination folder were not overwritten
  expect_equal(readLines(file.path(destinationFolder, "file1.txt")), "This is source file1 content")

  # Clean up: delete the temporary directories and files
  unlink(tempDir, recursive = TRUE)
})

# initialize logging. Is always needed
projectConfiguration <- suppressMessages(setUpTestProject(withModel = TRUE))

test_that("initProject creates project folder structure", {
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
