# testProject including projectconfiguration was set up by setup.R

test_that("Initialization works correctly", {
  expect_s3_class(projectConfiguration, "ProjectConfigurationRF")
})

test_that("Add-on file is added correctly", {
  # Define parameters for the add-on file
  property <- "testAdd on file"
  value <- "testfile.txt"
  description <- "A new add-on file"
  templatePath <- file.path(projectConfiguration$configurationsFolder, "..", "..", "template.txt")

  # Create a template file for testing
  writeLines("This is a template", templatePath)

  # Call the function to add an add-on file
  projectConfiguration$addAddOnFileToConfiguration(property, value, description, templatePath)

  # Check if the file exists
  expect_true(file.exists(file.path(projectConfiguration$configurationsFolder, value)))

  # Check if the property was added to the private data
  expect_true(property %in% names(projectConfiguration$addOns))

  # Clean up the template file
  file.remove(templatePath)
})

test_that("Add-on folder is added correctly", {
  # Define parameters for the add-on folder with absolute path
  property <- "test folder"
  value <- file.path(projectConfiguration$outputFolder, "testfolder")
  description <- "test folder"

  # Call the function to add an add-on folder
  projectConfiguration$addAddOnFolderToConfiguration(property, value, description)

  # Check if the directory exists
  expect_true(dir.exists(value))

  # Check if the property was added to the private data
  expect_true(property %in% names(projectConfiguration$addOns))


  # Define parameters for the add-on folder with relative path
  property <- "test folder 2"
  pathTestFolder <- file.path(projectConfiguration$outputFolder, "testfolder")
  value <- fs::path_rel(pathTestFolder, start = projectConfiguration$configurationsFolder)
  description <- "test folder 2"

  # Call the function to add an add-on folder
  projectConfiguration$addAddOnFolderToConfiguration(property, value, description)

  # Check if the directory exists
  expect_true(dir.exists(pathTestFolder))

  # Check if the property was added to the private data
  expect_true(property %in% names(projectConfiguration$addOns))
})

test_that("addOns persists across a reload via createProjectConfiguration", {
  property    <- "reloadTestProp"
  value       <- "reloadTestFile.txt"
  description <- "persists across reload"
  templatePath <- file.path(projectConfiguration$configurationsFolder, "..", "..", "reload_template.txt")
  writeLines("reload template", templatePath)
  on.exit(file.remove(templatePath))

  projectConfiguration$addAddOnFileToConfiguration(property, value, description, templatePath)

  # Reload the configuration from disk
  reloaded <- createProjectConfiguration(
    path = projectConfiguration$projectConfigurationFilePath,
    ignoreVersionCheck = TRUE
  )

  expect_true(property %in% names(reloaded$addOns))
  expect_equal(
    as.character(reloaded$addOns[[property]]),
    as.character(file.path(projectConfiguration$configurationsFolder, value))
  )
})

test_that("ProjectConfigurationRF handles legacy main-sheet RF properties via .convertLegacyConfigSheet", {
  # Build a minimal ProjectConfiguration.xlsx that has an RF-specific property
  # sitting in the main sheet (legacy layout).
  tmpDir <- file.path(tempdir(), paste0("legacy_config_", Sys.getpid()))
  dir.create(tmpDir, recursive = TRUE)
  on.exit(unlink(tmpDir, recursive = TRUE))

  # Start from the RF template so all required properties are present
  rfTemplate <- system.file("templates", "ProjectConfiguration.xlsx",
                            package = "ospsuite.reportingframework")
  destXlsx <- file.path(tmpDir, "ProjectConfiguration.xlsx")
  file.copy(rfTemplate, destXlsx)

  # Inject an RF-specific legacy property directly into the main sheet
  wb <- openxlsx::loadWorkbook(destXlsx)
  mainSheet <- wb$sheet_names[1]
  dt <- ospsuite.reportingframework:::xlsxReadData(wb = wb, sheetName = mainSheet)
  dt <- rbind(dt, data.table::data.table(
    property    = "PKParameterFile",
    value       = "PKParameter.xlsx",
    description = "legacy RF prop"
  ))
  ospsuite.reportingframework:::xlsxWriteData(wb = wb, sheetName = mainSheet, dt = dt)
  # Stamp current esqlabsR version so the version check passes
  dt[property == "esqlabsRVersion",
     value := as.character(utils::packageVersion("esqlabsR"))]
  ospsuite.reportingframework:::xlsxWriteData(wb = wb, sheetName = mainSheet, dt = dt)
  openxlsx::saveWorkbook(wb, destXlsx, overwrite = TRUE)

  # createProjectConfiguration must convert the legacy layout without error
  cfg <- createProjectConfiguration(
    path = destXlsx,
    ignoreVersionCheck = TRUE
  )

  expect_s3_class(cfg, "ProjectConfigurationRF")
  # The RF-specific property must land in addOns, not cause an error
  expect_true("PKParameterFile" %in% names(cfg$addOns))
})

test_that("addAddOnFileToConfiguration rejects non-string inputs", {
  expect_error(projectConfiguration$addAddOnFileToConfiguration(123, "f.txt", "d", "t"))
  expect_error(projectConfiguration$addAddOnFileToConfiguration("p", 123, "d", "t"))
  expect_error(projectConfiguration$addAddOnFileToConfiguration("p", "f.txt", 123, "t"))
})

test_that("addAddOnFolderToConfiguration rejects non-string inputs", {
  expect_error(projectConfiguration$addAddOnFolderToConfiguration(123, "folder", "d"))
  expect_error(projectConfiguration$addAddOnFolderToConfiguration("p", 123, "d"))
  expect_error(projectConfiguration$addAddOnFolderToConfiguration("p", "folder", 123))
})

