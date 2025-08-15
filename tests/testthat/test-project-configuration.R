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
  value <- fs::path_rel(
    file.path(projectConfiguration$outputFolder, "testfolder"),
    getwd()
  )
  description <- "test folder 2"

  # Call the function to add an add-on folder
  projectConfiguration$addAddOnFolderToConfiguration(property, value, description)

  # Check if the directory exists
  expect_true(dir.exists(value))

  # Check if the property was added to the private data
  expect_true(property %in% names(projectConfiguration$addOns))
})
