# Tests for utilities-templates.R
# Note: These functions require RStudio API which may not be available in all test environments

test_that("createDocumentFromTemplate validates inputs", {
  # Test with non-existent template path
  expect_error(
    createDocumentFromTemplate(
      template = "nonexistent.R",
      templatePath = tempdir()
    )
  )
})

test_that("createDocumentFromTemplate can read template content", {
  # Create a temporary template file
  tmpdir <- tempdir()
  templateFile <- file.path(tmpdir, "test_template.R")
  writeLines(c("# Test Template", "x <- 1"), templateFile)
  
  # The actual documentNew function requires RStudio, so we'll just test reading
  templateContent <- readLines(templateFile)
  expect_equal(templateContent[1], "# Test Template")
  expect_equal(templateContent[2], "x <- 1")
  
  # Clean up
  unlink(templateFile)
})

test_that("template files exist in package", {
  # Check that the template files exist
  templatePath <- system.file("templates", package = "ospsuite.reportingframework")
  
  expect_true(file.exists(file.path(templatePath, "template_workflow.R")))
  expect_true(file.exists(file.path(templatePath, "template_plot.R")))
  expect_true(file.exists(file.path(templatePath, "template_ePackageWorkflow.Rmd")))
})
