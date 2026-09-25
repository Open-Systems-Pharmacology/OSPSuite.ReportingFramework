#!/usr/bin/env Rscript
# Update Excel data validation in package templates from data-raw/template_validity.yml.
#
# This script reads configuration from a YAML file (template_validity.yml) that defines
# Excel data validation rules for all columns in each template workbook. It then applies
# or removes these validations across the inst/templates directory.
#
# Configuration is read from data-raw/template_validity.yml where each workbook specifies:
#   - validity: list   -> Creates dropdown list validation with specified values
#   - validity: none   -> Explicitly removes any existing validation from a column
#   - validity: mixed  -> Preserves existing validation (intentionally mixed columns)
#
# Usage (from the package root):
#   Rscript data-raw/update-template-validations.R
#
# Required packages: openxlsx, yaml, checkmate

library(openxlsx)
library(yaml)
source("data-raw/update-template-validations_helper.R")


yamlFile <- file.path("data-raw", "template_validity.yml")
yamlFile <- normalizePath(yamlFile, mustWork = TRUE)
checkmate::assertFileExists(yamlFile)
templatesDirectory <- file.path("inst", "templates")
checkmate::assertDirectoryExists(templatesDirectory)

rules <- readValidityConfig(yamlFile)
if (length(rules) == 0L) {
  message("No validations configured; no workbooks changed.")
  return(invisible(NULL))
}

workbookRules <- split(
  rules,
  vapply(rules, `[[`, character(1L), "workbookName")
)
for (workbookName in names(workbookRules)) {
  if (!grepl("\\.xlsx$", workbookName, ignore.case = TRUE)) {
    abortWithMessage("Workbook key must end in '.xlsx': ", workbookName)
  }
  if (basename(workbookName) != workbookName) {
    abortWithMessage(
      "Workbook key must be a file name, not a path: ",
      workbookName
    )
  }

  templateFile <- file.path(templatesDirectory, workbookName)
  if (!file.exists(templateFile)) {
    abortWithMessage(
      "Configured workbook is missing from inst/templates: ",
      workbookName
    )
  }
  refreshWorkbook(templateFile, workbookRules[[workbookName]])
}
