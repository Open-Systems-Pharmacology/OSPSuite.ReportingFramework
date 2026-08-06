# helper: path to bundled template PKParameter.xlsx
.pkParameterFile <- function() {
  system.file(
    "templates",
    "PKParameter.xlsx",
    package = "ospsuite.reportingframework"
  )
}

# helper: Aciclovir simulation and PKAnalyses.csv co-shipped with ospsuite
.aciclovirSimulation <- function() {
  ospsuite::loadSimulation(
    system.file("extdata", "Aciclovir.pkml", package = "ospsuite"),
    loadFromCache = FALSE
  )
}
.pkAnalysesCsvPath <- function() {
  system.file("extdata", "PKAnalyses.csv", package = "ospsuite")
}

# readUserDefinedPKParameters -----------------------------------------------

test_that("readUserDefinedPKParameters returns a data.table with required columns", {
  dt <- readUserDefinedPKParameters(.pkParameterFile())

  expect_s3_class(dt, "data.table")
  expect_true(all(
    c("name", "standard PK parameter", "display Unit") %in% names(dt)
  ))
})

test_that("readUserDefinedPKParameters removes unit brackets from column names", {
  dt <- readUserDefinedPKParameters(.pkParameterFile())

  expect_false(any(grepl("\\[", names(dt))))
})

test_that("readUserDefinedPKParameters errors on missing displayUnit", {
  d <- withr::local_tempdir()
  brokenFile <- file.path(d, "broken.xlsx")

  # skipDescriptionRow=TRUE skips the first data row; include a description row
  # followed by the actual data row that has NA in display Unit
  df <- data.frame(
    name = c("(description)", "F_tEnd"),
    `standard PK parameter` = c("(description)", "C_trough"),
    `display Unit` = c("(description)", NA_character_),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Userdef PK Parameter")
  openxlsx::writeData(wb, "Userdef PK Parameter", df)
  openxlsx::saveWorkbook(wb, brokenFile, overwrite = TRUE)

  expect_error(readUserDefinedPKParameters(brokenFile))
})

# addUserDefinedParameters --------------------------------------------------

test_that("addUserDefinedParameters adds a user-defined PK parameter to ospsuite", {
  ospsuite::removeAllUserDefinedPKParameters()
  dt <- readUserDefinedPKParameters(.pkParameterFile())

  addUserDefinedParameters("F_tEnd", dt)

  expect_true("F_tEnd" %in% ospsuite::allPKParameterNames())
  ospsuite::removeAllUserDefinedPKParameters()
})

test_that("addUserDefinedParameters errors when name is not in definition table", {
  dt <- readUserDefinedPKParameters(.pkParameterFile())

  expect_error(
    addUserDefinedParameters("NonExistentParam", dt),
    "is not defined"
  )
})

test_that("addUserDefinedParameters errors when name appears more than once", {
  dt <- readUserDefinedPKParameters(.pkParameterFile())
  dtDup <- rbind(dt[1], dt[1])

  expect_error(
    addUserDefinedParameters(dt$name[1], dtDup),
    "not unique"
  )
})

# validatePKParameterDT -----------------------------------------------------

test_that("validatePKParameterDT passes for a correctly structured data.table", {
  dt <- data.table::data.table(
    scenario = "S1",
    pkParameter = "AUC_tEnd",
    individualId = 1L,
    value = 100,
    outputPathId = "path|A",
    displayNamePKParameter = "AUC",
    displayUnitPKParameter = "µmol*min/l"
  )

  expect_invisible(validatePKParameterDT(dt))
})

test_that("validatePKParameterDT errors when required columns are missing", {
  dt <- data.table::data.table(scenario = "S1", value = 1)

  expect_error(validatePKParameterDT(dt))
})

test_that("validatePKParameterDT errors on inconsistent displayUnitPKParameter", {
  dt <- data.table::data.table(
    scenario = c("S1", "S2"),
    pkParameter = c("AUC_tEnd", "AUC_tEnd"),
    individualId = c(1L, 1L),
    value = c(100, 200),
    outputPathId = c("path|A", "path|A"),
    displayNamePKParameter = c("AUC", "AUC"),
    displayUnitPKParameter = c("µmol*min/l", "mg*min/l")
  )

  expect_error(validatePKParameterDT(dt), "not consistent")
})

# setValueToRatio -----------------------------------------------------------

test_that("setValueToRatio divides base values by reference values", {
  base <- data.table::data.table(
    scenario = "S1",
    referenceScenario = "Ref",
    pkParameter = "AUC_tEnd",
    individualId = 1L,
    outputPathId = "path|A",
    value = 200,
    populationId = NA_character_
  )
  ref <- data.table::data.table(
    scenario = "Ref",
    pkParameter = "AUC_tEnd",
    individualId = 1L,
    outputPathId = "path|A",
    value = 100,
    populationId = NA_character_
  )

  result <- setValueToRatio(base, ref)

  expect_equal(result$value, 2)
})

# loadPkAnalysisRawData -----------------------------------------------------

test_that("loadPkAnalysisRawData returns a data.table with 14 PK parameters", {
  d <- withr::local_tempdir()
  outputFolder <- file.path(d, EXPORTDIR$pKAnalysisResults)
  dir.create(outputFolder, recursive = TRUE)
  file.copy(.pkAnalysesCsvPath(), file.path(outputFolder, "ScenarioA.csv"))

  pc <- list(outputFolder = d)
  sim <- .aciclovirSimulation()

  result <- suppressWarnings(loadPkAnalysisRawData(pc, "ScenarioA", sim))

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 14L)
  expect_true(all(
    c("individualId", "quantityPath", "parameter", "value", "unit") %in%
      names(result)
  ))
})

test_that("loadPkAnalysisRawData column names start with a lowercase letter", {
  d <- withr::local_tempdir()
  outputFolder <- file.path(d, EXPORTDIR$pKAnalysisResults)
  dir.create(outputFolder, recursive = TRUE)
  file.copy(.pkAnalysesCsvPath(), file.path(outputFolder, "ScenarioA.csv"))

  result <- suppressWarnings(
    loadPkAnalysisRawData(
      list(outputFolder = d),
      "ScenarioA",
      .aciclovirSimulation()
    )
  )

  expect_true(all(
    substr(names(result), 1, 1) == tolower(substr(names(result), 1, 1))
  ))
})

test_that("loadPkAnalysisRawData errors when CSV does not exist", {
  pc <- list(outputFolder = withr::local_tempdir())
  sim <- .aciclovirSimulation()

  expect_error(
    loadPkAnalysisRawData(pc, "NonExistentScenario", sim),
    "is not calculated"
  )
})

test_that("loadPkAnalysisRawData fills empty unit with empty string", {
  d <- withr::local_tempdir()
  outputFolder <- file.path(d, EXPORTDIR$pKAnalysisResults)
  dir.create(outputFolder, recursive = TRUE)
  file.copy(.pkAnalysesCsvPath(), file.path(outputFolder, "ScenarioA.csv"))

  result <- suppressWarnings(
    loadPkAnalysisRawData(
      list(outputFolder = d),
      "ScenarioA",
      .aciclovirSimulation()
    )
  )

  expect_false(any(is.na(result$unit)))
})
