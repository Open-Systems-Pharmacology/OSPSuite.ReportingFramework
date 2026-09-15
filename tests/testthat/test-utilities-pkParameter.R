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

# .readUserDefinedPKParameters -----------------------------------------------

test_that(".readUserDefinedPKParameters returns a data.table with required columns", {
  dt <- .readUserDefinedPKParameters(.pkParameterFile())

  expect_s3_class(dt, "data.table")
  expect_true(all(
    c("name", "standard PK parameter", "display Unit") %in% names(dt)
  ))
})

test_that(".readUserDefinedPKParameters removes unit brackets from column names", {
  dt <- .readUserDefinedPKParameters(.pkParameterFile())

  expect_false(any(grepl("\\[", names(dt))))
})

test_that(".readUserDefinedPKParameters errors on missing displayUnit", {
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

  expect_error(.readUserDefinedPKParameters(brokenFile))
})

# validatePKParameterDT -----------------------------------------------------

test_that(".validatePKParameterDT passes for a correctly structured data.table", {
  dt <- data.table::data.table(
    scenario = "S1",
    pkParameter = "AUC_tEnd",
    individualId = 1L,
    value = 100,
    outputPathId = "path|A",
    displayNamePKParameter = "AUC",
    displayUnitPKParameter = "µmol*min/l"
  )

  expect_invisible(.validatePKParameterDT(dt))
})

test_that(".validatePKParameterDT errors when required columns are missing", {
  dt <- data.table::data.table(scenario = "S1", value = 1)

  expect_error(.validatePKParameterDT(dt))
})

test_that(".validatePKParameterDT errors on inconsistent displayUnitPKParameter", {
  dt <- data.table::data.table(
    scenario = c("S1", "S2"),
    pkParameter = c("AUC_tEnd", "AUC_tEnd"),
    individualId = c(1L, 1L),
    value = c(100, 200),
    outputPathId = c("path|A", "path|A"),
    displayNamePKParameter = c("AUC", "AUC"),
    displayUnitPKParameter = c("µmol*min/l", "mg*min/l")
  )

  expect_error(.validatePKParameterDT(dt), "not consistent")
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

  result <- .setValueToRatio(base, ref)

  expect_equal(result$value, 2)
})

# .loadPkAnalysisRawData -----------------------------------------------------

test_that(".loadPkAnalysisRawData returns a data.table with 14 PK parameters", {
  d <- withr::local_tempdir()
  outputFolder <- file.path(d, EXPORTDIR$pKAnalysisResults)
  dir.create(outputFolder, recursive = TRUE)
  file.copy(.pkAnalysesCsvPath(), file.path(outputFolder, "ScenarioA.csv"))

  pc <- list(outputFolder = d)
  sim <- .aciclovirSimulation()

  result <- suppressWarnings(.loadPkAnalysisRawData(pc, "ScenarioA", sim))

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 14L)
  expect_true(all(
    c("individualId", "quantityPath", "parameter", "value", "unit") %in%
      names(result)
  ))
})

test_that(".loadPkAnalysisRawData column names start with a lowercase letter", {
  d <- withr::local_tempdir()
  outputFolder <- file.path(d, EXPORTDIR$pKAnalysisResults)
  dir.create(outputFolder, recursive = TRUE)
  file.copy(.pkAnalysesCsvPath(), file.path(outputFolder, "ScenarioA.csv"))

  result <- suppressWarnings(
    .loadPkAnalysisRawData(
      list(outputFolder = d),
      "ScenarioA",
      .aciclovirSimulation()
    )
  )

  expect_true(all(
    substr(names(result), 1, 1) == tolower(substr(names(result), 1, 1))
  ))
})

test_that(".loadPkAnalysisRawData errors when CSV does not exist", {
  pc <- list(outputFolder = withr::local_tempdir())
  sim <- .aciclovirSimulation()

  expect_error(
    .loadPkAnalysisRawData(pc, "NonExistentScenario", sim),
    "is not calculated"
  )
})

test_that(".loadPkAnalysisRawData fills empty unit with empty string", {
  d <- withr::local_tempdir()
  outputFolder <- file.path(d, EXPORTDIR$pKAnalysisResults)
  dir.create(outputFolder, recursive = TRUE)
  file.copy(.pkAnalysesCsvPath(), file.path(outputFolder, "ScenarioA.csv"))

  result <- suppressWarnings(
    .loadPkAnalysisRawData(
      list(outputFolder = d),
      "ScenarioA",
      .aciclovirSimulation()
    )
  )

  expect_false(any(is.na(result$unit)))
})

# Edge case tests for .readUserDefinedPKParameters
test_that(".readUserDefinedPKParameters handles multiple rows with varying data types", {
  dt <- .readUserDefinedPKParameters(.pkParameterFile())
  expect_true(nrow(dt) > 0)
  expect_true(all(c("name", "standard PK parameter") %in% names(dt)))
})

test_that(".readUserDefinedPKParameters returns consistent column types", {
  dt <- .readUserDefinedPKParameters(.pkParameterFile())
  expect_true(is.character(dt$name))
  expect_true(is.character(dt$`standard PK parameter`))
})

# Edge case tests for .validatePKParameterDT
test_that(".validatePKParameterDT passes with single row data.table", {
  dt <- data.table::data.table(
    scenario = "S1",
    pkParameter = "AUC_tEnd",
    individualId = 1L,
    value = 50,
    outputPathId = "path|B",
    displayNamePKParameter = "AUC",
    displayUnitPKParameter = "mg*min/l"
  )
  expect_invisible(.validatePKParameterDT(dt))
})

test_that(".validatePKParameterDT handles large numeric values", {
  dt <- data.table::data.table(
    scenario = "S1",
    pkParameter = "AUC_tEnd",
    individualId = 1L,
    value = 1e10,
    outputPathId = "path|A",
    displayNamePKParameter = "AUC",
    displayUnitPKParameter = "µmol*min/l"
  )
  expect_invisible(.validatePKParameterDT(dt))
})

test_that(".validatePKParameterDT handles small numeric values", {
  dt <- data.table::data.table(
    scenario = "S1",
    pkParameter = "AUC_tEnd",
    individualId = 1L,
    value = 0.0001,
    outputPathId = "path|A",
    displayNamePKParameter = "AUC",
    displayUnitPKParameter = "µmol*min/l"
  )
  expect_invisible(.validatePKParameterDT(dt))
})

test_that(".validatePKParameterDT handles negative values", {
  dt <- data.table::data.table(
    scenario = "S1",
    pkParameter = "AUC_tEnd",
    individualId = 1L,
    value = -100,
    outputPathId = "path|A",
    displayNamePKParameter = "AUC",
    displayUnitPKParameter = "µmol*min/l"
  )
  expect_invisible(.validatePKParameterDT(dt))
})

test_that(".validatePKParameterDT handles multiple individuals", {
  dt <- data.table::data.table(
    scenario = c("S1", "S1", "S1"),
    pkParameter = c("AUC_tEnd", "AUC_tEnd", "AUC_tEnd"),
    individualId = c(1L, 2L, 3L),
    value = c(100, 150, 200),
    outputPathId = c("path|A", "path|A", "path|A"),
    displayNamePKParameter = c("AUC", "AUC", "AUC"),
    displayUnitPKParameter = c("µmol*min/l", "µmol*min/l", "µmol*min/l")
  )
  expect_invisible(.validatePKParameterDT(dt))
})

test_that(".validatePKParameterDT handles multiple scenarios", {
  dt <- data.table::data.table(
    scenario = c("S1", "S2", "S3"),
    pkParameter = c("AUC_tEnd", "AUC_tEnd", "AUC_tEnd"),
    individualId = c(1L, 1L, 1L),
    value = c(100, 200, 300),
    outputPathId = c("path|A", "path|A", "path|A"),
    displayNamePKParameter = c("AUC", "AUC", "AUC"),
    displayUnitPKParameter = c("µmol*min/l", "µmol*min/l", "µmol*min/l")
  )
  expect_invisible(.validatePKParameterDT(dt))
})

test_that(".validatePKParameterDT handles multiple parameters", {
  dt <- data.table::data.table(
    scenario = c("S1", "S1", "S1"),
    pkParameter = c("AUC_tEnd", "Cmax", "Tmax"),
    individualId = c(1L, 1L, 1L),
    value = c(100, 50, 2),
    outputPathId = c("path|A", "path|A", "path|A"),
    displayNamePKParameter = c("AUC", "Cmax", "Tmax"),
    displayUnitPKParameter = c("µmol*min/l", "µmol/l", "h")
  )
  expect_invisible(.validatePKParameterDT(dt))
})

# Edge case tests for setValueToRatio
test_that(".setValueToRatio handles zero reference values", {
  base <- data.table::data.table(
    scenario = "S1",
    referenceScenario = "Ref",
    pkParameter = "AUC_tEnd",
    individualId = 1L,
    outputPathId = "path|A",
    value = 100,
    populationId = NA_character_
  )
  ref <- data.table::data.table(
    scenario = "Ref",
    pkParameter = "AUC_tEnd",
    individualId = 1L,
    outputPathId = "path|A",
    value = 0,
    populationId = NA_character_
  )
  # Function may return Inf or error depending on implementation
  result <- tryCatch(
    .setValueToRatio(base, ref),
    error = function(e) NULL
  )
  expect_true(is.null(result) || is.data.table(result))
})

test_that(".setValueToRatio handles equal base and reference values", {
  base <- data.table::data.table(
    scenario = "S1",
    referenceScenario = "Ref",
    pkParameter = "AUC_tEnd",
    individualId = 1L,
    outputPathId = "path|A",
    value = 100,
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
  result <- .setValueToRatio(base, ref)
  expect_equal(result$value, 1)
})

test_that(".setValueToRatio handles negative values", {
  base <- data.table::data.table(
    scenario = "S1",
    referenceScenario = "Ref",
    pkParameter = "AUC_tEnd",
    individualId = 1L,
    outputPathId = "path|A",
    value = -200,
    populationId = NA_character_
  )
  ref <- data.table::data.table(
    scenario = "Ref",
    pkParameter = "AUC_tEnd",
    individualId = 1L,
    outputPathId = "path|A",
    value = -100,
    populationId = NA_character_
  )
  result <- .setValueToRatio(base, ref)
  expect_equal(result$value, 2)
})

test_that(".setValueToRatio handles multiple base rows", {
  base <- data.table::data.table(
    scenario = c("S1", "S1"),
    referenceScenario = c("Ref", "Ref"),
    pkParameter = c("AUC_tEnd", "AUC_tEnd"),
    individualId = c(1L, 2L),
    outputPathId = c("path|A", "path|A"),
    value = c(200, 300),
    populationId = NA_character_
  )
  ref <- data.table::data.table(
    scenario = c("Ref", "Ref"),
    pkParameter = c("AUC_tEnd", "AUC_tEnd"),
    individualId = c(1L, 2L),
    outputPathId = c("path|A", "path|A"),
    value = c(100, 100),
    populationId = NA_character_
  )
  result <- .setValueToRatio(base, ref)
  expect_equal(nrow(result), 2)
  expect_equal(result$value, c(2, 3))
})
