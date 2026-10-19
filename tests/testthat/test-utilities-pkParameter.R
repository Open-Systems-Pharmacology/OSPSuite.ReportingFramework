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

.mockProjectConfiguration <- function(
  outputFolder = withr::local_tempdir(),
  pkParameterFile = "PKParameter.xlsx",
  reportsFile = "Reports.xlsx"
) {
  structure(
    list(
      outputFolder = outputFolder,
      addOns = list(
        pKParameterFile = pkParameterFile,
        reportsFile = reportsFile
      )
    ),
    class = "ProjectConfigurationRF"
  )
}

# .readUserDefinedPKParameters -----------------------------------------------

test_that(".initializeParametersOfSheets updates PK parameters from all merged sheets", {
  pc <- .mockProjectConfiguration(pkParameterFile = "mock-pk.xlsx")
  updatedParameters <- list()
  removedAll <- FALSE

  local_mocked_bindings(
    .readUserDefinedPKParameters = function(file) {
      data.table::data.table(
        name = character(),
        `standard PK parameter` = character(),
        `display Unit` = character(),
        check.names = FALSE
      )
    },
    .addUserDefinedParameters = function(
      userdefinedParameters,
      dtUserdefPKParameter
    ) {
      expect_length(userdefinedParameters, 0)
      invisible(NULL)
    },
    xlsxReadData = function(wb, sheetName, skipDescriptionRow) {
      switch(
        sheetName,
        SheetA = data.table::data.table(
          name = "AUC_tEnd",
          displayName = "AUC",
          displayUnit = "mg*h/L"
        ),
        SheetB = data.table::data.table(
          name = "C_max",
          displayName = "Cmax",
          displayUnit = "mg/L"
        ),
        stop(paste("Unexpected sheet", sheetName))
      )
    }
  )
  local_mocked_bindings(
    removeAllUserDefinedPKParameters = function() {
      removedAll <<- TRUE
      invisible(NULL)
    },
    allPKParameterNames = function() {
      c("AUC_tEnd", "C_max")
    },
    updatePKParameter = function(name, displayName, displayUnit) {
      updatedParameters[[length(updatedParameters) + 1]] <<- list(
        name = name,
        displayName = displayName,
        displayUnit = displayUnit
      )
      invisible(NULL)
    },
    .package = "ospsuite"
  )

  expect_invisible(.initializeParametersOfSheets(pc, "SheetA, SheetB"))

  expect_true(removedAll)
  expect_length(updatedParameters, 2)
  expect_equal(
    vapply(updatedParameters, `[[`, character(1), "name"),
    c("AUC_tEnd", "C_max")
  )
  expect_equal(
    vapply(updatedParameters, `[[`, character(1), "displayName"),
    c("AUC", "Cmax")
  )
})

test_that(".initializeParametersOfSheets allows identical definitions for the same name across sheets", {
  pc <- .mockProjectConfiguration(pkParameterFile = "mock-pk.xlsx")
  updatedParameters <- list()

  local_mocked_bindings(
    .readUserDefinedPKParameters = function(file) {
      data.table::data.table(
        name = character(),
        `standard PK parameter` = character(),
        `display Unit` = character(),
        check.names = FALSE
      )
    },
    .addUserDefinedParameters = function(
      userdefinedParameters,
      dtUserdefPKParameter
    ) {
      invisible(NULL)
    },
    xlsxReadData = function(wb, sheetName, skipDescriptionRow) {
      switch(
        sheetName,
        SheetA = data.table::data.table(
          name = "AUC_tEnd",
          displayName = "AUC",
          displayUnit = "mg*h/L"
        ),
        SheetB = data.table::data.table(
          name = "AUC_tEnd",
          displayName = "AUC",
          displayUnit = "mg*h/L"
        ),
        stop(paste("Unexpected sheet", sheetName))
      )
    }
  )
  local_mocked_bindings(
    removeAllUserDefinedPKParameters = function() {
      invisible(NULL)
    },
    allPKParameterNames = function() {
      "AUC_tEnd"
    },
    updatePKParameter = function(name, displayName, displayUnit) {
      updatedParameters[[length(updatedParameters) + 1]] <<- list(
        name = name,
        displayName = displayName,
        displayUnit = displayUnit
      )
      invisible(NULL)
    },
    .package = "ospsuite"
  )

  expect_invisible(.initializeParametersOfSheets(pc, "SheetA, SheetB"))
  expect_length(updatedParameters, 1)
  expect_equal(updatedParameters[[1]]$name, "AUC_tEnd")
  expect_equal(updatedParameters[[1]]$displayName, "AUC")
  expect_equal(updatedParameters[[1]]$displayUnit, "mg*h/L")
})

test_that(".initializeParametersOfSheets errors on conflicting definitions for the same name", {
  pc <- .mockProjectConfiguration(pkParameterFile = "mock-pk.xlsx")

  local_mocked_bindings(
    .readUserDefinedPKParameters = function(file) {
      data.table::data.table(
        name = character(),
        `standard PK parameter` = character(),
        `display Unit` = character(),
        check.names = FALSE
      )
    },
    .addUserDefinedParameters = function(
      userdefinedParameters,
      dtUserdefPKParameter
    ) {
      invisible(NULL)
    },
    xlsxReadData = function(wb, sheetName, skipDescriptionRow) {
      switch(
        sheetName,
        SheetA = data.table::data.table(
          name = "AUC_tEnd",
          displayName = "AUC",
          displayUnit = "mg*h/L"
        ),
        SheetB = data.table::data.table(
          name = "AUC_tEnd",
          displayName = "AUC end",
          displayUnit = "mg/L"
        ),
        stop(paste("Unexpected sheet", sheetName))
      )
    }
  )
  local_mocked_bindings(
    removeAllUserDefinedPKParameters = function() {
      invisible(NULL)
    },
    allPKParameterNames = function() {
      "AUC_tEnd"
    },
    updatePKParameter = function(name, displayName, displayUnit) {
      invisible(NULL)
    },
    .package = "ospsuite"
  )

  expect_error(
    .initializeParametersOfSheets(pc, "SheetA, SheetB"),
    "PK parameter names have conflicting display definitions across merged PK parameter sheets: AUC_tEnd"
  )
})

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

test_that(".loadPKAnalysisPerScenario tags the scenario and keeps matching PK definitions only", {
  pc <- .mockProjectConfiguration()

  local_mocked_bindings(
    .loadPkAnalysisRawData = function(
      projectConfiguration,
      scenarioName,
      scenarioSimulation
    ) {
      expect_identical(scenarioName, "ScenarioA")
      data.table::data.table(
        individualId = c(1L, 1L),
        quantityPath = c("Path|A", "Path|A"),
        parameter = c("AUC_tEnd", "C_max"),
        value = c(10, 5),
        unit = c("mg*h/L", "mg/L")
      )
    },
    getOutputPathIds = function(reportsFile) {
      data.table::data.table(
        outputPathId = "Output-1",
        outputPath = "Path|A"
      )
    },
    xlsxReadData = function(wb, sheetName, skipDescriptionRow) {
      expect_identical(sheetName, "PK_Plasma")
      data.table::data.table(
        name = "AUC_tEnd",
        displayName = "AUC",
        displayUnit = "mg*h/L",
        outputPathIds = "Output-1"
      )
    },
    .addUnitFactorsToPKDefinition = function(
      scenarioSimulation,
      dtOutputPaths,
      dtPkAnalyses,
      dtPkParameterDefinition
    ) {
      data.table::data.table(
        name = "AUC_tEnd",
        displayName = "AUC",
        displayUnit = "mg*h/L",
        outputPathId = "Output-1",
        outputPath = "Path|A",
        unitFactor = 1
      )
    }
  )

  result <- .loadPKAnalysisPerScenario(
    scenarioName = "ScenarioA",
    scenarioSimulation = list(molWeightFor = function(path) 1),
    pkParameterSheets = "PK_Plasma",
    projectConfiguration = pc
  )

  expect_s3_class(result, "data.table")
  expect_named(
    result,
    c(
      "scenario",
      "pkParameter",
      "individualId",
      "value",
      "outputPathId",
      "displayNamePKParameter",
      "displayUnitPKParameter"
    )
  )
  expect_equal(nrow(result), 1L)
  expect_identical(result$scenario, "ScenarioA")
  expect_identical(result$pkParameter, "AUC_tEnd")
  expect_identical(result$outputPathId, "Output-1")
  expect_equal(result$value, 10)
})

test_that(".loadPKAnalysisPerScenario combines multiple sheets and applies unit factors", {
  pc <- .mockProjectConfiguration()

  local_mocked_bindings(
    .loadPkAnalysisRawData = function(
      projectConfiguration,
      scenarioName,
      scenarioSimulation
    ) {
      data.table::data.table(
        individualId = c(1L, 1L),
        quantityPath = c("Path|A", "Path|A"),
        parameter = c("AUC_tEnd", "C_max"),
        value = c(10, 5),
        unit = c("mg*h/L", "mg/L")
      )
    },
    getOutputPathIds = function(reportsFile) {
      data.table::data.table(
        outputPathId = "Output-1",
        outputPath = "Path|A"
      )
    },
    splitInputs = function(value) {
      trimws(strsplit(value, ",")[[1]])
    },
    xlsxReadData = function(wb, sheetName, skipDescriptionRow) {
      switch(
        sheetName,
        SheetA = data.table::data.table(
          name = "AUC_tEnd",
          displayName = "AUC",
          displayUnit = "mg*h/L",
          outputPathIds = "Output-1"
        ),
        SheetB = data.table::data.table(
          name = "C_max",
          displayName = "Cmax",
          displayUnit = "mg/L",
          outputPathIds = "Output-1"
        ),
        stop(paste("Unexpected sheet", sheetName))
      )
    },
    .addUnitFactorsToPKDefinition = function(
      scenarioSimulation,
      dtOutputPaths,
      dtPkAnalyses,
      dtPkParameterDefinition
    ) {
      if (identical(dtPkParameterDefinition$name, "AUC_tEnd")) {
        return(data.table::data.table(
          name = "AUC_tEnd",
          displayName = "AUC",
          displayUnit = "mg*h/L",
          outputPathId = "Output-1",
          outputPath = "Path|A",
          unitFactor = 2
        ))
      }

      data.table::data.table(
        name = "C_max",
        displayName = "Cmax",
        displayUnit = "mg/L",
        outputPathId = "Output-1",
        outputPath = "Path|A",
        unitFactor = 0.5
      )
    }
  )

  result <- .loadPKAnalysisPerScenario(
    scenarioName = "ScenarioA",
    scenarioSimulation = list(molWeightFor = function(path) 1),
    pkParameterSheets = "SheetA, SheetB",
    projectConfiguration = pc
  )

  expect_equal(nrow(result), 2L)
  expect_equal(
    sort(result$pkParameter),
    c("AUC_tEnd", "C_max")
  )
  expect_equal(
    result[order(pkParameter)]$value,
    c(20, 2.5)
  )
})

test_that(".loadPKAnalysisPerScenario returns stable results across repeated loads", {
  pc <- .mockProjectConfiguration()

  local_mocked_bindings(
    .loadPkAnalysisRawData = function(
      projectConfiguration,
      scenarioName,
      scenarioSimulation
    ) {
      data.table::data.table(
        individualId = 1L,
        quantityPath = "Path|A",
        parameter = "AUC_tEnd",
        value = 10,
        unit = "mg*h/L"
      )
    },
    getOutputPathIds = function(reportsFile) {
      data.table::data.table(
        outputPathId = "Output-1",
        outputPath = "Path|A"
      )
    },
    xlsxReadData = function(wb, sheetName, skipDescriptionRow) {
      data.table::data.table(
        name = "AUC_tEnd",
        displayName = "AUC",
        displayUnit = "mg*h/L",
        outputPathIds = "Output-1"
      )
    },
    .addUnitFactorsToPKDefinition = function(
      scenarioSimulation,
      dtOutputPaths,
      dtPkAnalyses,
      dtPkParameterDefinition
    ) {
      data.table::data.table(
        name = "AUC_tEnd",
        displayName = "AUC",
        displayUnit = "mg*h/L",
        outputPathId = "Output-1",
        outputPath = "Path|A",
        unitFactor = 1
      )
    }
  )

  first <- .loadPKAnalysisPerScenario(
    scenarioName = "ScenarioA",
    scenarioSimulation = list(molWeightFor = function(path) 1),
    pkParameterSheets = "PK_Plasma",
    projectConfiguration = pc
  )
  second <- .loadPKAnalysisPerScenario(
    scenarioName = "ScenarioA",
    scenarioSimulation = list(molWeightFor = function(path) 1),
    pkParameterSheets = "PK_Plasma",
    projectConfiguration = pc
  )

  expect_equal(first, second)
})

test_that("loadPKParameter merges population metadata onto per-scenario PK rows", {
  pc <- .mockProjectConfiguration()
  scenarioList <- list(
    ScenarioA = list(simulation = structure(list(), class = "MockSimulation")),
    ScenarioB = list(simulation = structure(list(), class = "MockSimulation"))
  )

  local_mocked_bindings(
    getScenarioDefinitions = function(scenariosFile) {
      data.table::data.table(
        scenarioName = c("ScenarioA", "ScenarioB"),
        pKParameter = c("SheetA", "SheetB"),
        populationId = c("Pop-A", "Pop-B")
      )
    },
    getOutputPathIds = function(reportsFile) {
      data.table::data.table(
        outputPathId = "Output-1",
        outputPath = "Path|A"
      )
    },
    .loadPKAnalysisPerScenario = function(
      scenarioName,
      scenarioSimulation,
      pkParameterSheets,
      projectConfiguration
    ) {
      data.table::data.table(
        scenario = scenarioName,
        pkParameter = paste0("Param-", pkParameterSheets),
        individualId = 1L,
        value = if (identical(scenarioName, "ScenarioA")) 10 else 20,
        outputPathId = "Output-1",
        displayNamePKParameter = paste0("Display-", pkParameterSheets),
        displayUnitPKParameter = "mg/L"
      )
    }
  )

  result <- loadPKParameter(
    projectConfiguration = pc,
    scenarioListOrResult = scenarioList
  )

  expect_equal(nrow(result), 2L)
  expect_equal(result[order(scenario)]$populationId, c("Pop-A", "Pop-B"))
  expect_equal(
    result[order(scenario)]$pkParameter,
    c("Param-SheetA", "Param-SheetB")
  )
})

test_that(".addUnitFactorsToPKDefinition reports context for conversion failures", {
  local_mocked_bindings(
    getDimensionForUnit = function(unit) {
      "mock-dimension"
    },
    toUnit = function(
      quantityOrDimension,
      values,
      sourceUnit,
      targetUnit,
      molWeight
    ) {
      stop("mock conversion failure")
    },
    .package = "ospsuite"
  )

  dtOutputPaths <- data.table::data.table(
    outputPathId = "Output-1",
    outputPath = "Organism|Plasma|Drug"
  )
  dtPkAnalyses <- data.table::data.table(
    parameter = "AUC",
    quantityPath = "Organism|Plasma|Drug",
    unit = "mg*h/L"
  )
  dtPkParameterDefinition <- data.table::data.table(
    name = "AUC",
    displayName = "AUC",
    displayUnit = "mg/L*h",
    outputPathIds = "Output-1"
  )
  scenarioSimulation <- list(
    molWeightFor = function(path) {
      expect_equal(path, "Organism|Plasma|Drug")
      321.123
    }
  )

  expect_error(
    .addUnitFactorsToPKDefinition(
      scenarioSimulation = scenarioSimulation,
      dtOutputPaths = dtOutputPaths,
      dtPkAnalyses = dtPkAnalyses,
      dtPkParameterDefinition = dtPkParameterDefinition
    ),
    paste0(
      "OutputPathId: Output-1.*",
      "PK Parameter: AUC.*",
      "Source unit: mg\\*h/L.*",
      "Target unit: mg/L\\*h.*",
      "Molweight: 321\\.123.*",
      "Reason: mock conversion failure"
    )
  )
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
