# Unit tests for pure helper functions in plotSensitivity.R.
# Integration tests (runPlot, vdiffr, real project setup) belong in the
# integration-test package.

# ---------------------------------------------------------------------------
# .sensitivityAnalysisName
# ---------------------------------------------------------------------------

test_that(".sensitivityAnalysisName builds correct filename", {
  result <- ospsuite.reportingframework:::.sensitivityAnalysisName(
    "myScenario",
    "mySheet"
  )
  expect_equal(result, "myScenario_mySheet.csv")
})

test_that(".sensitivityAnalysisName handles vectorised scenario input", {
  result <- ospsuite.reportingframework:::.sensitivityAnalysisName(
    c("sc1", "sc2"),
    "sheet1"
  )
  expect_equal(result, c("sc1_sheet1.csv", "sc2_sheet1.csv"))
})

# ---------------------------------------------------------------------------
# getCaptionForSensitivityPlot
# ---------------------------------------------------------------------------

makeSensitivityPlotData <- function(
  outputPathId = "Plasma",
  pKParameter = "AUC",
  scenarioLongName = "Adults",
  displayNameOutput = "Plasma concentration",
  plotTag = "A",
  threshold = 0
) {
  data.table::data.table(
    outputPathId = outputPathId,
    pKParameter = pKParameter,
    scenarioLongName = scenarioLongName,
    displayNameOutput = displayNameOutput,
    plotTag = plotTag,
    threshold = threshold
  )
}

test_that("getCaptionForSensitivityPlot builds caption with correct spacing", {
  plotData <- makeSensitivityPlotData()

  # mock configEnv$outputPaths for the duration of this test
  configEnv <- get0("configEnv", envir = .GlobalEnv, inherits = FALSE)
  mockEnv <- if (is.null(configEnv)) new.env(parent = emptyenv()) else configEnv
  oldOutputPaths <- mockEnv$outputPaths
  on.exit(mockEnv$outputPaths <- oldOutputPaths, add = TRUE)
  mockEnv$outputPaths <- data.table::data.table(
    outputPathId = "Plasma",
    displayNameOutput = "Plasma concentration"
  )
  assign("configEnv", mockEnv, envir = .GlobalEnv)

  result <- ospsuite.reportingframework:::getCaptionForSensitivityPlot(
    plotData = plotData,
    projectConfiguration = NULL,
    plotCaptionAddon = NA
  )

  expect_match(result, "Sensitivity of")
  expect_match(result, "AUC")
  expect_match(result, "Plasma concentration")
  expect_match(result, "Adults")
  expect_match(result, "sorted by absolute sensitivity")
  # verify spaces are present between tokens
  expect_false(grepl("ofAUC", result))
  expect_false(grepl("AUCfor", result))
})

test_that("getCaptionForSensitivityPlot appends plotCaptionAddon", {
  plotData <- makeSensitivityPlotData()

  configEnv <- get0("configEnv", envir = .GlobalEnv, inherits = FALSE)
  mockEnv <- if (is.null(configEnv)) new.env(parent = emptyenv()) else configEnv
  oldOutputPaths <- mockEnv$outputPaths
  on.exit(mockEnv$outputPaths <- oldOutputPaths, add = TRUE)
  mockEnv$outputPaths <- data.table::data.table(
    outputPathId = "Plasma",
    displayNameOutput = "Plasma concentration"
  )
  assign("configEnv", mockEnv, envir = .GlobalEnv)

  result <- ospsuite.reportingframework:::getCaptionForSensitivityPlot(
    plotData = plotData,
    projectConfiguration = NULL,
    plotCaptionAddon = "See appendix."
  )
  expect_match(result, "See appendix")
})

test_that("getCaptionForSensitivityPlot handles multiple PK parameters", {
  plotData <- rbind(
    makeSensitivityPlotData(pKParameter = "AUC", plotTag = "A"),
    makeSensitivityPlotData(pKParameter = "Cmax", plotTag = "B")
  )

  configEnv <- get0("configEnv", envir = .GlobalEnv, inherits = FALSE)
  mockEnv <- if (is.null(configEnv)) new.env(parent = emptyenv()) else configEnv
  oldOutputPaths <- mockEnv$outputPaths
  on.exit(mockEnv$outputPaths <- oldOutputPaths, add = TRUE)
  mockEnv$outputPaths <- data.table::data.table(
    outputPathId = "Plasma",
    displayNameOutput = "Plasma concentration"
  )
  assign("configEnv", mockEnv, envir = .GlobalEnv)

  result <- ospsuite.reportingframework:::getCaptionForSensitivityPlot(
    plotData = plotData,
    projectConfiguration = NULL,
    plotCaptionAddon = NA
  )
  expect_match(result, "AUC")
  expect_match(result, "Cmax")
})

test_that("getCaptionForSensitivityPlot includes threshold text when threshold > 0", {
  plotData <- makeSensitivityPlotData(threshold = 0.15)

  configEnv <- get0("configEnv", envir = .GlobalEnv, inherits = FALSE)
  mockEnv <- if (is.null(configEnv)) new.env(parent = emptyenv()) else configEnv
  oldOutputPaths <- mockEnv$outputPaths
  on.exit(mockEnv$outputPaths <- oldOutputPaths, add = TRUE)
  mockEnv$outputPaths <- data.table::data.table(
    outputPathId = "Plasma",
    displayNameOutput = "Plasma concentration"
  )
  assign("configEnv", mockEnv, envir = .GlobalEnv)

  result <- ospsuite.reportingframework:::getCaptionForSensitivityPlot(
    plotData = plotData,
    projectConfiguration = NULL,
    plotCaptionAddon = NA
  )

  expect_match(result, "Filtering threshold")
  expect_match(result, ">= 0.15")
  expect_match(result, "sorted by absolute sensitivity\\. Filtering threshold")
  expect_false(grepl(" \\\\.$", result))
})

test_that("getCaptionForSensitivityPlot omits threshold text when threshold is 0", {
  plotData <- makeSensitivityPlotData(threshold = 0)

  configEnv <- get0("configEnv", envir = .GlobalEnv, inherits = FALSE)
  mockEnv <- if (is.null(configEnv)) new.env(parent = emptyenv()) else configEnv
  oldOutputPaths <- mockEnv$outputPaths
  on.exit(mockEnv$outputPaths <- oldOutputPaths, add = TRUE)
  mockEnv$outputPaths <- data.table::data.table(
    outputPathId = "Plasma",
    displayNameOutput = "Plasma concentration"
  )
  assign("configEnv", mockEnv, envir = .GlobalEnv)

  result <- ospsuite.reportingframework:::getCaptionForSensitivityPlot(
    plotData = plotData,
    projectConfiguration = NULL,
    plotCaptionAddon = NA
  )

  expect_false(grepl("Filtering threshold", result))
})

# ---------------------------------------------------------------------------
# loadSensitivityPKValues — ratio computation with mock CSVs
# ---------------------------------------------------------------------------

writeMockPKCsv <- function(folder, name, dt) {
  data.table::fwrite(
    dt,
    file.path(folder, paste0(name, "-PKAnalysisResults.csv"))
  )
}

makePKCsv <- function(individualIds = 0:2, values = c(10, 20, 30)) {
  data.table::data.table(
    IndividualId = individualIds,
    QuantityPath = "Plasma",
    Parameter = "AUC",
    Value = values
  )
}

test_that("loadSensitivityPKValues returns single-scenario data unchanged", {
  tmp <- tempdir()
  writeMockPKCsv(tmp, "sc1", makePKCsv())
  result <- loadSensitivityPKValues(
    scenarioFiles = c(sc1 = "dummy"),
    outputPaths = "Plasma",
    pkParameter = "AUC",
    outFolder = tmp
  )
  expect_equal(result$Value, c(10, 20, 30))
  expect_true("PKParameter" %in% names(result))
  expect_false("Parameter" %in% names(result))
})

test_that("loadSensitivityPKValues computes ratio for two scenarios", {
  tmp <- tempdir()
  writeMockPKCsv(tmp, "ctrl", makePKCsv(values = c(10, 20, 30)))
  writeMockPKCsv(tmp, "trt", makePKCsv(values = c(20, 40, 60)))
  result <- loadSensitivityPKValues(
    scenarioFiles = c(ctrl = "dummy1", trt = "dummy2"),
    outputPaths = "Plasma",
    pkParameter = "AUC",
    outFolder = tmp
  )
  expect_equal(result$Value, c(2, 2, 2))
})

test_that("loadSensitivityPKValues filters by outputPaths", {
  tmp <- tempdir()
  dt <- data.table::rbindlist(list(
    makePKCsv(values = c(1, 2, 3)),
    data.table::data.table(
      IndividualId = 0:2,
      QuantityPath = "Liver",
      Parameter = "AUC",
      Value = c(5, 6, 7)
    )
  ))
  writeMockPKCsv(tmp, "sc_filt", dt)
  result <- loadSensitivityPKValues(
    scenarioFiles = c(sc_filt = "dummy"),
    outputPaths = "Plasma",
    pkParameter = "AUC",
    outFolder = tmp
  )
  expect_true(all(result$QuantityPath == "Plasma"))
})

test_that("loadSensitivityPKValues errors when reference file missing", {
  expect_error(
    loadSensitivityPKValues(
      scenarioFiles = c(missing = "dummy"),
      outputPaths = "Plasma",
      pkParameter = "AUC",
      outFolder = tempdir()
    )
  )
})

# ---------------------------------------------------------------------------
# prepareSensitivityPopulation — factors generation (pure logic)
# ---------------------------------------------------------------------------

test_that("factors vector contains baseline 1 and symmetric values", {
  # Exercise the factors-generation logic extracted from prepareSensitivityPopulation
  variationRange <- 0.1
  numberOfSteps <- 2L
  positive <- 1 + variationRange * (seq_len(numberOfSteps) / numberOfSteps)
  factors <- sort(unique(c(positive, 1 / positive, 1)))

  expect_true(1 %in% factors)
  expect_equal(length(factors[factors > 1]), length(factors[factors < 1]))
  expect_true(all(factors > 0))
  expect_equal(max(factors), 1 + variationRange, tolerance = 1e-10)
})

test_that("factors vector grows with numberOfSteps", {
  mkFactors <- function(steps) {
    r <- 0.1
    p <- 1 + r * (seq_len(steps) / steps)
    sort(unique(c(p, 1 / p, 1)))
  }
  expect_lt(length(mkFactors(2L)), length(mkFactors(4L)))
})

test_that("validateCommonInputs errors on non-numeric variationRange", {
  expect_error(
    ospsuite.reportingframework:::prepareSensitivityPopulation(
      scenarioFiles = c(s = "dummy.pkml"),
      sensitivityParameter = list(CL = "path"),
      variationRange = "bad",
      numberOfSteps = 2L
    ),
    'variationRange'
  )
})

# ---------------------------------------------------------------------------
# .sensitivityAnalysisName — edge cases
# ---------------------------------------------------------------------------

test_that(".sensitivityAnalysisName handles special characters in scenario names", {
  result <- ospsuite.reportingframework:::.sensitivityAnalysisName(
    "scenario-with-dashes",
    "sheet_with_underscores"
  )
  expect_equal(result, "scenario-with-dashes_sheet_with_underscores.csv")
})

test_that(".sensitivityAnalysisName handles numeric scenario names", {
  result <- ospsuite.reportingframework:::.sensitivityAnalysisName(
    "123",
    "456"
  )
  expect_equal(result, "123_456.csv")
})

test_that(".sensitivityAnalysisName handles spaces in input", {
  result <- ospsuite.reportingframework:::.sensitivityAnalysisName(
    "scenario name",
    "sheet name"
  )
  expect_equal(result, "scenario name_sheet name.csv")
})

test_that(".sensitivityAnalysisName handles empty string scenario", {
  result <- ospsuite.reportingframework:::.sensitivityAnalysisName(
    "",
    "sheet"
  )
  expect_equal(result, "_sheet.csv")
})

# ---------------------------------------------------------------------------
# loadSensitivityPKValues — edge cases
# ---------------------------------------------------------------------------

test_that("loadSensitivityPKValues handles multiple output paths", {
  tmp <- tempdir()
  dt <- data.table::rbindlist(list(
    data.table::data.table(
      IndividualId = 0:1,
      QuantityPath = "Plasma",
      Parameter = "AUC",
      Value = c(10, 20)
    ),
    data.table::data.table(
      IndividualId = 0:1,
      QuantityPath = "Liver",
      Parameter = "AUC",
      Value = c(5, 10)
    )
  ))
  data.table::fwrite(
    dt,
    file.path(tmp, "multi-PKAnalysisResults.csv")
  )
  result <- loadSensitivityPKValues(
    scenarioFiles = c(multi = "dummy"),
    outputPaths = c("Plasma", "Liver"),
    pkParameter = "AUC",
    outFolder = tmp
  )
  expect_true(all(c("Plasma", "Liver") %in% result$QuantityPath))
})

test_that("loadSensitivityPKValues handles different parameters", {
  tmp <- tempdir()
  dt <- data.table::rbindlist(list(
    data.table::data.table(
      IndividualId = 0:2,
      QuantityPath = "Plasma",
      Parameter = "AUC",
      Value = c(10, 20, 30)
    ),
    data.table::data.table(
      IndividualId = 0:2,
      QuantityPath = "Plasma",
      Parameter = "Cmax",
      Value = c(5, 10, 15)
    )
  ))
  data.table::fwrite(
    dt,
    file.path(tmp, "multi_param-PKAnalysisResults.csv")
  )
  result <- loadSensitivityPKValues(
    scenarioFiles = c(multi_param = "dummy"),
    outputPaths = "Plasma",
    pkParameter = "AUC",
    outFolder = tmp
  )
  expect_true(all(result$Parameter == "AUC"))
})

test_that("loadSensitivityPKValues computes correct ratio for multiple individuals", {
  tmp <- tempdir()
  writeMockPKCsv(tmp, "base", makePKCsv(values = c(100, 200, 300)))
  writeMockPKCsv(tmp, "alt", makePKCsv(values = c(150, 300, 450)))
  result <- loadSensitivityPKValues(
    scenarioFiles = c(base = "dummy1", alt = "dummy2"),
    outputPaths = "Plasma",
    pkParameter = "AUC",
    outFolder = tmp
  )
  expect_equal(result$Value, c(1.5, 1.5, 1.5))
})

# ---------------------------------------------------------------------------
# getCaptionForSensitivityPlot — edge cases
# ---------------------------------------------------------------------------

test_that("getCaptionForSensitivityPlot handles single plot tag", {
  plotData <- data.table::data.table(
    outputPathId = "Plasma",
    pKParameter = "AUC",
    scenarioLongName = "Adult",
    displayNameOutput = "Plasma",
    plotTag = "A"
  )

  configEnv <- get0("configEnv", envir = .GlobalEnv, inherits = FALSE)
  mockEnv <- if (is.null(configEnv)) new.env(parent = emptyenv()) else configEnv
  oldOutputPaths <- mockEnv$outputPaths
  on.exit(mockEnv$outputPaths <- oldOutputPaths, add = TRUE)
  mockEnv$outputPaths <- data.table::data.table(
    outputPathId = "Plasma",
    displayNameOutput = "Plasma"
  )
  assign("configEnv", mockEnv, envir = .GlobalEnv)

  result <- ospsuite.reportingframework:::getCaptionForSensitivityPlot(
    plotData = plotData,
    projectConfiguration = NULL,
    plotCaptionAddon = NA
  )

  expect_true(nchar(result) > 0)
  expect_match(result, "Sensitivity")
})

test_that("getCaptionForSensitivityPlot handles NA plotCaptionAddon", {
  plotData <- makeSensitivityPlotData()

  configEnv <- get0("configEnv", envir = .GlobalEnv, inherits = FALSE)
  mockEnv <- if (is.null(configEnv)) new.env(parent = emptyenv()) else configEnv
  oldOutputPaths <- mockEnv$outputPaths
  on.exit(mockEnv$outputPaths <- oldOutputPaths, add = TRUE)
  mockEnv$outputPaths <- data.table::data.table(
    outputPathId = "Plasma",
    displayNameOutput = "Plasma concentration"
  )
  assign("configEnv", mockEnv, envir = .GlobalEnv)

  result <- ospsuite.reportingframework:::getCaptionForSensitivityPlot(
    plotData = plotData,
    projectConfiguration = NULL,
    plotCaptionAddon = NA
  )

  expect_false(grepl("NA", result))
})

# ---------------------------------------------------------------------------
# validateSensitivityConfig
# ---------------------------------------------------------------------------

makeSensitivityConfigTable <- function(threshold = 0.5) {
  data.table::data.table(
    level = NA_integer_,
    header = NA_character_,
    plotName = "Sensitivity Plot",
    scenario = "Scenario1",
    scenarioLongName = "Scenario 1",
    outputPathIds = "Plasma",
    pKParameters = "AUC",
    sensitivityParameterSheet = "Sensitivity Parameters",
    threshold = threshold
  )
}

mockSensitivityConfigEnv <- function() {
  packageEnv <- asNamespace("ospsuite.reportingframework")
  mockEnv <- get("configEnv", envir = packageEnv)

  oldOutputPaths <- mockEnv$outputPaths
  oldDataGroupIds <- mockEnv$dataGroupIds
  oldScenarios <- mockEnv$scenarios

  mockEnv$outputPaths <- data.table::data.table(
    outputPathId = factor("Plasma"),
    outputPath = "Organism|PeripheralVenousBlood|Plasma",
    displayNameOutput = "Plasma concentration",
    displayUnit = "mg/l"
  )
  mockEnv$dataGroupIds <- data.table::data.table(
    group = factor("Group1"),
    displayNameData = "Group 1"
  )
  mockEnv$scenarios <- data.table::data.table(
    scenarioName = "Scenario1"
  )

  function() {
    mockEnv$outputPaths <- oldOutputPaths
    mockEnv$dataGroupIds <- oldDataGroupIds
    mockEnv$scenarios <- oldScenarios
  }
}

test_that("validateSensitivityConfig accepts threshold values >= 0", {
  restoreConfigEnv <- mockSensitivityConfigEnv()
  on.exit(restoreConfigEnv(), add = TRUE)

  expect_no_error(
    ospsuite.reportingframework:::validateSensitivityConfig(
      makeSensitivityConfigTable(threshold = 0)
    )
  )

  expect_no_error(
    ospsuite.reportingframework:::validateSensitivityConfig(
      makeSensitivityConfigTable(threshold = 1)
    )
  )

  expect_no_error(
    ospsuite.reportingframework:::validateSensitivityConfig(
      makeSensitivityConfigTable(threshold = 1.1)
    )
  )
})

test_that("validateSensitivityConfig rejects negative threshold values", {
  restoreConfigEnv <- mockSensitivityConfigEnv()
  on.exit(restoreConfigEnv(), add = TRUE)

  expect_error(
    ospsuite.reportingframework:::validateSensitivityConfig(
      makeSensitivityConfigTable(threshold = -0.1)
    ),
    "threshold"
  )
})

# ---------------------------------------------------------------------------
# Factors generation — additional edge cases
# ---------------------------------------------------------------------------

test_that("factors vector with numberOfSteps = 1", {
  r <- 0.1
  p <- 1 + r * (seq_len(1) / 1)
  factors <- sort(unique(c(p, 1 / p, 1)))

  expect_true(1 %in% factors)
  expect_true(length(factors) > 1)
  expect_true(all(factors > 0))
})

test_that("factors vector is symmetric around 1", {
  r <- 0.2
  steps <- 3L
  p <- 1 + r * (seq_len(steps) / steps)
  factors <- sort(unique(c(p, 1 / p, 1)))

  # For each factor > 1, there should be a corresponding factor < 1
  factors_gt_1 <- factors[factors > 1]
  factors_lt_1 <- factors[factors < 1]

  for (f in factors_gt_1) {
    expect_true(any(abs(factors_lt_1 - (1 / f)) < 1e-10))
  }
})

# =====================================================================
# PHASE 1 & 2: EMPTY DATA & NUMERIC BOUNDS VALIDATION
# =====================================================================

test_that("plotSensitivity rejects empty sensitivity data", {
  emptySensData <- data.table::data.table(
    scenario = character(0),
    parameter = character(0),
    value = numeric(0)
  )

  expect_error(
    checkmate::assertDataTable(emptySensData, min.rows = 1),
    "at least 1 rows"
  )
})

test_that("plotSensitivity rejects invalid threshold values (Inf)", {
  expect_error(
    checkmate::assertNumeric(Inf, finite = TRUE),
    "finite"
  )
})

test_that("plotSensitivity rejects negative threshold when inappropriate", {
  # Most sensitivity thresholds should be non-negative
  expect_error(
    checkmate::assertNumeric(-0.1, lower = 0),
    ">= 0"
  )
})


test_that("prepareSensitivityPlotData reads enhanced sensitivity CSV format", {
  tmp <- withr::local_tempdir()
  sensitivityDir <- file.path(tmp, EXPORTDIR$sensitivityResults)
  dir.create(sensitivityDir, recursive = TRUE)

  data.table::fwrite(
    data.table::data.table(
      SensitivityParameter = c("P1|CL", "P2|V"),
      QuantityPath = c("Organism|Plasma|Drug", "Organism|Plasma|Drug"),
      PKParameter = c("AUC", "AUC"),
      sens = c(0.3, 0.1)
    ),
    file.path(sensitivityDir, "ScenarioA_default.csv")
  )

  configEnv <- get0("configEnv", envir = .GlobalEnv, inherits = FALSE)
  mockEnv <- if (is.null(configEnv)) {
    new.env(parent = emptyenv())
  } else {
    configEnv
  }
  oldOutputPaths <- mockEnv$outputPaths
  on.exit(mockEnv$outputPaths <- oldOutputPaths, add = TRUE)
  mockEnv$outputPaths <- data.table::data.table(
    outputPathId = factor("plasma", levels = "plasma", ordered = TRUE),
    outputPath = "Organism|Plasma|Drug",
    displayNameOutput = "Plasma"
  )
  assign("configEnv", mockEnv, envir = .GlobalEnv)

  local_mocked_bindings(
    getPKParameterOverview = function(...) {
      data.table::data.table(
        pKParameter = "AUC",
        displayNamePKParameter = "AUC",
        displayUnitPKParameter = "mg*h/L",
        outputPathId = "plasma"
      )
    },
    xlsxReadData = function(...) {
      data.table::data.table(
        parameter = c("Clearance", "Volume"),
        parameterPath = c("P1|CL", "P2|V")
      )
    },
    .package = "ospsuite.reportingframework"
  )

  plotData <- ospsuite.reportingframework:::prepareSensitivityPlotData(
    onePlotConfig = data.table::data.table(
      scenario = "ScenarioA",
      scenarioLongName = "Adults",
      outputPathIds = "plasma",
      pKParameters = "AUC",
      sensitivityParameterSheet = "default",
      threshold = 0.15
    ),
    projectConfiguration = list(
      outputFolder = tmp,
      addOns = list(
        sensitivityFile = file.path(tmp, "SensitivityParameter.xlsx")
      )
    ),
    scenarioList = list(ScenarioA = list(simulation = "unused"))
  )

  expect_equal(nrow(plotData), 1)
  expect_equal(as.character(plotData$parameterName), "Clearance")
  expect_equal(plotData$sens, 0.3)
  expect_equal(as.character(plotData$outputPathId), "plasma")
  expect_equal(as.character(plotData$pKParameter), "AUC")
})

test_that("prepareSensitivityPlotData reads grouped enhanced sensitivity labels", {
  tmp <- withr::local_tempdir()
  sensitivityDir <- file.path(tmp, EXPORTDIR$sensitivityResults)
  dir.create(sensitivityDir, recursive = TRUE)

  data.table::fwrite(
    data.table::data.table(
      SensitivityParameter = "Clearance",
      QuantityPath = "Organism|Plasma|Drug",
      PKParameter = "AUC",
      sens = 0.3
    ),
    file.path(sensitivityDir, "ScenarioA_default.csv")
  )

  configEnv <- get0("configEnv", envir = .GlobalEnv, inherits = FALSE)
  mockEnv <- if (is.null(configEnv)) {
    new.env(parent = emptyenv())
  } else {
    configEnv
  }
  oldOutputPaths <- mockEnv$outputPaths
  on.exit(mockEnv$outputPaths <- oldOutputPaths, add = TRUE)
  mockEnv$outputPaths <- data.table::data.table(
    outputPathId = factor("plasma", levels = "plasma", ordered = TRUE),
    outputPath = "Organism|Plasma|Drug",
    displayNameOutput = "Plasma"
  )
  assign("configEnv", mockEnv, envir = .GlobalEnv)

  local_mocked_bindings(
    getPKParameterOverview = function(...) {
      data.table::data.table(
        pKParameter = "AUC",
        displayNamePKParameter = "AUC",
        displayUnitPKParameter = "mg*h/L",
        outputPathId = "plasma"
      )
    },
    xlsxReadData = function(...) {
      data.table::data.table(
        parameter = c("Clearance", "Clearance"),
        parameterPath = c("P1|CL", "P2|CL")
      )
    },
    .package = "ospsuite.reportingframework"
  )

  plotData <- ospsuite.reportingframework:::prepareSensitivityPlotData(
    onePlotConfig = data.table::data.table(
      scenario = "ScenarioA",
      scenarioLongName = "Adults",
      outputPathIds = "plasma",
      pKParameters = "AUC",
      sensitivityParameterSheet = "default",
      threshold = 0.15
    ),
    projectConfiguration = list(
      outputFolder = tmp,
      addOns = list(
        sensitivityFile = file.path(tmp, "SensitivityParameter.xlsx")
      )
    ),
    scenarioList = list(ScenarioA = list(simulation = "unused"))
  )

  expect_equal(nrow(plotData), 1)
  expect_equal(as.character(plotData$parameterName), "Clearance")
  expect_equal(plotData$sens, 0.3)
})

test_that("prepareSensitivityPlotData exposes representative strata in facet labels", {
  tmp <- withr::local_tempdir()
  sensitivityDir <- file.path(tmp, EXPORTDIR$sensitivityResults)
  dir.create(sensitivityDir, recursive = TRUE)

  data.table::fwrite(
    data.table::data.table(
      SensitivityParameter = c("P1|CL", "P1|CL"),
      QuantityPath = c("Organism|Plasma|Drug", "Organism|Plasma|Drug"),
      PKParameter = c("AUC", "AUC"),
      sens = c(0.3, 0.25),
      RepresentativeOutputPath = c(
        "Organism|Plasma|Drug",
        "Organism|Plasma|Drug"
      ),
      RepresentativePKParameter = c("AUC", "AUC"),
      RepresentativeQuantile = c("p5", "p50"),
      RepresentativeIndividualId = c("ID_1", "ID_2")
    ),
    file.path(sensitivityDir, "ScenarioA_default_representatives.csv")
  )

  configEnv <- get0("configEnv", envir = .GlobalEnv, inherits = FALSE)
  mockEnv <- if (is.null(configEnv)) {
    new.env(parent = emptyenv())
  } else {
    configEnv
  }
  oldOutputPaths <- mockEnv$outputPaths
  on.exit(mockEnv$outputPaths <- oldOutputPaths, add = TRUE)
  mockEnv$outputPaths <- data.table::data.table(
    outputPathId = factor("plasma", levels = "plasma", ordered = TRUE),
    outputPath = "Organism|Plasma|Drug",
    displayNameOutput = "Plasma"
  )
  assign("configEnv", mockEnv, envir = .GlobalEnv)

  local_mocked_bindings(
    getPKParameterOverview = function(...) {
      data.table::data.table(
        pKParameter = "AUC",
        displayNamePKParameter = "AUC",
        displayUnitPKParameter = "mg*h/L",
        outputPathId = "plasma"
      )
    },
    xlsxReadData = function(...) {
      data.table::data.table(
        parameter = "Clearance",
        parameterPath = "P1|CL"
      )
    },
    .package = "ospsuite.reportingframework"
  )

  plotData <- ospsuite.reportingframework:::prepareSensitivityPlotData(
    onePlotConfig = data.table::data.table(
      scenario = "ScenarioA",
      scenarioLongName = "Adults",
      outputPathIds = "plasma",
      pKParameters = "AUC",
      sensitivityParameterSheet = "default",
      threshold = 0.15
    ),
    projectConfiguration = list(
      outputFolder = tmp,
      addOns = list(
        sensitivityFile = file.path(tmp, "SensitivityParameter.xlsx")
      )
    ),
    scenarioList = list(ScenarioA = list(simulation = "unused"))
  )

  expect_equal(length(unique(as.character(plotData$plotTag))), 2)
  expect_true(all(grepl("p(5|50)", as.character(plotData$plotFacetLabel))))
  expect_true(all(grepl("ID_", plotData$representativeStratum)))
})
