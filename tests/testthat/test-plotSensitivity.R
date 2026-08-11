# Unit tests for pure helper functions in plotSensitivity.R.
# Integration tests (runPlot, vdiffr, real project setup) belong in the
# integration-test package.

# ---------------------------------------------------------------------------
# sensitivityAnalysisName
# ---------------------------------------------------------------------------

test_that("sensitivityAnalysisName builds correct filename", {
  result <- ospsuite.reportingframework:::sensitivityAnalysisName(
    "myScenario",
    "mySheet"
  )
  expect_equal(result, "myScenario_mySheet.csv")
})

test_that("sensitivityAnalysisName handles vectorised scenario input", {
  result <- ospsuite.reportingframework:::sensitivityAnalysisName(
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
  plotTag = "A"
) {
  data.table::data.table(
    outputPathId = outputPathId,
    pKParameter = pKParameter,
    scenarioLongName = scenarioLongName,
    displayNameOutput = displayNameOutput,
    plotTag = plotTag
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
    )
  )
})
