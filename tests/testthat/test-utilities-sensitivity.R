# Tests for utilities-sensitivity.R

test_that("addSensitivityTable with scenarioList validates scenario name", {
  expect_error(
    addSensitivityTable(
      projectConfiguration = projectConfiguration,
      scenarioList = scenarioList,
      scenarioName = "nonexistent_scenario"
    )
  )
})

# Tests for new sensitivity functions

test_that(".loadPKValues computes ratio correctly when two scenario files exist", {
  # Prepare a temporary folder and write two small PKAnalysis CSVs
  tmpdir <- tempdir()
  on.exit({
    # cleanup files (best-effort)
    f1 <- file.path(tmpdir, "scA-PKAnalysisResults.csv")
    f2 <- file.path(tmpdir, "scB-PKAnalysisResults.csv")
    if (file.exists(f1)) file.remove(f1)
    if (file.exists(f2)) file.remove(f2)
  }, add = TRUE)

  # Create small data.tables
  dt_ref <- data.table::data.table(
    QuantityPath = c("Q1", "Q2"),
    Parameter = c("P1", "P2"),
    Value = c(2, 5),
    IndividualId = c(0, 0)
  )
  dt_num <- data.table::data.table(
    QuantityPath = c("Q1", "Q2"),
    Parameter = c("P1", "P2"),
    Value = c(4, 10),
    IndividualId = c(0, 0)
  )

  data.table::fwrite(dt_ref, file = file.path(tmpdir, "scA-PKAnalysisResults.csv"))
  data.table::fwrite(dt_num, file = file.path(tmpdir, "scB-PKAnalysisResults.csv"))

  scenarioFiles <- c(scA = "fileA.pkml", scB = "fileB.pkml")
  out <- ospsuite.reportingframework:::.loadPKValues(
    scenarioFiles = scenarioFiles,
    outputPaths = c("Q1", "Q2"),
    pkParameter = c("P1", "P2"),
    outFolder = tmpdir
  )

  # The returned Value should equal numerator/reference
  expect_true(all(out[order(QuantityPath), Value] == c(2, 2)))
  expect_equal(nrow(out), 2)
  expect_true("PKParameter" %in% names(out))
  expect_true("IndividualId" %in% names(out))
})

test_that(".loadPKValues errors when files missing", {
  tmpdir <- tempdir()
  scenarioFiles <- c(scA = "fileA.pkml")
  # ensure file missing
  missing_file <- file.path(tmpdir, "scA-PKAnalysisResults.csv")
  if (file.exists(missing_file)) file.remove(missing_file)
  expect_error(
    ospsuite.reportingframework:::.loadPKValues(
      scenarioFiles = scenarioFiles,
      outputPaths = "Q1",
      pkParameter = "P1",
      outFolder = tmpdir
    ),
    regexp = "PK analysis file not found"
  )
})

test_that(".prepareSensitivityPopulation errors when none of the requested parameter paths exist", {
  # Use a simulation file that should exist in inst/extdata
  test_simulation_file <- system.file("extdata", "SimulationResults", "i123413_iv.pkml", package = "ospsuite.reportingframework")
  
  # Skip test if file doesn't exist
  skip_if_not(file.exists(test_simulation_file), "Test simulation file not found")

  scenarioFiles <- c(scA = test_simulation_file)

  # All paths are bogus and should not exist
  sensitivityParameter_all_missing <- list(
    AllBad = c("No|Such|Path1", "No|Such|Path2")
  )

  expect_error(
    suppressWarnings(ospsuite.reportingframework:::.prepareSensitivityPopulation(
      scenarioFiles = scenarioFiles,
      sensitivityParameter = sensitivityParameter_all_missing,
      variationRange = 0.1,
      numberOfSteps = 2
    )),
    regexp = "None of the requested sensitivity parameter paths were found",
    fixed = FALSE
  )
})
