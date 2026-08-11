# Unit tests for pure helper functions in plotDemographics.R.
# Integration tests (runPlot, vdiffr, real project setup) belong in the
# integration-test package.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

makeCaptionData <- function(
  scenarioLongName = "Adults",
  plotTag = "A",
  displayNameOutput = NULL
) {
  dt <- data.table::data.table(
    scenarioLongName = scenarioLongName,
    plotTag = plotTag
  )
  if (!is.null(displayNameOutput)) {
    dt[, displayNameOutput := displayNameOutput]
  }
  dt
}

# ---------------------------------------------------------------------------
# getCaptionForDemographicPlot
# ---------------------------------------------------------------------------

test_that("getCaptionForDemographicPlot builds histogram caption", {
  result <- ospsuite.reportingframework:::getCaptionForDemographicPlot(
    idData = makeCaptionData(),
    valueLabel = "Weight",
    binLabel = NULL,
    valueScale = "linear",
    plotCaptionAddon = NA
  )
  expect_match(result, "Weight")
  expect_match(result, "linear")
  expect_match(result, "Adults")
  expect_false(grepl("dependency", result))
})

test_that("getCaptionForDemographicPlot builds range plot caption with bin", {
  result <- ospsuite.reportingframework:::getCaptionForDemographicPlot(
    idData = makeCaptionData(),
    valueLabel = "Weight",
    binLabel = "Age",
    valueScale = "log",
    plotCaptionAddon = NA
  )
  expect_match(result, "Weight dependency")
  expect_match(result, "vs Age")
  expect_match(result, "logarithmic")
})

test_that("getCaptionForDemographicPlot appends plotCaptionAddon", {
  result <- ospsuite.reportingframework:::getCaptionForDemographicPlot(
    idData = makeCaptionData(),
    valueLabel = "Weight",
    binLabel = NULL,
    valueScale = NULL,
    plotCaptionAddon = "See study protocol"
  )
  expect_match(result, "See study protocol")
})

test_that("getCaptionForDemographicPlot omits scale text when valueScale is NULL", {
  result <- ospsuite.reportingframework:::getCaptionForDemographicPlot(
    idData = makeCaptionData(),
    valueLabel = "Weight",
    binLabel = NULL,
    valueScale = NULL,
    plotCaptionAddon = NA
  )
  expect_false(grepl("scale", result))
})

test_that("getCaptionForDemographicPlot includes output name when present", {
  result <- ospsuite.reportingframework:::getCaptionForDemographicPlot(
    idData = makeCaptionData(displayNameOutput = "AUC"),
    valueLabel = "PK",
    binLabel = "Age",
    valueScale = "linear",
    plotCaptionAddon = NA
  )
  expect_match(result, "AUC")
})

# ---------------------------------------------------------------------------
# getFootnoteLinesForRangePlots
# ---------------------------------------------------------------------------

test_that("getFootnoteLinesForRangePlots formats single label", {
  result <- ospsuite.reportingframework:::getFootnoteLinesForRangePlots("mean")
  expect_match(result, "mean")
  expect_match(result, "\\.")
})

test_that("getFootnoteLinesForRangePlots formats two labels with 'and'", {
  result <- ospsuite.reportingframework:::getFootnoteLinesForRangePlots(c(
    "mean",
    "SD"
  ))
  expect_match(result, "mean and SD")
})

test_that("getFootnoteLinesForRangePlots formats three labels", {
  result <- ospsuite.reportingframework:::getFootnoteLinesForRangePlots(c(
    "5th",
    "50th",
    "95th"
  ))
  expect_match(result, "5th, 50th and 95th")
})

# ---------------------------------------------------------------------------
# getNFacetsForDemographics
# ---------------------------------------------------------------------------

makeFacetData <- function(plotTags, scenarios, outputNames = NULL) {
  dt <- data.table::data.table(plotTag = plotTags, scenario = scenarios)
  if (!is.null(outputNames)) {
    dt[, displayNameOutput := outputNames]
  }
  dt
}

test_that("getNFacetsForDemographics returns NULL for single plotTag", {
  dt <- makeFacetData(rep("A", 4), rep("sc1", 4))
  expect_null(
    ospsuite.reportingframework:::getNFacetsForDemographics(
      dt,
      isRangePlot = FALSE
    )
  )
})

test_that("getNFacetsForDemographics returns 1 for range plot with multiple tags", {
  dt <- makeFacetData(c("A", "B"), c("sc1", "sc2"))
  expect_equal(
    ospsuite.reportingframework:::getNFacetsForDemographics(
      dt,
      isRangePlot = TRUE
    ),
    1
  )
})

test_that("getNFacetsForDemographics uses nMaxFacetRows for histogram", {
  dt <- makeFacetData(c("A", "B", "C", "D"), c("s1", "s2", "s3", "s4"))
  expect_equal(
    ospsuite.reportingframework:::getNFacetsForDemographics(
      dt,
      isRangePlot = FALSE,
      nMaxFacetRows = 2
    ),
    2
  )
})

test_that("getNFacetsForDemographics uses output count with multiple outputs and scenarios", {
  dt <- makeFacetData(
    c("A", "B", "C", "D"),
    c("s1", "s1", "s2", "s2"),
    outputNames = c("out1", "out2", "out1", "out2")
  )
  expect_equal(
    ospsuite.reportingframework:::getNFacetsForDemographics(
      dt,
      isRangePlot = FALSE,
      nMaxFacetRows = 2
    ),
    2
  )
})

# ---------------------------------------------------------------------------
# setPlotTag
# ---------------------------------------------------------------------------

test_that("setPlotTag assigns 'A' when no faceting identifier present", {
  dt <- data.table::data.table(scenario = c("s1", "s1"), value = c(1, 2))
  result <- ospsuite.reportingframework:::setPlotTag(
    dt,
    asRangePlot = TRUE,
    usePKParameter = FALSE
  )
  expect_true("plotTag" %in% names(result))
  expect_equal(unique(result$plotTag), "A")
})

test_that("setPlotTag creates one tag per scenario for histograms", {
  dt <- data.table::data.table(
    scenario = c("s1", "s1", "s2", "s2"),
    value = 1:4
  )
  result <- ospsuite.reportingframework:::setPlotTag(
    dt,
    asRangePlot = FALSE,
    usePKParameter = FALSE
  )
  expect_equal(data.table::uniqueN(result$plotTag), 2)
})

test_that("setPlotTag creates one tag per output when usePKParameter and range plot", {
  dt <- data.table::data.table(
    displayNameOutput = c("AUC", "AUC", "Cmax", "Cmax"),
    value = 1:4
  )
  result <- ospsuite.reportingframework:::setPlotTag(
    dt,
    asRangePlot = TRUE,
    usePKParameter = TRUE
  )
  expect_equal(data.table::uniqueN(result$plotTag), 2)
})
