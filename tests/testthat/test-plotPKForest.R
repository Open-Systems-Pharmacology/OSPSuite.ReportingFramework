# Unit tests for pure helper functions in plotPKForest.R.
# Integration tests (runPlot, vdiffr, real project setup) belong in the
# integration-test package.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

makeForestPlotData <- function(
    pkParameter = c("AUC", "AUC", "Cmax"),
    outputPathId = c("Plasma", "Plasma", "Liver"),
    scenario = c("pediatric", "adult", "pediatric"),
    displayNamePKParameter = pkParameter,
    displayUnitPKParameter = c("mg*h/l", "mg*h/l", "mg/l"),
    displayNameOutput = outputPathId,
    scenarioShortName = scenario,
    scenarioGroup = rep("", 3),
    xValues = c(1.2, 1.0, 0.8),
    xErrorType = rep(ospsuite::DataErrorType$GeometricStdDev, 3),
    dataType = rep("simulated", 3),
    plotTag = c("A", "A", "B"),
    N = rep(100L, 3)
) {
    data.table::data.table(
        pkParameter = pkParameter,
        outputPathId = outputPathId,
        scenario = scenario,
        displayNamePKParameter = factor(displayNamePKParameter),
        displayUnitPKParameter = displayUnitPKParameter,
        displayNameOutput = displayNameOutput,
        scenarioShortName = factor(scenarioShortName),
        scenarioGroup = factor(scenarioGroup),
        xValues = xValues,
        xErrorType = xErrorType,
        dataType = dataType,
        plotTag = plotTag,
        N = N
    )
}

# ---------------------------------------------------------------------------
# updateScalevector
# ---------------------------------------------------------------------------

test_that("updateScalevector returns defaults when input is empty", {
    result <- ospsuite.reportingframework:::updateScalevector(list())
    expect_equal(result$simulated$color, "black")
    expect_equal(result$observed$shape, "triangle filled")
})

test_that("updateScalevector merges user-supplied values", {
    result <- ospsuite.reportingframework:::updateScalevector(
        list(simulated = list(color = "red"))
    )
    expect_equal(result$simulated$color, "red")
    expect_equal(result$simulated$fill, "black")
})

test_that("updateScalevector does not alter unspecified observed defaults", {
    result <- ospsuite.reportingframework:::updateScalevector(
        list(simulated = list(shape = "square filled"))
    )
    expect_equal(result$observed$color, "darkgrey")
})

# ---------------------------------------------------------------------------
# getColumnSelectionForPKForest
# ---------------------------------------------------------------------------

test_that("getColumnSelectionForPKForest sets yColumn to scenarioShortName for ratioMode none", {
    dt <- makeForestPlotData()
    result <- ospsuite.reportingframework:::getColumnSelectionForPKForest(
        dt,
        ratioMode = "none"
    )
    expect_equal(result$yColumn, "scenarioShortName")
    expect_match(result$xLabel, "AUC")
})

test_that("getColumnSelectionForPKForest sets xLabel to 'Ratio' for ratio mode", {
    dt <- makeForestPlotData()
    result <- ospsuite.reportingframework:::getColumnSelectionForPKForest(
        dt,
        ratioMode = "individualRatios"
    )
    expect_equal(result$yColumn, "displayNamePKParameter")
    expect_equal(result$xLabel, "Ratio")
})

test_that("getColumnSelectionForPKForest sets xFacetColumn when multiple plotTags", {
    dt <- makeForestPlotData()
    result <- ospsuite.reportingframework:::getColumnSelectionForPKForest(
        dt,
        ratioMode = "none"
    )
    expect_equal(result$xFacetColumn, "plotTag")
})

test_that("getColumnSelectionForPKForest sets xFacetColumn NULL for single plotTag", {
    dt <- makeForestPlotData(plotTag = c("A", "A", "A"))
    result <- ospsuite.reportingframework:::getColumnSelectionForPKForest(
        dt,
        ratioMode = "none"
    )
    expect_null(result$xFacetColumn)
})

# ---------------------------------------------------------------------------
# getCaptionForForestPlot
# ---------------------------------------------------------------------------

test_that("getCaptionForForestPlot builds absolute caption with linear scale", {
    dt <- makeForestPlotData()
    result <- ospsuite.reportingframework:::getCaptionForForestPlot(
        plotData = dt,
        xScale = "linear",
        plotCaptionAddon = NA,
        ratioMode = "none"
    )
    expect_match(result, "linear")
    expect_false(grepl("ratios", result))
    expect_match(result, "Plasma")
})

test_that("getCaptionForForestPlot builds ratio caption", {
    dt <- makeForestPlotData()
    result <- ospsuite.reportingframework:::getCaptionForForestPlot(
        plotData = dt,
        xScale = "log",
        plotCaptionAddon = NA,
        ratioMode = "individualRatios"
    )
    expect_match(result, "ratios")
    expect_match(result, "logarithmic")
})

test_that("getCaptionForForestPlot appends plotCaptionAddon", {
    dt <- makeForestPlotData()
    result <- ospsuite.reportingframework:::getCaptionForForestPlot(
        plotData = dt,
        xScale = "linear",
        plotCaptionAddon = "See appendix.",
        ratioMode = "none"
    )
    expect_match(result, "See appendix")
})

# ---------------------------------------------------------------------------
# getFootnoteLinesForForestPlots
# ---------------------------------------------------------------------------

test_that("getFootnoteLinesForForestPlots returns simulated text with no observed data", {
    dt <- makeForestPlotData()
    result <- ospsuite.reportingframework:::getFootnoteLinesForForestPlots(
        plotData = dt,
        ratioMode = "none",
        asPointeEstimate = FALSE,
        dtDataReference = NULL
    )
    expect_match(result[1], "Simulated")
    expect_false(grepl("observed", result[1]))
})

test_that("getFootnoteLinesForForestPlots adds ratio-of-population note", {
    dt <- makeForestPlotData()
    result <- ospsuite.reportingframework:::getFootnoteLinesForForestPlots(
        plotData = dt,
        ratioMode = "ratioOfPopulation",
        asPointeEstimate = FALSE,
        dtDataReference = NULL
    )
    expect_true(length(result) > 1)
    expect_match(result[2], "ratios of population summary statistics")
})

# ---------------------------------------------------------------------------
# validatePointEstimateInputs
# ---------------------------------------------------------------------------

test_that("validatePointEstimateInputs passes with valid inputs", {
    expect_invisible(
        ospsuite.reportingframework:::validatePointEstimateInputs(
            nBootstrap = 100L,
            confLevel = 0.9,
            statFun = c("geo mean" = function(y) exp(mean(log(y[y > 0]))))
        )
    )
})

test_that("validatePointEstimateInputs errors on non-integer nBootstrap", {
    expect_error(
        ospsuite.reportingframework:::validatePointEstimateInputs(
            nBootstrap = 1.5,
            confLevel = 0.9,
            statFun = c("geo mean" = function(y) y)
        )
    )
})

test_that("validatePointEstimateInputs errors on confLevel outside [0,1]", {
    expect_error(
        ospsuite.reportingframework:::validatePointEstimateInputs(
            nBootstrap = 100L,
            confLevel = 1.5,
            statFun = c("geo mean" = function(y) y)
        )
    )
})

test_that("validatePointEstimateInputs errors on unnamed statFun", {
    expect_error(
        ospsuite.reportingframework:::validatePointEstimateInputs(
            nBootstrap = 100L,
            confLevel = 0.9,
            statFun = list(function(y) y)
        )
    )
})

# ---------------------------------------------------------------------------
# getRatioMode
# ---------------------------------------------------------------------------

test_that("getRatioMode returns 'none' when asRatio is FALSE", {
    result <- ospsuite.reportingframework:::getRatioMode(
        onePlotConfig = data.table::data.table(),
        pkParameterDT = data.table::data.table(),
        asRatio = FALSE
    )
    expect_equal(result, "none")
})

test_that("getRatioMode returns 'individualRatios' when populations match", {
    config <- data.table::data.table(
        plotName = "p1",
        scenario = "s1",
        referenceScenario = "s2"
    )
    pkDT <- data.table::data.table(
        scenario = c("s1", "s2"),
        populationId = c("pop1", "pop1")
    )
    result <- ospsuite.reportingframework:::getRatioMode(
        config,
        pkDT,
        asRatio = TRUE
    )
    expect_equal(result, "individualRatios")
})

test_that("getRatioMode returns 'ratioOfPopulation' when populations differ", {
    config <- data.table::data.table(
        plotName = "p1",
        scenario = "s1",
        referenceScenario = "s2"
    )
    pkDT <- data.table::data.table(
        scenario = c("s1", "s2"),
        populationId = c("pop1", "pop2")
    )
    result <- ospsuite.reportingframework:::getRatioMode(
        config,
        pkDT,
        asRatio = TRUE
    )
    expect_equal(result, "ratioOfPopulation")
})

# ---------------------------------------------------------------------------
# filterParameterObserved
# ---------------------------------------------------------------------------

test_that("filterParameterObserved returns NULL when dataObservedPK is NULL", {
    result <- ospsuite.reportingframework:::filterParameterObserved(
        dataObservedPK = NULL,
        onePlotConfig = data.table::data.table()
    )
    expect_null(result)
})

test_that("filterParameterObserved renames value columns and filters by config", {
    obs <- data.table::data.table(
        group = "g1",
        pkParameter = "AUC",
        outputPathId = "Plasma",
        values = 1.5,
        minValue = 1.0,
        maxValue = 2.0
    )
    config <- data.table::data.table(
        dataGroupId = "g1",
        pkParameter = "AUC",
        outputPathId = "Plasma"
    )
    result <- ospsuite.reportingframework:::filterParameterObserved(obs, config)
    expect_true("xValues" %in% names(result))
    expect_true("xMin" %in% names(result))
    expect_equal(nrow(result), 1)
})
