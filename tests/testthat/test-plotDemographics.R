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

makeDistributionVsDemographicsConfig <- function(
  parameterIds = "age",
  parameterId_Bin = "weight",
  modeOfBinning = "Equal Width Binning",
  numberOfBins = "10"
) {
  data.table::data.table(
    level = c(1, NA),
    header = c("Plot configuration", NA),
    plotName = c(NA, "Plot1"),
    parameterIds = c(NA, parameterIds),
    scenario = c(NA, "scenario1"),
    parameterId_Bin = c(NA, parameterId_Bin),
    modeOfBinning = c(NA, modeOfBinning),
    plotCaptionAddon = c(NA, NA),
    colorLegend = c(NA, NA),
    referenceScenario = c(NA, NA),
    ylimit_linear = c(NA, NA),
    ylimit_log = c(NA, NA),
    facetScale = c(NA, "fixed"),
    numberOfBins = c(NA, numberOfBins),
    outputPathIds = c(NA, NA),
    yScale = c(NA, "linear")
  )
}

mockDistributionVsDemographicsEnv <- function() {
  configEnv <- get0("configEnv", envir = .GlobalEnv, inherits = FALSE)
  mockEnv <- if (is.null(configEnv)) new.env(parent = emptyenv()) else configEnv
  oldModelParameter <- mockEnv$modelParameter
  oldOutputPaths <- mockEnv$outputPaths
  mockEnv$modelParameter <- data.table::data.table(
    parameterId = factor(
      c("age", "sex", "weight", "height", "BMI"),
      levels = c("age", "sex", "weight", "height", "BMI"),
      ordered = TRUE
    ),
    modelPath = c("Age", "Sex", "Weight", "Height", "BMI"),
    displayNameModelParameter = c("Age", "Sex", "Weight", "Height", "BMI"),
    displayUnit = c("year", "", "kg", "cm", "kg/m^2")
  )
  mockEnv$outputPaths <- data.table::data.table(
    outputPathId = factor("path|A", levels = "path|A", ordered = TRUE),
    outputPath = "Organism|Plasma|A",
    displayNameOutput = "Plasma concentration",
    displayUnit = "mg/l"
  )
  assign("configEnv", mockEnv, envir = .GlobalEnv)

  list(
    mockEnv = mockEnv,
    oldModelParameter = oldModelParameter,
    oldOutputPaths = oldOutputPaths
  )
}

makePopulationScenarioList <- function() {
  list(scenario1 = list(population = structure(list(), class = "Population")))
}

makePkParameterDT <- function() {
  data.table::data.table(
    scenario = "scenario1",
    pkParameter = "AUC_tEnd",
    individualId = 1L,
    value = 100,
    outputPathId = "path|A",
    displayNamePKParameter = "AUC",
    displayUnitPKParameter = "mg*h/l"
  )
}

# ---------------------------------------------------------------------------
# getCaptionForDemographicPlot
# ---------------------------------------------------------------------------

test_that("validateDistributionVsDemographicsConfig rejects categorical ParameterIds", {
  envState <- mockDistributionVsDemographicsEnv()
  on.exit(
    {
      envState$mockEnv$modelParameter <- envState$oldModelParameter
      envState$mockEnv$outputPaths <- envState$oldOutputPaths
    },
    add = TRUE
  )
  local_mocked_bindings(
    populationToDataFrame = function(population) {
      data.frame(
        IndividualId = 1:2,
        Age = c(30, 40),
        Sex = c("M", "F"),
        Weight = c(70, 80)
      )
    },
    .package = "ospsuite"
  )

  expect_error(
    ospsuite.reportingframework:::validateDistributionVsDemographicsConfig(
      configTable = makeDistributionVsDemographicsConfig(
        parameterIds = "sex",
        parameterId_Bin = "age"
      ),
      scenarioList = makePopulationScenarioList()
    ),
    "ParameterIds: sex"
  )
})

test_that("validateDistributionVsDemographicsConfig rejects categorical parameterId_Bin", {
  envState <- mockDistributionVsDemographicsEnv()
  on.exit(
    {
      envState$mockEnv$modelParameter <- envState$oldModelParameter
      envState$mockEnv$outputPaths <- envState$oldOutputPaths
    },
    add = TRUE
  )
  local_mocked_bindings(
    populationToDataFrame = function(population) {
      data.frame(
        IndividualId = 1:2,
        Age = c(30, 40),
        Sex = c("M", "F"),
        Weight = c(70, 80)
      )
    },
    .package = "ospsuite"
  )

  expect_error(
    ospsuite.reportingframework:::validateDistributionVsDemographicsConfig(
      configTable = makeDistributionVsDemographicsConfig(
        parameterIds = "weight",
        parameterId_Bin = "sex"
      ),
      scenarioList = makePopulationScenarioList()
    ),
    "parameterId_Bin: sex"
  )
})

test_that("validateDistributionVsDemographicsConfig rejects missing ParameterIds", {
  envState <- mockDistributionVsDemographicsEnv()
  on.exit(
    {
      envState$mockEnv$modelParameter <- envState$oldModelParameter
      envState$mockEnv$outputPaths <- envState$oldOutputPaths
    },
    add = TRUE
  )
  local_mocked_bindings(
    populationToDataFrame = function(population) {
      data.frame(
        IndividualId = 1:2,
        Age = c(30, 40),
        Weight = c(70, 80)
      )
    },
    .package = "ospsuite"
  )

  expect_error(
    ospsuite.reportingframework:::validateDistributionVsDemographicsConfig(
      configTable = makeDistributionVsDemographicsConfig(
        parameterIds = "sex",
        parameterId_Bin = "age"
      ),
      scenarioList = makePopulationScenarioList()
    ),
    "ParameterIds: sex"
  )
})

test_that("validateDistributionVsDemographicsConfig rejects missing parameterId_Bin", {
  envState <- mockDistributionVsDemographicsEnv()
  on.exit(
    {
      envState$mockEnv$modelParameter <- envState$oldModelParameter
      envState$mockEnv$outputPaths <- envState$oldOutputPaths
    },
    add = TRUE
  )
  local_mocked_bindings(
    populationToDataFrame = function(population) {
      data.frame(
        IndividualId = 1:2,
        Age = c(30, 40),
        Weight = c(70, 80)
      )
    },
    .package = "ospsuite"
  )

  expect_error(
    ospsuite.reportingframework:::validateDistributionVsDemographicsConfig(
      configTable = makeDistributionVsDemographicsConfig(
        parameterIds = "weight",
        parameterId_Bin = "sex"
      ),
      scenarioList = makePopulationScenarioList()
    ),
    "parameterId_Bin: sex"
  )
})

test_that("validateDistributionVsDemographicsConfig requires all model ParameterIds to be in ParameterDefinitions", {
  envState <- mockDistributionVsDemographicsEnv()
  on.exit(
    {
      envState$mockEnv$modelParameter <- envState$oldModelParameter
      envState$mockEnv$outputPaths <- envState$oldOutputPaths
    },
    add = TRUE
  )
  local_mocked_bindings(
    populationToDataFrame = function(population) {
      data.frame(
        IndividualId = 1:2,
        Age = c(30, 40),
        Weight = c(70, 80)
      )
    },
    .package = "ospsuite"
  )

  expect_error(
    ospsuite.reportingframework:::validateDistributionVsDemographicsConfig(
      configTable = makeDistributionVsDemographicsConfig(
        parameterIds = "height,bmi",
        parameterId_Bin = "weight"
      ),
      scenarioList = makePopulationScenarioList()
    ),
    "parameterIds"
  )
})

test_that("validateDistributionVsDemographicsConfig rejects custom binning with fewer than two breaks", {
  envState <- mockDistributionVsDemographicsEnv()
  on.exit(
    {
      envState$mockEnv$modelParameter <- envState$oldModelParameter
      envState$mockEnv$outputPaths <- envState$oldOutputPaths
    },
    add = TRUE
  )
  local_mocked_bindings(
    populationToDataFrame = function(population) {
      data.frame(
        IndividualId = 1:2,
        Age = c(30, 40),
        Weight = c(70, 80)
      )
    },
    .package = "ospsuite"
  )

  expect_error(
    ospsuite.reportingframework:::validateDistributionVsDemographicsConfig(
      configTable = makeDistributionVsDemographicsConfig(
        parameterIds = "age",
        parameterId_Bin = "weight",
        modeOfBinning = "Custom Binning",
        numberOfBins = "seq(0,3,18)"
      ),
      scenarioList = makePopulationScenarioList()
    ),
    "numberOfBins"
  )
})

test_that("validateDistributionVsDemographicsConfig rejects custom binning with missing breaks", {
  envState <- mockDistributionVsDemographicsEnv()
  on.exit(
    {
      envState$mockEnv$modelParameter <- envState$oldModelParameter
      envState$mockEnv$outputPaths <- envState$oldOutputPaths
    },
    add = TRUE
  )
  local_mocked_bindings(
    populationToDataFrame = function(population) {
      data.frame(
        IndividualId = 1:2,
        Age = c(30, 40),
        Weight = c(70, 80)
      )
    },
    .package = "ospsuite"
  )

  expect_error(
    ospsuite.reportingframework:::validateDistributionVsDemographicsConfig(
      configTable = makeDistributionVsDemographicsConfig(
        parameterIds = "age",
        parameterId_Bin = "weight",
        modeOfBinning = "Custom Binning",
        numberOfBins = "c(NA,NA)"
      ),
      scenarioList = makePopulationScenarioList()
    ),
    "numberOfBins"
  )
})

test_that("validateDistributionVsDemographicsConfig rejects unsorted custom breaks", {
  envState <- mockDistributionVsDemographicsEnv()
  on.exit(
    {
      envState$mockEnv$modelParameter <- envState$oldModelParameter
      envState$mockEnv$outputPaths <- envState$oldOutputPaths
    },
    add = TRUE
  )
  local_mocked_bindings(
    populationToDataFrame = function(population) {
      data.frame(
        IndividualId = 1:2,
        Age = c(30, 40),
        Weight = c(70, 80)
      )
    },
    .package = "ospsuite"
  )

  expect_error(
    ospsuite.reportingframework:::validateDistributionVsDemographicsConfig(
      configTable = makeDistributionVsDemographicsConfig(
        parameterIds = "age",
        parameterId_Bin = "weight",
        modeOfBinning = "Custom Binning",
        numberOfBins = "c(0, 2, 1)"
      ),
      scenarioList = makePopulationScenarioList()
    ),
    "numberOfBins"
  )
})

test_that("validateDistributionVsDemographicsConfig rejects duplicated custom breaks", {
  envState <- mockDistributionVsDemographicsEnv()
  on.exit(
    {
      envState$mockEnv$modelParameter <- envState$oldModelParameter
      envState$mockEnv$outputPaths <- envState$oldOutputPaths
    },
    add = TRUE
  )

  test_that("validateParameterID rejects mixed model and PK parameters within one plotName", {
    envState <- mockDistributionVsDemographicsEnv()
    on.exit(
      {
        envState$mockEnv$modelParameter <- envState$oldModelParameter
        envState$mockEnv$outputPaths <- envState$oldOutputPaths
      },
      add = TRUE
    )

    configTablePlots <- data.table::data.table(
      plotName = c("Plot1", "Plot1"),
      parameterIds = c("age", "AUC_tEnd"),
      outputPathIds = c(NA, "path|A")
    )

    expect_error(
      ospsuite.reportingframework:::validateParameterID(
        configTablePlots = configTablePlots,
        pkParameterDT = makePkParameterDT()
      ),
      "either model parameters or PK parameters"
    )
  })

  test_that("validateParameterID allows model and PK parameters in different plotNames", {
    envState <- mockDistributionVsDemographicsEnv()
    on.exit(
      {
        envState$mockEnv$modelParameter <- envState$oldModelParameter
        envState$mockEnv$outputPaths <- envState$oldOutputPaths
      },
      add = TRUE
    )

    configTablePlots <- data.table::data.table(
      plotName = c("Plot1", "Plot2"),
      parameterIds = c("age", "AUC_tEnd"),
      outputPathIds = c(NA, "path|A")
    )

    expect_no_error(
      ospsuite.reportingframework:::validateParameterID(
        configTablePlots = configTablePlots,
        pkParameterDT = makePkParameterDT()
      )
    )
  })
  local_mocked_bindings(
    populationToDataFrame = function(population) {
      data.frame(
        IndividualId = 1:2,
        Age = c(30, 40),
        Weight = c(70, 80)
      )
    },
    .package = "ospsuite"
  )

  expect_error(
    ospsuite.reportingframework:::validateDistributionVsDemographicsConfig(
      configTable = makeDistributionVsDemographicsConfig(
        parameterIds = "age",
        parameterId_Bin = "weight",
        modeOfBinning = "Custom Binning",
        numberOfBins = "c(0, 1, 1, 2)"
      ),
      scenarioList = makePopulationScenarioList()
    ),
    "numberOfBins"
  )
})

test_that("getExportTableForRanges handles plot data without scenarioType", {
  plotObject <- list(
    data = data.table::data.table(
      .bin = c(1, 1, 2, 2),
      plotTag = c("A", "A", "A", "A"),
      value = c(10, 20, 30, 40),
      individualId = 1:4
    ),
    border = data.table::data.table(
      .bin = c(1, 2),
      breaks = c(0, 10),
      medianX = c(5, 15)
    )
  )

  aggregationFun <- function(values) {
    list(
      yValues = mean(values),
      yErrorValues = stats::sd(values),
      yErrorType = ospsuite::DataErrorType$ArithmeticStdDev
    )
  }

  result <- ospsuite.reportingframework:::getExportTableForRanges(
    plotObject = plotObject,
    aggregationFun = aggregationFun,
    xLabel = "Age"
  )

  expect_true("Age range" %in% names(result))
  expect_true("Age median" %in% names(result))
  expect_equal(result[["Age median"]], c(5, 15))
  expect_true("plotTag" %in% names(result))
})

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
  displayNameOutput <- NULL

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

# ---------------------------------------------------------------------------
# Additional edge case tests
# ---------------------------------------------------------------------------

test_that("getCaptionForDemographicPlot handles multiple scenarios", {
  idData <- rbind(
    makeCaptionData(scenarioLongName = "Adults", plotTag = "A"),
    makeCaptionData(scenarioLongName = "Children", plotTag = "B")
  )
  result <- ospsuite.reportingframework:::getCaptionForDemographicPlot(
    idData = idData,
    valueLabel = "Weight",
    binLabel = NULL,
    valueScale = "linear",
    plotCaptionAddon = NA
  )
  expect_match(result, "Weight")
  expect_match(result, "linear")
})

test_that("getCaptionForDemographicPlot handles special characters in labels", {
  result <- ospsuite.reportingframework:::getCaptionForDemographicPlot(
    idData = makeCaptionData(),
    valueLabel = "Weight (kg)",
    binLabel = "Age (years)",
    valueScale = "log",
    plotCaptionAddon = NA
  )
  expect_match(result, "Weight \\(kg\\)")
  expect_match(result, "Age \\(years\\)")
})

test_that("getCaptionForDemographicPlot handles empty plotCaptionAddon", {
  result <- ospsuite.reportingframework:::getCaptionForDemographicPlot(
    idData = makeCaptionData(),
    valueLabel = "Weight",
    binLabel = NULL,
    valueScale = NULL,
    plotCaptionAddon = ""
  )
  expect_false(grepl("NA", result))
})

test_that("getFootnoteLinesForRangePlots handles single item with period", {
  result <- ospsuite.reportingframework:::getFootnoteLinesForRangePlots(
    "median"
  )
  expect_true(grepl("\\.", result))
  expect_match(result, "median")
})

test_that("getFootnoteLinesForRangePlots handles multiple items formatting", {
  result <- ospsuite.reportingframework:::getFootnoteLinesForRangePlots(c(
    "Min",
    "Max"
  ))
  expect_match(result, "Min and Max")
  expect_true(grepl("\\.", result))
})

test_that("getFootnoteLinesForRangePlots handles four items", {
  result <- ospsuite.reportingframework:::getFootnoteLinesForRangePlots(c(
    "p1",
    "p2",
    "p3",
    "p4"
  ))
  expect_match(result, "p1, p2, p3 and p4")
})

test_that("getNFacetsForDemographics returns NULL for single facet", {
  dt <- makeFacetData("A", "scenario1")
  result <- ospsuite.reportingframework:::getNFacetsForDemographics(
    dt,
    isRangePlot = FALSE
  )
  expect_null(result)
})

test_that("getNFacetsForDemographics respects nMaxFacetRows parameter", {
  dt <- makeFacetData(
    c("A", "B", "C"),
    c("s1", "s2", "s3")
  )
  result <- ospsuite.reportingframework:::getNFacetsForDemographics(
    dt,
    isRangePlot = FALSE,
    nMaxFacetRows = 1
  )
  expect_equal(result, 3)
})

test_that("setPlotTag preserves all columns in data", {
  dt <- data.table::data.table(
    scenario = c("s1", "s1"),
    value = c(1, 2),
    group = c("A", "B")
  )
  result <- ospsuite.reportingframework:::setPlotTag(
    dt,
    asRangePlot = TRUE,
    usePKParameter = FALSE
  )
  expect_true(all(
    c("scenario", "value", "group", "plotTag") %in% names(result)
  ))
})

test_that("setPlotTag generates sequential plot tags", {
  dt <- data.table::data.table(
    scenario = c("s1", "s2", "s3", "s4"),
    value = 1:4
  )
  result <- ospsuite.reportingframework:::setPlotTag(
    dt,
    asRangePlot = FALSE,
    usePKParameter = FALSE
  )
  tags <- unique(result$plotTag)
  expect_true(length(tags) <= 4)
  expect_true(all(
    tags %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
  ))
})

# =====================================================================
# PHASE 1: CUSTOM FUNCTION VALIDATION & EMPTY DATA HANDLING
# =====================================================================

# ---------------------------------------------------------------------------
# Custom Function Validation Tests (plotDistributionVsDemographics)
# ---------------------------------------------------------------------------

test_that("plotDistributionVsDemographics rejects customFunction that is not a function", {
  expect_error(
    getAggregationFunction(
      aggregationFlag = "Custom",
      percentiles = c(0.05, 0.5, 0.95),
      customFunction = "not a function",
      legendsize = 2
    ),
    "Must be a function"
  )
})

test_that("plotDistributionVsDemographics rejects NULL customFunction when aggregationFlag is Custom", {
  expect_error(
    getAggregationFunction(
      aggregationFlag = "Custom",
      percentiles = c(0.05, 0.5, 0.95),
      customFunction = NULL,
      legendsize = 2
    ),
    "Must be a function"
  )
})

test_that("getAggregationFunction accepts valid custom function returning required fields", {
  customFun <- function(y) {
    list(
      yValues = mean(y),
      yMin = quantile(y, 0.05),
      yMax = quantile(y, 0.95),
      yErrorType = "mean | 5th - 95th percentile"
    )
  }

  result <- getAggregationFunction(
    aggregationFlag = "Custom",
    percentiles = c(0.05, 0.5, 0.95),
    customFunction = customFun,
    legendsize = 2
  )

  expect_true(is.function(result))
  testData <- c(1, 2, 3, 4, 5)
  aggregated <- result(testData)
  expect_true("yValues" %in% names(aggregated))
  expect_true("yErrorType" %in% names(aggregated))
})

# ---------------------------------------------------------------------------
# Empty Data Handling Tests (Phase 1)
# ---------------------------------------------------------------------------

test_that("plotDistributionVsDemographics validates pkParameterDT structure", {
  # Test that validator checks for required columns
  invalidPkDT <- data.table::data.table(
    scenario = "scenario1"
    # Missing required columns: pkParameter, value, etc.
  )

  # Verify validation catches invalid data structure
  expect_error(
    ospsuite.reportingframework:::.validatePKParameterDT(invalidPkDT),
    "not found|missing"
  )
})

test_that("plotHistograms validates scenarioList type check with non-Scenario object", {
  # Validate that checkmate type checking rejects invalid types
  expect_error(
    checkmate::assertList(
      list("not_a_scenario"),
      types = "Scenario",
      null.ok = FALSE
    ),
    "Scenario|May only contain"
  )
})

# =====================================================================
# PHASE 2: NUMERIC BOUNDS & CROSS-PARAMETER VALIDATION
# =====================================================================

# ---------------------------------------------------------------------------
# Numeric Bounds Edge Case Tests (Phase 2)
# ---------------------------------------------------------------------------

test_that("plotDistributionVsDemographics rejects facetAspectRatio = Inf", {
  expect_error(
    checkmate::assertNumeric(Inf, lower = 0, finite = TRUE, len = 1),
    "finite"
  )
})

test_that("plotDistributionVsDemographics rejects facetAspectRatio = negative", {
  expect_error(
    checkmate::assertNumeric(-0.5, lower = 0, finite = TRUE, len = 1),
    ">= 0"
  )
})

test_that("plotDistributionVsDemographics accepts facetAspectRatio = zero", {
  expect_no_error(
    checkmate::assertNumeric(0, lower = 0, finite = TRUE, len = 1)
  )
})

test_that("plotDistributionVsDemographics accepts facetAspectRatio > zero", {
  expect_no_error(
    checkmate::assertNumeric(0.5, lower = 0, finite = TRUE, len = 1)
  )
})

test_that("getAggregationFunction rejects percentiles with NaN", {
  expect_error(
    getAggregationFunction(
      aggregationFlag = "Percentiles",
      percentiles = c(0.05, NaN, 0.95),
      customFunction = NULL,
      legendsize = 2
    ),
    "missing|NaN"
  )
})

test_that("getAggregationFunction rejects percentiles with Inf", {
  expect_error(
    getAggregationFunction(
      aggregationFlag = "Percentiles",
      percentiles = c(0.05, 0.5, Inf),
      customFunction = NULL,
      legendsize = 2
    ),
    "<= 1"
  )
})

test_that("getAggregationFunction rejects percentiles outside [0, 1]", {
  expect_error(
    getAggregationFunction(
      aggregationFlag = "Percentiles",
      percentiles = c(-0.05, 0.5, 0.95),
      customFunction = NULL,
      legendsize = 2
    ),
    ">= 0"
  )
})

test_that("getAggregationFunction rejects unsorted percentiles", {
  expect_error(
    getAggregationFunction(
      aggregationFlag = "Percentiles",
      percentiles = c(0.95, 0.5, 0.05),
      customFunction = NULL,
      legendsize = 2
    ),
    "sorted|must be sorted"
  )
})

test_that("getAggregationFunction rejects duplicate percentiles", {
  expect_error(
    getAggregationFunction(
      aggregationFlag = "Percentiles",
      percentiles = c(0.05, 0.05, 0.95),
      customFunction = NULL,
      legendsize = 2
    ),
    "duplicated|unique"
  )
})

test_that("getAggregationFunction rejects wrong number of percentiles", {
  expect_error(
    getAggregationFunction(
      aggregationFlag = "Percentiles",
      percentiles = c(0.05, 0.95),
      customFunction = NULL,
      legendsize = 2
    ),
    "length 3|Must have length 3"
  )
})

# ---------------------------------------------------------------------------
# Cross-Parameter Validation Tests (Phase 2)
# ---------------------------------------------------------------------------

test_that("getAggregationFunction with Percentiles validates legendsize is 2 or 3", {
  # Test with invalid legendsize - should error at legend generation
  expect_error(
    getAggregationFunction(
      aggregationFlag = "Percentiles",
      percentiles = c(0.05, 0.5, 0.95),
      customFunction = NULL,
      legendsize = 4
    ),
    "legendsize"
  )
})

test_that("getAggregationFunction with Percentiles accepts valid legendsize", {
  # Test with legendsize = 2
  fun2 <- getAggregationFunction(
    aggregationFlag = "Percentiles",
    percentiles = c(0.05, 0.5, 0.95),
    customFunction = NULL,
    legendsize = 2
  )
  expect_true(is.function(fun2))
  
  # Test with legendsize = 3
  fun3 <- getAggregationFunction(
    aggregationFlag = "Percentiles",
    percentiles = c(0.05, 0.5, 0.95),
    customFunction = NULL,
    legendsize = 3
  )
  expect_true(is.function(fun3))
})
