# testProject was set up by setup.R

# Test cases for concatWithAnd function
test_that("concatWithAnd works correctly", {
  # Test with no elements
  expect_equal(.concatWithAnd(character(0)), "")

  # Test with one element
  expect_equal(.concatWithAnd(c("apple")), "apple")

  # Test with two elements
  expect_equal(.concatWithAnd(c("apple", "banana")), "apple and banana")

  # Test with three elements
  expect_equal(.concatWithAnd(c("apple", "banana", "cherry")), "apple, banana and cherry")

  # Test with four elements
  expect_equal(.concatWithAnd(c("apple", "banana", "cherry", "date")), "apple, banana, cherry and date")

  # Test with special characters
  expect_equal(.concatWithAnd(c("apple!", "banana@", "cherry#")), "apple!, banana@ and cherry#")

  # Test with empty strings
  expect_equal(.concatWithAnd(c("", "banana")), "banana")
  expect_equal(.concatWithAnd(c("apple", "", "cherry")), "apple and cherry")

  # Test with ws
  expect_equal(.concatWithAnd(c("apple ", "", " cherry")), "apple and cherry")
})

test_that("formatPercentiles returns correct labels for specific values", {
  suffix <- " percentile"

  expect_equal(formatPercentiles(c(0), suffix), c("min"))
  expect_equal(formatPercentiles(c(0.5), suffix), c("median"))
  expect_equal(formatPercentiles(c(1), suffix), c("max"))
})

test_that("formatPercentiles formats integers correctly", {
  suffix <- " percentile"

  result <- formatPercentiles(c(25, 75, 99) / 100, suffix)
  expect_equal(result[[1]], "25th percentile")
  expect_equal(result[[2]], "75th percentile")
  expect_equal(result[[3]], "99th percentile")
})

test_that("formatPercentiles formats non-integers correctly", {
  suffix <- " percentile"

  result <- formatPercentiles(c(99.5, 33.3) / 100, suffix)
  expect_equal(result[[1]], "99.5th percentile")
  expect_equal(result[[2]], "33.3th percentile")
})

test_that("formatPercentiles handles mixed input correctly", {
  suffix <- " percentile"

  result <- formatPercentiles(c(0, 50, 100, 25.5, 75) / 100, suffix)
  expect_equal(result[[1]], "min")
  expect_equal(result[[2]], "median")
  expect_equal(result[[3]], "max")
  expect_equal(result[[4]], "25.5th percentile")
  expect_equal(result[[5]], "75th percentile")
})

test_that("formatPercentiles handles mixed input correctly when allAsPercentiles is TRUE", {
  suffix <- " percentile"

  result <- formatPercentiles(c(0, 50, 100, 25.5, 75) / 100, suffix, TRUE)
  expect_equal(result, c("0th percentile", "50th percentile", "100th percentile", "25.5th percentile", "75th percentile"))
})


# Create example data for testing
mockConfigTable <- function() {
  data.table(
    level = c(1, 2, NA, NA, NA),
    header = c("Header1", "Header2", NA, NA, NA),
    value1 = c(NA, NA, 10, 30, 40),
    value2 = c(NA, NA, "A", "B", NA)
  )
}

# Write unit tests for the function
test_that("validateHeaders function test", {
  # Test if the function returns the correct configuration table without header lines
  configTable <- mockConfigTable()
  expect_equal(nrow(validateHeaders(configTable)), 3)

  # Test if the function throws an error when the plot configuration table has non-empty columns
  configTable <- mockConfigTable()
  configTable$value1[1] <- 3
  expect_error(validateHeaders(configTable))

  # Test if the function throws an error when the plot configuration table has missing header for level
  configTable <- mockConfigTable()
  configTable$header[1] <- NA
  expect_error(validateHeaders(configTable))
})

# Create example data for testing
configTablePlots <- data.table(
  CharacterColumn1 = c("A", "B", "C", "D"),
  CharacterColumn2 = c("X", NA, "Y", "Z"),
  NumericColumn1 = c(1, 2, 3, NA),
  LogicalColumn1 = c(TRUE, FALSE, TRUE, TRUE),
  NumericRangeColumn1 = c("c(1,2)", "c(3,NA)", NA, "c(5,6)")
)

test_that("validateConfigTablePlots function test", {
  expect_no_error(validateConfigTablePlots(configTablePlots,
    charactersWithoutMissing = c("CharacterColumn1"),
    charactersWithMissing = c("CharacterColumn2"),
    numericColumns = "NumericColumn1",
    logicalColumns = "LogicalColumn1",
    numericRangeColumns = "NumericRangeColumn1",
    subsetList = list(list(
      cols = c("CharacterColumn1"),
      allowedValues = c("A", "B", "C", "D")
    ))
  ))


  # Test if the function correctly validates character columns without missing values
  expect_error(validateConfigTablePlots(configTablePlots,
    charactersWithoutMissing = c("CharacterColumn2")
  ))

  # Test if the function correctly validates numeric columns
  expect_error(validateConfigTablePlots(configTablePlots,
    numericColumns = c("CharacterColumn2")
  ))

  # Test if the function correctly validates logical columns
  expect_error(validateConfigTablePlots(configTablePlots,
    logicalColumns = "CharacterColumn1"
  ))

  # Test if the function correctly validates numeric range columns
  expect_error(validateConfigTablePlots(configTablePlots, numericRangeColumns = "NumericColumn1"))

  # Test if the function correctly validates subset list
  expect_error(validateConfigTablePlots(configTablePlots,
    subsetList = list(list(
      cols = c("CharacterColumn2"),
      allowedValues = c("A", "B", "C", "D")
    ))
  ))
})


# Create example data for testing
configTablePlots <- data.table(
  Column1 = c(1, 2, NA, 4),
  Column2 = c("A", "B", NA, "D"),
  Column3 = c(TRUE, FALSE, NA, TRUE)
)

test_that("validateAtleastOneEntry function test", {
  # Test if the function correctly validates that each plot row needs at least one entry in specified columns
  expect_error(validateAtleastOneEntry(configTablePlots, c("Column1", "Column2", "Column3")))
})


# Create example data for testing
dtOutputPaths <- data.table(
  outputPathId = c("id1", "id1", "id2", "id2"),
  displayNameOutput = c("Display1", "Display1", "Display2", "Display2"),
  displayUnit = c("Unit1", "Unit1", "Unit1", "Unit2")
)

# Write unit tests for the function
test_that("validateOutputIdsForPlot function test", {
  # Test if the function correctly find the inconsistency in displayUnit
  expect_error(.validateOutputIdsForPlot(dtOutputPaths))
})

# ColorVector
test_that("getColorVectorForLegend works correctly with valid inputs", {
  colorLegend <- "red|green|blue"
  colorVector <- c(red = "#FF0000", green = "#00FF00", blue = "#0000FF")

  result <- .getColorVectorForLegend(colorLegend, colorVector)

  expect_equal(result, c(red = "#FF0000", green = "#00FF00", blue = "#0000FF"))
})

test_that("getColorVectorForLegend returns default colors when colorVector is empty", {
  colorLegend <- "red|green|blue"
  colorVector <- character(0) # Empty character vector

  result <- .getColorVectorForLegend(colorLegend, colorVector)

  expect_no_error(.validateColorVector(result))
})

test_that("getColorVectorForLegend handles missing colors gracefully", {
  colorLegend <- "red|green|blue"
  colorVector <- c(red = "#FF0000", blue = "#0000FF") # Missing green

  result <- .getColorVectorForLegend(colorLegend, colorVector)

  expect_no_error(.validateColorVector(result))
})


test_that("validateNumericVectorColumns handles nuemric vactor validation", {
  dt <- data.table(
    myValidCol = c("c(1,2,3)", "c(4,5,6)"),
    myRangeCol = c("c(1,2)", "c(NA,5)"),
    myInvalidCol1 = c("c(1,2,3)", "c(4,5,6"),
    myInvalidCol2 = c("c(1,2,3)", "c(4,5,6s)")
  )

  expect_no_error(validateNumericVectorColumns("myValidCol", dt))
  expect_no_error(validateNumericVectorColumns("myRangeCol", dt, len = 2))
  expect_error(validateNumericVectorColumns("myValidCol", dt, len = 2))
  expect_error(validateNumericVectorColumns("myInvalidCol1", dt))
  expect_error(validateNumericVectorColumns("myInvalidCol2", dt))
})


# Create example data for testing
configTablePlots <- data.table(
  plotName = c("Plot1", "Plot1", "Plot2", "Plot2"),
  column1 = c("A", "A", "B", "B"),
  column2 = c("X", "X", "X", "Y"),
  outputPathIds = c("id1", "id2", "id3", "id4")
)

test_that("validateGroupConsistency function test", {
  # Test if the function correctly checks for unique values of panel columns for each PlotName
  expect_error(validateGroupConsistency(configTablePlots, c("column1", "column2")))
})



# Create example data for testing
configTablePlots <- data.table(
  value1 = c(1, 2, 3, 4),
  value2 = c("A", "B", "C", "D"),
  timeRange_Valid1 = c(NA, "total", "firstApplication", "lastApplication"),
  timeRange_Valid2 = c("c(0,30)", NA, "c(0,40)", "lastApplication"),
  timeRange_invalid1 = c("total", "invalid", "firstApplication", "lastApplication"),
  timeRange_invalid1 = c("c(0,30,50)", "total", "firstApplication", "lastApplication"),
  timeRange_invalid1 = c("c(0,NA)", "total", "firstApplication", "lastApplication")
)

# Write unit tests for the function
test_that("validateTimeRangeColumns function test", {
  # Test if the function correctly validates correct TimeRange Columns
  expect_no_error(.validateTimeRangeColumns(configTablePlots[, c(1, 2, 3, 4)]))

  # Test if the function correctly checks for at least one TimeRange Column
  expect_error(.validateTimeRangeColumns(configTablePlots[, c(1, 2)]))

  # Test if the function correctly validates the inputs in the TimeRange columns
  expect_error(.validateTimeRangeColumns(configTablePlots[, c(1, 2, 3, 4, 5)]))

  # Test if the function correctly validates the inputs in the TimeRange columns
  expect_error(.validateTimeRangeColumns(configTablePlots[, c(1, 2, 3, 4, 6)]))

  # Test if the function correctly validates the inputs in the TimeRange columns
  expect_error(.validateTimeRangeColumns(configTablePlots[, c(1, 2, 3, 4, 7)]))
})

# Test cases for applyConfigDefaults function
test_that("applyConfigDefaults adds missing columns with default values", {
  # Create a simple config table without some columns
  testConfig <- data.table::data.table(
    plotName = c("plot1", "plot2"),
    scenario = c("scen1", "scen2")
  )
  
  # Define defaults for columns that don't exist
  testDefaults <- list(
    timeUnit = "h",
    timeOffset = 0,
    yScale = "linear"
  )
  
  # Apply defaults - expect warnings for each missing column
  expect_warning(
    expect_warning(
      expect_warning(
        result <- .applyConfigDefaults(testConfig, testDefaults),
        "was missing"
      ),
      "was missing"
    ),
    "was missing"
  )
  
  # Check that columns were added
  expect_true("timeUnit" %in% names(result))
  expect_true("timeOffset" %in% names(result))
  expect_true("yScale" %in% names(result))
  
  # Check that values are correct
  expect_equal(result$timeUnit, c("h", "h"))
  expect_equal(result$timeOffset, c(0, 0))
  expect_equal(result$yScale, c("linear", "linear"))
})

test_that("applyConfigDefaults fills columns with all NA values", {
  # Create a config table with columns that have all NA values
  testConfig <- data.table::data.table(
    plotName = c("plot1", "plot2"),
    timeUnit = c(NA_character_, NA_character_),
    timeOffset = c(NA_real_, NA_real_)
  )
  
  testDefaults <- list(
    timeUnit = "h",
    timeOffset = 0
  )
  
  # Apply defaults - expect warnings for each empty column
  expect_warning(
    expect_warning(
      result <- .applyConfigDefaults(testConfig, testDefaults),
      "was empty"
    ),
    "was empty"
  )
  
  # Check that NA values were replaced
  expect_equal(result$timeUnit, c("h", "h"))
  expect_equal(result$timeOffset, c(0, 0))
})

test_that("applyConfigDefaults fills partial NA values in columns", {
  # Create a config table with some NA values
  testConfig <- data.table::data.table(
    plotName = c("plot1", "plot2", "plot3"),
    timeUnit = c("h", NA_character_, "min"),
    timeOffset = c(0, NA_real_, 5)
  )
  
  testDefaults <- list(
    timeUnit = "h",
    timeOffset = 0
  )
  
  # Apply defaults - expect warnings for each column with missing values
  expect_warning(
    expect_warning(
      result <- .applyConfigDefaults(testConfig, testDefaults),
      "missing value"
    ),
    "missing value"
  )
  
  # Check that only NA values were replaced
  expect_equal(result$timeUnit, c("h", "h", "min"))
  expect_equal(result$timeOffset, c(0, 0, 5))
})

test_that("applyConfigDefaults does not modify columns without NA values", {
  # Create a config table with no NA values
  testConfig <- data.table::data.table(
    plotName = c("plot1", "plot2"),
    timeUnit = c("h", "min"),
    timeOffset = c(0, 5)
  )
  
  testDefaults <- list(
    timeUnit = "h",
    timeOffset = 0
  )
  
  # Apply defaults - should not generate warnings or modify data
  expect_silent(
    result <- .applyConfigDefaults(testConfig, testDefaults)
  )
  
  # Check that values remain unchanged
  expect_equal(result$timeUnit, c("h", "min"))
  expect_equal(result$timeOffset, c(0, 5))
})

test_that("applyConfigDefaults handles numeric defaults correctly", {
  testConfig <- data.table::data.table(
    plotName = c("plot1", "plot2")
  )
  
  testDefaults <- list(
    timeOffset = 0,
    nFacetColumns = 3,
    plot_TimeProfiles = 1
  )
  
  # Apply defaults - suppress warnings since we're testing functionality
  result <- suppressWarnings(.applyConfigDefaults(testConfig, testDefaults))
  
  # Check numeric values
  expect_equal(result$timeOffset, c(0, 0))
  expect_equal(result$nFacetColumns, c(3, 3))
  expect_equal(result$plot_TimeProfiles, c(1, 1))
  expect_type(result$timeOffset, "double")
  expect_type(result$nFacetColumns, "double")
})

test_that("applyConfigDefaults handles character defaults correctly", {
  testConfig <- data.table::data.table(
    plotName = c("plot1", "plot2")
  )
  
  testDefaults <- list(
    timeUnit = "h",
    yScale = "linear, log",
    facetScale = "fixed"
  )
  
  # Apply defaults - suppress warnings since we're testing functionality
  result <- suppressWarnings(.applyConfigDefaults(testConfig, testDefaults))
  
  # Check character values
  expect_equal(result$timeUnit, c("h", "h"))
  expect_equal(result$yScale, c("linear, log", "linear, log"))
  expect_equal(result$facetScale, c("fixed", "fixed"))
  expect_type(result$timeUnit, "character")
})

test_that("applyConfigDefaults works with TIMEPROFILES_CONFIG_DEFAULTS", {
  # Create a minimal config table
  testConfig <- data.table::data.table(
    plotName = c("plot1", "plot2"),
    scenario = c("scen1", "scen2")
  )
  
  # Apply actual TIMEPROFILES_CONFIG_DEFAULTS - suppress warnings since we're testing functionality
  result <- suppressWarnings(.applyConfigDefaults(testConfig, TIMEPROFILES_CONFIG_DEFAULTS))
  
  # Check that all default columns were added
  expect_true("timeUnit" %in% names(result))
  expect_true("timeOffset" %in% names(result))
  expect_true("timeOffset_Reference" %in% names(result))
  expect_true("splitPlotsPerTimeRange" %in% names(result))
  expect_true("nFacetColumns" %in% names(result))
  expect_true("yScale" %in% names(result))
  expect_true("facetScale" %in% names(result))
  expect_true("plot_TimeProfiles" %in% names(result))
  expect_true("plot_PredictedVsObserved" %in% names(result))
  expect_true("plot_ResidualsAsHistogram" %in% names(result))
  expect_true("plot_ResidualsVsTime" %in% names(result))
  expect_true("plot_ResidualsVsObserved" %in% names(result))
  expect_true("plot_QQ" %in% names(result))
  
  # Check specific default values
  expect_equal(result$timeUnit, c("h", "h"))
  expect_equal(result$timeOffset, c(0, 0))
  expect_equal(result$nFacetColumns, c(3, 3))
  expect_equal(result$yScale, c("linear, log", "linear, log"))
})

test_that("applyConfigDefaults returns data.table", {
  testConfig <- data.table::data.table(
    plotName = c("plot1", "plot2")
  )
  
  testDefaults <- list(timeUnit = "h")
  
  expect_warning(
    result <- .applyConfigDefaults(testConfig, testDefaults)
  )
  
  # Check that result is a data.table
  expect_s3_class(result, "data.table")
})

test_that("applyConfigDefaults preserves existing columns", {
  testConfig <- data.table::data.table(
    plotName = c("plot1", "plot2"),
    scenario = c("scen1", "scen2"),
    existingCol = c("val1", "val2")
  )
  
  testDefaults <- list(timeUnit = "h")
  
  expect_warning(
    result <- .applyConfigDefaults(testConfig, testDefaults)
  )
  
  # Check that original columns are preserved
  expect_true("plotName" %in% names(result))
  expect_true("scenario" %in% names(result))
  expect_true("existingCol" %in% names(result))
  expect_equal(result$plotName, c("plot1", "plot2"))
  expect_equal(result$scenario, c("scen1", "scen2"))
  expect_equal(result$existingCol, c("val1", "val2"))
})

# Tests for .handleNoConfigTable
test_that(".handleNoConfigTable works with suppressExport = FALSE", {
  rmdPlotManager <- RmdPlotManager$new(
    rmdName = "test_no_config",
    rmdfolder = projectConfiguration$outputFolder,
    nameOfplotFunction = "plotTimeProfiles",
    suppressExport = FALSE
  )
  
  # Create minimal inputs
  inputs <- list(
    scenarioResults = scenarioResults,
    dataObserved = NULL
  )
  
  result <- ospsuite.reportingframework:::.handleNoConfigTable(
    rmdPlotManager = rmdPlotManager,
    projectConfiguration = projectConfiguration,
    inputs = inputs,
    suppressExport = FALSE
  )
  
  # Should return empty list when not suppressing export
  expect_type(result, "list")
  expect_equal(length(result), 0)
})

test_that(".handleNoConfigTable works with suppressExport = TRUE", {
  rmdPlotManager <- RmdPlotManager$new(
    rmdName = "test_no_config_suppress",
    rmdfolder = projectConfiguration$outputFolder,
    nameOfplotFunction = "plotTimeProfiles",
    suppressExport = TRUE
  )
  
  inputs <- list(
    scenarioResults = scenarioResults,
    dataObserved = NULL
  )
  
  result <- ospsuite.reportingframework:::.handleNoConfigTable(
    rmdPlotManager = rmdPlotManager,
    projectConfiguration = projectConfiguration,
    inputs = inputs,
    suppressExport = TRUE
  )
  
  # Should return plot list when suppressing export
  expect_type(result, "list")
})

# Tests for .getDefaultColorsForScaleVector with n > 10
test_that(".getDefaultColorsForScaleVector works for n <= 10", {
  colors <- ospsuite.reportingframework:::.getDefaultColorsForScaleVector(5)
  expect_length(colors, 5)
  expect_type(colors, "character")
})

test_that(".getDefaultColorsForScaleVector works for n > 10", {
  # This should use the default color map
  colors <- ospsuite.reportingframework:::.getDefaultColorsForScaleVector(15)
  expect_length(colors, 15)
  expect_type(colors, "character")
})

test_that(".getDefaultColorsForScaleVector errors when n exceeds available colors", {
  # Get the max available colors
  max_colors <- length(ospsuite.plots::colorMaps[["ospDefault"]])
  
  expect_error(
    ospsuite.reportingframework:::.getDefaultColorsForScaleVector(max_colors + 1)
  )
})

# Tests for .getDefaultShapesForScaleVector
test_that(".getDefaultShapesForScaleVector returns correct number of shapes", {
  shapes <- ospsuite.reportingframework:::.getDefaultShapesForScaleVector(3)
  expect_length(shapes, 3)
})

test_that(".getDefaultShapesForScaleVector errors when n exceeds available shapes", {
  # Get available shapes
  available_shapes <- ospsuite.plots::getOspsuite.plots.option(
    optionKey = ospsuite.plots::OptionKeys$shapeValues
  )
  
  expect_error(
    ospsuite.reportingframework:::.getDefaultShapesForScaleVector(length(available_shapes) + 1)
  )
})

# Tests for .validateConfigTableForPlots
test_that(".validateConfigTableForPlots validates config table", {
  # Load a valid config table
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)
  configTable <- xlsxReadData(wb = wb, sheetName = "TimeProfileTest")
  
  # Should not error with valid table
  expect_silent(
    ospsuite.reportingframework:::.validateConfigTableForPlots(configTable)
  )
})
