# testProject was set up by setup.R, this provides variable projectconfiguration and test data
dataObserved <- readObservedDataByDictionary(projectConfiguration)

test_that("It should read and process data based on the provided project configuration", {
  expect_true(data.table::is.data.table(dataObserved), "Processed data should be a data table")
  expect_equal(nrow(dataObserved), expected = 396, label = "Processed data should have the expected number of rows")

  # extract biometrics
  dtIndividualBiometrics <- xlsxReadData(
    wb = projectConfiguration$individualsFile,
    sheetName = "IndividualBiometrics"
  )
  expect_gte(nrow(dtIndividualBiometrics), 6)

  dtOutputPaths <- getOutputPathIds(projectConfiguration$plotsFile)
  expect_contains(dtOutputPaths$outputPathId, c("Plasma", "CYP3A4total", "CYP3A4Liver"))
})

test_that("It should filter data by fileIds parameter", {
  # Read the DataFiles sheet to get available fileIdentifiers
  wb <- openxlsx::loadWorkbook(projectConfiguration$dataImporterConfigurationFile)
  dataList <- xlsxReadData(
    wb = wb,
    sheetName = "DataFiles",
    skipDescriptionRow = TRUE
  )

  # Get available file identifiers
  availableFileIds <- unique(dataList$fileIdentifier)

  # Test that we can filter by a subset of fileIds
  if (length(availableFileIds) > 1) {
    # Select first fileId
    selectedFileId <- availableFileIds[1]

    dataObservedFiltered <- readObservedDataByDictionary(
      projectConfiguration,
      fileIds = selectedFileId,
      spreadData = FALSE
    )

    # Should return a valid data.table
    expect_true(
      data.table::is.data.table(dataObservedFiltered),
      "Filtered data should be a data table"
    )

    # Should have fewer rows than the full dataset
    expect_lt(nrow(dataObservedFiltered), nrow(dataObserved),
      label = "Filtered data should have fewer rows than unfiltered data"
    )
  }

  # Test that passing invalid fileIds raises an error
  expect_error(
    readObservedDataByDictionary(
      projectConfiguration,
      fileIds = "nonexistent_file_id"
    ),
    "subset"
  )
})


test_that("It should check the validity of the observed dataset", {
  # Create a sample observed dataset for testing
  dataObservedTest <- data.table(
    individualId = c(1, 2, 3),
    group = c(1, 1, 2),
    outputPathId = c(101, 102, 103),
    xValues = c(10, 20, 30),
    yValues = c(5.6, 7.8, 9.3),
    yUnit = c("mg/L", "mg/L", "mg/L"),
    lloq = c(1.0, 1.0, 1.0)
  )
  data.table::setattr(dataObservedTest[["individualId"]], "columnType", "identifier")
  data.table::setattr(dataObservedTest[["group"]], "columnType", "identifier")
  data.table::setattr(dataObservedTest[["outputPathId"]], "columnType", "identifier")
  data.table::setattr(dataObservedTest[["xValues"]], "columnType", "timeprofile")
  data.table::setattr(dataObservedTest[["yValues"]], "columnType", "timeprofile")
  data.table::setattr(dataObservedTest[["yUnit"]], "columnType", "timeprofile")
  data.table::setattr(dataObservedTest[["lloq"]], "columnType", "timeprofile")

  # Add your assertions here to test the validation result
  expect_invisible(validateObservedData(dataObservedTest, dataClassType = "timeprofile"))

  # Test for uniqueness of `individualId`, group, `outputPathId`, and time columns
  expect_error(validateObservedData(
    data = rbind(dataObservedTest, dataObservedTest)
  ))

  # Test for NAs or empty values in columns other than lloq and yUnit
  dataObservedTestChanged <- data.table::copy(dataObservedTest)
  dataObservedTestChanged[1, yValues := NA]

  expect_warning(validateObservedData(dataDT = dataObservedTestChanged, dataClassType = "timeprofile"))

  # Test for uniqueness of yUnit within each outputPathId
  dataObservedTestChanged <- data.table::copy(dataObservedTest)
  dataObservedTestChanged[1, yUnit := "m"]
  dataObservedTestChanged[2, outputPathId := 101]

  expect_warning(validateObservedData(
    dataDT = dataObservedTestChanged,
    dataClassType = "timeprofile"
  ))
})


# Unit tests for groupDataByIdentifier function
test_that("groupDataByIdentifier function test", {
  groupedData <- .groupDataByIdentifier(dataObserved)

  # Add assertions based on the expected output of the function
  expect_s3_class(groupedData, "list")
  expect_true(
    length(groupedData) ==
      dataObserved |>
        dplyr::select(.getColumnsForColumnType(dataObserved, columnTypes = "identifier")) |>
        unique() |>
        nrow()
  )
})

# Unit tests for getColumnsForColumnType function
test_that("getColumnsForColumnType function test", {
  columnTypes <- c("identifier")
  columnNames <- .getColumnsForColumnType(dataObserved, columnTypes)

  #
  expect_equal(length(columnNames), 7)
  expect_contains(columnNames, c("studyId", "subjectId", "individualId", "group", "outputPathId"))
})

# Unit tests for createDataSets function
test_that("createDataSets function test", {
  tmpData <- dataObserved[outputPathId == "Plasma"]

  groupedData <- .groupDataByIdentifier(tmpData)

  dataSet <- .createDataSets(groupData = groupedData[[1]])

  expect_s3_class(dataSet, "DataSet")
  expect_equal(dataSet$LLOQ, unique(tmpData$lloq))

  # unique yUnit
  tmpData$yUnit[1] <- "pmol/L"
  groupedData <- .groupDataByIdentifier(tmpData)
  expect_error(.createDataSets(groupedData[[1]]))

  # Warning for different LLOQ
  tmpData <- dataObserved[outputPathId == "Plasma"]
  tmpData$lloq[1] <- 2
  groupedData <- .groupDataByIdentifier(tmpData)
  expect_warning(dataSet <- .createDataSets(groupedData[[1]]))
})

# Unit tests for addMetaDataToDataSet function
test_that("addMetaDataToDataSet function test", {
  tmpData <- dataObserved[outputPathId == "Plasma"]
  groupedData <- .groupDataByIdentifier(tmpData)

  dataSet <- .createDataSets(groupData = groupedData[[1]])

  dataSetWithMeta <- .addMetaDataToDataSet(dataSet, groupData = groupedData[[1]])

  # Add assertions based on the expected output of the function
  expect_s3_class(dataSetWithMeta, "DataSet")
  expect_equal(length(dataSetWithMeta$metaData), 13)
  expect_contains(
    names(dataSetWithMeta$metaData),
    c(
      "studyId", "subjectId", "individualId", "group", "outputPathId",
      "age", "weight", "height", "gender", "population"
    )
  )
})

# Unit tests for convertDataTableToDataCombined function
test_that("convertDataTableToDataCombined function test", {
  tmpData <- dataObserved[outputPathId == "Plasma"]

  dataCombined <- convertDataTableToDataCombined(tmpData)

  # Add assertions based on the expected output of the function
  expect_s3_class(dataCombined, "DataCombined")
})


test_that("convertIdentifierColumns function works as expected", {
  testDt <- data.table(col1 = c("a,b,c", "d,e,f"), col2 = c("x,y,z", "1,2,3"), col3 = c("1,2,3", "4,5,6"))

  identifierCols <- c("col1", "col2")
  updatedDt <- suppressWarnings(.convertIdentifierColumns(testDt, identifierCols))

  # Check if commas were replaced by underscores
  expect_equal(updatedDt$col1[1], "a_b_c")
  expect_equal(updatedDt$col2[1], "x_y_z")


  expect_warning(updatedDt <- .convertIdentifierColumns(testDt, "col1"))
})

# Test for aggregateObservedDataGroups

mockData <- copy(dataObserved)
mockData[outputPathId != "Plasma", lloq := 0.6]

# Test for Aggregartion with LLOQ check
test_that("Aggregation with LLOQ check works", {
  result <- aggregateObservedDataGroups(
    dataObserved = mockData,
    groups = NULL,
    aggregationFlag = "GeometricStdDev"
  )

  expect_s3_class(result, "data.table")
  expect_true("group" %in% names(result))
  expect_contains(names(result), expected = c("yValues", "yErrorValues", "yErrorType", "numberOfIndividuals", "nBelowLLOQ"))
  expect_equal(unique(result$yErrorType), ospsuite::DataErrorType$GeometricStdDev)
  expect_true(all(is.na(result[numberOfIndividuals * 2 / 3 < nBelowLLOQ]$yValues)))

  # Test with Custom Function
  customFunc <- function(y) {
    return(list(
      yValues = mean(y, na.rm = TRUE),
      yMin = min(y, na.rm = TRUE),
      yMax = max(y, na.rm = TRUE),
      yErrorType = "Mean | range"
    ))
  }

  expect_error(result <- aggregateObservedDataGroups(dataObserved,
    groups = grep("iv", unique(dataObserved$group), value = TRUE),
    aggregationFlag = "Custom",
    customFunction = customFunc
  ))

  result <- aggregateObservedDataGroups(dataObserved,
    groups = grep("iv", unique(dataObserved$group), value = TRUE),
    aggregationFlag = "Custom",
    customFunction = customFunc,
    lloqCheckColumns1of2 = c("yMin", "yMax"),
    lloqCheckColumns2of3 = c("yValues")
  )

  expect_s3_class(result, "data.table")
  expect_contains(names(result), expected = c("yValues", "yMin", "yMax", "yErrorType", "numberOfIndividuals", "nBelowLLOQ"))
  expect_equal(unique(result$yErrorType), "Mean | range")
  expect_true(all(is.na(result[numberOfIndividuals * 1 / 2 < nBelowLLOQ]$yMin)))
  expect_true(all(is.na(result[numberOfIndividuals * 2 / 3 < nBelowLLOQ]$yValues)))
})


# Sample data for testing add Unique columns
originalData <- data.table(
  group = rep(c("A", "B"), each = 3),
  outputPathId = rep("parent", each = 6),
  xValues = rep(1:3, times = 2),
  yValues = c(5, 6, NA, 7, 8, 9),
  additionalCol1 = c("foo", "bar", "foo", "baz", "bar", "baz"),
  additionalCol2 = rep(c("Ax", "Bx"), each = 3)
)

aggregatedData <- data.table(
  group = c("A", "A", "B", "B"),
  outputPathId = rep("parent", each = 4),
  xValues = c(1, 2, 1, 2),
  yValues = c(5, 6, 7, 8),
  yErrorValues = c(1, 1, 1, 1),
  yErrorType = c("Type1", "Type1", "Type2", "Type2")
)

# Test for addUniqueColumns
test_that("addUniqueColumns works correctly", {
  result <- .addUniqueColumns(dataObserved = originalData, aggregatedData = aggregatedData)

  # Check that the result is a data.table
  expect_s3_class(result, "data.table")

  # Check the number of rows
  expect_equal(nrow(result), nrow(aggregatedData))

  # Check that unique columns are added correctly
  expect_false("additionalCol1" %in% names(result))
  expect_true("additionalCol2" %in% names(result))
})


# Unit tests for debugMode functionality
test_that("debugMode parameter converts validation errors to warnings", {
  # Create a sample observed dataset with duplicated rows (which should cause an error)
  dataObservedTest <- data.table(
    individualId = c(1, 1, 2),
    group = c(1, 1, 2),
    outputPathId = c(101, 101, 102),
    xValues = c(10, 10, 20),
    yValues = c(5.6, 7.8, 9.3),
    yUnit = c("mg/L", "mg/L", "mg/L"),
    lloq = c(1.0, 1.0, 1.0)
  )
  data.table::setattr(dataObservedTest[["individualId"]], "columnType", "identifier")
  data.table::setattr(dataObservedTest[["group"]], "columnType", "identifier")
  data.table::setattr(dataObservedTest[["outputPathId"]], "columnType", "identifier")
  data.table::setattr(dataObservedTest[["xValues"]], "columnType", "timeprofile")
  data.table::setattr(dataObservedTest[["yValues"]], "columnType", "timeprofile")
  data.table::setattr(dataObservedTest[["yUnit"]], "columnType", "timeprofile")
  data.table::setattr(dataObservedTest[["lloq"]], "columnType", "timeprofile")

  # Without debugMode, should throw an error
  expect_error(
    validateObservedData(dataDT = dataObservedTest, dataClassType = "timeprofile", debugMode = FALSE),
    "Data must be unique"
  )

  # With debugMode, should throw a warning instead
  expect_warning(
    validateObservedData(dataDT = dataObservedTest, dataClassType = "timeprofile", debugMode = TRUE),
    "Data must be unique"
  )
})


test_that("debugMode warns about filter column containing '1'", {
  # Create test data
  testData <- data.table(
    col1 = c("A", "B", "C"),
    col2 = c(10, 20, 30),
    col3 = c("x", "y", "z")
  )

  # Create a dictionary with filter = "1" (simulating TRUE() function from Excel)
  testDict <- data.table(
    targetColumn = c("col1", "newCol"),
    sourceColumn = c("col1", NA),
    filter = c(NA, "1"),
    filterValue = c(NA, "'test'"),
    type = c("identifier", "metadata")
  )

  # With debugMode TRUE, should warn about filter containing "1"
  expect_warning(
    .convertDataByDictionary(
      data = testData,
      dataFilter = NA,
      dict = testDict,
      dictionaryName = "testDict",
      debugMode = TRUE
    ),
    "Filter column contains '1'"
  )

  # Without debugMode, should not warn
  expect_no_warning(
    .convertDataByDictionary(
      data = testData,
      dataFilter = NA,
      dict = testDict,
      dictionaryName = "testDict",
      debugMode = FALSE
    )
  )
})


test_that("debugMode parameter is passed through the function chain", {
  # This test verifies that debugMode is correctly passed from readObservedDataByDictionary
  # We can't easily test the full chain without a complete test project,
  # but we can verify that the parameter is accepted and doesn't cause errors

  # Test that readObservedDataByDictionary accepts debugMode parameter
  expect_no_error({
    # This should work without errors (using existing test project)
    dataObservedDebug <- readObservedDataByDictionary(
      projectConfiguration,
      debugMode = TRUE,
      spreadData = FALSE
    )
  })

  # Verify it's still a valid data.table
  expect_true(data.table::is.data.table(dataObservedDebug))
})


test_that("debugMode is not allowed during valid runs", {
  # Save the current option state
  originalOption <- getOption("OSPSuite.RF.stopHelperFunction")

  # Set the option to simulate a valid run
  options(OSPSuite.RF.stopHelperFunction = TRUE)

  # Test that debugMode is rejected during valid runs
  expect_error(
    readObservedDataByDictionary(
      projectConfiguration,
      debugMode = TRUE
    ),
    "debugMode is not allowed during valid runs"
  )

  # Test that debugMode = FALSE works during valid runs
  expect_no_error({
    dataObserved <- readObservedDataByDictionary(
      projectConfiguration,
      debugMode = FALSE,
      spreadData = FALSE
    )
  })

  # Restore the original option
  if (is.null(originalOption)) {
    options(OSPSuite.RF.stopHelperFunction = NULL)
  } else {
    options(OSPSuite.RF.stopHelperFunction = originalOption)
  }
})
