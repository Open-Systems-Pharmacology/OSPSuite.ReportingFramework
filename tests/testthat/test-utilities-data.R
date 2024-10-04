# initialize logging. Is always needed
projectConfiguration <- setUpTestProject()
initLogfunction(projectConfiguration, verbose = FALSE)

test_that("It should read and process data based on the provided project configuration", {
  # Create a sample project configuration for testing
  setDataDictionary(projectConfiguration)
  addRandomSourceData(projectConfiguration)

  # Call the function and test the output
  observedData <- suppressWarnings(readObservedDataByDictionary(projectConfiguration))
  # Add your assertions here to test the processed data
  # For example:
  expect_true(data.table::is.data.table(observedData), "Processed data should be a data table")
  expect_equal(nrow(observedData), expected = 140, label = "Processed data should have the expected number of rows")

  # extract biometrics
  dtIndividualBiometrics <- xlsxReadData(
    wb = projectConfiguration$individualsFile,
    sheetName = "IndividualBiometrics"
  )
  expect_gte(nrow(dtIndividualBiometrics), 10)

  dtOutputPaths <- getOutputPathIds(projectConfiguration)
  expect_contains(dtOutputPaths$outputPathId, c("PARENT", "METABOLITE"))
})


test_that("It should check the validity of the observed dataset", {

  # Create a sample observed dataset for testing
  observedData <- data.table(
    individualId = c(1, 2, 3),
    group = c(1, 1, 2),
    outputPathId = c(101, 102, 103),
    xValues = c(10, 20, 30),
    yValues = c(5.6, 7.8, 9.3),
    yUnit = c("mg/L", "mg/L", "mg/L"),
    lloq = c(1.0, 1.0, 1.0)
  )
  data.table::setattr(observedData[['individualId']], "columnType", 'identifier')
  data.table::setattr(observedData[['group']], "columnType", 'identifier')
  data.table::setattr(observedData[['outputPathId']], "columnType", 'identifier')
  data.table::setattr(observedData[['xValues']], "columnType", 'timeprofile')
  data.table::setattr(observedData[['yValues']], "columnType", 'timeprofile')
  data.table::setattr(observedData[['yUnit']], "columnType", 'timeprofile')
  data.table::setattr(observedData[['lloq']], "columnType", 'timeprofile')

  # Add your assertions here to test the validation result
  expect_invisible(validateObservedData(observedData))

  # Test for uniqueness of `individualId`, group, `outputPathId`, and time columns
  expect_error(validateObservedData(
    data = rbind(observedData, observedData)
  ))

  # Test for NAs or empty values in columns other than lloq and yUnit
  observedDataChanged <- data.table::copy(observedData)
  observedDataChanged[1, yValues := NA]

  expect_warning(validateObservedData(
    data = observedDataChanged
  ))

  # Test for uniqueness of yUnit within each outputPathId
  observedDataChanged <- data.table::copy(observedData)
  observedDataChanged[1, yUnit := "m"]
  observedDataChanged[2, outputPathId := 101]

  expect_error(validateObservedData(
    data = rbind(observedData, observedData)
  ))
})


# Unit tests for groupDataByIdentifier function
test_that("groupDataByIdentifier function test", {
  dataDT <- randomObservedData()

  groupedData <- groupDataByIdentifier(dataDT)

  # Add assertions based on the expected output of the function
  expect_s3_class(groupedData, "list")
  expect_true(
    length(groupedData) ==
      dataDT %>%
        dplyr::select(getColumnsForColumnType(dataDT, columnTypes = "identifier")) %>%
        unique() %>%
        nrow()
  )
})

# Unit tests for getColumnsForColumnType function
test_that("getColumnsForColumnType function test", {
  dataDT <- randomObservedData()

  columnTypes <- c("identifier")
  columnNames <- getColumnsForColumnType(dataDT, columnTypes)

  #
  expect_equal(length(columnNames), 5)
  expect_contains(columnNames, c("studyId", "subjectId", "individualId", "group", "outputPathId"))
})

# Unit tests for createDataSets function
test_that("createDataSets function test", {
  dataDT <- randomObservedData()

  groupedData <- groupDataByIdentifier(dataDT)

  dataSet <- createDataSets(groupData = groupedData[[1]])

  expect_s3_class(dataSet, "DataSet")
  expect_equal(dataSet$LLOQ, 10)

  # unique yUnit
  dataDT <- randomObservedData()
  dataDT$yUnit[1] <- "pmol/L"
  groupedData <- groupDataByIdentifier(dataDT)
  expect_error(createDataSets(groupedData[[1]]))

  # Warning for different LLOQ
  dataDT <- randomObservedData()
  dataDT$lloq[1] <- 2
  groupedData <- groupDataByIdentifier(dataDT)
  expect_warning(dataSet <- createDataSets(groupedData[[1]]))
})

# Unit tests for addMetaDataToDataSet function
test_that("addMetaDataToDataSet function test", {
  dataDT <- randomObservedData()
  groupedData <- groupDataByIdentifier(dataDT)

  dataSet <- createDataSets(groupData = groupedData[[1]])

  dataSetWithMeta <- addMetaDataToDataSet(dataSet, groupData = groupedData[[1]])

  # Add assertions based on the expected output of the function
  expect_s3_class(dataSetWithMeta, "DataSet")
  expect_equal(length(dataSetWithMeta$metaData), 10)
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
  dataDT <- randomObservedData()

  dataCombined <- convertDataTableToDataCombined(dataDT)

  # Add assertions based on the expected output of the function
  expect_s3_class(dataCombined, "DataCombined")
})


test_that("convertIdentifierColumns function works as expected", {
  testDt <- data.table(col1 = c("a,b,c", "d,e,f"), col2 = c("x,y,z", "1,2,3"), col3 = c("1,2,3", "4,5,6"))

  identifierCols <- c("col1", "col2")
  updatedDt <- suppressWarnings(convertIdentifierColumns(testDt, identifierCols))

  # Check if commas were replaced by underscores
  expect_equal(updatedDt$col1[1], "a_b_c")
  expect_equal(updatedDt$col2[1], "x_y_z")


  expect_warning(updatedDt <- convertIdentifierColumns(testDt, "col1"))
})


# Example observed data for aggregation test
dataObserved <- data.table(
  individualId = rep(1:10, each = 3),
  outputPathId = rep("parent", each = 30),
  group = rep(c("A", "B"), each = 15),
  xValues = rep(c(1, 2, 3), times = 10),
  yValues = c(5, 6, NA, 7, 8, 9, 2, 3, 4, 1, 2, 3, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, NA, 7, 8, 9, 10, 8, 4, 6),
  lloq = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
  dataType = rep("observed", 30),
  dataClass = rep(DATACLASS$tpIndividual, 30)
)


# Test for aggregatedObservedDataGroups
test_that("aggregatedObservedDataGroups works correctly", {
  # Test with default aggregation (GeometricStdDev)
  result <- aggregatedObservedDataGroups(dataObserved, groups = c("A", "B"), aggregationFlag = "GeometricStdDev")

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 6) # Expecting 2 groups x 3 unique xValues

  # Test with Percentiles
  result <- aggregatedObservedDataGroups(dataObserved, groups = c("A", "B"), aggregationFlag = "Percentiles", percentiles = c(0.25, 0.5, 0.75))

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 6) # Expecting 2 groups x 3 unique xValues

  # Test with Custom Function
  customFunc <- function(y) {
    return(list(
      yValues = mean(y, na.rm = TRUE),
      yMin = min(y, na.rm = TRUE),
      yMax = max(y, na.rm = TRUE),
      yErrorType = "CustomError"
    ))
  }

  result <- aggregatedObservedDataGroups(dataObserved, groups = c("A", "B"), aggregationFlag = "Custom", customFunction = customFunc)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 6) # Expecting 2 groups x 3 unique xValues
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
  result <- addUniqueColumns(dataObserved = originalData, aggregatedData = aggregatedData)

  # Check that the result is a data.table
  expect_s3_class(result, "data.table")

  # Check the number of rows
  expect_equal(nrow(result), nrow(aggregatedData))

  # Check that unique columns are added correctly
  expect_false("additionalCol1" %in% names(result))
  expect_true("additionalCol2" %in% names(result))
})



cleanupLogFileForTest(projectConfiguration)
