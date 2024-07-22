# initialize logging. Is always needed
projectPath <- iniLogFileForTest()

test_that("It should read and process data based on the provided project configuration", {
  # Create a sample project configuration for testing
  projectConfiguration <- setUpTestProject(projectPath)
  setDataDictionary(projectConfiguration)
  addRandomSourceData(projectConfiguration)

  # Call the function and test the output
  observedData <- readObservedDataByDictionary(projectConfiguration, dataType = DATATYPE$individual)

  # Add your assertions here to test the processed data
  # For example:
  expect_true(data.table::is.data.table(observedData), "Processed data should be a data table")
  expect_equal(nrow(observedData), expected = 140, label = "Processed data should have the expected number of rows")


  expect_no_error(addBiometricsToConfig(observedData = observedData,
                        projectConfiguration = projectConfiguration))

  # extract biometrics
  dtIndividualBiometrics <- xlsxReadData(wb = projectConfiguration$individualsFile,
                                         sheetName = "IndividualBiometrics")

  expect_gte(nrow(dtIndividualBiometrics), 10)

})


test_that("It should check the validity of the observed dataset", {
  # Create a sample observed dataset for testing
  observedData <- data.table(
    individualId = c(1, 2, 3),
    groupId = c(1, 1, 2),
    outputPathId = c(101, 102, 103),
    time = c(10, 20, 30),
    dv = c(5.6, 7.8, 9.1),
    dvUnit = c("mg/L", "mg/L", "mg/L"),
    lloq = c(1.0, 1.0, 1.0),
    weight = c(70, 65, NA) # Adding NA value for testing empty entries
    # Add more sample data as per your requirements
  )

  # Call the function and test the validation
  suppressWarnings(validationResult <-
    capture.output(
      validateObservedData(observedData, stopIfValidationFails = FALSE)
    ))

  # Add your assertions here to test the validation result
  expect_true(grep("empty entries in weight", validationResult) == 1)

  # Test for uniqueness of individualId, groupId, outputPathId, and time columns
  expect_error(validateObservedData(
    data = rbind(observedData, observedData),
    stopIfValidationFails = TRUE
  ))

  # Test for NAs or empty values in columns other than lloq and dvUnit
  observedDataChanged <- data.table::copy(observedData)
  observedDataChanged[1, dv := NA]

  expect_error(validateObservedData(
    data = observedDataChanged,
    stopIfValidationFails = TRUE
  ))

  # Test for uniqueness of dvUnit within each outputPathId
  observedDataChanged <- data.table::copy(observedData)
  observedDataChanged[1, dvUnit := "m"]
  observedDataChanged[2, outputPathId := 101]

  expect_error(validateObservedData(
    data = rbind(observedData, observedData),
    stopIfValidationFails = TRUE
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
  expect_true(all(columnNames %in% c("studyId", "subjectId", "individualId", "groupId", "outputPathId")))
})

# Unit tests for createDataSets function
test_that("createDataSets function test", {
  dataDT <- randomObservedData()

  groupedData <- groupDataByIdentifier(dataDT)

  dataSet <- createDataSets(groupedData[[1]])

  expect_s3_class(dataSet, "DataSet")
  expect_equal(dataSet$LLOQ, 10)

  # unique dvUnit
  dataDT <- randomObservedData()
  dataDT$dvUnit[1] <- "pmol/L"
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

  dataSet <- createDataSets(groupedData[[1]])

  dataSetWithMeta <- addMetaDataToDataSet(dataSet, groupedData[[1]])

  # Add assertions based on the expected output of the function
  expect_s3_class(dataSetWithMeta, "DataSet")
  expect_equal(length(dataSetWithMeta$metaData), 10)
  expect_true(all(names(dataSetWithMeta$metaData) %in%
    c(
      "studyId", "subjectId", "individualId", "groupId", "outputPathId",
      "age", "weight", "height", "gender", "population"
    )))
})

# Unit tests for convertDataTableToDataCombined function
test_that("convertDataTableToDataCombined function test", {
  dataDT <- randomObservedData()

  dataCombined <- convertDataTableToDataCombined(dataDT)

  # Add assertions based on the expected output of the function
  expect_s3_class(dataCombined, "DataCombined")
  # Add more specific assertions based on the expected properties of DataCombined object
})


cleanupLogFileForTest(projectPath)
