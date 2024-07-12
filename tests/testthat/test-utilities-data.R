test_that("It should read and process data based on the provided project configuration", {
  # Create a sample project configuration for testing
  projectConfiguration <- list(
    dataImporterConfigurationFile = system.file(
      "extdata",
      'dataImportConfiguration.xlsx',
      package = "OSPSuite.ReportingFramework",
      mustWork = TRUE
    ),
    projectConfigurationDirPath = system.file(
      "extdata",
      package = "OSPSuite.ReportingFramework",
      mustWork = TRUE
    )
  )

  # Call the function and test the output
  observedData <- readObservedDataByDictionary(projectConfiguration)

  # Add your assertions here to test the processed data
  # For example:
  expect_true(data.table::is.data.table(observedData), "Processed data should be a data table")
  expect_equal(nrow(processedData), 80, "Processed data should have the expected number of rows")
  # Add more assertions as per your requirements
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
    weight = c(70, 65, NA)  # Adding NA value for testing empty entries
    # Add more sample data as per your requirements
  )

  # Call the function and test the validation
  suppressMessages(validationResult <-
                     capture.output(
                       validateObservedData(observedData, stopIfValidationFails = FALSE)
                     ))

  # Add your assertions here to test the validation result
  expect_true(grep('empty entries in weight',validationResult) == 1)

  observedData[,weight:=1]

  # Test for uniqueness of individualId, groupId, outputPathId, and time columns
  expect_error(validateObservedData(
    data = rbind(observedData, observedData),
    stopIfValidationFails = TRUE))

  # Test for NAs or empty values in columns other than lloq and dvUnit
  observedDataChanged = data.table::copy(observedData)
  observedDataChanged[1,dv:=NA]

  expect_error(validateObservedData(
    data = rbind(observedData, observedData),
    stopIfValidationFails = TRUE))

  # Test for uniqueness of dvUnit within each outputPathId
  observedDataChanged = data.table::copy(observedData)
  observedDataChanged[1,dvUnit:='m']
  observedDataChanged[2,outputPathId:=101]

  expect_error(validateObservedData(
    data = rbind(observedData, observedData),
    stopIfValidationFails = TRUE))
})
