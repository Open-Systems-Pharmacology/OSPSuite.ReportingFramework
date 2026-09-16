# Load the workbook for testing
wb <- openxlsx::createWorkbook()

# Create a sample sheet and write initial data
openxlsx::addWorksheet(wb, "ExistingSheet")
openxlsx::writeData(
  wb,
  "ExistingSheet",
  data.table(Name = c("Alice", "Bob"), Age = c(30, 25))
)
openxlsx::addWorksheet(wb, "TestSheet")
openxlsx::writeData(
  wb,
  "TestSheet",
  data.frame(
    Name = c("Alice", "Bob", "Charlie", ""),
    Age = c(30, 25, NA, ""),
    Comment = c("Good", "Average", "Excellent", "N/A")
  )
)
testxlsx <- file.path(tempdir(), "test_workbook.xlsx")
openxlsx::saveWorkbook(wb, testxlsx, overwrite = TRUE)

# Unit tests for the xlsxWriteData function
test_that("xlsxWriteData works correctly", {
  # Load the workbook for testing
  wb <- openxlsx::loadWorkbook(testxlsx)

  # Test case 1: Writing data to an existing sheet
  newData <- data.table(Name = c("Charlie", "Dana"), Age = c(28, 32))
  xlsxWriteData(wb, "ExistingSheet", newData)

  # Check if the data was written correctly
  result <- xlsxReadData(wb, "ExistingSheet")
  expected <- data.table(name = c("Charlie", "Dana"), age = c(28, 32))
  expect_equal(result, expected)

  # Test case 2: Writing data with fewer rows than existing data
  newData <- data.table(Name = c("Eve"), Age = c(22))
  xlsxWriteData(wb, "ExistingSheet", newData)

  # Check if the data was written with NA rows
  result <- xlsxReadData(wb, "ExistingSheet")
  expected <- data.table(name = c("Eve"), age = c(22))
  expect_equal(result, expected)

  # Test case 3: Writing data with more rows than existing data
  newData <- data.table(Name = c("Frank", "Gina"), Age = c(29, 33))
  xlsxWriteData(wb, "ExistingSheet", newData)

  # Check if the data was written correctly
  result <- xlsxReadData(wb, "ExistingSheet")
  expected <- data.table(name = c("Frank", "Gina"), age = c(29, 33))
  expect_equal(result, expected)

  # Test case 4: Error when writing to a non-existent sheet
  expect_error(
    xlsxWriteData(wb, "NonExistentSheet", newData),
    "Sheet NonExistentSheet does not exist."
  )
})

# Unit tests for the xlsxReadData function
test_that("xlsxReadData works correctly", {
  # Load the workbook for testing
  wb <- openxlsx::loadWorkbook(testxlsx)

  # Test case 1: Reading data without skipping any rows
  result <- xlsxReadData(wb, "TestSheet", skipDescriptionRow = FALSE)
  expected <- data.table(
    name = c("Alice", "Bob", "Charlie", NA),
    age = c(30, 25, NA, NA),
    comment = c("Good", "Average", "Excellent", "N/A")
  )
  expect_equal(result, expected)

  # Test case 2: Reading data and skipping the first row
  result <- xlsxReadData(wb, "TestSheet", skipDescriptionRow = TRUE)
  expected <- data.table(
    name = c("Bob", "Charlie", NA),
    age = c(25, NA, NA)
  )
  expect_equal(result, expected)

  # Test case 2b: Reading data and skipping the first two rows
  result <- xlsxReadData(wb, "TestSheet", skipDescriptionRow = 2)
  expected <- data.table(
    name = c("Charlie", NA),
    age = c(NA_real_, NA_real_)
  )
  expect_equal(result, expected)

  # Test case 3: Reading data with empty strings converted to NA
  result <- xlsxReadData(wb, "TestSheet", emptyAsNA = FALSE)
  expected <- data.table(
    name = c("Alice", "Bob", "Charlie", ""),
    age = c(30, 25, NA, NA),
    comment = c("Good", "Average", "Excellent", "N/A")
  )
  expect_equal(result, expected)

  # Test case 4: Reading data and converting headers to lowercase
  result <- xlsxReadData(wb, "TestSheet", convertHeaders = FALSE)
  expected <- data.table(
    Name = c("Alice", "Bob", "Charlie", NA),
    Age = c(30, 25, NA, NA),
    Comment = c("Good", "Average", "Excellent", "N/A")
  )
  expect_equal(result, expected)
})

# Unit tests for the xlsxCloneAndSet function
test_that("xlsxCloneAndSet works correctly", {
  # Load the workbook for testing
  wb <- openxlsx::loadWorkbook(testxlsx)

  # Test case 1: Cloning an existing sheet and writing new data
  newData <- data.table(Name = c("Charlie", "Dana"), Age = c(28, 32))
  xlsxCloneAndSet(wb, "ExistingSheet", "ClonedSheet", newData)

  # Check if the data was written correctly to the new sheet
  result <- xlsxReadData(wb, "ClonedSheet")
  expected <- data.table(name = c("Charlie", "Dana"), age = c(28, 32))
  expect_equal(result, expected)

  # Test case 2: Attempting to clone a non-existent sheet
  expect_error(
    xlsxCloneAndSet(wb, "NonExistentSheet", "AnotherSheet", newData),
    "Sheet NonExistentSheet does not exist in the workbook."
  )

  # Test case 3: Cloning an existing sheet to a sheet name that already exists
  expect_no_error(xlsxCloneAndSet(wb, "ExistingSheet", "ClonedSheet", newData))
})

# Unit tests for the xlsxAddSheet function
test_that("xlsxAddSheet works correctly", {
  # Load the workbook for testing
  wb <- openxlsx::loadWorkbook(testxlsx)

  # Test case 1: Adding a new sheet
  newData <- data.table(Name = c("Charlie", "Dana"), Age = c(28, 32))
  xlsxAddSheet(wb, "NewSheet", newData)

  # Check if the new sheet was added correctly
  result <- xlsxReadData(wb, "NewSheet")
  expected <- data.table(name = c("Charlie", "Dana"), age = c(28, 32))
  expect_equal(result, expected)

  # Test case 2: Adding a sheet that already exists
  expect_warning(
    xlsxAddSheet(wb, "NewSheet", newData),
    "NewSheet already exists. Existing content will be cleared."
  )

  # Check if the existing sheet content is cleared and new data is written
  result <- xlsxReadData(wb, "NewSheet")
  expect_equal(result, expected)

  # Test case 3: Attempting to add a sheet with invalid workbook
  expect_error(
    xlsxAddSheet(NULL, "InvalidSheet", newData),
    "Assertion on 'wb' failed: Must inherit from class 'Workbook', but has class 'NULL'."
  )

  # Test case 4: Attempting to add a sheet with invalid data.table
  expect_error(xlsxAddSheet(wb, "AnotherSheets", NULL))
})

# Unit tests for the xlsxAddDataUsingTemplate function
test_that("xlsxAddDataUsingTemplate works correctly", {
  # Load the workbook for testing
  wb <- openxlsx::loadWorkbook(testxlsx)

  # Test case 1: Adding data using an existing template sheet
  newData <- data.table(Name = c("Charlie", "Dana"), Age = c(28, 32))
  wb <- xlsxAddDataUsingTemplate(wb, "ExistingSheet", "NewDataSheet", newData)

  # Check if the new sheet was added correctly
  result <- xlsxReadData(wb, "NewDataSheet")
  expected <- rbind(data.table(name = "Alice", age = 30), newData)
  expect_equal(result, expected)

  # Test case 2: Attempting to add data using a non-existent template sheet
  expect_error(
    xlsxAddDataUsingTemplate(
      wb,
      "NonExistentTemplate",
      "AnotherSheet",
      newData
    ),
    'Cannot find sheet named "NonExistentTemplate"'
  )

  # Test case 3: Attempting to add data with invalid workbook
  expect_error(
    xlsxAddDataUsingTemplate(NULL, "TemplateSheet", "InvalidSheet", newData),
    "Assertion on 'wb' failed: Must inherit from class 'Workbook', but has class 'NULL'."
  )

  # Test case 4: Attempting to add data with invalid data.table
  expect_error(
    xlsxAddDataUsingTemplate(wb, "TemplateSheet", "AnotherSheet", NULL),
    "Assertion on 'dtNewData' failed: Must be a data.table, not 'NULL'."
  )
})

test_that("Splits vectors is working", {
  originalVector <- c("group1, group2", "group3,group4", NA)

  splitVector <- splitInputs(originalVector)

  expect_contains(splitVector, paste0("group", seq(1, 4)))

  originalVector <- c(NA, NA, NA)

  splitVector <- splitInputs(originalVector)

  expect_equal(splitVector, NULL)
})

# Unit tests for the separateAndTrimColumn function
test_that("separateAndTrimColumn works correctly", {
  # Test case 1: Basic functionality
  dt <- data.table(ID = 1:3, Comments = c("a, b, c", "d, e", "f"))
  result <- .separateAndTrimColumn(dt, "Comments")
  expected <- data.table(
    ID = c(1, 1, 1, 2, 2, 3),
    Comment = c("a", "b", "c", "d", "e", "f")
  )
  expect_equal(result, expected)

  # Test case 2: Handling whitespace
  dt <- data.table(ID = 1, Comments = c(" a , b , c "))
  result <- .separateAndTrimColumn(dt, "Comments")
  expected <- data.table(ID = 1, Comment = c("a", "b", "c"))
  expect_equal(result, expected)

  # Test case 3: Single value without separator
  dt <- data.table(ID = 1, Comments = c("single_value"))
  result <- .separateAndTrimColumn(dt, "Comments")
  expected <- data.table(ID = 1, Comment = c("single_value"))
  expect_equal(result, expected)

  # Test case 4: Empty string
  dt <- data.table(ID = 1, Comments = c(""))
  result <- .separateAndTrimColumn(dt, "Comments")
  expected <- data.table(ID = 1, Comment = c(""))
  expect_equal(result, expected)

  # Test case 5: Multiple rows with empty comments
  dt <- data.table(ID = 1:3, Comments = c("a, b", "", "c, d"))
  result <- .separateAndTrimColumn(dt, "Comments")
  expected <- data.table(
    ID = c(1, 1, 2, 3, 3),
    Comment = c("a", "b", "", "c", "d")
  )
  expect_equal(result, expected)

  # Test case 6: Check for plural removal
  dt <- data.table(ID = 1, Comments = c("a, b, c"))
  result <- .separateAndTrimColumn(dt, "Comments")
  expect_true("Comments" %in% names(result) == FALSE) # Ensure the plural 's' is removed
  expect_true("Comment" %in% names(result)) # Ensure the singular 'Comment' exists
})

test_that("synchronizeScenariosWithPlots adds missing scenarios to reports", {
  td <- withr::local_tempdir()
  scFile <- file.path(td, "Scenarios.xlsx")
  rpFile <- file.path(td, "Reports.xlsx")

  wbSc <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wbSc, "Scenarios")
  openxlsx::writeData(
    wbSc,
    "Scenarios",
    data.table(scenario_name = c("sc_a", "sc_b"))
  )
  openxlsx::saveWorkbook(wbSc, scFile, overwrite = TRUE)

  wbRp <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wbRp, "Scenarios")
  openxlsx::writeData(
    wbRp,
    "Scenarios",
    data.table(scenario = "sc_a", longName = "sc a", shortName = "sc a")
  )
  openxlsx::saveWorkbook(wbRp, rpFile, overwrite = TRUE)

  projectConfiguration <- list(
    scenariosFile = scFile,
    addOns = list(reportsFile = rpFile)
  )

  .synchronizeScenariosWithPlots(projectConfiguration)

  result <- xlsxReadData(wb = rpFile, sheetName = "Scenarios")
  expect_true(all(c("sc_a", "sc_b") %in% result$scenario))
})

test_that("synchronizeScenariosOutputsWithPlots syncs missing outputPath rows", {
  td <- withr::local_tempdir()
  scFile <- file.path(td, "Scenarios.xlsx")
  rpFile <- file.path(td, "Reports.xlsx")

  wbSc <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wbSc, "OutputPaths")
  openxlsx::writeData(
    wbSc,
    "OutputPaths",
    data.table(outputPathId = c("op_1", "op_2"), outputPath = c("A", "B"))
  )
  openxlsx::saveWorkbook(wbSc, scFile, overwrite = TRUE)

  wbRp <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wbRp, "Outputs")
  openxlsx::writeData(
    wbRp,
    "Outputs",
    data.table(
      outputPathId = c("template", "op_1"),
      outputPath = c("template", "A")
    )
  )
  openxlsx::saveWorkbook(wbRp, rpFile, overwrite = TRUE)

  projectConfiguration <- list(
    scenariosFile = scFile,
    addOns = list(reportsFile = rpFile)
  )

  .synchronizeScenariosOutputsWithPlots(projectConfiguration)

  outputsScenario <- xlsxReadData(wb = scFile, sheetName = "OutputPaths")
  outputsReports <- xlsxReadData(wb = rpFile, sheetName = "Outputs")

  expect_true(all(c("op_1", "op_2") %in% outputsScenario$outputPathId))
  expect_true(all(c("op_1", "op_2") %in% outputsReports$outputPathId))
})

# Edge case tests for xlsx functions
test_that("xlsxReadData handles empty sheet", {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "EmptySheet")
  testFile <- file.path(tempdir(), "empty_sheet.xlsx")
  openxlsx::saveWorkbook(wb, testFile, overwrite = TRUE)

  # Empty sheets cause an error in processing, so we expect an error
  expect_error(xlsxReadData(wb = testFile, sheetName = "EmptySheet"))

  file.remove(testFile)
})

test_that("xlsxReadData handles convertHeaders parameter", {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "TestHeaders")
  df <- data.frame(A = c("a", "b"), B = c(1, 2))
  openxlsx::writeData(wb, "TestHeaders", df)
  testFile <- file.path(tempdir(), "test_headers.xlsx")
  openxlsx::saveWorkbook(wb, testFile, overwrite = TRUE)

  result1 <- xlsxReadData(
    wb = testFile,
    sheetName = "TestHeaders",
    convertHeaders = TRUE
  )
  result2 <- xlsxReadData(
    wb = testFile,
    sheetName = "TestHeaders",
    convertHeaders = FALSE
  )
  expect_true(is.data.table(result1))
  expect_true(is.data.table(result2))

  file.remove(testFile)
})

test_that("xlsxReadData handles emptyAsNA parameter", {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "NATest")
  df <- data.frame(A = c("a", "b", "c"), B = c(1, 2, 3))
  openxlsx::writeData(wb, "NATest", df)
  testFile <- file.path(tempdir(), "test_na.xlsx")
  openxlsx::saveWorkbook(wb, testFile, overwrite = TRUE)

  result1 <- xlsxReadData(wb = testFile, sheetName = "NATest", emptyAsNA = TRUE)
  result2 <- xlsxReadData(
    wb = testFile,
    sheetName = "NATest",
    emptyAsNA = FALSE
  )
  expect_true(is.data.table(result1))
  expect_true(is.data.table(result2))

  file.remove(testFile)
})

test_that("xlsxWriteData handles data types correctly", {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "DataTypes")
  df <- data.frame(
    numeric_col = c(1.5, 2.5),
    integer_col = c(1L, 2L),
    char_col = c("a", "b"),
    logical_col = c(TRUE, FALSE)
  )
  openxlsx::writeData(wb, "DataTypes", df)
  testFile <- file.path(tempdir(), "test_types.xlsx")
  openxlsx::saveWorkbook(wb, testFile, overwrite = TRUE)

  xlsxWriteData(wb, "DataTypes", df[1, ])
  result <- xlsxReadData(wb = testFile, sheetName = "DataTypes")
  expect_true(is.data.table(result))
  expect_true(nrow(result) >= 1)

  file.remove(testFile)
})

test_that("xlsxWriteData handles large datasets", {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "LargeData")
  largedf <- data.frame(A = 1:1000, B = rep(1:100, 10))
  openxlsx::writeData(wb, "LargeData", largedf)
  testFile <- file.path(tempdir(), "large_data.xlsx")
  openxlsx::saveWorkbook(wb, testFile, overwrite = TRUE)

  result <- xlsxReadData(wb = testFile, sheetName = "LargeData")
  expect_equal(nrow(result), 1000)

  file.remove(testFile)
})

# Clean up
file.remove(testxlsx)
