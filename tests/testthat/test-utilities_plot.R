# initialize logging. Is always needed
projectPath <- iniLogFileForTest()

test_that("generation of default template works", {
  projectConfiguration <- suppressMessages(setUpTestProject(projectPath))

  addDefaultConfigForTimeProfilePlots(projectConfiguration = projectConfiguration,
                                      sheetName = 'TimeProfileNew',overwrite = FALSE)

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)
  expect_contains(wb$sheet_names,'TimeProfileNew')
})

cleanupLogFileForTest(projectPath)
