# # initialize projectconfiguration
# projectConfiguration <- suppressMessages(setUpTestProject(withModel = TRUE))
#
#
# mock_dataObserved <- data.table(
#   group = c("Group1", "Group2", "Group1"),
#   dataClass = DATACLASS$tpIndividual,
#   dataType = 'observed',
#   individualId = c(1, 2, 3)
# )
#
# # Test for setupVirtualTwinPopConfig
# test_that("setupVirtualTwinPopConfig works correctly", {
#   # # Create a mock workbook
#   # openxlsx::write.xlsx(data.frame(PopulationId = c("Pop1", "Pop2")),
#   #                      file = mock_projectConfiguration$populationsFile,
#   #                      sheetName = "Scenarios")
#
#   expect_null(setupVirtualTwinPopConfig(projectConfiguration, mock_dataObserved, groups = c("Group1", "Group2")))
#
#   # Check if the workbook was modified
#   wb <- openxlsx::loadWorkbook(mock_projectConfiguration$populationsFile)
#   expect_true("VirtualTwinPopulation" %in% wb$sheet_names)
# })
#
# # Test for generateVirtualTwinPopulation
# test_that("generateVirtualTwinPopulation works correctly", {
#   # Create necessary mock files
#   openxlsx::write.xlsx(data.frame(IndividualId = c(1, 2, 3)),
#                        file = mock_projectConfiguration$individualsFile,
#                        sheetName = "IndividualBiometrics")
#
#   openxlsx::write.xlsx(data.frame(ModelParameterSheets = c("Sheet1")),
#                        file = mock_projectConfiguration$modelParamsFile,
#                        sheetName = "ModelParameterSheets")
#
#   expect_null(generateVirtualTwinPopulation(mock_projectConfiguration, "mock_model_file", overwrite = TRUE))
#
#   # Check if the output CSV files are created
#   expect_true(file.exists(file.path(mock_projectConfiguration$populationsFolder, "Group1.csv")))
#   expect_true(file.exists(file.path(mock_projectConfiguration$populationsFolder, "Group2.csv")))
# })
#
# # Test for.buildVirtualTwinPopulation
# test_that(".buildVirtualTwinPopulation works correctly", {
#   params <- list(IndividualId = list(Sheet1 = list(value = 1)))
#   dPop <- data.table(PopulationName = "Pop1", DataGroup = "Group1", IndividualId = 1)
#
#   result <- .buildVirtualTwinPopulation(mock_projectConfiguration, params, dPop)
#
#   expect_true(nrow(result) > 0)
#   expect_true("PopulationName" %in% names(result))
#   expect_true("DataGroup" %in% names(result))
# })
#
# # Test for.getAllParameterForSheets
# test_that(".getAllParameterForSheets works correctly", {
#   openxlsx::write.xlsx(data.frame(paths = c("Param1", "Param2"), values = c(10, 20)),
#                        file = mock_projectConfiguration$modelParamsFile,
#                        sheetName = "Sheet1")
#
#   result <- .getAllParameterForSheets(mock_projectConfiguration, c("Sheet1"), mock_projectConfiguration$modelParamsFile, sim = NULL)
#
#   expect_true("Param1" %in% names(result$Sheet1))
#   expect_true("Param2" %in% names(result$Sheet1))
# })
#
# # Test for.cleanUpSheetList
# test_that(".cleanUpSheetList works correctly", {
#   sheets <- c("Sheet1, Sheet2", "Sheet3")
#   result <- .cleanUpSheetList(sheets)
#
#   expect_equal(result, c("Sheet1", "Sheet2", "Sheet3"))
# })
#
# # Test for.convertBiomForIndStatics
# test_that(".convertBiomForIndStatics works correctly", {
#   biomForInd <- data.table(IndividualId = 1, Population = "Pop1", Gender = "M",
#                            `Height [cm]` = 180, `Age [year(s)]` = 25)
#
#   result <- .convertBiomForIndStatics(biomForInd)
#
#   expect_true("ObservedIndividualId" %in% names(result))
#   expect_true("Organism|Height" %in% names(result))
# })
