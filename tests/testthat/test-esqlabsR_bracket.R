# testProject was set up by setup.R  which involved most of the esqlabsR_bracket scripts,
# it also provides a projectConfiguration, scenrioList and scenarioListInd

test_that("initProject copies files from sourceFolder to destination", {
  configurationDirectory <- projectConfiguration$configurationsFolder

  # Check if the files were copied to the destination folder
  fileList <- list.files(configurationDirectory)
  expect_gte(length(fileList), expected = 9)
  expect_true("Plots.xlsx" %in% fileList)
})

test_that("initProject respects the overwrite flag", {
  tmpDir <- file.path(tempdir(), paste0("initProject_overwrite_", Sys.getpid()))
  dir.create(tmpDir, recursive = TRUE)
  on.exit(unlink(tmpDir, recursive = TRUE))

  # First init
  initProject(configurationDirectory = tmpDir,
               overwrite = FALSE)
  configDir <- file.path(tmpDir, "Configurations")
  plotsPath <- file.path(configDir, "Plots.xlsx")
  expect_true(file.exists(plotsPath))
  t1 <- file.mtime(plotsPath)

  Sys.sleep(1)

  # Second init with overwrite=FALSE should NOT replace the file
  initProject(configurationDirectory = tmpDir, overwrite = FALSE)
  expect_equal(file.mtime(plotsPath), t1)

  # Second init with overwrite=TRUE should replace the file
  initProject(configurationDirectory = tmpDir, overwrite = TRUE)
  t2 <- file.mtime(plotsPath)
  expect_gt(t2, t1)
})

test_that("initProject creates directories listed in the configuration", {
  tmpDir <- file.path(tempdir(), paste0("initProject_dirs_", Sys.getpid()))
  dir.create(tmpDir, recursive = TRUE)
  on.exit(unlink(tmpDir, recursive = TRUE))

  initProject(configurationDirectory = tmpDir)
  # The RF ProjectConfiguration.xlsx lists an output folder; check it was made
  expect_true(dir.exists(file.path(tmpDir, "Configurations")))
})

test_that("initProject adds PKParameter sheet to Scenarios.xlsx when absent", {
  tmpDir <- file.path(tempdir(), paste0("initProject_pk_", Sys.getpid()))
  dir.create(tmpDir, recursive = TRUE)
  on.exit(unlink(tmpDir, recursive = TRUE))

  initProject(configurationDirectory = tmpDir)
  scenariosPath <- file.path(tmpDir, "Configurations", "Scenarios.xlsx")
  skip_if_not(file.exists(scenariosPath), "Scenarios.xlsx not created by initProject in this environment")

  wb <- openxlsx::loadWorkbook(scenariosPath)
  expect_true("PKParameter" %in% wb$sheet_names)
})

# ── .setEsqlabsRVersionInConfig ───────────────────────────────────────────────

test_that(".setEsqlabsRVersionInConfig updates the esqlabsRVersion row", {
  tmpXlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmpXlsx))

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  openxlsx::writeData(wb, "Sheet1", data.frame(
    Property = c("esqlabsRVersion", "someOtherProp"),
    Value    = c("0.0.0", "someValue"),
    Description = c("version", "other")
  ))
  openxlsx::saveWorkbook(wb, tmpXlsx, overwrite = TRUE)

  ospsuite.reportingframework:::.setEsqlabsRVersionInConfig(tmpXlsx)

  wbResult <- openxlsx::loadWorkbook(tmpXlsx)
  dt <- ospsuite.reportingframework:::xlsxReadData(wb = wbResult, sheetName = 1)
  installedVersion <- as.character(utils::packageVersion("esqlabsR"))
  expect_equal(dt[property == "esqlabsRVersion", value], installedVersion)
})

test_that(".setEsqlabsRVersionInConfig emits a message when esqlabsRVersion row is absent", {
  tmpXlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmpXlsx))

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  openxlsx::writeData(wb, "Sheet1", data.frame(
    Property = "someOtherProp",
    Value    = "someValue",
    Description = "other"
  ))
  openxlsx::saveWorkbook(wb, tmpXlsx, overwrite = TRUE)

  expect_message(
    ospsuite.reportingframework:::.setEsqlabsRVersionInConfig(tmpXlsx),
    regexp = "esqlabsRVersion"
  )
})

test_that(".setEsqlabsRVersionInConfig is a no-op for non-existent file", {
  expect_silent(
    ospsuite.reportingframework:::.setEsqlabsRVersionInConfig(
      file.path(tempdir(), "does_not_exist_12345.xlsx")
    )
  )
})

# ── .mergeEsqlabsRConfigProperties ────────────────────────────────────────────

test_that(".mergeEsqlabsRConfigProperties appends missing rows", {
  rfXlsx  <- tempfile(fileext = ".xlsx")
  esqXlsx <- tempfile(fileext = ".xlsx")
  on.exit({ unlink(rfXlsx); unlink(esqXlsx) })

  # RF config has only "propA"
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Config")
  openxlsx::writeData(wb, "Config", data.frame(
    Property    = "propA",
    Value       = "valA",
    Description = "descA"
  ))
  openxlsx::saveWorkbook(wb, rfXlsx, overwrite = TRUE)

  # esqlabsR config has "propA" and "propB"
  wb2 <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb2, "Config")
  openxlsx::writeData(wb2, "Config", data.frame(
    Property    = c("propA", "propB"),
    Value       = c("valA",  "valB"),
    Description = c("descA", "descB")
  ))
  openxlsx::saveWorkbook(wb2, esqXlsx, overwrite = TRUE)

  ospsuite.reportingframework:::.mergeEsqlabsRConfigProperties(rfXlsx, esqXlsx)

  wbResult <- openxlsx::loadWorkbook(rfXlsx)
  dt <- ospsuite.reportingframework:::xlsxReadData(wb = wbResult, sheetName = 1)
  expect_true("propA" %in% dt$property)
  expect_true("propB" %in% dt$property)
  # propA should not be duplicated
  expect_equal(sum(dt$property == "propA"), 1L)
})

test_that(".mergeEsqlabsRConfigProperties is a no-op when nothing is missing", {
  rfXlsx  <- tempfile(fileext = ".xlsx")
  esqXlsx <- tempfile(fileext = ".xlsx")
  on.exit({ unlink(rfXlsx); unlink(esqXlsx) })

  sharedData <- data.frame(
    Property    = c("propA", "propB"),
    Value       = c("valA",  "valB"),
    Description = c("descA", "descB")
  )

  for (f in c(rfXlsx, esqXlsx)) {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Config")
    openxlsx::writeData(wb, "Config", sharedData)
    openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  }

  tBefore <- file.mtime(rfXlsx)
  Sys.sleep(1)
  ospsuite.reportingframework:::.mergeEsqlabsRConfigProperties(rfXlsx, esqXlsx)
  # File should not be rewritten when nothing changed
  expect_equal(file.mtime(rfXlsx), tBefore)
})

test_that(".mergeEsqlabsRConfigProperties is a no-op for non-existent files", {
  expect_silent(
    ospsuite.reportingframework:::.mergeEsqlabsRConfigProperties(
      rfConfigPath      = file.path(tempdir(), "missing_rf.xlsx"),
      esqlabsRConfigPath = file.path(tempdir(), "missing_esq.xlsx")
    )
  )
})

# ── .convertLegacyConfigSheet ─────────────────────────────────────────────────

test_that(".convertLegacyConfigSheet is a no-op when all properties are allowed", {
  tmpXlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmpXlsx))

  # Use only a property that exists in the RF template
  allowedProp <- ospsuite.reportingframework:::xlsxReadData(
    system.file("templates", "ProjectConfiguration.xlsx",
                package = "ospsuite.reportingframework")
  )$property[[1]]

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Config")
  openxlsx::writeData(wb, "Config", data.frame(
    Property    = allowedProp,
    Value       = "someValue",
    Description = "desc"
  ))
  openxlsx::saveWorkbook(wb, tmpXlsx, overwrite = TRUE)

  tBefore <- file.mtime(tmpXlsx)
  Sys.sleep(1)
  ospsuite.reportingframework:::.convertLegacyConfigSheet(tmpXlsx)
  # File should not be rewritten when there are no leftover properties
  expect_equal(file.mtime(tmpXlsx), tBefore)
})

test_that(".convertLegacyConfigSheet moves unknown properties to RFAddons sheet", {
  tmpXlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmpXlsx))

  allowedProp <- ospsuite.reportingframework:::xlsxReadData(
    system.file("templates", "ProjectConfiguration.xlsx",
                package = "ospsuite.reportingframework")
  )$property[[1]]

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Config")
  openxlsx::writeData(wb, "Config", data.frame(
    Property    = c(allowedProp, "rfSpecificProp"),
    Value       = c("v1",        "rfValue"),
    Description = c("d1",        "rfDesc")
  ))
  openxlsx::saveWorkbook(wb, tmpXlsx, overwrite = TRUE)

  ospsuite.reportingframework:::.convertLegacyConfigSheet(tmpXlsx)

  wbResult <- openxlsx::loadWorkbook(tmpXlsx)
  expect_true("RFAddons" %in% wbResult$sheet_names)

  dtMain   <- ospsuite.reportingframework:::xlsxReadData(wb = wbResult, sheetName = 1)
  dtAddOns <- ospsuite.reportingframework:::xlsxReadData(wb = wbResult, sheetName = "RFAddons")

  expect_false("rfSpecificProp" %in% dtMain$property)
  expect_true("rfSpecificProp"  %in% dtAddOns$property)
  expect_equal(dtAddOns[property == "rfSpecificProp", value], "rfValue")
})

test_that(".convertLegacyConfigSheet appends to an existing RFAddons sheet without duplication", {
  tmpXlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmpXlsx))

  allowedProp <- ospsuite.reportingframework:::xlsxReadData(
    system.file("templates", "ProjectConfiguration.xlsx",
                package = "ospsuite.reportingframework")
  )$property[[1]]

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Config")
  openxlsx::writeData(wb, "Config", data.frame(
    Property    = c(allowedProp, "rfProp1", "rfProp2"),
    Value       = c("v1", "rv1", "rv2"),
    Description = c("d1", "rd1", "rd2")
  ))
  openxlsx::addWorksheet(wb, "RFAddons")
  openxlsx::writeData(wb, "RFAddons", data.frame(
    property    = "existingAddon",
    value       = "exVal",
    description = "exDesc"
  ))
  openxlsx::saveWorkbook(wb, tmpXlsx, overwrite = TRUE)

  ospsuite.reportingframework:::.convertLegacyConfigSheet(tmpXlsx)

  wbResult <- openxlsx::loadWorkbook(tmpXlsx)
  dtAddOns <- ospsuite.reportingframework:::xlsxReadData(wb = wbResult, sheetName = "RFAddons")

  expect_true("existingAddon" %in% dtAddOns$property)
  expect_true("rfProp1"       %in% dtAddOns$property)
  expect_true("rfProp2"       %in% dtAddOns$property)
  expect_equal(sum(dtAddOns$property == "existingAddon"), 1L)
})

# ── createProjectConfiguration ────────────────────────────────────────────────

test_that("createProjectConfiguration returns a ProjectConfigurationRF object", {
  expect_s3_class(projectConfiguration, "ProjectConfigurationRF")
})



test_that("TestProject has correct format", {
  expect_s3_class(projectConfiguration, "ProjectConfiguration")

  # Perform assertions
  expect_true(length(scenarioList) > 0)

  expect_true(length(list.files(file.path(projectConfiguration$outputFolder, EXPORTDIR$simulationResult))) > 0)
})


test_that("loadScenarioResults loads existing scenarios", {
  scenarioNames <- names(scenarioList)[c(1, 2)]

  result <- loadScenarioResultsToFramework(
    projectConfiguration = projectConfiguration,
    scenarioNames = scenarioNames
  )

  expect_type(result, "list")
  expect_equal(length(result), length(scenarioNames))
  expect_true(all(scenarioNames %in% names(result)))
})

test_that("loadScenarioResults throws an error for missing scenarios", {
  scenarioNames <- c("nonexistentScenario")

  expect_error(loadScenarioResults(projectConfiguration, scenarioNames))
})

test_that("runAndSaveScenarios runs and saves scenarios", {
  scenarioNames <- names(scenarioListInd)[c(2, 3)]

  projectConfigurationTest <- projectConfiguration$clone()

  outfolderOld <- projectConfigurationTest$outputFolder
  projectConfigurationTest$outputFolder <-
    file.path("..", "..", "outputTestSimulation")
  if (!dir.exists(projectConfigurationTest$outputFolder)) dir.create(projectConfigurationTest$outputFolder)

  result <- runAndSaveScenarios(projectConfigurationTest, scenarioListInd[scenarioNames])

  expect_type(result, "list")
  expect_true(all(scenarioNames %in% names(result)))

  # Verify saved results
  for (sc in scenarioNames) {
    resultFile <- file.path(projectConfigurationTest$outputFolder, EXPORTDIR$simulationResult, paste0(sc, ".csv"))
    expect_true(file.exists(resultFile))
  }

  # simulate one additional and load the ones before
  scenarioNames <- names(scenarioListInd)[c(1, 2, 3)]
  result <- runOrLoadScenarios(projectConfigurationTest, scenarioListInd[scenarioNames])

  expect_type(result, "list")
  expect_true(all(scenarioNames %in% names(result)))

  # Verify saved results
  for (sc in scenarioNames) {
    resultFile <- file.path(projectConfigurationTest$outputFolder, EXPORTDIR$simulationResult, paste0(sc, ".csv"))
    expect_true(file.exists(resultFile))
  }

  # Clean up
  rm(projectConfigurationTest)
})

test_that(".fixFilePathsInScenarioConfigurations handles hyphen/dash variants", {
  # Skip if model files don't exist
  skip_if_not(dir.exists(projectConfiguration$modelFolder))

  # Get existing model files
  modelFiles <- list.files(projectConfiguration$modelFolder, pattern = "\\.pkml$")
  skip_if(length(modelFiles) == 0, "No model files found")

  # create a model file with a dash in the name
  modelFileWithDash <- 'file-with-dashes.pkml'
  invisible(file.copy(from = file.path(projectConfiguration$modelFolder,modelFiles[1]),
            to = file.path(projectConfiguration$modelFolder,modelFileWithDash),overwrite = TRUE))

  # Create a test scenario configuration with EN DASH in filename
  testModelFileWithEnDash <- gsub("-", "\u2013", modelFileWithDash) # Replace dash with EN DASH

  # Create mock scenario configuration
  mockScenarioConfig <- list(
    list(
      scenarioName = "test_scenario",
      modelFile = testModelFileWithEnDash
    )
  )

  # Test that the function corrects the file path
  correctedConfigs <- .fixFilePathsInScenarioConfigurations(
    scenarioConfigurations = mockScenarioConfig,
    projectConfiguration = projectConfiguration
  )

  # Check that the file path was corrected
  expect_equal(correctedConfigs[[1]]$modelFile, modelFileWithDash)
  expect_false(correctedConfigs[[1]]$modelFile == testModelFileWithEnDash)
})

test_that(".fixFilePathsInScenarioConfigurations throws error for nonexistent files", {
  # Create a mock scenario configuration with a nonexistent file
  mockScenarioConfig <- list(
    list(
      scenarioName = "test_scenario",
      modelFile = "nonexistent\u2013file.pkml"
    )
  )

  # Test that the function throws an error
  expect_error(
    .fixFilePathsInScenarioConfigurations(
      scenarioConfigurations = mockScenarioConfig,
      projectConfiguration = projectConfiguration
    ),
    regexp = "Model file not found"
  )
})

test_that(".fixFilePathsInScenarioConfigurations throws error for NULL modelFile", {
  # Create a mock scenario configuration with NULL modelFile
  mockScenarioConfig <- list(
    list(
      scenarioName = "test_scenario_null",
      modelFile = NULL
    )
  )

  # Test that the function throws an error
  expect_error(
    .fixFilePathsInScenarioConfigurations(
      scenarioConfigurations = mockScenarioConfig,
      projectConfiguration = projectConfiguration
    ),
    regexp = "Invalid scenario configuration.*modelFile is NULL"
  )
})

test_that(".fixFilePathsInScenarioConfigurations throws error for empty modelFile", {
  # Create a mock scenario configuration with empty modelFile
  mockScenarioConfig <- list(
    list(
      scenarioName = "test_scenario_empty",
      modelFile = ""
    )
  )

  # Test that the function throws an error
  expect_error(
    .fixFilePathsInScenarioConfigurations(
      scenarioConfigurations = mockScenarioConfig,
      projectConfiguration = projectConfiguration
    ),
    regexp = "Invalid scenario configuration.*modelFile is empty"
  )
})


# Tests for .extendPopulationFromXLS_RF
test_that(".extendPopulationFromXLS_RF validates inputs", {
  skip_if(length(scenarioList) == 0, "No scenarios available")

  # Get a population from scenario
  scenarioName <- names(scenarioList)[1]
  population <- scenarioList[[scenarioName]]$population

  # Test with invalid population
  expect_error(
    ospsuite.reportingframework:::.extendPopulationFromXLS_RF(
      population = "not a population",
      XLSpath = "dummy.xlsx"
    )
  )

  # Test with invalid XLSpath
  expect_error(
    ospsuite.reportingframework:::.extendPopulationFromXLS_RF(
      population = population,
      XLSpath = 123
    )
  )
})

test_that(".extendPopulationFromXLS_RF handles file with correct structure", {
  skip_if(length(scenarioList) == 0, "No scenarios available")

  # Create a test XLS file with the expected structure
  tmpdir <- tempdir()
  testFile <- file.path(tmpdir, "test_population_params.xlsx")

  # Create data with expected column names
  test_data <- data.table(
    "Container Path" = c("Organism", "Organism"),
    "Parameter Name" = c("Weight", "Height"),
    "Mean" = c(70, 175),
    "SD" = c(10, 15),
    "Distribution" = c("Normal", "Normal")
  )

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  openxlsx::writeData(wb, "Sheet1", test_data)
  openxlsx::saveWorkbook(wb, testFile, overwrite = TRUE)

  population <- ospsuite::loadPopulation(list.files(projectConfiguration$populationsFolder,full.names = TRUE)[1])

  popBefore <- ospsuite::populationToDataFrame(population)

  # Should not error with correct structure
  result <- ospsuite.reportingframework:::.extendPopulationFromXLS_RF(
    population = population,
    XLSpath = testFile,
    sheet = "Sheet1"
  )

  popAfter <- ospsuite::populationToDataFrame(population)
  expect_true(all(popAfter$`Organism|Height` != popBefore$`Organism|Height`))

  # Clean up
  unlink(testFile)
})

test_that(".extendPopulationFromXLS_RF errors with wrong structure", {
  skip_if(length(scenarioList) == 0, "No scenarios available")

  # Create a test XLS file with wrong column names
  tmpdir <- tempdir()
  testFile <- file.path(tmpdir, "test_wrong_structure.xlsx")

  test_data <- data.table(
    "WrongColumn1" = c(1, 2),
    "WrongColumn2" = c(3, 4)
  )

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  openxlsx::writeData(wb, "Sheet1", test_data)
  openxlsx::saveWorkbook(wb, testFile, overwrite = TRUE)

  population <- ospsuite::loadPopulation(list.files(projectConfiguration$populationsFolder,full.names = TRUE)[1])

  expect_error(
    ospsuite.reportingframework:::.extendPopulationFromXLS_RF(
      population = population,
      XLSpath = testFile,
      sheet = 1
    )
  )

  # Clean up
  unlink(testFile)
})
