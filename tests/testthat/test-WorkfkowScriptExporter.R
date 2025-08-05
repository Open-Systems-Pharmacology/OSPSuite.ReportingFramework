# testProject was set up by setup.R
# that provides projectConfiguration,scenarioList

# Unit tests
test_that("WorkflowScriptExporter initializes correctly", {
  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    scenarioNames = names(scenarioList)[seq(1, 4)],
    workflowRmd = NULL
  )

  expect_equal(exporter$wfIdentifier, 1)
  expect_equal(exporter$scenarioNames, names(scenarioList)[seq(1, 4)])
  expect_null(exporter$workflowRmd)
  expect_null(exporter$fileNameReplacements)
  expect_true(dir.exists(projectConfiguration$addOns$electronicPackageFolder))

  cleanupElectronicPackage(projectConfiguration)
})

test_that("WorkflowScriptExporter initialization fails with both scenarioNames and workflowRmd", {
  expect_error(
    WorkflowScriptExporter$new(
      projectConfiguration = projectConfiguration,
      wfIdentifier = 1,
      scenarioNames = names(scenarioList)[seq(1, 4)],
      workflowRmd = "some_file.Rmd"
    )
  )

  cleanupElectronicPackage(projectConfiguration)
})

test_that("WorkflowScriptExporter initialization fails with neither scenarioNames nor workflowRmd", {
  expect_error(
    WorkflowScriptExporter$new(
      projectConfiguration = projectConfiguration,
      wfIdentifier = 1,
      scenarioNames = NULL,
      workflowRmd = NULL
    )
  )

  cleanupElectronicPackage(projectConfiguration)
})

test_that("exportWorkflowText creates the correct output file", {
  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    scenarioNames = names(scenarioList)[seq(1, 4)],
    workflowRmd = NULL
  )

  exporter$exportWorkflowText(projectConfiguration)

  outputFile <- file.path(projectConfiguration$addOns$electronicPackageFolder, "w1_workflow_r.txt")
  expect_true(file.exists(outputFile))

  outputContent <- readLines(outputFile)

  # Check placeholder are replaced in the output
  expect_false(any(grepl("XXwfIdentifierXX", outputContent)))
  expect_false(any(grepl("XXscenarioNamesXX", outputContent)))
  expect_false(any(grepl("XXprojectDirectoryXX", outputContent)))

  cleanupElectronicPackage(projectConfiguration)
})

test_that("getEPackageInputfiles populates inputFiles correctly", {
  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    scenarioNames = names(scenarioList)[seq(1, 4)],
    workflowRmd = NULL
  )

  expect_warning(exporter$addEPackageInputfilesForScenarioNames(projectConfiguration))

  expect_equal(nrow(exporter$inputFiles), 5) # Expecting 5 input files (4 population, 1 model)
  expect_equal(unique(exporter$inputFiles$fileType), c("population", "model"))

  cleanupElectronicPackage(projectConfiguration)
})

test_that("exportInputFiles fails with duplicate or non existing file names", {
  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    scenarioNames = names(scenarioList)[seq(1, 4)],
    workflowRmd = NULL
  )

  # Export duplicate files names
  exporter$inputFiles <- data.table(source = c("file1.csv", "file1.csv"), fileName = c("file1.csv", "file1.csv"), fileType = c("population2", "population"))

  expect_error(
    exporter$exportInputFiles()
  )

  # Export non existing files names
  exporter$inputFiles <- data.table(source = c("file1.csv", "file2.csv"), fileName = c("file1.csv", "file1.csv"), fileType = c("population", "population"))

  expect_error(
    exporter$exportInputFiles()
  )

  cleanupElectronicPackage(projectConfiguration)
})

test_that("fileNameReplacements are applied correctly", {
  # Define file name replacements
  fileNameReplacements <- c("adults.csv", "population1.csv", "iv 1 mg (5 min).pkml", "model1_updated.pkml")

  # Initialize the WorkflowScriptExporter with fileNameReplacements
  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    scenarioNames = names(scenarioList)[seq(1)],
    workflowRmd = NULL,
    fileNameReplacements = fileNameReplacements
  )

  # Add input files for the scenario names
  exporter$addEPackageInputfilesForScenarioNames(projectConfiguration)

  # Check if the file names have been replaced correctly
  expectedInputFiles <- data.table(
    source = c(
      file.path(projectConfiguration$populationsFolder, "adults.csv"),
      file.path(projectConfiguration$modelFolder, "iv 1 mg (5 min).pkml")
    ),
    fileName = c("population1.csv", "model1_updated.xml"),
    fileType = c("population", "model")
  )

  expect_equal(exporter$inputFiles$fileName, expectedInputFiles$fileName)
  expect_equal(exporter$inputFiles$fileType, expectedInputFiles$fileType)

  cleanupElectronicPackage(projectConfiguration)
})
test_that("fileNameReplacements are invalid", {
  # Define file name replacements
  fileNameReplacements <- c("adults.csv", "1startWithnumeric.csv")

  # Initialize the WorkflowScriptExporter with fileNameReplacements
  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    scenarioNames = names(scenarioList)[seq(1)],
    workflowRmd = NULL,
    fileNameReplacements = fileNameReplacements
  )

  # Add input files for the scenario names
  expect_error(exporter$addEPackageInputfilesForScenarioNames(projectConfiguration))

  fileNameReplacements <- c("iv 1 mg (5 min).pkml", "veeeeeeeeeerrrrrrrrrrrrrrrryyyyyyyyyyyyyyyyyyyyyyyloooooooooooooooooooooooongmodel.pkml")

  # Initialize the WorkflowScriptExporter with fileNameReplacements
  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    scenarioNames = names(scenarioList)[seq(1)],
    workflowRmd = NULL,
    fileNameReplacements = fileNameReplacements
  )

  # Add input files for the scenario names
  expect_error(exporter$addEPackageInputfilesForScenarioNames(projectConfiguration))

  cleanupElectronicPackage(projectConfiguration)
})

test_that("addEPackageConfigurationForScenarioNames populates configurationSheets correctly", {
  # Initialize the WorkflowScriptExporter
  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    scenarioNames = c(
      names(scenarioListInd)[c(1, 2)],
      names(scenarioList)[1]
    ),
    workflowRmd = NULL,
    fileNameReplacements = c("1234_adults.csv", "study1234_adults.csv")
  )

  # Call the function to populate configuration sheets
  exporter$addEPackageConfigurationForScenarioNames(projectConfiguration)

  # Check that configuration sheets are populated
  expect_contains(
    names(exporter$configurationSheets),
    c("ProjectConfiguration", "Scenarios", "Individuals", "ModelParameters", "Applications")
  )

  expect_contains(
    names(exporter$configurationSheets$Scenarios),
    c("Scenarios", "OutputPaths", "PKParameter")
  )

  expect_length(exporter$configurationSheets$Scenarios$Scenarios$rows, 3)
  expect_length(exporter$configurationSheets$Scenarios$OutputPaths$rows, 3)

  expect_contains(
    names(exporter$configurationSheets$Individuals),
    c("IndividualBiometrics")
  )

  expect_length(exporter$configurationSheets$Individuals$IndividualBiometrics$rows, 1)

  cleanupElectronicPackage(projectConfiguration)
})

test_that("exportConfigSheets creates the correct output file", {
  # Initialize the WorkflowScriptExporter
  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    scenarioNames = names(scenarioListInd)[seq(1, 4)],
    workflowRmd = NULL
  )

  # Populate configuration sheets first
  exporter$addEPackageConfigurationForScenarioNames(projectConfiguration)

  # Call the exportConfigSheets method
  exporter$exportConfigSheets()

  # Define the expected output file path
  outputFile <- file.path(projectConfiguration$addOns$electronicPackageFolder, "w1_config_json.txt")

  # Check if the output file exists
  expect_true(file.exists(outputFile))

  # Read the content of the output file
  outputContent <- readLines(outputFile)

  # Parse the JSON content
  jsonData <- jsonlite::fromJSON(paste(outputContent, collapse = "\n"))

  # Check that the JSON structure matches expected structure
  expect_true("Scenarios" %in% names(jsonData))
  expect_true("Individuals" %in% names(jsonData))

  # Additional checks can be made based on the expected content of the configuration sheets
  expect_true(length(jsonData$Scenarios) > 0) # Ensure there are scenarios
  expect_true(length(jsonData$Individuals) > 0) # Ensure there are individuals if applicable

  cleanupElectronicPackage(projectConfiguration)
})

test_that("Full workflow test for WorkflowScriptExporter simulation export", {
  expect_warning(exportSimulationWorkflowToEPackage(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    scenarioNames = names(scenarioList)[seq(1, 4)]
  ))

  # Check if the workflow text file is created
  outputWorkflowFile <- file.path(projectConfiguration$addOns$electronicPackageFolder, "w1_workflow_r.txt")
  expect_true(file.exists(outputWorkflowFile))

  # Check the content of the workflow text file
  outputWorkflowContent <- readLines(outputWorkflowFile)
  expect_false(any(grepl("XXwfIdentifierXX", outputWorkflowContent))) # Placeholder should be replaced
  expect_false(any(grepl("XXscenarioNamesXX", outputWorkflowContent))) # Placeholder should be replaced

  # Check if input files are exported correctly
  expect_true(file.exists(file.path(projectConfiguration$addOns$electronicPackageFolder, "w1_input_files.csv")))
  inputFiles <- data.table::fread(file = file.path(
    projectConfiguration$addOns$electronicPackageFolder,
    "w1_input_files.csv"
  ))
  expect_equal(nrow(inputFiles), expected = 5)
  for (f in inputFiles$fileName) {
    expect_true(file.exists(file.path(projectConfiguration$addOns$electronicPackageFolder, f)))
  }

  # Check if the configuration sheets file is created
  outputconfigFile <- file.path(projectConfiguration$addOns$electronicPackageFolder, "w1_config_json.txt")
  expect_true(file.exists(outputconfigFile))

  cleanupElectronicPackage(projectConfiguration)
})
test_that("extracts default code chunks correctly", {
  # Step 1: Initialize the WorkflowScriptExporter with a valid workflowRmd
  workflowRmdPath <- system.file("templates", "template_ePackageWorkflow.Rmd",
    package = "ospsuite.reportingframework"
  )

  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    workflowRmd = workflowRmdPath
  )

  # Step 2: Call the extractCodeChunks method
  exporter$extractCodeChunks()

  # Step 3: Check that codeChunks is populated
  expect_true(length(exporter$codeChunks) > 0)

  # Step 4: Check that specific required code chunks are present
  expect_true(any(grepl("copy$", names(exporter$codeChunks))))

  cleanupElectronicPackage(projectConfiguration)
})

test_that("evaluates default chuncs correctly", {
  workflowRmdPath <- file.path(
    projectConfiguration$configurationsFolder,
    "myWorkflow.Rmd"
  )

  generateMockRmd(projectConfiguration,
    codeChunkList = list(),
    filename = workflowRmdPath
  )

  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    workflowRmd = workflowRmdPath
  )

  exporter$extractCodeChunks()

  exporter$evalCodeChunks()

  # should delete chunk `customfunctions-replace`
  expect_length(names(exporter$codeChunks), n = 3)

  cleanupElectronicPackage(projectConfiguration)
})

test_that("evaluates chunk for custom functions correctly", {
  workflowRmdPath <- file.path(
    projectConfiguration$configurationsFolder,
    "myWorkflow.Rmd"
  )

  generateMockRmd(projectConfiguration,
    codeChunkList = list(
      "pathsCustomfunctions-eval" = c(
        "# Create two test custom function files",
        "createTestCustomFunction <- function(fileName, content) {",
        "  writeLines(content, fileName)",
        "}",
        "",
        "# Specify the file names",
        "file1 <- file.path(projectConfiguration$configurationsFolder,'custom_function_1.R')",
        "file2 <- file.path(projectConfiguration$configurationsFolder,'custom_function_2.R')",
        "",
        "# Create the custom function files in the current directory",
        "createTestCustomFunction(fileName = file1,",
        "                          content = '#Content1')",
        "createTestCustomFunction(fileName = file2,",
        "                         content = c('# function with two lines',",
        "                          '#Line2'))",
        "",
        "# Add the file names to pathsCustomfunctions",
        "pathsCustomfunctions <- c(file1, file2)"
      )
    ),
    filename = workflowRmdPath
  )

  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    workflowRmd = workflowRmdPath
  )

  exporter$extractCodeChunks()

  projectConfiguration <- projectConfiguration$clone()

  exporter$evalCodeChunks()

  # should have two lines which sources the custom functions
  expect_length(grep("custom_function", exporter$codeChunks$`pathsCustomfunctions-copy`), n = 2)

  # export input files
  exporter$exportInputFiles()

  expect_true(file.exists(file.path(
    projectConfiguration$addOns$electronicPackageFolder,
    "custom_function_1.txt"
  )))

  cleanupElectronicPackage(projectConfiguration)
  rm(projectConfiguration)
})

test_that("throw error for invalid custom functions", {
  # create mock rmd
  workflowRmdPath <- file.path(
    projectConfiguration$configurationsFolder,
    "myWorkflow.Rmd"
  )

  # Case 1: Prepare workflowRmd no pathsCustomfunctions
  generateMockRmd(projectConfiguration,
    codeChunkList = list(
      "pathsCustomfunctions-eval" = c(
        "# Returns no pathsCustomfunctions"
      )
    ),
    filename = workflowRmdPath
  )

  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    workflowRmd = workflowRmdPath
  )

  exporter$extractCodeChunks()

  expect_error(exporter$evalCodeChunks())

  # Case 2: Prepare workflowRmd with invalid pathsCustomfunctions
  generateMockRmd(projectConfiguration,
    codeChunkList = list(
      "pathsCustomfunctions-eval" = c(
        "pathsCustomfunctions = c(1,2,3)"
      )
    ),
    filename = workflowRmdPath
  )

  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    workflowRmd = workflowRmdPath
  )
  exporter$extractCodeChunks()

  expect_error(exporter$evalCodeChunks())

  # Case 3: Prepare workflowRmd with  pathsCustomfunctions containing non existing files
  generateMockRmd(projectConfiguration,
    codeChunkList = list(
      "pathsCustomfunctions-eval" = c(
        "pathsCustomfunctions = c('file_which_does_not_exist.R')"
      )
    ),
    filename = workflowRmdPath
  )

  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    workflowRmd = workflowRmdPath
  )
  exporter$extractCodeChunks()

  expect_error(exporter$evalCodeChunks())

  cleanupElectronicPackage(projectConfiguration)
})

test_that("evaluates chunk for scenarioNames chunk correctly", {
  workflowRmdPath <- file.path(
    projectConfiguration$configurationsFolder,
    "myWorkflow.Rmd"
  )

  generateMockRmd(projectConfiguration,
    codeChunkList = list(
      "scenarioNames-eval" = c(
        paste0(
          "scenarioNames = c('",
          paste(names(scenarioList)[seq(2)], collapse = "', '"),
          "')"
        )
      )
    ),
    filename = workflowRmdPath
  )

  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    workflowRmd = workflowRmdPath
  )

  exporter$extractCodeChunks()

  exporter$evalCodeChunks()

  expect_true(all(exporter$scenarioNames %in% names(scenarioList)))

  expect_true(all(lapply(
    exporter$scenarioNames,
    function(x) {
      length(grep(x, exporter$codeChunks[["scenarioNames-copy"]])) > 0
    }
  ) %>%
    unlist()))

  cleanupElectronicPackage(projectConfiguration)
})

test_that("throw error for invalid or missing scenarioNames chunk", {
  workflowRmdPath <- file.path(
    projectConfiguration$configurationsFolder,
    "myWorkflow.Rmd"
  )

  generateMockRmd(projectConfiguration,
    codeChunkList = list(
      "scenarioNames-eval" =
        "scenarioNames = c(1,2,3)"
    ),
    filename = workflowRmdPath
  )

  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    workflowRmd = workflowRmdPath
  )

  exporter$extractCodeChunks()

  expect_error(exporter$evalCodeChunks())

  generateMockRmd(projectConfiguration,
    codeChunkList = list(
      "scenarioNames-eval" =
        "#Does not evaluate to scenarioNames"
    ),
    filename = workflowRmdPath
  )

  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    workflowRmd = workflowRmdPath
  )

  exporter$extractCodeChunks()

  expect_error(exporter$evalCodeChunks())


  cleanupElectronicPackage(projectConfiguration)
})

test_that("evaluates chunk for dataImport chunk correctly", {
  workflowRmdPath <- file.path(
    projectConfiguration$configurationsFolder,
    "myWorkflow.Rmd"
  )

  generateMockRmd(projectConfiguration,
    codeChunkList = list(
      "dataObserved-eval" = c(
        "dataObserved <- readObservedDataByDictionary(projectConfiguration = projectConfiguration,",
        "                                            dataClassType = 'timeprofile',",
        "                                            fileIds = NULL)",
        "",
        "dataObserved <- rbind(dataObserved,",
        "                      aggregateObservedDataGroups(dataObserved),fill = TRUE)"
      ),
      "dataObservedPK-eval" = c(
        "dataObservedPK <- readObservedDataByDictionary(projectConfiguration = projectConfiguration,",
        "                                               dataClassType = 'pkParameter',",
        "                                               fileIds = NULL)"
      )
    ),
    filename = workflowRmdPath
  )

  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    workflowRmd = workflowRmdPath
  )

  exporter$extractCodeChunks()

  suppressWarnings(exporter$evalCodeChunks())

  expect_contains(
    names(exporter$configurationSheets$DataImportConfiguration),
    c("DataFiles", "tpIndividualDictionary", "tpAggregatedDictionary", "pkAggregatedDictionary")
  )
  expect_length(exporter$configurationSheets$DataImportConfiguration$DataFiles$rows, n = 4)

  expect_length(list.files(file.path(projectConfiguration$addOns$electronicPackageFolder),
    pattern = "data*"
  ), n = 3)

  expect_contains(names(exporter$codeChunks), "dataObserved-copy")
  expect_contains(names(exporter$codeChunks), "dataObservedPK-copy")

  cleanupElectronicPackage(projectConfiguration)
})

test_that("throws error for invalid dataImport", {
  workflowRmdPath <- file.path(
    projectConfiguration$configurationsFolder,
    "myWorkflow.Rmd"
  )

  generateMockRmd(projectConfiguration,
    codeChunkList = list(
      "dataObserved-eval" = "#does not evaluate to dataobserved"
    ),
    filename = workflowRmdPath
  )

  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    workflowRmd = workflowRmdPath
  )

  exporter$extractCodeChunks()

  expect_error(exporter$evalCodeChunks())

  generateMockRmd(projectConfiguration,
    codeChunkList = list(
      "dataObserved-eval" = "dataObserved <- c(1,2,3)"
    ),
    filename = workflowRmdPath
  )

  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    workflowRmd = workflowRmdPath
  )

  exporter$extractCodeChunks()

  expect_error(exporter$evalCodeChunks())


  cleanupElectronicPackage(projectConfiguration)
})

test_that("extract sheetNames for plotting correctly", {
  workflowRmdPath <- file.path(
    projectConfiguration$configurationsFolder,
    "myWorkflow.Rmd"
  )

  generateMockRmd(projectConfiguration,
    codeChunkList = list(
      "runPlot-copy" = c(
        "runPlot(",
        'nameOfplotFunction = "plotTimeProfiles",',
        'configTableSheet = "TimeProfiles",',
        'configTableSheet="PKParameter_Boxplot",',
        "configTableSheet='PKParameter_Forest',",
        'someTextBeforeconfigTableSheet = "Histograms"someTextBehind,',
        'configTableSheet = "DistributionVsRange"configTableSheet = "SensitivityPlots",',
        "projectConfiguration = projectConfiguration,",
        'plotNames = c("Individuals_withData", "Individuals_withoutData"),',
        "inputs = list(",
        "scenarioResults = scenarioResultsInd,",
        "dataObserved = dataObserved",
        ")"
      )
    ),
    filename = workflowRmdPath
  )

  exporter <- WorkflowScriptExporter$new(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    workflowRmd = workflowRmdPath
  )

  exporter$extractCodeChunks()

  exporter$addEPackageConfigurationForPlotting(projectConfiguration = projectConfiguration)

  expect_contains(names(exporter$configurationSheets), "Plots")
  expect_equal(
    names(exporter$configurationSheets$Plots),
    c(
      "TimeProfiles", "PKParameter_Boxplot", "PKParameter_Forest", "Histograms",
      "DistributionVsRange", "SensitivityPlots", "TimeRange"
    )
  )

  cleanupElectronicPackage(projectConfiguration)
})

test_that("Full workflow test for WorkflowScriptExporter TLF export", {
  workflowRmdPath <- file.path(
    projectConfiguration$configurationsFolder,
    "myWorkflow.Rmd"
  )

  generateMockRmd(projectConfiguration,
    codeChunkList = list(
      "pathsCustomfunctions-eval" = c(
        "# Create two test custom function files",
        "createTestCustomFunction <- function(fileName, content) {",
        "  writeLines(content, fileName)",
        "}",
        "",
        "# Specify the file names",
        "file1 <- file.path(projectConfiguration$configurationsFolder,'custom_function_1.R')",
        "",
        "# Create the custom function files in the current directory",
        "createTestCustomFunction(fileName = file1,",
        "                          content = '#Content1')",
        "",
        "# Add the file names to pathsCustomfunctions",
        "pathsCustomfunctions <- c(file1)"
      ),
      "scenarioNames-eval" = c(
        paste0(
          "scenarioNames = c('",
          paste(names(scenarioList)[seq(2)], collapse = "', '"),
          "')"
        )
      ),
      "dataObserved-eval" = c(
        "dataObserved <- readObservedDataByDictionary(projectConfiguration = projectConfiguration,",
        "                                            dataClassType = 'timeprofile',",
        "                                            fileIds = NULL)",
        "",
        "dataObserved <- rbind(dataObserved,",
        "                      aggregateObservedDataGroups(dataObserved),fill = TRUE)"
      ),
      "dataObservedPK-eval" = c(
        "dataObservedPK <- readObservedDataByDictionary(projectConfiguration = projectConfiguration,",
        "                                               dataClassType = 'pkParameter',",
        "                                               fileIds = NULL)"
      )
    ),
    filename = workflowRmdPath
  )

  suppressWarnings(exportTLFWorkflowToEPackage(
    projectConfiguration = projectConfiguration,
    wfIdentifier = 1,
    workflowRmd = workflowRmdPath
  ))

  # Check if the workflow text file is created
  outputWorkflowFile <- file.path(projectConfiguration$addOns$electronicPackageFolder, "w1_workflow_r.txt")
  expect_true(file.exists(outputWorkflowFile))

  # Check the content of the workflow text file
  outputWorkflowContent <- readLines(outputWorkflowFile)
  expect_false(any(grepl("XXwfIdentifierXX", outputWorkflowContent))) # Placeholder should be replaced
  expect_false(any(grepl("XXscenarioNamesXX", outputWorkflowContent))) # Placeholder should be replaced
  expect_false(any(grepl("^XXX", outputWorkflowContent))) # Placeholder should be replaced

  # Check if input files are exported correctly
  expect_true(file.exists(file.path(projectConfiguration$addOns$electronicPackageFolder, "w1_input_files.csv")))
  inputFiles <- data.table::fread(file = file.path(
    projectConfiguration$addOns$electronicPackageFolder,
    "w1_input_files.csv"
  ))
  expect_equal(nrow(inputFiles), expected = 7)
  for (f in inputFiles$fileName) {
    expect_true(file.exists(file.path(projectConfiguration$addOns$electronicPackageFolder, f)))
  }

  # Check if the configuration sheets file is created
  outputconfigFile <- file.path(projectConfiguration$addOns$electronicPackageFolder, "w1_config_json.txt")
  expect_true(file.exists(outputconfigFile))

  cleanupElectronicPackage(projectConfiguration)
})

test_that("Import workflow generated by WorkflowScriptExporter Simulation export in new directory", {
  wfIdentifier <- 3

  expect_warning(exportSimulationWorkflowToEPackage(
    projectConfiguration = projectConfiguration,
    wfIdentifier = wfIdentifier,
    scenarioNames = names(scenarioListInd)[seq(1, 2)],
    fileNameReplacements = c("1234_adults.csv", "study1234_adults.csv")
  ))

  projectDirectory <-
    fs::path_abs(
      file.path(
        fs::path_common(path = c(
          projectConfiguration$configurationsFolder,
          projectConfiguration$outputFolder
        )),
        "..", "testproject2"
      )
    )

  if (!dir.exists(projectDirectory)) dir.create(projectDirectory)

  projectConfigurationNew <-
    suppressMessages(importWorkflow(
      projectDirectory = projectDirectory,
      wfIdentifier = wfIdentifier,
      ePackageFolder = projectConfiguration$addOns$electronicPackageFolder
    ))

  expect_true(dir.exists(projectConfigurationNew$configurationsFolder))
  expect_true(dir.exists(projectConfigurationNew$modelFolder))

  cleanupElectronicPackage(projectConfiguration)
})

