.makeUtilitiesDataProjectConfigurationAI <- function() {
    projectDir <- tempfile(pattern = "rf_util_ai_")
    dir.create(projectDir, recursive = TRUE, showWarnings = FALSE)
    tutorialDir <- system.file(
        "extdata",
        "Tutorial",
        package = "ospsuite.reportingframework"
    )

    filesToCopy <- c(
        "DataImportConfiguration.xlsx",
        "Individuals.xlsx",
        "Reports.xlsx",
        "Scenarios.xlsx"
    )

    file.copy(
        from = file.path(tutorialDir, filesToCopy),
        to = file.path(projectDir, filesToCopy),
        overwrite = TRUE
    )

    dataDir <- file.path(projectDir, "Data")
    dir.create(dataDir, recursive = TRUE, showWarnings = FALSE)
    dataFiles <- c(
        "timeprofiles_study1234_iv.csv",
        "timeprofiles_study1234_po.csv"
    )
    file.copy(
        from = file.path(tutorialDir, dataFiles),
        to = file.path(dataDir, dataFiles),
        overwrite = TRUE
    )

    for (rootDir in unique(c(
        tempdir(),
        dirname(tempdir()),
        dirname(dirname(tempdir())),
        projectDir
    ))) {
        tempDataDir <- file.path(rootDir, "Data")
        dir.create(tempDataDir, recursive = TRUE, showWarnings = FALSE)
        srcFiles <- file.path(dataDir, dataFiles)
        dstFiles <- file.path(tempDataDir, dataFiles)
        if (
            !all(
                normalizePath(srcFiles, winslash = "/") ==
                    normalizePath(dstFiles, winslash = "/", mustWork = FALSE)
            )
        ) {
            file.copy(
                from = srcFiles,
                to = dstFiles,
                overwrite = TRUE
            )
        }
    }

    return(list(
        dataImporterConfigurationFile = file.path(
            projectDir,
            "DataImportConfiguration.xlsx"
        ),
        projectConfigurationDirPath = tempdir(),
        scenariosFile = file.path(projectDir, "Scenarios.xlsx"),
        addOns = list(reportsFile = file.path(projectDir, "Reports.xlsx"))
    ))
}

test_that("exportWorkflowContext returns expected sections and writes JSON", {
    projectConfiguration <- .makeUtilitiesDataProjectConfigurationAI()
    jsonFile <- tempfile(fileext = ".json")

    context <- exportWorkflowContext(
        projectConfiguration = projectConfiguration,
        file = jsonFile,
        includeNumericValues = FALSE
    )

    expect_type(context, "list")
    expect_true(file.exists(jsonFile))
    expect_true("configurationFiles" %in% names(context))
    expect_true("dataClassValues" %in% names(context))
    expect_true("scenarioNames" %in% names(context))
    expect_true("outputPathIds" %in% names(context))
})

test_that("createAIWorkflowPrompt returns project-specific prompt", {
    projectConfiguration <- .makeUtilitiesDataProjectConfigurationAI()

    prompt <- createAIWorkflowPrompt(
        projectConfiguration = projectConfiguration,
        task = "create-workflow",
        includeExamples = TRUE,
        maxItemsPerSection = 10
    )

    expect_type(prompt, "character")
    expect_true(length(prompt) == 1)
    expect_true(grepl("Task: create-workflow", prompt, fixed = TRUE))
    expect_true(grepl("Available scenarios:", prompt, fixed = TRUE))
    expect_true(grepl("Allowed DataClass values:", prompt, fixed = TRUE))
})

test_that("createAIPromptFromTask returns task-specific guidance", {
    projectConfiguration <- .makeUtilitiesDataProjectConfigurationAI()

    prompt <- createAIPromptFromTask(
        projectConfiguration = projectConfiguration,
        task = "import-data"
    )

    expect_type(prompt, "character")
    expect_true(length(prompt) == 1)
    expect_true(grepl("Task: import-data", prompt, fixed = TRUE))
    expect_true(grepl("Task-specific guidance:", prompt, fixed = TRUE))
    expect_true(grepl("data import setup", prompt, fixed = TRUE))
})

test_that("createAIPromptFromTask rejects invalid task", {
    projectConfiguration <- .makeUtilitiesDataProjectConfigurationAI()

    expect_error(
        createAIPromptFromTask(
            projectConfiguration = projectConfiguration,
            task = "invalid-task"
        )
    )
})

test_that("createAIPromptFromTask can omit examples", {
    projectConfiguration <- .makeUtilitiesDataProjectConfigurationAI()

    prompt <- createAIPromptFromTask(
        projectConfiguration = projectConfiguration,
        task = "build-plots",
        includeExamples = FALSE
    )

    expect_true(grepl("Task: build-plots", prompt, fixed = TRUE))
    expect_true(grepl("Task-specific guidance:", prompt, fixed = TRUE))
    expect_false(grepl("Example commands:", prompt, fixed = TRUE))
})

test_that("validateWorkflowForAI reports ok for valid fixture", {
    projectConfiguration <- .makeUtilitiesDataProjectConfigurationAI()

    validation <- validateWorkflowForAI(
        projectConfiguration = projectConfiguration
    )

    expect_type(validation, "list")
    expect_true("ok" %in% names(validation))
    expect_true("errors" %in% names(validation))
    expect_true(length(validation$errors) == 0)
})

test_that("validateWorkflowForAI reports unknown DataClass values", {
    projectConfiguration <- .makeUtilitiesDataProjectConfigurationAI()

    invalidDataImportFile <- tempfile(fileext = ".xlsx")
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "DataFiles")
    openxlsx::writeData(
        wb = wb,
        sheet = "DataFiles",
        x = data.table::data.table(
            FileIdentifier = c("description", "invalid-1"),
            DataFile = c("description", "file.csv"),
            Dictionary = c("description", "tpDictionary"),
            DataFilter = c("description", ""),
            DataClass = c("description", "invalid data class")
        )
    )
    openxlsx::saveWorkbook(wb, invalidDataImportFile, overwrite = TRUE)
    projectConfiguration$dataImporterConfigurationFile <- invalidDataImportFile

    validation <- validateWorkflowForAI(
        projectConfiguration = projectConfiguration
    )

    expect_false(validation$ok)
    expect_true(any(grepl("Unknown DataClass", validation$errors)))
})
