.makeUtilitiesDataProjectConfigurationAI <- function() {
    .makeTutorialDataProjectConfiguration(
        projectDirPrefix = "rf_util_ai_",
        includeScenariosFile = TRUE
    )
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
    projectConfiguration$addOns$dataImportMapFile <- invalidDataImportFile

    validation <- validateWorkflowForAI(
        projectConfiguration = projectConfiguration
    )

    expect_false(validation$ok)
    expect_true(any(grepl("Unknown DataClass", validation$errors)))
})
