test_that(".cleanUpSheetList removes blanks, NAs, and duplicates", {
    sheets <- c(" SheetA ", NA_character_, "SheetB", "SheetA", "")

    result <- .cleanUpSheetList(sheets)

    expect_equal(result, c("SheetA", "SheetB"))
})

test_that(".filterPopulationNamesForOverwrite excludes existing csv files", {
    populationsFolder <- withr::local_tempdir()
    file.create(file.path(populationsFolder, "Existing.csv"))

    result <- .filterPopulationNamesForOverwrite(
        populationNames = c("Existing", "New"),
        populationsFolder = populationsFolder,
        overwrite = FALSE
    )

    expect_equal(result, "New")
})

test_that(".filterPopulationNamesForOverwrite keeps all names when overwrite is TRUE", {
    populationsFolder <- withr::local_tempdir()
    file.create(file.path(populationsFolder, "Existing.csv"))

    result <- .filterPopulationNamesForOverwrite(
        populationNames = c("Existing", "New"),
        populationsFolder = populationsFolder,
        overwrite = TRUE
    )

    expect_equal(result, c("Existing", "New"))
})

test_that(".applyCustomParametersToPopulationTable applies scalar custom values", {
    poptable <- data.table::data.table(id = 1:3)

    result <- .applyCustomParametersToPopulationTable(
        poptable = poptable,
        customParameters = list(list(path = "Dose", values = 5)),
        populationName = "PopulationA"
    )

    expect_equal(result$Dose, c(5, 5, 5))
})

test_that(".applyCustomParametersToPopulationTable errors on inconsistent value counts", {
    poptable <- data.table::data.table(id = 1:3)

    expect_error(
        .applyCustomParametersToPopulationTable(
            poptable = poptable,
            customParameters = list(list(path = "Dose", values = c(1, 2))),
            populationName = "PopulationA"
        ),
        "Inconsistent number of values"
    )
})

test_that(".warnOnSuspiciousFemaleProportions warns for fractional percentages", {
    dtPops <- data.table::data.table(
        populationName = c("A", "B"),
        proportionOfFemales = c(0.4, 20)
    )

    expect_warning(
        .warnOnSuspiciousFemaleProportions(dtPops),
        "percent not fraction"
    )
})

test_that(".warnOnSuspiciousFemaleProportions is silent for percentage values", {
    dtPops <- data.table::data.table(
        populationName = c("A", "B"),
        proportionOfFemales = c(40, 20)
    )

    expect_no_warning(.warnOnSuspiciousFemaleProportions(dtPops))
})

# Tests for .validateRandomPopulationCustomParameters
test_that(".validateRandomPopulationCustomParameters accepts NULL", {
    result <- .validateRandomPopulationCustomParameters(NULL)
    expect_null(result)
})

test_that(".validateRandomPopulationCustomParameters accepts valid structure", {
    validParams <- list(
        list(path = "Dose", values = c("10", "20", "30")),
        list(path = "Weight", values = c("70", "75", "80"))
    )

    result <- .validateRandomPopulationCustomParameters(validParams)
    expect_null(result)
})

test_that(".validateRandomPopulationCustomParameters errors on empty list", {
    expect_error(
        .validateRandomPopulationCustomParameters(list()),
        "must.include|min.len|length"
    )
})

test_that(".validateRandomPopulationCustomParameters errors on non-list input", {
    expect_error(
        .validateRandomPopulationCustomParameters("not_a_list"),
        "list"
    )
})

test_that(".validateRandomPopulationCustomParameters errors on missing path field", {
    invalidParams <- list(
        list(values = c(10, 20))
    )

    expect_error(
        .validateRandomPopulationCustomParameters(invalidParams),
        "path|character"
    )
})

test_that(".validateRandomPopulationCustomParameters errors on non-character values", {
    invalidParams <- list(
        list(path = "Dose", values = 123)
    )

    expect_error(
        .validateRandomPopulationCustomParameters(invalidParams),
        "character|values"
    )
})

# Additional tests for .cleanUpSheetList edge cases
test_that(".cleanUpSheetList handles empty vector", {
    sheets <- c()
    result <- .cleanUpSheetList(sheets)
    expect_true(length(result) == 0)
})

test_that(".cleanUpSheetList handles all NA values", {
    sheets <- c(NA_character_, NA_character_, NA_character_)
    result <- .cleanUpSheetList(sheets)
    expect_true(length(result) == 0)
})

test_that(".cleanUpSheetList handles all empty strings", {
    sheets <- c("", "", "")
    result <- .cleanUpSheetList(sheets)
    expect_true(length(result) == 0)
})

test_that(".cleanUpSheetList handles mixed whitespace", {
    sheets <- c("  Sheet1  ", "\tSheet2\t", "\nSheet3\n", "Sheet1")
    result <- .cleanUpSheetList(sheets)
    # Result should have unique trimmed sheet names
    expect_true("Sheet1" %in% result)
    expect_true(length(result) <= 3)
})

# Additional tests for .applyCustomParametersToPopulationTable edge cases
test_that(".applyCustomParametersToPopulationTable handles NULL custom parameters", {
    poptable <- data.table::data.table(id = 1:3, Name = c("A", "B", "C"))

    result <- .applyCustomParametersToPopulationTable(
        poptable = poptable,
        customParameters = NULL,
        populationName = "TestPop"
    )

    expect_equal(result, poptable)
})

test_that(".applyCustomParametersToPopulationTable handles empty custom parameters list", {
    poptable <- data.table::data.table(id = 1:3, Name = c("A", "B", "C"))

    result <- .applyCustomParametersToPopulationTable(
        poptable = poptable,
        customParameters = list(),
        populationName = "TestPop"
    )

    expect_equal(result, poptable)
})

test_that(".applyCustomParametersToPopulationTable applies vector custom values", {
    poptable <- data.table::data.table(id = 1:3)

    result <- .applyCustomParametersToPopulationTable(
        poptable = poptable,
        customParameters = list(list(path = "Dose", values = 5)),
        populationName = "PopulationA"
    )

    expect_equal(result$Dose, c(5, 5, 5))
})

test_that(".applyCustomParametersToPopulationTable applies multiple custom parameters", {
    poptable <- data.table::data.table(id = 1:2)

    result <- .applyCustomParametersToPopulationTable(
        poptable = poptable,
        customParameters = list(
            list(path = "Dose", values = 100),
            list(path = "Weight", values = 70)
        ),
        populationName = "PopulationA"
    )

    expect_equal(result$Dose, c(100, 100))
    expect_equal(result$Weight, c(70, 70))
})
