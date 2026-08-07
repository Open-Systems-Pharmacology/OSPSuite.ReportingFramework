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
