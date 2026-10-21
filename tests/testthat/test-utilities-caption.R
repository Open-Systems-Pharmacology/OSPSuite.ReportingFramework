# unit tests for caption helper utilities in utilities-caption.R -----------

test_that("captionPrefixSimulated returns expected prefix", {
    expect_equal(
        ospsuite.reportingframework:::captionPrefixSimulated(),
        "Simulated"
    )
})

test_that("captionScaleText renders linear and logarithmic scales", {
    expect_equal(
        ospsuite.reportingframework:::captionScaleText("linear", "x"),
        "on a linear x-scale"
    )
    expect_equal(
        ospsuite.reportingframework:::captionScaleText("log", "y"),
        "on a logarithmic y-scale"
    )
})

test_that("addCaptionTextAddon appends periods and addon text", {
    expect_equal(
        ospsuite.reportingframework:::addCaptionTextAddon(
            "Base text",
            NA_character_
        ),
        "Base text."
    )
    expect_equal(
        ospsuite.reportingframework:::addCaptionTextAddon("Base text", "Addon"),
        "Base text. Addon."
    )
})

test_that("concatWithAnd handles empty, single, pair, and list", {
    expect_equal(ospsuite.reportingframework:::concatWithAnd(character(0)), "")
    expect_equal(ospsuite.reportingframework:::concatWithAnd("A"), "A")
    expect_equal(
        ospsuite.reportingframework:::concatWithAnd(c("A", "B")),
        "A and B"
    )
    expect_equal(
        ospsuite.reportingframework:::concatWithAnd(c("A", "B", "C")),
        "A, B and C"
    )
})

test_that("pasteFigureTags returns base text and optional punctuation controls", {
    dt <- data.table::data.table(
        label = c("Adults", "Adults"),
        plotTag = c("A", "B")
    )

    expect_equal(
        ospsuite.reportingframework:::pasteFigureTags(
            dtCaption = dt,
            captionColumn = "label"
        ),
        "Adults"
    )

    expect_equal(
        ospsuite.reportingframework:::pasteFigureTags(
            dtCaption = dt,
            captionColumn = "label",
            endWithDot = TRUE,
            startWithBlank = TRUE
        ),
        " Adults."
    )
})

test_that("formatPercentiles maps common percentile labels", {
    expect_equal(
        ospsuite.reportingframework:::formatPercentiles(c(0, 0.5, 1)),
        c("min", "median", "max")
    )
    expect_equal(
        ospsuite.reportingframework:::formatPercentiles(c(0.05, 0.95)),
        c("5th", "95th")
    )
})

test_that("generatePlotTag maps index to uppercase alphabet", {
    expect_equal(ospsuite.reportingframework:::generatePlotTag(1), "A")
    expect_equal(ospsuite.reportingframework:::generatePlotTag(26), "Z")
    expect_error(ospsuite.reportingframework:::generatePlotTag(27))
})
