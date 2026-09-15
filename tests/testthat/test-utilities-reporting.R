# testProject was set up by setup.R

# Create a mock projectConfiguration for testing
projectConfiguration <- list(
  outputFolder = tempdir()
)

test_that("Rendering", {
  skip_if(
    length(quarto::quarto_path()) == 0 || !nzchar(quarto::quarto_path()),
    "Quarto not available in this environment"
  )

  rmdPlotManager <- QmdPlotManager$new(
    rmdName = "test",
    rmdfolder = projectConfiguration$outputFolder,
    nameOfplotFunction = "plotTimeProfiles"
  )

  rmdPlotManager$addHeader("Section 1")

  plotObject <- ggplot2::ggplot(data.frame(x = seq(1:3), y = seq(2:4))) +
    ggplot2::geom_point(ggplot2::aes(x = x, y = y))

  rmdPlotManager$addAndExportFigure(
    plotObject = plotObject,
    caption = "My First Figure with footnotes",
    footNoteLines = c("footnote 1", "footnote 2"),
    figureKey = "Fig1"
  )

  tableKey <- "quantiles"
  dt <- data.table(
    x = rnorm(1000),
    class = sample(c("Female", "Male"), size = 1000, replace = TRUE)
  ) %>%
    .[, as.list(quantile(x)), by = "class"]

  rmdPlotManager$addAndExportTable(
    table = dt,
    caption = "my Table",
    tableKey = "myTable"
  )

  testPath <- file.path(projectConfiguration$outputFolder, "Test.qmd")
  rmdPlotManager$writeQmd(basename(testPath))

  renderWord(testPath, quiet = TRUE)
  expect_true(file.exists(file.path(
    projectConfiguration$outputFolder,
    "Test.docx"
  )))
})
