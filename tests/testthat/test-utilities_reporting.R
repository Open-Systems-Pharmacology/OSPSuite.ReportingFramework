# set up directory with figures
projectPath <- iniLogFileForTest()

test_that("Rendering", {
  rmdContainer <- RmdContainer$new(rmdfolder = projectPath, "timeProfiles")

  rmdContainer$addHeader("Section 1")

  plotObject <- ggplot2::ggplot(data.frame(x = seq(1:3), y = seq(2:4))) +
    ggplot2::geom_point(ggplot2::aes(x = x, y = y))

  rmdContainer$addAndExportFigure(
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

  rmdContainer$addAndExportTable(
    table = dt,
    caption = "my Table",
    tableKey = "myTable"
  )

  testPath <- file.path(projectPath, "Test.Rmd")
  rmdContainer$writeRmd(basename(testPath))

  renderWord(testPath, quiet = TRUE)
  expect_true(file.exists(file.path(projectPath, "Test.docx")))
})

cleanupLogFileForTest(projectPath)
