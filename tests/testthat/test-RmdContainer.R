projectConfiguration <- setUpTestProject()

test_that("Creation and print of startlines", {
  rmdPlotManager <- RmdPlotManager$new(rmdfolder = projectConfiguration$outputFolder, "timeProfiles")
  expect_s3_class(rmdPlotManager, "RmdPlotManager")

  testPath <- file.path(projectConfiguration$outputFolder, "Test.Rmd")
  expect_error(rmdPlotManager$writeRmd(projectConfiguration$outputFolder))

  rmdPlotManager$writeRmd(basename(testPath))
  expect_true(file.exists(testPath))
})

test_that("Headers, newlines", {
  rmdPlotManager <- RmdPlotManager$new(rmdfolder = projectConfiguration$outputFolder, "timeProfiles")

  rmdPlotManager$addHeader("Level 1")
  rmdPlotManager$addHeader("Level 2", level = 2)
  rmdPlotManager$addNewline()
  rmdPlotManager$addNewpage()

  testPath <- file.path(projectConfiguration$outputFolder, "Test.Rmd")
  rmdPlotManager$writeRmd(basename(testPath))

  tmp <- readLines(testPath)

  expect_contains(tmp, "# Level 1  ")
  expect_contains(tmp, "\\newpage  ")
})

test_that("Figure export", {
  rmdPlotManager <- RmdPlotManager$new(rmdfolder = projectConfiguration$outputFolder, "timeProfiles")

  rmdPlotManager$addHeader("Section 1")

  plotObject <- ggplot2::ggplot(data.frame(x = seq(1:3), y = seq(2:4))) +
    ggplot2::geom_point(ggplot2::aes(x = x, y = y))

  rmdPlotManager$addAndExportFigure(
    plotObject = plotObject,
    caption = "My First Figure with footnotes",
    footNoteLines = c("footnote 1", "footnote 2"),
    figureKey = "Fig1"
  )

  # files are exported
  expect_true(file.exists(file.path(projectConfiguration$outputFolder, "timeProfiles", "Fig1.png")))
  expect_true(file.exists(file.path(projectConfiguration$outputFolder, "timeProfiles", "Fig1.caption")))
  expect_true(file.exists(file.path(projectConfiguration$outputFolder, "timeProfiles", "Fig1.footnote")))

  # it should not be possible to add the same key twice
  expect_error(
    rmdPlotManager$addAndExportFigure(
      plotObject = plotObject,
      caption = "My First Figure with footnotes",
      footNoteLines = c("footnote 1", "footnote 2"),
      figureKey = "Fig1"
    )
  )

  rmdPlotManager$addHeader("Section 2")

  for (i in seq(1, 3)) {
    rmdPlotManager$addAndExportFigure(
      plotObject = plotObject,
      caption = paste("Figure in Loop", i),
      figureKey = paste0("LoopFig", i)
    )
  }

  testPath <- file.path(projectConfiguration$outputFolder, "Test.Rmd")
  expect_no_error(rmdPlotManager$writeRmd(basename(testPath)))
})


test_that("Table export export", {
  rmdPlotManager <- RmdPlotManager$new(rmdfolder = projectConfiguration$outputFolder, "timeProfiles")

  rmdPlotManager$addHeader("Section 1")

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

  dt <- data.table(
    d = c(pi, pi * 1e-1, pi * 100),
    d2 = c(pi, pi * 1e-2, pi * 100),
    d3 = c(pi, pi * 1e-6, pi * 100),
    i = seq(1, 3)
  )

  rmdPlotManager$addAndExportTable(
    table = dt,
    caption = "my Table",
    tableKey = "myTable2"
  )

  # change digits of significance from 3 (default) to not allowed number
  expect_error(rmdPlotManager$digitsOfSignificance <- -1)

  # change digits of significance from 3 (default) to 4
  rmdPlotManager$digitsOfSignificance <- 4

  rmdPlotManager$addAndExportTable(
    table = dt,
    caption = "my Table",
    tableKey = "myTabledetailed"
  )

  testPath <- file.path(projectConfiguration$outputFolder, "Test.Rmd")
  expect_no_error(rmdPlotManager$writeRmd(basename(testPath)))
})


cleanupLogFileForTest(projectConfiguration)
