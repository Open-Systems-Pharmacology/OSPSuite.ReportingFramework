# Create a minimal projectConfiguration for testing
projectConfiguration <- list(outputFolder = tempdir())

test_that("Creation and print of startlines", {
  rmdfolder <- projectConfiguration$outputFolder
  expect_error(QmdPlotManager$new(
    qmdName = NULL,
    qmdfolder = projectConfiguration$outputFolder,
    nameOfplotFunction = "plotTimeProfiles"
  ))

  qmdPlotManager <- QmdPlotManager$new(
    qmdName = "test",
    qmdfolder = projectConfiguration$outputFolder,
    nameOfplotFunction = "plotTimeProfiles"
  )
  expect_s3_class(qmdPlotManager, "QmdPlotManager")

  testPath <- file.path(projectConfiguration$outputFolder, "Test.qmd")
  expect_error(qmdPlotManager$writeQmd(projectConfiguration$outputFolder))

  qmdPlotManager$writeQmd(basename(testPath))
  expect_true(file.exists(testPath))
})


test_that("Initialization with invalid parameters", {
  expect_error(QmdPlotManager$new(
    qmdName = NULL,
    qmdfolder = projectConfiguration$outputFolder,
    nameOfplotFunction = "plotTimeProfiles"
  ))
  expect_error(QmdPlotManager$new(
    qmdName = "test",
    qmdfolder = NULL,
    nameOfplotFunction = "plotTimeProfiles"
  ))
  expect_error(QmdPlotManager$new(
    qmdName = "test",
    qmdfolder = projectConfiguration$outputFolder,
    nameOfplotFunction = 123
  ))
  expect_error(QmdPlotManager$new(
    qmdName = "test",
    qmdfolder = projectConfiguration$outputFolder,
    nameOfplotFunction = "nonExistentFunction"
  ))
})

test_that("Headers, newlines", {
  qmdPlotManager <- QmdPlotManager$new(
    qmdName = "test",
    qmdfolder = projectConfiguration$outputFolder,
    nameOfplotFunction = "plotTimeProfiles"
  )

  qmdPlotManager$addHeader("Level 1")
  qmdPlotManager$addHeader("Level 2", level = 2)
  qmdPlotManager$addNewline()
  qmdPlotManager$addNewpage()

  testPath <- file.path(projectConfiguration$outputFolder, "Test.qmd")
  qmdPlotManager$writeQmd(basename(testPath))

  tmp <- readLines(testPath)

  expect_contains(tmp, "# Level 1  ")
  expect_contains(tmp, "\\newpage  ")
})

test_that("Figure export", {
  qmdPlotManager <- QmdPlotManager$new(
    qmdName = "test",
    qmdfolder = projectConfiguration$outputFolder,
    nameOfplotFunction = "plotTimeProfiles"
  )

  qmdPlotManager$addHeader("Section 1")

  plotObject <- ggplot2::ggplot(data.frame(x = seq(1:3), y = seq(2:4))) +
    ggplot2::geom_point(ggplot2::aes(x = x, y = y))

  qmdPlotManager$addAndExportFigure(
    plotObject = plotObject,
    caption = "My First Figure with footnotes",
    footNoteLines = c("footnote 1", "footnote 2"),
    figureKey = "Fig1"
  )

  # files are exported
  expect_true(file.exists(file.path(
    projectConfiguration$outputFolder,
    "test",
    "Fig1.png"
  )))
  expect_true(file.exists(file.path(
    projectConfiguration$outputFolder,
    "test",
    "Fig1.caption"
  )))
  expect_true(file.exists(file.path(
    projectConfiguration$outputFolder,
    "test",
    "Fig1.footnote"
  )))

  # it should not be possible to add the same key twice
  expect_error(
    qmdPlotManager$addAndExportFigure(
      plotObject = plotObject,
      caption = "My First Figure with footnotes",
      footNoteLines = c("footnote 1", "footnote 2"),
      figureKey = "Fig1"
    )
  )

  qmdPlotManager$addHeader("Section 2")

  for (i in seq(1, 3)) {
    qmdPlotManager$addAndExportFigure(
      plotObject = plotObject,
      caption = paste("Figure in Loop", i),
      figureKey = paste0("LoopFig", i)
    )
  }

  testPath <- file.path(projectConfiguration$outputFolder, "Test.qmd")
  expect_no_error(qmdPlotManager$writeQmd(basename(testPath)))
})


test_that("Table export export", {
  qmdPlotManager <- QmdPlotManager$new(
    qmdName = "test",
    qmdfolder = projectConfiguration$outputFolder,
    nameOfplotFunction = "plotTimeProfiles"
  )

  qmdPlotManager$addHeader("Section 1")

  tableKey <- "quantiles"
  dt <- data.table(
    x = rnorm(1000),
    class = sample(c("Female", "Male"), size = 1000, replace = TRUE)
  ) %>%
    .[, as.list(quantile(x)), by = "class"]

  qmdPlotManager$addAndExportTable(
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

  qmdPlotManager$addAndExportTable(
    table = dt,
    caption = "my Table",
    tableKey = "myTable2"
  )

  # change digits of significance from 3 (default) to not allowed number
  expect_error(qmdPlotManager$digitsOfSignificance <- -1)

  # change digits of significance from 3 (default) to 4
  qmdPlotManager$digitsOfSignificance <- 4

  qmdPlotManager$addAndExportTable(
    table = dt,
    caption = "my Table",
    tableKey = "myTabledetailed"
  )

  testPath <- file.path(projectConfiguration$outputFolder, "Test.qmd")
  expect_no_error(qmdPlotManager$writeQmd(basename(testPath)))
})
