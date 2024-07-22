projectPath <- iniLogFileForTest()

test_that("Creation and print of startlines", {
  rmdContainer <- RmdContainer$new(rmdfolder = projectPath, "timeProfiles")
  expect_s3_class(rmdContainer, "RmdContainer")

  testPath <- file.path(projectPath, "Test.Rmd")
  expect_error(rmdContainer$writeRmd(projectPath))

  rmdContainer$writeRmd(basename(testPath))
  expect_true(file.exists(testPath))
})

test_that("Headers, newlines", {
  rmdContainer <- RmdContainer$new(rmdfolder = projectPath, "timeProfiles")

  rmdContainer$addHeader("Level 1")
  rmdContainer$addHeader("Level 2", level = 2)
  rmdContainer$addNewline()
  rmdContainer$addNewpage()

  testPath <- file.path(projectPath, "Test.Rmd")
  rmdContainer$writeRmd(basename(testPath))

  tmp <- readLines(testPath)

  expect_contains(tmp, "# Level 1  ")
  expect_contains(tmp, "\\newpage  ")
})

test_that("Figure export", {
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

  # files are exported
  expect_true(file.exists(file.path(projectPath, "timeProfiles", "Fig1.png")))
  expect_true(file.exists(file.path(projectPath, "timeProfiles", "Fig1.caption")))
  expect_true(file.exists(file.path(projectPath, "timeProfiles", "Fig1.footnote")))

  # it should not be possible to add the same key twice
  expect_error(
    rmdContainer$addAndExportFigure(
      plotObject = plotObject,
      caption = "My First Figure with footnotes",
      footNoteLines = c("footnote 1", "footnote 2"),
      figureKey = "Fig1"
    )
  )

  rmdContainer$addHeader("Section 2")

  for (i in seq(1, 3)) {
    rmdContainer$addAndExportFigure(
      plotObject = plotObject,
      caption = paste("Figure in Loop", i),
      figureKey = paste0("LoopFig", i)
    )
  }

  testPath <- file.path(projectPath, "Test.Rmd")
  expect_no_error(rmdContainer$writeRmd(basename(testPath)))
})


test_that("Table export export", {
  rmdContainer <- RmdContainer$new(rmdfolder = projectPath, "timeProfiles")

  rmdContainer$addHeader("Section 1")

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

  dt <- data.table(
    d = c(pi, pi * 1e-1, pi * 100),
    d2 = c(pi, pi * 1e-2, pi * 100),
    d3 = c(pi, pi * 1e-6, pi * 100),
    i = seq(1, 3)
  )

  rmdContainer$addAndExportTable(
    table = dt,
    caption = "my Table",
    tableKey = "myTable2"
  )

  # change digits of significance from 3 (default) to not allowed number
  expect_error(rmdContainer$digitsOfSignificance <- -1)

  # change digits of significance from 3 (default) to 4
  rmdContainer$digitsOfSignificance <- 4

  rmdContainer$addAndExportTable(
    table = dt,
    caption = "my Table",
    tableKey = "myTabledetailed"
  )

  testPath <- file.path(projectPath, "Test.Rmd")
  expect_no_error(rmdContainer$writeRmd(basename(testPath)))
})


cleanupLogFileForTest(projectPath)
