# Tests for RmdPlotManager.R
# testproject with variable projectconfiguration is set up by the setup.R for all tests simulataneously

test_that("Creation and print of startlines", {
  rmdfolder <- projectConfiguration$outputFolder
  expect_error(RmdPlotManager$new(
    rmdName = NULL,
    rmdfolder = projectConfiguration$outputFolder,
    nameOfplotFunction = "plotTimeProfiles"
  ), messages$errorProvideValidRmdName())

  rmdPlotManager <- RmdPlotManager$new(
    rmdName = "test",
    rmdfolder = projectConfiguration$outputFolder,
    nameOfplotFunction = "plotTimeProfiles"
  )
  expect_s3_class(rmdPlotManager, "RmdPlotManager")

  testPath <- file.path(projectConfiguration$outputFolder, "Test.Rmd")
  expect_error(rmdPlotManager$writeRmd(projectConfiguration$outputFolder), messages$errorProvideFileNameAsBasename())

  rmdPlotManager$writeRmd(basename(testPath))
  expect_true(file.exists(testPath))
})


test_that("Initialization with invalid parameters", {
  expect_error(RmdPlotManager$new(rmdName = NULL, rmdfolder = projectConfiguration$outputFolder, nameOfplotFunction = "plotTimeProfiles"), messages$errorProvideValidRmdName())
  expect_error(RmdPlotManager$new(rmdName = "test", rmdfolder = NULL, nameOfplotFunction = "plotTimeProfiles"))
  expect_error(RmdPlotManager$new(rmdName = "test", rmdfolder = projectConfiguration$outputFolder, nameOfplotFunction = 123))
  expect_error(RmdPlotManager$new(rmdName = "test", rmdfolder = projectConfiguration$outputFolder, nameOfplotFunction = "nonExistentFunction"), messages$errorFunctionDoesNotExist("nonExistentFunction"))
})

test_that("Headers, newlines", {
  rmdPlotManager <- RmdPlotManager$new(
    rmdName = "test",
    rmdfolder = projectConfiguration$outputFolder,
    nameOfplotFunction = "plotTimeProfiles"
  )

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
  rmdPlotManager <- RmdPlotManager$new(
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

  # files are exported
  expect_true(file.exists(file.path(projectConfiguration$outputFolder, "test", "Fig1.png")))
  expect_true(file.exists(file.path(projectConfiguration$outputFolder, "test", "Fig1.caption")))
  expect_true(file.exists(file.path(projectConfiguration$outputFolder, "test", "Fig1.footnote")))

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
  rmdPlotManager <- RmdPlotManager$new(
    rmdName = "test",
    rmdfolder = projectConfiguration$outputFolder,
    nameOfplotFunction = "plotTimeProfiles"
  )

  rmdPlotManager$addHeader("Section 1")

  tableKey <- "quantiles"
  dt <- data.table(
    x = rnorm(1000),
    class = sample(c("Female", "Male"), size = 1000, replace = TRUE)
  )[, as.list(quantile(x)), by = "class"]

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

test_that("RmdPlotManager initialization requires valid parameters", {
  expect_error(
    RmdPlotManager$new(
      rmdName = NULL,
      rmdfolder = projectConfiguration$outputFolder,
      nameOfplotFunction = "plotTimeProfiles"
    ),
    regexp = "Rmd"
  )

  expect_error(
    RmdPlotManager$new(
      rmdName = "test",
      rmdfolder = projectConfiguration$outputFolder,
      nameOfplotFunction = "nonExistentFunction"
    )
  )
})

test_that("RmdPlotManager can be created with valid parameters", {
  rmdPlotManager <- RmdPlotManager$new(
    rmdName = "test_rmd",
    rmdfolder = projectConfiguration$outputFolder,
    nameOfplotFunction = "plotTimeProfiles"
  )

  expect_s3_class(rmdPlotManager, "RmdPlotManager")
  expect_false(rmdPlotManager$suppressExport)
})

test_that("RmdPlotManager with suppressExport = TRUE", {
  rmdPlotManager <- RmdPlotManager$new(
    rmdName = "test_suppress",
    rmdfolder = projectConfiguration$outputFolder,
    nameOfplotFunction = "plotTimeProfiles",
    suppressExport = TRUE
  )

  expect_true(rmdPlotManager$suppressExport)

  # writeRmd should return without error when suppressExport is TRUE
  expect_silent(rmdPlotManager$writeRmd())
})

# Tests for exportPlotList
test_that("exportPlotList handles ggplot objects", {
  skip_if_not_installed("ggplot2")

  rmdPlotManager <- RmdPlotManager$new(
    rmdName = "test_export_plot",
    rmdfolder = projectConfiguration$outputFolder,
    nameOfplotFunction = "plotTimeProfiles",
    suppressExport = FALSE
  )

  # Create a simple ggplot with caption attribute
  plot_obj <- ggplot2::ggplot(data.frame(x = 1:10, y = 1:10), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  attr(plot_obj, "caption") <- "Test plot caption"

  plotList <- list(testPlot = plot_obj)

  # Should not error
  expect_silent(rmdPlotManager$exportPlotList(plotList))
  expect_true(file.exists(file.path(projectConfiguration$outputFolder,'test_export_plot','testPlot.png')))
})

test_that("exportPlotList handles data.table objects", {
  rmdPlotManager <- RmdPlotManager$new(
    rmdName = "test_export_table",
    rmdfolder = projectConfiguration$outputFolder,
    nameOfplotFunction = "plotTimeProfiles",
    suppressExport = FALSE
  )

  # Create a simple data.table with caption attribute
  table_obj <- data.table(a = 1:5, b = letters[1:5])
  attr(table_obj, "caption") <- "Test table caption"

  plotList <- list(testTable = table_obj)

  # Should not error
  expect_silent(rmdPlotManager$exportPlotList(plotList))
  expect_true(file.exists(file.path(projectConfiguration$outputFolder,'test_export_table','testTable.csv')))

})

test_that("exportPlotList warns when caption is missing", {
  rmdPlotManager <- RmdPlotManager$new(
    rmdName = "test_export_no_caption",
    rmdfolder = projectConfiguration$outputFolder,
    nameOfplotFunction = "plotTimeProfiles",
    suppressExport = FALSE
  )

  # Create a plot without caption
  plot_obj <- ggplot2::ggplot(data.frame(x = 1:10, y = 1:10), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  plotList <- list(testPlot = plot_obj)

  # Should warn about missing caption
  expect_warning(
    rmdPlotManager$exportPlotList(plotList),
    regexp = "Caption"
  )
})

test_that("exportPlotList returns early when suppressExport = TRUE", {
  rmdPlotManager <- RmdPlotManager$new(
    rmdName = "test_suppress_export",
    rmdfolder = projectConfiguration$outputFolder,
    nameOfplotFunction = "plotTimeProfiles",
    suppressExport = TRUE
  )

  plot_obj <- ggplot2::ggplot(data.frame(x = 1:10, y = 1:10), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  attr(plot_obj, "caption") <- "Test"

  plotList <- list(testPlot = plot_obj)

  # Should return invisibly without processing
  result <- rmdPlotManager$exportPlotList(plotList)
  expect_false(file.exists(file.path(projectConfiguration$outputFolder,'test_suppress_export','testPlot.png')))

})
