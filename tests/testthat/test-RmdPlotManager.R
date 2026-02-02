# Tests for RmdPlotManager.R
# Note: More comprehensive tests exist in test-RmdContainer.R

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
    regexp = "caption"
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
  expect_null(result)
})
