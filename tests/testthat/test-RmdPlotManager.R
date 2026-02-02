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
