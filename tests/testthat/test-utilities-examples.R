test_that("multiplication works", {
  startPath <- getwd()

  expect_no_error(suppressWarnings(runExample1()))

  expect_no_error(suppressWarnings(runExample2()))

  setwd(startPath)
})
