test_that("multiplication works", {

  startPath = getwd()

  expect_no_error(suppressWarnings(runExample1()))


  setwd(startPath)
})
