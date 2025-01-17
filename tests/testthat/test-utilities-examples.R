test_that("multiplication works", {

  startPath = getwd()

  suppressWarnings(runExample1())


  setwd(startPath)
})
