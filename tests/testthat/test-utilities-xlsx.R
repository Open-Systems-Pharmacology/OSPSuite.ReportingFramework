test_that("Splits vectors is working", {

  originalVector <- c('group1, group2','group3,group4',NA)

  splitVector <- splitInputs(originalVector)

  expect_contains(splitVector,paste0('group',seq(1,4)))

  originalVector <- c(NA,NA,NA)

  splitVector <- splitInputs(originalVector)

  expect_equal(splitVector,NULL)

})
