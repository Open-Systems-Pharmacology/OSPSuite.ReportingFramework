test_that("stopHelperFunction stops with error when option is NULL", {
  options(OSPSuite.RF.stopHelperFunction = NULL) # Set option to NULL
  expect_error(stopHelperFunction())

  options(OSPSuite.RF.stopHelperFunction = TRUE) # Set option to TRUE
  expect_error(stopHelperFunction())

  options(OSPSuite.RF.stopHelperFunction = FALSE) # Set option to FALSE
  expect_silent(stopHelperFunction()) # Should not throw an error
})

