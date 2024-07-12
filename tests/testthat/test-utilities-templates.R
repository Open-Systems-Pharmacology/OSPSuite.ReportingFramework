# Test for templateDirectory function
test_that("templateDirectory returns the path of the template directory", {
  expect_true(file.exists(templateDirectory()))
})
