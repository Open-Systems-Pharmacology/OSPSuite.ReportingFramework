
test_that("Valid filenames return TRUE", {
  expect_true(checkFileNameValidity("valid_file.txt"))
  expect_true(checkFileNameValidity("another_valid_file.csv", isDataSet = FALSE))
  expect_true(checkFileNameValidity("valid_data_file.txt", isDataSet = TRUE))
})

test_that("Filenames that start with a number return warnings", {
  expect_warning(checkFileNameValidity("1invalid_file.txt"))
})

test_that("Filenames with invalid extensions return warnings", {
  expect_warning(checkFileNameValidity("invalid_file.xmls"))
  expect_warning(checkFileNameValidity("another_invalid_file.doc"))
})

test_that("Filenames exceeding length limit return warnings", {
  expect_warning(checkFileNameValidity("this_filename_is_way_too_long_to_be_valid.txt", isDataSet = TRUE))
  expect_no_warning(checkFileNameValidity("this_filename_is_way_too_long_to_be_valid.txt", isDataSet = FALSE))
  expect_warning(checkFileNameValidity("this_filename_is_way_too_long_to_be_valid_because_it_is_realy_very_long.csv", isDataSet = FALSE))
})

test_that("Filenames with valid extensions but invalid pattern return warnings", {
  expect_warning(checkFileNameValidity("invalid_file_name_with space.txt"))
})

test_that("Filenames without extensions return warnings", {
  expect_warning(checkFileNameValidity("invalid_file_without_extension"))
})

