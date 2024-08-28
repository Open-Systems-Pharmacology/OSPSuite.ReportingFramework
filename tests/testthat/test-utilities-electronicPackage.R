# Unit tests for checkFileNameValidity function
test_that("checkFileNameValidity function returns the correct results", {
  files <- c("file1.xml", "file2.txt", "file3.csv", "invalid_file.pdf")
  dataSets <- c(
    "dataset1.txt", "dataset2.txt",
    "invalid_dataset_with_a_veryveryveryveryveryveryveryverylongName.pdf"
  )

  inValidFileNames <- checkFileNameValidity(files, dataSets)

  expect_contains(inValidFileNames, c(files[4], dataSets[3]))

  files <- c("File1.xml", "fi-le2.txt", "fi,&le3.csv")
  inValidFileNames <- checkFileNameValidity(files)
  expect_contains(inValidFileNames, files)
})
