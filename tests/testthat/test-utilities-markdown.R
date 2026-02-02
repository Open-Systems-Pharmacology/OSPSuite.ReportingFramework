# Tests for utilities-markdown.R

test_that("mdNewline produces correct output", {
  output <- capture.output(mdNewline())
  expect_equal(output, "  ")
  
  output <- capture.output(mdNewline(3))
  expect_equal(length(output), 3)
})

test_that("mdPaste produces correct output", {
  output <- capture.output(mdPaste("Hello", "World"))
  expect_true(grepl("Hello World", output[1]))
})

test_that("mdPaste0 produces correct output", {
  output <- capture.output(mdPaste0("Hello", "World"))
  expect_true(grepl("HelloWorld", output[1]))
})

test_that("mdNewpage produces correct output", {
  output <- capture.output(mdNewpage())
  expect_true(any(grepl("\\\\newpage", output)))
})

test_that("mdHeading produces correct output with different levels", {
  output <- capture.output(mdHeading("Test Heading", level = 1))
  expect_true(any(grepl("^# Test Heading", output)))
  
  output <- capture.output(mdHeading("Test Heading", level = 3))
  expect_true(any(grepl("^### Test Heading", output)))
})

test_that("mdHeading validates level parameter", {
  expect_error(mdHeading("Test", level = 0))
  expect_error(mdHeading("Test", level = 7))
})

test_that("mdBullet produces correct output", {
  output <- capture.output(mdBullet("Item 1"))
  expect_true(any(grepl("^- Item 1", output)))
})

test_that("mdBullet handles different levels", {
  output <- capture.output(mdBullet("Item 1", level = 2))
  expect_true(any(grepl("^  - Item 1", output)))
})

test_that("mdBullet0 produces correct output", {
  output <- capture.output(mdBullet0("Item", "1"))
  expect_true(any(grepl("^- Item1", output)))
})

test_that("mdLink produces correct output", {
  output <- capture.output(mdLink(label = "Click here", filename = "file.txt", folder = "docs"))
  expect_true(any(grepl("\\[Click here\\]\\(docs/file.txt\\)", output)))
})

test_that("mdLink handles prefix parameter", {
  output <- capture.output(mdLink(label = "Image", filename = "img.png", folder = "images", prefix = "!"))
  expect_true(any(grepl("^!\\[Image\\]", output)))
})

test_that("mdCaption reads and formats caption correctly", {
  # Create temporary files for testing
  tmpdir <- tempdir()
  captionFile <- file.path(tmpdir, "test_caption.txt")
  writeLines("This is a test caption", captionFile)
  
  output <- capture.output(mdCaption(
    subfolder = tmpdir,
    captionFile = "test_caption.txt",
    captionPrefix = "Figure 1:"
  ))
  
  expect_true(any(grepl("Figure 1:", output)))
  expect_true(any(grepl("This is a test caption", output)))
  
  # Clean up
  unlink(captionFile)
})

test_that("mdCaption handles custom styles", {
  tmpdir <- tempdir()
  captionFile <- file.path(tmpdir, "test_caption2.txt")
  writeLines("Caption with style", captionFile)
  
  output <- capture.output(mdCaption(
    subfolder = tmpdir,
    captionFile = "test_caption2.txt",
    captionPrefix = "Table 1:",
    captionStyle = "CustomStyle"
  ))
  
  expect_true(any(grepl("CustomStyle", output)))
  
  # Clean up
  unlink(captionFile)
})

test_that("mdFootNote reads and formats footnotes correctly", {
  tmpdir <- tempdir()
  footnoteFile <- file.path(tmpdir, "test_footnote.txt")
  writeLines(c("Footnote line 1", "Footnote line 2"), footnoteFile)
  
  output <- capture.output(mdFootNote(
    subfolder = tmpdir,
    footNoteFile = "test_footnote.txt"
  ))
  
  expect_true(any(grepl("Footnote line 1", output)))
  expect_true(any(grepl("Footnote line 2", output)))
  
  # Clean up
  unlink(footnoteFile)
})

test_that("mdFootNote handles non-existent file gracefully", {
  tmpdir <- tempdir()
  
  expect_silent(mdFootNote(
    subfolder = tmpdir,
    footNoteFile = "nonexistent.txt"
  ))
})

test_that("mergeRmds validates input extensions", {
  expect_error(mergeRmds(
    newName = "test.txt",
    title = "Test",
    sourceRmds = c("file1.Rmd"),
    projectConfiguration = projectConfiguration
  ))
  
  expect_error(mergeRmds(
    newName = "test.Rmd",
    title = "Test",
    sourceRmds = c("file1.txt"),
    projectConfiguration = projectConfiguration
  ))
})

test_that("mergeRmds creates output file", {
  # Create temporary Rmd files
  tmpdir <- projectConfiguration$outputFolder
  sourceRmd1 <- file.path(tmpdir, "source1.Rmd")
  sourceRmd2 <- file.path(tmpdir, "source2.Rmd")
  
  writeLines("# Source 1", sourceRmd1)
  writeLines("# Source 2", sourceRmd2)
  
  outputFile <- file.path(tmpdir, "merged.Rmd")
  
  mergeRmds(
    newName = "merged",
    title = "Merged Report",
    sourceRmds = c("source1", "source2"),
    projectConfiguration = projectConfiguration
  )
  
  expect_true(file.exists(outputFile))
  
  content <- readLines(outputFile)
  expect_true(any(grepl("Merged Report", content)))
  expect_true(any(grepl('child="source1.Rmd"', content)))
  expect_true(any(grepl('child="source2.Rmd"', content)))
  
  # Clean up
  unlink(c(sourceRmd1, sourceRmd2, outputFile))
})
