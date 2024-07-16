projectPath <- iniLogFileForTest()

test_that("Creation and print of startlines", {

  rmdContainer <- RmdContainer$new(rmdfolder = projectPath,'timeProfiles')
  expect_s3_class(rmdContainer,'RmdContainer')

  testPath = file.path(projectPath,'Test.Rmd')
  expect_error(rmdContainer$writeRmd(projectPath))

  rmdContainer$writeRmd(basename(testPath))
  expect_true(file.exists(testPath))
})

test_that("Headers, newlines", {

  rmdContainer <- RmdContainer$new(rmdfolder = projectPath,'timeProfiles')

  rmdContainer$addHeader('Level 1')
  rmdContainer$addHeader('Level 2',level = 2)
  rmdContainer$addNewline()
  rmdContainer$addNewpage()

  testPath = file.path(projectPath,'Test.Rmd')
  rmdContainer$writeRmd(basename(testPath))

  tmp <- readLines(testPath)

  expect_contains(tmp,'# Level 1  ')
  expect_contains(tmp,"\\newpage  ")

})

test_that("Figure export", {

  rmdContainer <- RmdContainer$new(rmdfolder = projectPath,'timeProfiles')

  rmdContainer$addHeader('Section 1')

  plotObject <- ggplot2::ggplot(data.frame(x = seq(1:3), y = seq(2:4))) +
    ggplot2::geom_point(ggplot2::aes(x = x, y = y))

  rmdContainer$addAndExportFigure(
    plotObject = plotObject,
    caption = "My First Figure with footnotes",
    footNoteLines = c('footnote 1','footnote 2'),
    figureKey = "Fig1"
  )

  rmdContainer$addHeader('Section 2')

  for (i in seq(1,3)){

    rmdContainer$addAndExportFigure(
      plotObject = plotObject,
      caption = paste("Figure in Loop",i),
      figureKey = paste0("LoopFig",i)
    )

  }

  testPath = file.path(projectPath,'Test.Rmd')
  rmdContainer$writeRmd(basename(testPath))

  expect_success(rmarkdown::render(testPath))

})

cleanupLogFileForTest(projectPath)
