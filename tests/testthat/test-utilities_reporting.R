# set up directory with figures
projectPath <- iniLogFileForTest()

getValidFilename <- function(x){x}

figurePath = file.path(projectPath,'results','figures')
subFigurePath = file.path(projectPath,'results','figures','timeprofiles')


dir.create(figurePath,recursive = TRUE)
dir.create(subFigurePath,recursive = TRUE)


plotObject = ggplot2::ggplot(pressure) +
  ggplot2::geom_point(ggplot2::aes(x=temperature, y = pressure))

figureKey = 'myFigure1'

ospsuite.plots::exportPlot(plotObject = plotObject,
                           filepath = subFigurePath,
                           filename = paste0(figureKey,'.png'))

writeLines(text = "Footnote1.\n Footnote2",
           con =  file.path(subFigurePath,paste0(figureKey,'.footnote')))


writeLines(text = "This is my caption.",
           con =  file.path(subFigurePath,paste0(figureKey,'.caption')))

renderWord(fileName = file.path(figurePath,'MyResults.Rmd'),
           wordConversionTemplate = file.path(projectPath,'Appendix.docx'),
           clean = FALSE)



test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

cleanupLogFileForTest(projectPath)
