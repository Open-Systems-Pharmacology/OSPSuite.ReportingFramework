renderWord <- function(fileName,
                       wordConversionTemplate,
                       appendDoc = TRUE,
                       ...){

  checkmate::assertFileExists(fileName,extension = '.Rmd')

  # Check if pandoc is available before trying to render word report
  if (!rmarkdown::pandoc_available()) {
    stop("Pandoc is not installed, word report was not created.")
    return(invisible())
  }
  if (is.null(wordConversionTemplate)){
    wordConversionTemplate <-
      system.file("extdata", "reference.docx", package = "OSPSuite.ReportingFramework")
    appendDoc = FALSE
  }
  checkmate::assertFileExists(wordConversionTemplate)

  rmarkdown::render(fileName,
                    output_format = 'word_document',
                    output_options = list(reference_docx = wordConversionTemplate),
                    ...
  )

  # Place the content of the newly create word document to the reference docuemnt.
  # cursor is set to Bookmark
  if (appendDoc){
    docReportPath <- paste0(tools::file_path_sans_ext(fileName),'.docx')
    checkmate::assertFileExists(docReportPath)

    docMerged <- officer::read_docx(wordConversionTemplate) %>%
      officer::cursor_bookmark('REPORTBLOCK') %>%
      officer::body_add_docx(src = docReportPath,pos = 'after')


    docMerged <- docMerged %>%
      officer::cursor_reachbody_add_caption(docMerged, caption)



    docMerged <- docMerged %>%
      officer::body_add_blocks(figureList)

    print(docMerged, target = docReportPath)
  }


}

