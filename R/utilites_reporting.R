#' converts .Rmd file to word
#'
#' @param fileName name of .Rmd file to convert to word format (".docx")
#' @param wordConversionTemplate  template used for conversion
#' @param doAppendDocToTemplate `boolean`, if TRUE and `wordConversionTemplate` is given,
#'   new generated document is added to the template after bookmark REPORTBLOCK
#' @param customStyles list of custom styles usable for figure and table captions and footnotes
#'    available list elements for styles are: "FigureCaption", "FigureFootnote", "TableCaption" and "TableFootnote"
#'    The selected styles should be defined in the `wordConversionTemplate`
#' @param ... passed to render
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example with customstyles for Footnotes
#' renderWord(
#'   fileName = "myReport.Rmd",
#'   wordConversionTemplate = "path/to/template/myTemplate.docx",
#'   customStyles = list(FigureFootnote = "myFootnoteFormat", TableFootnote = "myFootnoteFormat")
#' )
#' }
renderWord <- function(fileName,
                       wordConversionTemplate = NULL,
                       doAppendDocToTemplate = TRUE,
                       customStyles = list(
                         FigureCaption = NULL,
                         FigureFootnote = NULL,
                         TableCaption = NULL,
                         TableFootnote = NULL
                       ),
                       ...) {
  checkmate::assertFileExists(fileName, extension = ".Rmd")
  checkmate::assertList(customStyles)
  if (length(customStyles) > 0) {
    checkmate::assertNames(names(customStyles),
      subset.of = c(
        "FigureCaption", "FigureFootnote",
        "TableCaption", "TableFootnote"
      )
    )
  }

  # Check if pandoc is available before trying to render word report
  if (!rmarkdown::pandoc_available()) {
    stop("Pandoc is not installed, word report was not created.")
    return(invisible())
  }
  if (is.null(wordConversionTemplate)) {
    wordConversionTemplate <-
      system.file("extdata", "reference.docx", package = "OSPSuite.ReportingFramework")
    doAppendDocToTemplate <- FALSE
  }
  checkmate::assertFileExists(wordConversionTemplate)

  rmarkdown::render(fileName,
    output_format = "word_document",
    output_options = list(reference_docx = wordConversionTemplate),
    params = list(customStyles = customStyles),
    ...
  )

  # Place the content of the newly create word document to the template document.
  if (doAppendDocToTemplate) {
    appendDocToTemplate(
      wordConversionTemplate = wordConversionTemplate,
      docReportPath = paste0(tools::file_path_sans_ext(fileName), ".docx")
    )
  }

  return(invisible())
}


#' Place the content of the newly create word document to the template document.
#'
#' cursor is set to Bookmark, file will be overwritten
#'
#' @param wordConversionTemplate path of template (should contain a bookmark REPORTBLOCK
#'   to mark the place for inclusion)
#' @param docReportPath path of word document to append
#'
#' @export
appendDocToTemplate <- function(wordConversionTemplate,
                                docReportPath) {
  checkmate::assertFileExists(docReportPath)

  docMerged <- officer::read_docx(wordConversionTemplate) %>%
    officer::cursor_bookmark("REPORTBLOCK") %>%
    officer::body_add_docx(src = docReportPath, pos = "after")

  print(docMerged, target = docReportPath)

  return(invisible())
}
