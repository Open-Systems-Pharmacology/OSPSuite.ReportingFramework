#' converts .Rmd file to word
#'
#' @param fileName name of .Rmd file to convert to word format (".docx")
#' @param wordConversionTemplate  template used for conversion
#' @param customStyles list of custom styles usable for figure and table captions and footnotes
#'    available list elements for styles are: `FigureCaption`, `FigureFootnote`, `TableCaption` and `TableFootnote`
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
      system.file("extdata", "reference.docx", package = "ospsuite.reportingframework")
  }
  checkmate::assertFileExists(wordConversionTemplate)

  rmarkdown::render(fileName,
    output_format = "word_document",
    output_options = list(reference_docx = wordConversionTemplate),
    params = list(customStyles = customStyles),
    ...
  )

  return(invisible())
}
