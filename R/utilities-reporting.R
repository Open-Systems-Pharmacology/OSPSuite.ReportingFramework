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
  checkmate::assertFileExists(fileName, extension = ".qmd")
  checkmate::assertList(customStyles)
  if (length(customStyles) > 0) {
    checkmate::assertNames(names(customStyles),
      subset.of = c(
        "FigureCaption", "FigureFootnote",
        "TableCaption", "TableFootnote"
      )
    )
    nonNullStyles <- Filter(Negate(is.null), customStyles)
    if (length(nonNullStyles) > 0) {
      checkmate::assertList(nonNullStyles, types = "character")
    }
  }

  # Check if pandoc is available before trying to render word report
  if (!nzchar(quarto::quarto_path())) {
    stop(messages$errorutilitiesreportingL1())
    return(invisible())
  }
  if (is.null(wordConversionTemplate)) {
    wordConversionTemplate <-
      system.file("extdata", "reference.docx", package = "ospsuite.reportingframework")
  }
  checkmate::assertFileExists(wordConversionTemplate)

  quarto::quarto_render(
    input = fileName,
    execute_params = list(customStyles = customStyles),
    pandoc_args = c("--reference-doc", wordConversionTemplate),
    ...
  )

  return(invisible())
}
