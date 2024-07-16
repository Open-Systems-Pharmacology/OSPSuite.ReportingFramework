
#' mdNewline
#'
#' `cat` a line end and start a new line.
#'
#' @param n Number of new lines. Defaults to 1.
#'
#' @export
mdNewline = function(n = 1) {
  for (i in seq_len(n)) {
    cat("  \n")
  }

  return(invisible(NULL))
}


#' mdPaste
#'
#' `cat` text using `paste` and optionally end with newlines.
#'
#' @param ... Elements to be `paste`d.
#' @param sep Passed to `paste`. Defaults to `" "`.
#' @param collapse Passed to `paste`. Defaults to `NULL`.
#' @param newlines Number of newlines to insert afterwards. Defaults to 1.
#'
#' @return `NULL`, invisibly
#' @export
#'
#' @examples
mdPaste = function(..., sep = " ", collapse = NULL, newlines = 1) {
  cat(paste(..., sep = sep, collapse = collapse))

  mdNewline(newlines)

  return(invisible(NULL))
}


#' mdPaste0
#'
#' `cat` text using `paste0` and optionally end with newlines.
#'
#' @param ... Elements to be `paste0`d.
#' @param collapse Passed to `paste0`. Defaults to `NULL`.
#' @param newlines Number of newlines to insert afterwards. Defaults to 1.
#'
#' @return `NULL`, invisibly
#' @export
#'
#' @examples
mdPaste0  = function(..., collapse = NULL, newlines = 1) {
  cat(paste0(..., collapse = collapse))

  mdNewline(newlines)

  return(invisible(NULL))
}


#' mdNewpage
#'
#' Insert a page break and a newline.
#'
#' @return `NULL`, invisibly
#' @export
#'
#' @examples
mdNewpage = function() {
  mdNewline(2)
  mdPaste("\\newpage", newlines = 2)
}


#' mdHeading
#'
#' Insert a heading with a specified level.
#'
#' @param ... passed to mdPaste
#' @param level The header level, i.e. the number of `#`s. Defaults to 1.
#' @param newlines The number of newlines inserted after the heading. Defaults to 2.
#'
#' @export
mdHeading = function(..., level=1, newlines = 2) {
  checkmate::assert_int(level, lower = 1, upper = 6)

  mdNewline(3)
  prefix = paste0(rep("#", level), collapse = "")
  mdPaste(prefix, ..., newlines = newlines)
}

#' Insert a bulleted item, making sure that there are sufficient newlines to build a bulleted list
#'
#' @param ... passed to `mdPaste`
#' @param level `integer`, specifies the indentation level. `level=1` has no indentation
#'
#' @export
mdBullet = function(..., bullet = "-", level = 1) {
  checkmate::assertString(bullet)
  checkmate::assertInt(level, lower = 1)

  if (level > 1) {
    prefix = paste0(rep("  ", 2 * (level - 1) - 1), collapse = "")
  } else {
    prefix = NULL
  }

  mdPaste(prefix, bullet, ..., newlines = 2)
}

#' Insert a bulleted item, making sure that there are sufficient newlines to build a bulleted list
#'
#' @param ... passed to `mdPaste0`
#' @param level `integer`, specifies the indentation level. `level=1` has no indentation
#'
#' @export
mdBullet0 = function(..., bullet = "-", level = 1) {
  checkmate::assertString(bullet)
  checkmate::assertInt(level, lower = 1)

  if (level > 1) {
    prefix = paste0(rep("  ", 2 * (level - 1) - 1), collapse = "")
  } else {
    prefix = NULL
  }
  mdPaste0(prefix, bullet, " ", ..., newlines = 2)
}


#' mdFigure
#'
#' Include a figure file with caption
#'
#' @param figureFile file of Figure
#' @param captionFile file of Figure
#' @param subfolder The folder where the file is located relative to Rmd
#'
#' @export
mdFigure = function(
    figureNumber,
    figureFile,
    captionFile,
    footNoteFile = NULL,
    subfolder,
    addNewPage = TRUE
) {

  checkmate::assertFileExists(file.path(subfolder,figureFile))
  checkmate::assertFileExists(file.path(subfolder,captionFile))



  mdNewline()
  mdLink(label = '', filename = URLencode(figureFile), folder = subfolder, prefix = "!")
  mdNewline()

  # figure footnote
  if (file.exists(file.path(subfolder,footNoteFile))){
    footnoteLines <- readLines(file.path(subfolder,footNoteFile))

    if (length(footnoteLines) > 0){
       mdNewline()
      for (fL in footnoteLines){
        mdPaste('<div custom-style="Bayer Table Footnote">')
        mdPaste(fL)
        mdPaste('</div>')
      }
    }
  }

  #  caption
  caption <- readLines(file.path(rmdfolder,subfolder,captionFile))

  mdPaste0("**Figure",figureNumber,": ",caption,"**")
  mdNewline()
  if (addNewPage) mdNewpage()

  return(invisible())
}


#' mdLink
#'
#' Insert a link to another file, and a newline.
#'
#' @param label The text to display as the link.
#' @param filename The file to link to.
#' @param folder The folder where the file is located.
#' @param prefix An optional prefix to place in front of the link. Defaults to "".
#'
#' @export
#'
#' @examples
mdLink = function(label, filename, folder, prefix = "") {
  mdPaste0(prefix, "[", label, "](", file.path(folder, filename), ")  ")
  mdNewline()
}

#' mdTable
#'
#' Insert a table into a markdown document. Essentially a wrapper for `knitr::kable` with `format = "markdown"` and newlines.
#'
#' @param x An R object, typically a matrix or data frame.
#' @param ... passed to `knitr::kable`
#'
#' @return `x`, invisibly
#' @export
#'
#' @examples
mdTable = function(x, ...) {
  mdNewline()
  print(knitr::kable(x, format = "markdown", ...))
  mdNewline()

  return(invisible(x))
}



#' Prepare a data.table for prettier printing by converting linebreaks, large
#' number and percentages
#'
#' @param dt `data.table`, will not be modified
#' @param replaceLinebreaksIn `character` vector naming the columns of `dt` in
#'   which linebreaks will be replaced. `!guess` selects all character columns
#'   of `dt`
#' @param replaceLinebreaksBy `string` specifying how to replace linebreaks
#' @param convertToComma `character` vector naming the columns of `dt` to which
#'   `scales::comma` will be applied. `!guess` selects all integer columns of
#'   `dt`
#' @param commaAccuracy `number` specifying the rounding by `scales::comma`
#' @param convertToPercent `character` vector naming the columns of `dt` to
#'   which `scales::percent` will be applied. `!guess` selects all numeric,
#'   non-integer columns of `dt` with values between 0 and 1
#' @param percentAccuracy `number` specifying the rounding of `scales::percent`
#' @param convertToYesNo `character` vector naming the columns of `dt` which
#'   will be converted to "yes"/"no" depening on their logical status. `!guess`
#'   selects all logical columns of `dt`
#' @param skip `character` vector naming columns of `dt` that won't be modified
#'
#' @return `data.table` prepared for display
#' @export
#'
#' @examples
prepareDTForPrinting = function(
    dt,
    replaceLinebreaksIn = "!guess",
    replaceLinebreaksBy = " ",
    convertToComma = "!guess",
    commaAccuracy = 1,
    convertToPercent = "!guess",
    percentAccuracy = .1,
    convertToYesNo = "!guess",
    digits = 3,
    skip = NULL
) {

  checkmate::assertClass(dt, "data.table")
  checkmate::assertCharacter(replaceLinebreaksIn, null.ok = TRUE)
  checkmate::assertString(replaceLinebreaksBy)
  checkmate::assertCharacter(convertToComma, null.ok = TRUE)
  checkmate::assertNumber(commaAccuracy, lower = 0)
  checkmate::assertCharacter(convertToPercent, null.ok = TRUE)
  checkmate::assertNumber(percentAccuracy, lower = 0)
  checkmate::assertCharacter(convertToYesNo, null.ok = TRUE)
  checkmate::assertNumber(digits, lower = 0, null.ok = TRUE)
  checkmate::assertCharacter(skip, null.ok = TRUE)

  dt = copy(dt)

  unassignedColumns = setdiff(
    names(which(
      sapply(dt, is.numeric) |
        sapply(dt, is.logical) |
        (sapply(dt, is.integer) & !sapply(dt, is.factor))
    )),
    setdiff(
      c(
        replaceLinebreaksIn,
        convertToComma,
        convertToPercent,
        convertToYesNo
      ),
      "!guess"
    )
  )

  if (length(unassignedColumns) == 0) {
    # nothing left to assign
    replaceLinebreaksIn = NULL
    convertToComma = NULL
    convertToPercent = NULL
    convertToYesNo = NULL
  } else {
    if (!is.null(replaceLinebreaksIn) && all(replaceLinebreaksIn == "!guess")) {
      replaceLinebreaksIn = names(which(sapply(dt[, ..unassignedColumns], is.character)))
    }
    if (!is.null(convertToComma) && all(convertToComma == "!guess")) {
      convertToComma = names(which(sapply(dt[, ..unassignedColumns], is.integer)))
    }
    if (!is.null(convertToPercent) && all(convertToPercent == "!guess")) {
      convertToPercent = names(which(
        (sapply(dt[, ..unassignedColumns], is.numeric)) &
          (!sapply(dt[, ..unassignedColumns], is.integer)) &
          (sapply(dt[, ..unassignedColumns], min, na.rm = TRUE) >= 0) &
          (sapply(dt[, ..unassignedColumns], max, na.rm= TRUE) <= 1)
      ))
    }
    if (all(convertToYesNo == "!guess")) {
      convertToYesNo = names(which(sapply(dt[, ..unassignedColumns], is.logical)))
    }
  }

  requiredColumns = c(
    replaceLinebreaksIn,
    convertToComma,
    convertToPercent,
    convertToYesNo
  )
  if (!is.null(requiredColumns) && length(requiredColumns) > 0) {
    assertVariablesExist(requiredColumns, dt)
  }

  for (column in setdiff(replaceLinebreaksIn, skip)) {
    dt[, (column) := gsub("\n", replaceLinebreaksBy, get(column))]
  }

  for (column in setdiff(convertToComma, skip)) {
    useAccuracy = if_else(is.integer(dt[[column]]), 1, commaAccuracy)
    dt[, (column) := scales::comma(get(column), accuracy = useAccuracy)]
  }

  for (column in setdiff(convertToPercent, skip)) {
    dt[, (column) := scales::percent(get(column), accuracy = percentAccuracy)]
  }

  for (column in setdiff(convertToYesNo, skip)) {
    dt[, (column) := if_else(get(column), "yes", "no")]
  }

  if (!is.null(digits)) {
    numberColumns = setdiff(names(which(sapply(dt, is.numeric))), skip)

    for (column in numberColumns) {
      dt[, (column) := round(get(column), digits)]
    }
  }

  return(dt)
}

#' Loop on figure keys, add figure or table
#'
#' @param keyList list of keys
#' @param subfolder subfolder of rmdfolder where figures are saved
#' @param rmdfolder folder where rmd is filed
#' @param numbersOf list with numbers of tabes and figures in rmd
#'
#' @return list of numbers and figures after loop
#' @export
addFiguresAndTables <- function(keyList,
                                subfolder,
                                numbersOf){

  folderFiles <- list.files(subfolder)

  for (key in keyList){
    figureFile <- paste(key,dev,sep = '.')
    if (figureFile %in% folderFiles){

      numbersOf$figures <- numbersOf$figures +1

      mdFigure(figureNumber = numbersOf$figures,
               figureFile = figureFile,
               captionFile = paste(key,'caption',sep = '.'),
               footNoteFile = paste(key,'footnote',sep = '.'),
               subfolder = subfolder
      )
    }
  }

  return(numbersOf)
}
