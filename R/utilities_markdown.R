#' mdNewline
#'
#' `cat` a line end and start a new line.
#'
#' @param n Number of new lines. Defaults to 1.
#'
#' @export
mdNewline <- function(n = 1) {
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
#' @export
mdPaste <- function(..., sep = " ", collapse = NULL, newlines = 1) {
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
#' @export
mdPaste0 <- function(..., collapse = NULL, newlines = 1) {
  cat(paste0(..., collapse = collapse))

  mdNewline(newlines)

  return(invisible(NULL))
}


#' mdNewpage
#'
#' Insert a page break and a newline.
#'
#' @export
mdNewpage <- function() {
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
mdHeading <- function(..., level = 1, newlines = 2) {
  checkmate::assert_int(level, lower = 1, upper = 6)

  mdNewline(3)
  prefix <- paste0(rep("#", level), collapse = "")
  mdPaste(prefix, ..., newlines = newlines)
}

#' Insert a bulleted item, making sure that there are sufficient newlines to build a bulleted list
#'
#' @param ... passed to `mdPaste`
#' @param level `integer`, specifies the indentation level. `level=1` has no indentation
#' @param bullet `character` type of bullet
#'
#' @export
mdBullet <- function(..., bullet = "-", level = 1) {
  checkmate::assertString(bullet)
  checkmate::assertInt(level, lower = 1)

  if (level > 1) {
    prefix <- paste0(rep("  ", 2 * (level - 1) - 1), collapse = "")
  } else {
    prefix <- NULL
  }

  mdPaste(prefix, bullet, ..., newlines = 2)
}

#' Insert a bulleted item, making sure that there are sufficient newlines to build a bulleted list
#'
#' @param ... passed to `mdPaste0`
#' @param level `integer`, specifies the indentation level. `level=1` has no indentation
#' @param bullet `character` type of bullet
#'
#' @export
mdBullet0 <- function(..., bullet = "-", level = 1) {
  checkmate::assertString(bullet)
  checkmate::assertInt(level, lower = 1)

  if (level > 1) {
    prefix <- paste0(rep("  ", 2 * (level - 1) - 1), collapse = "")
  } else {
    prefix <- NULL
  }
  mdPaste0(prefix, bullet, " ", ..., newlines = 2)
}


#' mdFigure
#'
#' Include a figure file with caption
#'
#' @param figureFile file of Figure
#' @param captionFile file containing caption
#' @param subfolder The folder where the file is located relative to Rmd
#' @param figureNumber number of figure used in caption prefix
#' @param footNoteFile file containing footnotes
#' @param addNewPage boolean if TRUE (default) new page is added after
#' @param customStyles list of custom styles usable for figure and table captions and footnotes
#'
#' @export
mdFigure <- function(
    figureNumber,
    figureFile,
    captionFile,
    footNoteFile = NULL,
    subfolder,
    addNewPage = TRUE,
    customStyles = list()) {
  validateMdFigureTableInputs(subfolder = subfolder,
                              importFile = figureFile,
                              captionFile = captionFile,
                              customStyles = list())
  # add figure link
  mdNewline()
  mdLink(label = "", filename = utils::URLencode(figureFile), folder = subfolder, prefix = "!")
  mdNewline()

  # figure footnote
  mdFootNote(subfolder = subfolder,
             footNoteFile = footNoteFile,
             footNoteCustomStyle = customStyles$FigureFootnote)

  #  figure caption
  mdCaption(subfolder = subfolder,
            captionFile = captionFile,
            captionPrefix = paste0("Figure", figureNumber, ":"),
            captionStyle = customStyles$FigureCaption)

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
mdLink <- function(label, filename, folder, prefix = "") {
  mdPaste0(prefix, "[", label, "](", file.path(folder, filename), ")  ")
  mdNewline()
}

#' mdTable
#'
#' Insert a table into a markdown document. Essentially a wrapper for `knitr::kable` with `format = "markdown"` and newlines.
#'
#' @param tableNumber number of table for caption prefix
#' @param tableCsv path of .csv file
#' @inheritParams mdFigure
#' @param digitsOfSignificance significance digits to display (default 3)
#' @param ... passed to `knitr::kable`
#'
#' @return `x`, invisibly
#' @export
mdTable <- function(tableNumber,
                    tableCsv,
                    captionFile,
                    footNoteFile,
                    subfolder,
                    customStyles,
                    digitsOfSignificance = 3,
                    ...) {
  validateMdFigureTableInputs(subfolder = subfolder,
                              importFile = tableCsv,
                              captionFile = captionFile,
                              customStyles = customStyles)

  #  table caption
  mdCaption(subfolder = subfolder,
            captionFile = captionFile,
            captionPrefix = paste0("Table", tableNumber, ":"),
            captionStyle = customStyles$FigureCaption)

  dt = data.table::fread(file.path(subfolder,tableCsv))
  if (!is.null(digitsOfSignificance)) {
    colsToConvert <-
      names(dt)[unlist(lapply(names(dt), function(col) {
        is.numeric(dt[[col]]) & !is.integer(dt[[col]])
      }))]
    dt[, (colsToConvert) := signif(.SD, digits = digitsOfSignificance), .SDcols = colsToConvert]
  }

  mdNewline()
  print(knitr::kable(dt, format = "markdown", ...))
  mdNewline()

  # table footnote
  mdFootNote(subfolder = subfolder,
             footNoteFile = footNoteFile,
             footNoteCustomStyle = customStyles$FigureFootnote)


  return(invisible())
}

#' add footnote lines
#'
#' @inheritParams mdFigure
#' @param footNoteCustomStyle a character describing custom style footnotes
mdFootNote <- function(subfolder,footNoteFile,footNoteCustomStyle = NULL){

  if (file.exists(file.path(subfolder, footNoteFile))) {
    footnoteLines <- readLines(file.path(subfolder, footNoteFile))

    if (length(footnoteLines) > 0) {
      mdNewline()
      for (fL in footnoteLines) {
        if (!is.null(footNoteCustomStyle) && footNoteCustomStyle != ''){
          mdPaste('<div custom-style="', footNoteCustomStyle, '">')
          mdPaste(fL)
          mdPaste("</div>")
        } else{
          mdPaste(fL)
        }
      }
    }
  }

  return(invisible())
}


#' add Caption to .Rmd
#'
#' @inheritParams mdFigure
#' @param captionPrefix 'a character which starts the caption like 'Figure 1:'
#' @param captionStyle 'custom-style for captions'
#'
mdCaption <- function(subfolder,captionFile,captionPrefix,captionStyle = NULL){
  caption <- paste(captionPrefix,
                   paste(readLines(file.path(subfolder, captionFile)), collapse = "\n"))

  if (!is.null(captionStyle) && captionStyle != '') {
    mdPaste('<div custom-style="', captionStyle, '">')
    mdPaste(caption)
    mdPaste("</div>")
  } else{
    mdPaste0("**", caption, "**")
  }
  mdNewline()

  return(invisible())
}

#' validates common inputs for `mdFigure` and `mdTable`
#'
#' @inherit mdFigure
#' @param importFile figure file or table .csv file
#'
validateMdFigureTableInputs <- function(subfolder,importFile,captionFile,customStyles){
  checkmate::assertFileExists(file.path(subfolder, importFile))
  checkmate::assertFileExists(file.path(subfolder, captionFile))
  checkmate::assertList(customStyles)
  if (length(customStyles) > 0) {
    checkmate::assertNames(names(customStyles),
                           subset.of = c(
                             "FigureCaption", "FigureFootnote",
                             "TableCaption", "TableFootnote"
                           )
    )
  }
  return(invisible())
}

#' Loop on figure keys, add figure or table
#'
#' @param keyList list of keys
#' @param subfolder sub-folder of relative to .Rmd file where figures are saved
#' @param numbersOf list with numbers of tables and figures in rmd
#' @param customStyles custom-styles to render word document
#' @param digitsOfSignificance significance digits for tables
#'
#' @return list of numbers and figures after loop
#' @export
addFiguresAndTables <- function(keyList,
                                subfolder,
                                numbersOf,
                                customStyles = list(),
                                digitsOfSignificance = 3) {
  folderFiles <- list.files(subfolder)

  dev <- ospsuite.plots::getOspsuite.plots.option(optionKey = ospsuite.plots::OptionKeys$export.device)

  for (key in keyList) {
    figureFile <- paste(key, dev, sep = ".")
    tableCsv <- paste(key, 'csv', sep = ".")
    if (figureFile %in% folderFiles) {
      numbersOf$figures <- numbersOf$figures + 1

      mdFigure(
        figureNumber = numbersOf$figures,
        figureFile = figureFile,
        captionFile = paste(key, "caption", sep = "."),
        footNoteFile = paste(key, "footnote", sep = "."),
        subfolder = subfolder,
        customStyles = customStyles
      )
    } else if(tableCsv %in% folderFiles){
      numbersOf$tables <- numbersOf$tables + 1
      mdTable(
        tableNumber = numbersOf$tables,
        tableCsv = tableCsv,
        captionFile = paste(key, "caption", sep = "."),
        footNoteFile = paste(key, "footnote", sep = "."),
        subfolder = subfolder,
        customStyles = customStyles,
        digitsOfSignificance = 3
      )

    } else {
      stop(paste('No file exists for key. There should be either',figureFile,'or',tableCsv))
    }
  }

  return(numbersOf)
}


#' Prepare a data.table for prettier printing by converting line breaks, large
#' number and percentages
#'
#' @param dt `data.frame`, will be converted to data.table
#' @param replaceLinebreaksIn `character` vector naming the columns of `dt` in
#'   which line breaks will be replaced. `!guess` selects all character columns
#'   of `dt`
#' @param replaceLinebreaksBy `string` specifying how to replace line breaks
#' @param convertToComma `character` vector naming the columns of `dt` to which
#'   `scales::comma` will be applied. `!guess` selects all integer columns of
#'   `dt`
#' @param commaAccuracy `number` specifying the rounding by `scales::comma`
#' @param convertToPercent `character` vector naming the columns of `dt` to
#'   which `scales::percent` will be applied. `!guess` selects all numeric,
#'   non-integer columns of `dt` with values between 0 and 1
#' @param percentAccuracy `number` specifying the rounding of `scales::percent`
#' @param convertToYesNo `character` vector naming the columns of `dt` which
#'   will be converted to "yes"/"no" depending on their logical status. `!guess`
#'   selects all logical columns of `dt`
#' @param skip `character` vector naming columns of `dt` that won't be modified
#'
#' @return `data.table` prepared for display
#'
#' @export
prepareDTForPrinting <- function(
    dt,
    replaceLinebreaksIn = "!guess",
    replaceLinebreaksBy = " ",
    convertToComma = "!guess",
    commaAccuracy = 1,
    convertToPercent = "!guess",
    percentAccuracy = .1,
    convertToYesNo = "!guess",
    skip = NULL) {
  checkmate::assertDataFrame(dt)
  checkmate::assertCharacter(replaceLinebreaksIn, null.ok = TRUE)
  checkmate::assertString(replaceLinebreaksBy)
  checkmate::assertCharacter(convertToComma, null.ok = TRUE)
  checkmate::assertNumber(commaAccuracy, lower = 0)
  checkmate::assertCharacter(convertToPercent, null.ok = TRUE)
  checkmate::assertNumber(percentAccuracy, lower = 0)
  checkmate::assertCharacter(convertToYesNo, null.ok = TRUE)
  checkmate::assertNumber(digits, lower = 0, null.ok = TRUE)
  checkmate::assertCharacter(skip, null.ok = TRUE)

  dt <- data.table::copy(data.table::setDT(dt))

  unassignedColumns <- setdiff(
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

  if (length(unassignedColumns) >  0) {
    if (!is.null(replaceLinebreaksIn) && all(replaceLinebreaksIn == "!guess")) {
      replaceLinebreaksIn <- names(which(sapply(dt[, ..unassignedColumns], is.character)))
    }
    if (!is.null(convertToComma) && all(convertToComma == "!guess")) {
      convertToComma <- names(which(sapply(dt[, ..unassignedColumns], is.integer)))
    }
    if (!is.null(convertToPercent) && all(convertToPercent == "!guess")) {
      convertToPercent <- names(which(
        (sapply(dt[, ..unassignedColumns], is.numeric)) &
          (!sapply(dt[, ..unassignedColumns], is.integer)) &
          (sapply(dt[, ..unassignedColumns], min, na.rm = TRUE) >= 0) &
          (sapply(dt[, ..unassignedColumns], max, na.rm = TRUE) <= 1)
      ))
    }
    if (all(convertToYesNo == "!guess")) {
      convertToYesNo <- names(which(sapply(dt[, ..unassignedColumns], is.logical)))
    }
  } else{
    variables <- c("replaceLinebreaksIn", "convertToComma", "convertToPercent", "convertToYesNo")

    for (variable in variables) {
      if (all(eval(parse(text = variable)) == "!guess")) {
        assign(variable, NULL)
      }
    }
  }

  requiredColumns <- unique(c(replaceLinebreaksIn, convertToComma, convertToPercent, convertToYesNo))

  if (!is.null(requiredColumns) && length(requiredColumns) > 0) {
    checkmate::assertNames(requiredColumns,subset.of = names(dt))
  }

  for (column in setdiff(replaceLinebreaksIn, skip)) {
    dt[, (column) := gsub("\n", replaceLinebreaksBy, get(column))]
  }

  for (column in setdiff(convertToComma, skip)) {
    useAccuracy <- ifelse(is.integer(dt[[column]]), 1, commaAccuracy)
    dt[, (column) := scales::comma(get(column), accuracy = useAccuracy)]
  }

  for (column in setdiff(convertToPercent, skip)) {
    dt[, (column) := scales::percent(get(column), accuracy = percentAccuracy)]
  }

  for (column in setdiff(convertToYesNo, skip)) {
    dt[, (column) := if_else(get(column), "yes", "no")]
  }

  return(dt)
}

