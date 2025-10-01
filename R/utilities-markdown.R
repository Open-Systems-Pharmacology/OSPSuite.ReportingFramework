#' mdNewline
#'
#' `cat` a line end and start a new line.
#'
#' @param n Number of new lines. Defaults to 1.
#'
#' @export
#' @family markdown helper function
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
#' @family markdown helper function
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
#' @family markdown helper function
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
#' @family markdown helper function
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
#' @family markdown helper function
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
#' @family markdown helper function
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
#' @family markdown helper function
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
#' @family markdown helper function
mdFigure <- function(
    figureNumber,
    figureFile,
    captionFile,
    footNoteFile = NULL,
    subfolder,
    addNewPage = TRUE,
    customStyles = list()) {
  validateMdFigureTableInputs(
    subfolder = subfolder,
    importFile = figureFile,
    captionFile = captionFile,
    customStyles = list()
  )
  # add figure link
  mdNewline()
  mdLink(label = "", filename = utils::URLencode(figureFile), folder = subfolder, prefix = "!")
  mdNewline()

  # figure footnote
  mdFootNote(
    subfolder = subfolder,
    footNoteFile = footNoteFile,
    footNoteCustomStyle = customStyles$FigureFootnote
  )

  #  figure caption
  mdCaption(
    subfolder = subfolder,
    captionFile = captionFile,
    captionPrefix = paste0("Figure ", figureNumber, ":"),
    captionStyle = customStyles$FigureCaption
  )

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
#' @family markdown helper function
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
#' @family markdown helper function
mdTable <- function(tableNumber,
                    tableCsv,
                    captionFile,
                    footNoteFile,
                    subfolder,
                    customStyles,
                    digitsOfSignificance = 3,
                    addNewPage = TRUE,
                    ...) {
  validateMdFigureTableInputs(
    subfolder = subfolder,
    importFile = tableCsv,
    captionFile = captionFile,
    customStyles = customStyles
  )

  #  table caption
  mdCaption(
    subfolder = subfolder,
    captionFile = captionFile,
    captionPrefix = paste0("Table", tableNumber, ":"),
    captionStyle = customStyles$FigureCaption
  )

  dt <- data.table::fread(file.path(subfolder, tableCsv))
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
  mdFootNote(
    subfolder = subfolder,
    footNoteFile = footNoteFile,
    footNoteCustomStyle = customStyles$FigureFootnote
  )

  if (addNewPage) mdNewpage()

  return(invisible())
}

#' add footnote lines
#'
#' @inheritParams mdFigure
#' @param footNoteCustomStyle a character describing custom style footnotes
#' @family markdown helper function
mdFootNote <- function(subfolder, footNoteFile, footNoteCustomStyle = NULL) {
  if (file.exists(file.path(subfolder, footNoteFile))) {
    footnoteLines <- readLines(file.path(subfolder, footNoteFile))

    if (length(footnoteLines) > 0) {
      mdNewline()
      for (fL in footnoteLines) {
        if (!is.null(footNoteCustomStyle) && footNoteCustomStyle != "") {
          mdPaste('<div custom-style="', footNoteCustomStyle, '">')
          mdPaste(fL)
          mdPaste("</div>")
        } else {
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
#' @export
#' @family markdown helper function
mdCaption <- function(subfolder, captionFile, captionPrefix, captionStyle = NULL) {
  caption <- paste(
    captionPrefix,
    paste(readLines(file.path(subfolder, captionFile)), collapse = "\n")
  )

  if (!is.null(captionStyle) && captionStyle != "") {
    mdPaste('<div custom-style="', captionStyle, '">')
    mdPaste(caption)
    mdPaste("</div>")
  } else {
    mdPaste0("**", caption, "**")
  }
  mdNewline()

  return(invisible())
}

#' validates common inputs for `mdFigure` and `mdTable`
#'
#' @inherit mdFigure
#' @param importFile figure file or table .csv file
#' @keywords internal
validateMdFigureTableInputs <- function(subfolder, importFile, captionFile, customStyles) {
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
#' @family markdown helper function
addFiguresAndTables <- function(keyList,
                                subfolder,
                                numbersOf,
                                customStyles = list(),
                                digitsOfSignificance = 3) {
  folderFiles <- list.files(subfolder)

  dev <- ospsuite.plots::getOspsuite.plots.option(optionKey = ospsuite.plots::OptionKeys$export.device)

  for (key in keyList) {
    figureFile <- paste(key, dev, sep = ".")
    tableCsv <- paste(key, "csv", sep = ".")
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
    } else if (tableCsv %in% folderFiles) {
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
      stop(paste("No file exists for key. There should be either", figureFile, "or", tableCsv))
    }
  }

  return(numbersOf)
}


#' creates startlines of Rmd file
#'
#' @param title title of report
#'
#' @return character with startlines
#' @keywords internal
startRmd <- function(title = "Report") {
  return(c(
    "---",
    paste0('title: "', title, '"'),
    "output:",
    "  word_document",
    "params:",
    "  customStyles:",
    "    value:",
    "      FigureCaption: NULL",
    "      FigureFootnote: NULL",
    "      TableCaption: NULL",
    "      TableFootnote: NULL",
    "---",
    " "
  ))
}


#' Creates Rmd file which include specified  .Rmd s
#'
#' @param newName new Name of .Rmd
#' @param title Title of new Rmd
#' @param sourceRmds list of .rmds to include
#' @param projectConfiguration Object of class `ProjectConfiguration` containing information on paths and file names
#'
#' @export
#' @family functions called by workflow script
mergeRmds <- function(
    newName = "appendix",
    title = "Appendix",
    sourceRmds = c("Demographics", "TimeProfile", "PKParameter", "DDIRatio", "myFigures"),
    projectConfiguration) {
  checkmate::assertCharacter(newName, len = 1)
  checkmate::assertCharacter(title, len = 1)
  checkmate::assertCharacter(sourceRmds, min.len = 1, unique = TRUE)

  # Check for any other extensions the .Rmd
  if (any(grepl("\\.[^.Rmd]*$", sourceRmds))) {
    stop("Error: One or more elements of sourceRmds have an extension other than .Rmd.")
  } else {
    # Add.Rmd extension to elements that don't have it
    sourceRmds <- ifelse(grepl("\\.Rmd$", sourceRmds), sourceRmds, paste0(sourceRmds, ".Rmd"))
  }
  if (any(grepl("\\.[^.Rmd]*$", newName))) {
    stop("Error: NewName has an extension other than .Rmd.")
  } else {
    # Add.Rmd extension to elements that don't have it
    newName <- ifelse(grepl("\\.Rmd$", newName), newName, paste0(newName, ".Rmd"))
  }
  checkmate::assertFileExists(file.path(projectConfiguration$outputFolder, sourceRmds))

  rmdTxt <- c(
    startRmd(title = title),
    "  ",
    "```{r setup, include=FALSE}",
    'knitr::opts_chunk$set(echo = FALSE,warning = FALSE,results = "asis",error = FALSE,message = FALSE)',
    "setupDone <<- TRUE",
    "```",
    "  "
  )


  for (sourceRmd in sourceRmds) {
    rmdTxt <- c(
      rmdTxt,
      " ",
      paste0('```{r child="', sourceRmd, '"}'),
      " "
    )
  }

  writeLines(
    text = rmdTxt,
    con = file.path(projectConfiguration$outputFolder, newName),
    sep = "\n"
  )

  return(invisible())
}
