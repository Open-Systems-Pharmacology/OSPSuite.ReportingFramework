#' mdNewline
#' @param n Number of new lines (default 1).
#' @export
#' @family markdown helper function
mdNewline <- function(n = 1) {
  for (i in seq_len(n)) {
    cat("  \n")
  }
  invisible(NULL)
}

#' mdPaste
#' @param ... Elements to be `paste`d.
#' @param sep Separator (default `" "`).
#' @param collapse Collapse argument.
#' @param newlines Newlines afterwards (default 1).
#' @export
#' @family markdown helper function
mdPaste <- function(..., sep = " ", collapse = NULL, newlines = 1) {
  cat(paste(..., sep = sep, collapse = collapse))
  mdNewline(newlines)
  invisible(NULL)
}

#' mdPaste0
#' @param ... Elements to be `paste0`d.
#' @param collapse Collapse argument.
#' @param newlines Newlines afterwards (default 1).
#' @export
#' @family markdown helper function
mdPaste0 <- function(..., collapse = NULL, newlines = 1) {
  cat(paste0(..., collapse = collapse))
  mdNewline(newlines)
  invisible(NULL)
}


#' mdNewpage
#' Insert page break and newline.
#' @export
#' @family markdown helper function
mdNewpage <- function() {
  mdNewline(2)
  mdPaste("\\newpage", newlines = 2)
}

#' mdHeading
#' Insert heading with specified level (1-6).
#' @param ... Content passed to mdPaste.
#' @param level Header level (default 1).
#' @param newlines Newlines after heading (default 2).
#' @export
#' @family markdown helper function
mdHeading <- function(..., level = 1, newlines = 2) {
  checkmate::assert_int(level, lower = 1, upper = 6)
  mdNewline(3)
  mdPaste(paste0(rep("#", level), collapse = ""), ..., newlines = newlines)
}

#' Insert bulleted item with indentation.
#' @param ... Bullet content.
#' @param bullet Bullet character (default "-").
#' @param level Indentation level (default 1).
#' @export
#' @family markdown helper function
mdBullet <- function(..., bullet = "-", level = 1) {
  checkmate::assertString(bullet)
  checkmate::assertInt(level, lower = 1)
  prefix <- if (level > 1) {
    paste0(rep("  ", 2 * (level - 1) - 1), collapse = "")
  } else {
    NULL
  }
  mdPaste(prefix, bullet, ..., newlines = 2)
}

#' Insert bulleted item using paste0.
#' @rdname mdBullet
#' @export
mdBullet0 <- function(..., bullet = "-", level = 1) {
  checkmate::assertString(bullet)
  checkmate::assertInt(level, lower = 1)
  prefix <- if (level > 1) {
    paste0(rep("  ", 2 * (level - 1) - 1), collapse = "")
  } else {
    NULL
  }
  mdPaste0(prefix, bullet, " ", ..., newlines = 2)
}


#' Include figure with caption and footnote.
#' @param figureNumber Figure number for caption prefix.
#' @param figureFile Figure file name.
#' @param captionFile Caption file.
#' @param footNoteFile Footnote file (optional).
#' @param subfolder Folder relative to Rmd.
#' @param addNewPage Add page break after (default TRUE).
#' @param customStyles Custom styles list.
#' @export
#' @family markdown helper function
mdFigure <- function(
  figureNumber,
  figureFile,
  captionFile,
  footNoteFile = NULL,
  subfolder,
  addNewPage = TRUE,
  customStyles = list()
) {
  validateMdFigureTableInputs(subfolder, figureFile, captionFile, list())
  mdNewline()
  mdLink("", utils::URLencode(figureFile), subfolder, prefix = "!")
  mdNewline()
  mdFootNote(subfolder, footNoteFile, customStyles$FigureFootnote)
  mdCaption(
    subfolder,
    captionFile,
    paste0("Figure ", figureNumber, ":"),
    customStyles$FigureCaption
  )
  if (addNewPage) {
    mdNewpage()
  }
  invisible()
}


#' Insert markdown link.
#' @param label Link display text.
#' @param filename File to link to.
#' @param folder Folder location.
#' @param prefix Optional prefix (default "").
#' @export
#' @family markdown helper function
mdLink <- function(label, filename, folder, prefix = "") {
  mdPaste0(prefix, "[", label, "](", file.path(folder, filename), ")  ")
  mdNewline()
}

#' Insert table from CSV with caption.
#' @param tableNumber Table number for prefix.
#' @param tableCsv CSV file path.
#' @param captionFile Caption file.
#' @param footNoteFile Footnote file.
#' @param subfolder Folder location.
#' @param customStyles Custom styles list.
#' @param digitsOfSignificance Significant digits (default 3).
#' @param addNewPage Add page break after (default TRUE).
#' @param ... Passed to `knitr::kable`.
#' @export
#' @family markdown helper function
mdTable <- function(
  tableNumber,
  tableCsv,
  captionFile,
  footNoteFile,
  subfolder,
  customStyles,
  digitsOfSignificance = 3,
  addNewPage = TRUE,
  ...
) {
  validateMdFigureTableInputs(subfolder, tableCsv, captionFile, customStyles)
  mdCaption(
    subfolder,
    captionFile,
    paste0("Table", tableNumber, ":"),
    customStyles$FigureCaption
  )
  dt <- data.table::fread(file.path(subfolder, tableCsv))
  if (!is.null(digitsOfSignificance)) {
    colsToConvert <- names(dt)[unlist(lapply(names(dt), function(col) {
      is.numeric(dt[[col]]) & !is.integer(dt[[col]])
    }))]
    dt[,
      (colsToConvert) := signif(.SD, digits = digitsOfSignificance),
      .SDcols = colsToConvert
    ]
  }
  mdNewline()
  print(knitr::kable(dt, format = "markdown", ...))
  mdNewline()
  mdFootNote(subfolder, footNoteFile, customStyles$FigureFootnote)
  if (addNewPage) {
    mdNewpage()
  }
  invisible()
}

#' Add footnote lines from file.
#' @param subfolder Folder location.
#' @param footNoteFile Footnote file.
#' @param footNoteCustomStyle Custom style (optional).
#' @export
#' @family markdown helper function
mdFootNote <- function(subfolder, footNoteFile, footNoteCustomStyle = NULL) {
  fpath <- file.path(subfolder, footNoteFile)
  if (file.exists(fpath)) {
    footnoteLines <- readLines(fpath)
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
  invisible()
}


#' Add caption with optional custom style.
#' @param subfolder Folder location.
#' @param captionFile Caption file.
#' @param captionPrefix Prefix (e.g., "Figure 1:").
#' @param captionStyle Custom style (optional).
#' @export
#' @family markdown helper function
mdCaption <- function(
  subfolder,
  captionFile,
  captionPrefix,
  captionStyle = NULL
) {
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
  invisible()
}

#' Validate figure/table input files and styles.
#' @param subfolder Folder location.
#' @param importFile Figure/table file.
#' @param captionFile Caption file.
#' @param customStyles Custom styles list.
#' @keywords internal
validateMdFigureTableInputs <- function(
  subfolder,
  importFile,
  captionFile,
  customStyles
) {
  checkmate::assertFileExists(file.path(subfolder, importFile))
  checkmate::assertFileExists(file.path(subfolder, captionFile))
  checkmate::assertList(customStyles)
  if (length(customStyles) > 0) {
    checkmate::assertNames(
      names(customStyles),
      subset.of = c(
        "FigureCaption",
        "FigureFootnote",
        "TableCaption",
        "TableFootnote"
      )
    )
  }
  invisible()
}

#' Add figures and tables from keyTypes or keyList.
#' @param keyTypes Named list of figure/table types.
#' @param subfolder Subfolder relative to Rmd.
#' @param numbersOf List with figure/table counts.
#' @param customStyles Custom styles list.
#' @param digitsOfSignificance Digits for tables (default 3).
#' @param keyList Fallback list of keys.
#' @return Updated numbersOf list.
#' @export
#' @family markdown helper function
addFiguresAndTables <- function(
  keyTypes = NULL,
  subfolder,
  numbersOf,
  customStyles = list(),
  digitsOfSignificance = 3,
  keyList = NULL
) {
  dev <- ospsuite.plots::getOspsuite.plots.option(
    optionKey = ospsuite.plots::OptionKeys$export.device
  )

  if (!is.null(keyTypes)) {
    for (key in names(keyTypes)) {
      if (keyTypes[[key]] == "figure") {
        numbersOf$figures <- numbersOf$figures + 1
        mdFigure(
          figureNumber = numbersOf$figures,
          figureFile = paste(key, dev, sep = "."),
          captionFile = paste(key, "caption", sep = "."),
          footNoteFile = paste(key, "footnote", sep = "."),
          subfolder = subfolder,
          customStyles = customStyles
        )
      } else {
        numbersOf$tables <- numbersOf$tables + 1
        mdTable(
          tableNumber = numbersOf$tables,
          tableCsv = paste(key, "csv", sep = "."),
          captionFile = paste(key, "caption", sep = "."),
          footNoteFile = paste(key, "footnote", sep = "."),
          subfolder = subfolder,
          customStyles = customStyles,
          digitsOfSignificance = digitsOfSignificance
        )
      }
    }
  } else {
    # fallback: filesystem probing for .Rmd files generated before keyTypes was introduced
    folderFiles <- list.files(subfolder)
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
          digitsOfSignificance = digitsOfSignificance
        )
      } else {
        stop(messages$errorutilitiesmarkdownL4())
      }
    }
  }

  return(numbersOf)
}


#' Create standard Qmd header with params.
#' @param title Report title (default "Report").
#' @return Character vector of header lines.
#' @keywords internal
startQmd <- function(title = "Report") {
  c(
    "---",
    paste0('title: "', title, '"'),
    "format: docx",
    "params:",
    "  customStyles:",
    "    FigureCaption: ~",
    "    FigureFootnote: ~",
    "    TableCaption: ~",
    "    TableFootnote: ~",
    "---",
    " ",
    "```{r}",
    "#| include: false",
    'knitr::opts_chunk$set(echo = FALSE, warning = FALSE, results = "asis", error = FALSE, message = FALSE)',
    "```",
    " "
  )
}


#' Merge multiple Qmd files into one.
#' @param newName Output file name (default "appendix").
#' @param title Output title (default "Appendix").
#' @param sourceRmds Source Qmd files to include.
#' @param projectConfiguration ProjectConfiguration object.
#' @export
#' @family functions called by workflow script
mergeRmds <- function(
  newName = "appendix",
  title = "Appendix",
  sourceRmds = c(
    "Demographics",
    "TimeProfile",
    "PKParameter",
    "DDIRatio",
    "myFigures"
  ),
  projectConfiguration
) {
  checkmate::assertCharacter(newName, len = 1)
  checkmate::assertCharacter(title, len = 1)
  checkmate::assertCharacter(sourceRmds, min.len = 1, unique = TRUE)

  # Check for any other extensions the .Rmd
  if (any(grepl("\\.[^.qmd]*$", sourceRmds))) {
    stop(messages$errorutilitiesmarkdownL4X())
  } else {
    sourceRmds <- ifelse(
      grepl("\\.qmd$", sourceRmds),
      sourceRmds,
      paste0(sourceRmds, ".qmd")
    )
  }
  if (any(grepl("\\.[^.qmd]*$", newName))) {
    stop(messages$errorutilitiesmarkdownL4XX())
  } else {
    newName <- ifelse(
      grepl("\\.qmd$", newName),
      newName,
      paste0(newName, ".qmd")
    )
  }
  checkmate::assertFileExists(file.path(
    projectConfiguration$outputFolder,
    sourceRmds
  ))

  rmdTxt <- startQmd(title = title)

  for (sourceRmd in sourceRmds) {
    rmdTxt <- c(
      rmdTxt,
      " ",
      paste0("{{< include ", sourceRmd, " >}}")
    )
  }

  writeLines(
    text = rmdTxt,
    con = file.path(projectConfiguration$outputFolder, newName),
    sep = "\n"
  )

  return(invisible())
}
