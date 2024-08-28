#' @title RmdContainer
#' @docType class
#' @description An object to create a .Rmd file during a plot function
#' @export
RmdContainer <- R6::R6Class( # nolint
  "RmdContainer",
  inherit = ospsuite.utils::Printable,
  cloneable = TRUE,
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @param rmdfolder folder where rmd file should be saved
    #' @param subfolder folder name relative to rmd folder where figures and tables are exported
    #'
    #' @returns RmdContainer object
    initialize = function(rmdfolder, subfolder) {
      private$.rmdfolder <- rmdfolder
      private$.subfolder <- subfolder
      checkmate::assert_path_for_output(file.path(private$.rmdfolder, private$.subfolder),
        overwrite = TRUE
      )

      if (!dir.exists(file.path(private$.rmdfolder, private$.subfolder))) {
        dir.create(file.path(private$.rmdfolder, private$.subfolder), recursive = TRUE)
      }

      private$.rmdLines <- private$.startRMD(rmdfolder)

      return(self)
    },
    #' writes the .Rmd File
    #'
    #' @param fileName name of .Rmd file
    writeRmd = function(fileName) {
      checkmate::assertPathForOutput(fileName, extension = "Rmd", overwrite = TRUE)
      if (basename(fileName) != fileName) {
        stop("Please insert fileName as basename, File will be saved in folder defined by class object")
      }

      private$.closeFigureKeys()
      writeLines(
        text = private$.rmdLines,
        con = file.path(private$.rmdfolder, fileName),
        sep = "\n"
      )

      return(invisible())
    },
    #' Insert a heading with a specified level.
    #'
    #' @param ... passed to mdPaste
    #' @param level The header level, i.e. the number of `#`s. Defaults to 1.
    #' @param newlines The number of newlines inserted after the heading. Defaults to 2.
    addHeader = function(..., level = 1, newlines = 2) {
      private$.closeFigureKeys()
      tmp <- utils::capture.output(mdHeading(..., level = level, newlines = newlines))
      private$.rmdLines <- append(
        private$.rmdLines,
        tmp
      )
    },
    #' line end and start a new line.
    #'
    #' @param n Number of new lines. Defaults to 1.
    addNewline = function(n = 1) {
      private$.closeFigureKeys()

      tmp <- utils::capture.output(mdNewline(n = n))
      private$.rmdLines <- append(
        private$.rmdLines,
        tmp
      )
    },
    #' @description Insert a page break and a newline.
    addNewpage = function() {
      private$.closeFigureKeys()

      tmp <- utils::capture.output(mdNewpage())
      private$.rmdLines <- append(
        private$.rmdLines,
        tmp
      )
    },
    #' @description adds and exports figure with caption and footnote
    #'
    #' @param plotObject ggplot object to export
    #'
    #' @param figureKey key which is used to generate file names should be unique for folder
    #' @param caption character caption text
    #' @param footNoteLines character text of figure footnotes
    #' @param ... parameters passed to `ospsuite.plots::exportPlot()`
    #'
    addAndExportFigure = function(plotObject,
                                  caption,
                                  figureKey,
                                  footNoteLines = NULL,
                                  ...) {
      private$.checkKeyIsUnique(key = figureKey)

      dev <- ospsuite.plots::getOspsuite.plots.option(optionKey = ospsuite.plots::OptionKeys$export.device)
      ospsuite.plots::exportPlot(
        plotObject = plotObject,
        filepath = file.path(private$.rmdfolder, private$.subfolder),
        filename = paste0(figureKey),
        ...
      )

      private$.exportLines(
        textLines = caption,
        key = figureKey,
        extension = ".caption"
      )

      private$.exportLines(
        textLines = footNoteLines,
        key = figureKey,
        extension = ".footnote"
      )

      private$.addKeyToList(key = figureKey)
    },
    #' @description adds and exports tables with caption and footnote
    #'
    #' @param table data.table object to export
    #'
    #' @param tableKey key which is used to generate filenames should be unique for folder
    #' @param caption character caption text
    #' @param footNoteLines character text of figure footnotes
    #' @param ... parameters passed to `utils::write.csv`
    #'
    addAndExportTable = function(table,
                                 caption,
                                 tableKey,
                                 footNoteLines = NULL,
                                 ...) {
      private$.checkKeyIsUnique(key = tableKey)
      checkmate::assertCharacter(names(table), unique = TRUE)

      # export
      utils::write.csv(
        x = table,
        file = file.path(
          private$.rmdfolder,
          private$.subfolder,
          ospsuite.plots::validateFilename(paste0(tableKey, ".csv"))
        ),
        na = "",
        row.names = FALSE,
        fileEncoding = "UTF-8", ...
      )

      private$.exportLines(
        textLines = caption,
        key = tableKey,
        extension = ".caption"
      )

      private$.exportLines(
        textLines = footNoteLines,
        key = tableKey,
        extension = ".footnote"
      )

      private$.addKeyToList(key = tableKey)
    }
  ),
  active = list(
    #' @field digitsOfSignificance digits for significance in table display
    digitsOfSignificance = function(value) {
      if (missing(value)) {
        value <- private$.digitsOfSignificance
      } else {
        checkmate::assertInt(value, lower = 1, .var.name = digitsOfSignificance)
        if (private$.digitsOfSignificance != value) {
          private$.closeFigureKeys()
        }
      }
      private$.digitsOfSignificance <- value
    }
  ),
  private = list(
    # the final rmds in lines
    .rmdLines = c(),
    # folder where .Rmd is saved
    .rmdfolder = NULL,
    # subfolder of rmdfolder where figures are saved
    .subfolder = NULL,
    # digits for table display
    .digitsOfSignificance = 3,
    # boolean to tell, if last entry was a Key
    .keyCollectionIsOpen = FALSE,
    # boolean to tell, if last entry was a Key
    .listOfKeys = c(),
    # boolean to tell, if last entry was a Key
    .listOfALLKeys = c(),
    # function to initialize rmdLines
    .startRMD = function(rmdfolder) {
      return(c(
        startRmd(),
        "  ",
        paste0("```{r setup_", private$.subfolder, ", include=FALSE}"),
        'if (!exists("setupDone"))',
        '   knitr::opts_chunk$set(echo = FALSE,warning = FALSE,results = "asis",error = FALSE,message = FALSE)',
        "```",
        "  ",
        "```{r}",
        "numbersOf <- list(figures = 0,",
        "         tables = 0)",
        "```"
      ))
    },
    # switches keyCollectionIsOpen to FALSE, and call functions for figure settings
    .closeFigureKeys = function() {
      if (!private$.keyCollectionIsOpen) {
        return()
      }

      tmp <- c(
        "  ",
        "```{r}",
        paste0('keyList <- c("', paste(private$.listOfKeys, collapse = '",\n"'), '")'),
        " ",
        "numbersOf <- addFiguresAndTables(keyList = keyList,",
        paste0('            subfolder = "', private$.subfolder, '",'),
        "            numbersOf = numbersOf,",
        "            customStyles = params$customStyles,",
        paste0("            digitsOfSignificance = ", self$digitsOfSignificance, ")"),
        "```",
        "  "
      )
      private$.rmdLines <- append(
        private$.rmdLines,
        tmp
      )

      private$.keyCollectionIsOpen <- FALSE
      private$.listOfKeys <- c()

      return(invisible())
    },
    # writes caption and footnotes
    .exportLines = function(textLines, key, extension) {
      if (is.null(textLines)) {
        return(invisible())
      }
      writeLines(
        text = textLines,
        con = file.path(private$.rmdfolder, private$.subfolder, paste0(key, extension))
      )

      return(invisible())
    },
    # add key to lists
    .addKeyToList = function(key) {
      private$.keyCollectionIsOpen <- TRUE
      private$.listOfKeys <- append(private$.listOfKeys, key)
      private$.listOfALLKeys <- append(private$.listOfKeys, key)
    },
    # only export if key is unique
    .checkKeyIsUnique = function(key) {
      if (key %in% private$.listOfALLKeys) {
        stop(paste0('key "', key, '" was already added. The figure and table keys must be unique'))
      }
    }
  )
)
