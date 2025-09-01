#' @title RmdPlotManager
#' @docType class
#' @description Manages the creation and writing of .Rmd files for plots.
#' @export
RmdPlotManager <- R6::R6Class( # nolint
  "RmdPlotManager",
  inherit = ospsuite.utils::Printable,
  cloneable = TRUE,
  # public ----
  public = list(
    #' @description
    #' Initialize a new instance of the class.
    #' @param rmdfolder Folder where the .Rmd file should be saved.
    #' @param rmdName A character string for the name of the .Rmd file (without extension).
    #' @param suppressExport A logical value indicating whether to suppress export. Default is FALSE.
    #' @param nameOfplotFunction The name of the plot-function as character.
    #' @param digitsOfSignificance Number of significant digits to display in tables.
    #'     #'
    #' @return An instance of the RmdPlotManager object.
    initialize = function(rmdfolder,
                          rmdName,
                          nameOfplotFunction,
                          suppressExport = FALSE,
                          digitsOfSignificance = 3) {
      private$.rmdfolder <- rmdfolder
      self$suppressExport <- suppressExport
      self$validateConfigTableFunction <- validateConfigTableForPlots
      self$digitsOfSignificance <- digitsOfSignificance

      if (!suppressExport) {
        if (is.null(rmdName)) {
          stop("Please provide a valid name for the .Rmd file and its subfolder.")
        }
        tools::file_path_sans_ext(rmdName)

        private$.rmdName <- rmdName

        checkmate::assert_path_for_output(file.path(private$.rmdfolder, private$.rmdName),
          overwrite = TRUE
        )

        if (!dir.exists(file.path(private$.rmdfolder, private$.rmdName))) {
          dir.create(file.path(private$.rmdfolder, private$.rmdName), recursive = TRUE)
        }
      }

      checkmate::assertCharacter(nameOfplotFunction)
      if (!exists(nameOfplotFunction, where = globalenv(), mode = "function")) {
        stop(paste("Function", nameOfplotFunction, "does not exist"))
      }
      self$plotFunction <- get(nameOfplotFunction)

      # Generate the name of the validation function based on the string name of plotFunction
      nameOfValidationFunction <- paste0(gsub("^plot", "validate", nameOfplotFunction), "Config")

      # Check if the validation function exists
      if (exists(nameOfValidationFunction, where = globalenv(), mode = "function")) {
        self$validateConfigTableFunction <- get(nameOfValidationFunction)
      } else {
        # otherwise use default function
        message("No specific plotconfiguration validation function available.")
        self$validateConfigTableFunction <- validateConfigTableForPlots
      }

      # add start of rmd
      private$.rmdLines <- private$.startRMD(rmdfolder)

      return(self)
    },
    #' @description
    #' Write the .Rmd file.
    #' @param fileName Name of the .Rmd file. If NULL, the file name will default to the subfolder name.
    #' @return NULL. This function writes the .Rmd file to the specified location and returns nothing.
    writeRmd = function(fileName = NULL) {
      if (private$.suppressExport) {
        return(invisible())
      }

      if (is.null(fileName)) fileName <- paste0(private$.rmdName, ".Rmd")

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
    #' @description
    #' Insert a heading with a specified level.
    #' @param ... Arguments passed to `mdPaste`.
    #' @param level The header level, i.e., the number of `#`s. Defaults to 1.
    #' @param newlines The number of newlines inserted after the heading. Defaults to 2.
    #' @return NULL. The function modifies the internal Rmd lines.
    addHeader = function(..., level = 1, newlines = 2) {
      private$.closeFigureKeys()
      tmp <- utils::capture.output(mdHeading(..., level = level, newlines = newlines))
      private$.rmdLines <- append(
        private$.rmdLines,
        tmp
      )
    },
    #' @description
    #' Insert line endings and start a new line.
    #' @param n Number of new lines. Defaults to 1.
    #' @return NULL. The function modifies the internal Rmd lines.
    addNewline = function(n = 1) {
      private$.closeFigureKeys()

      tmp <- utils::capture.output(mdNewline(n = n))
      private$.rmdLines <- append(
        private$.rmdLines,
        tmp
      )
    },
    #' @description
    #' Insert a page break and a newline.
    #' @return NULL. The function modifies the internal Rmd lines.
    addNewpage = function() {
      private$.closeFigureKeys()

      tmp <- utils::capture.output(mdNewpage())
      private$.rmdLines <- append(
        private$.rmdLines,
        tmp
      )
    },
    #' @description
    #' Export a list of plots.
    #' @param plotList A list of plots to export.
    #' @return NULL. The function exports the plots to the specified location.
    exportPlotList = function(plotList) {
      if (private$.suppressExport) {
        return(invisible())
      }
      for (key in names(plotList)) {
        obj <- plotList[[key]]
        caption <- attr(obj, "caption")
        if (is.null(caption)) {
          warning(paste("Caption is missing for key", caption))
          caption <- "Missing"
        }

        if (is.ggplot(obj) | "CombinedPlot" %in% class(obj)) {
          self$addAndExportFigure(
            plotObject = obj,
            caption = caption,
            figureKey = key,
            footNoteLines = attr(obj, "footNoteLines"),
            exportArguments = attr(obj, "exportArguments")
          )
        } else if (is.data.table(obj)) {
          self$addAndExportTable(
            table = obj,
            caption = caption,
            tableKey = key,
            footNoteLines = attr(obj, "footNoteLines")
          )
        }
      }
    },
    #' @description
    #' Add and export a figure with caption and footnote.
    #' @param plotObject A ggplot object to export.
    #' @param figureKey A key used to generate file names; it should be unique for the folder.
    #' @param caption A character string for the caption text.
    #' @param footNoteLines A character string for figure footnotes.
    #' @param exportArguments additional arguments passed on to ospsuite.plots::export
    #' @param ... Additional parameters passed to `ospsuite.plots::exportPlot()`.
    #' @return NULL. The function exports the figure and its metadata.
    addAndExportFigure = function(plotObject,
                                  caption,
                                  figureKey,
                                  footNoteLines = NULL,
                                  exportArguments = NULL) {
      private$.checkKeyIsUnique(key = figureKey)

      if (private$.suppressExport) {
        return(invisible())
      }
      exportArguments <- private$.checkAndAdjustExportArguments(plotObject, exportArguments)
      do.call(
        what = ospsuite.plots::exportPlot,
        args = c(
          list(
            plotObject = plotObject,
            filepath = file.path(private$.rmdfolder, private$.rmdName),
            filename = figureKey
          ),
          exportArguments
        )
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
    #' @description
    #' Add and export tables with caption and footnote.
    #' @param table A data.table object to export.
    #' @param tableKey A key used to generate filenames; it should be unique for the folder.
    #' @param caption A character string for the caption text.
    #' @param footNoteLines A character string for table footnotes.
    #' @return NULL. The function exports the table and its metadata.
    addAndExportTable = function(table,
                                 caption,
                                 tableKey,
                                 footNoteLines = NULL) {
      private$.checkKeyIsUnique(key = tableKey)
      checkmate::assertCharacter(names(table), unique = TRUE)

      if (private$.suppressExport) {
        return(invisible())
      }
      # export
      utils::write.csv(
        x = table,
        file = file.path(
          private$.rmdfolder,
          private$.rmdName,
          ospsuite.plots::validateFilename(paste0(tableKey, ".csv"))
        ),
        na = "",
        row.names = FALSE,
        fileEncoding = "UTF-8"
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
  # active ----
  active = list(
    #' @field digitsOfSignificance Digits for significance in table display.
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
    },
    #' @field configTable Configuration table.
    configTable = function(value) {
      if (missing(value)) {
        value <- private$.configTable
      } else {
        checkmate::assertDataTable(value, null.ok = TRUE)
      }
      private$.configTable <- value
    },
    #' @field plotFunction Function to create plot list.
    plotFunction = function(value) {
      if (missing(value)) {
        return(private$.plotFunction) # Return the stored function
      } else {
        checkmate::assertFunction(value) # Ensure the value is a function
        private$.plotFunction <- value # Store the function directly
      }
    },
    #' @field validateConfigTableFunction Function to read config table.
    validateConfigTableFunction = function(value) {
      if (missing(value)) {
        return(private$.validateConfigTableFunction) # Return the stored function
      } else {
        checkmate::assertFunction(value) # Ensure the value is a function
        private$.validateConfigTableFunction <- value # Store the function directly
      }
    },
    #' @field suppressExport A logical value indicating whether to suppress export.
    suppressExport = function(value) {
      if (missing(value)) {
        return(private$.suppressExport) # Return the stored function
      } else {
        checkmate::assertFlag(value) # Ensure the value is a function
        private$.suppressExport <- value # Store the function directly
      }
    }
  ),
  # private ----
  private = list(
    # the final rmds in lines
    .rmdLines = c(),
    # folder where .Rmd is saved
    .rmdfolder = NULL,
    # subfolder of rmdfolder where figures are saved
    .rmdName = NULL,
    # digits for table display
    .digitsOfSignificance = 3,
    # boolean to tell, if last entry was a Key
    .keyCollectionIsOpen = FALSE,
    # boolean to tell, if last entry was a Key
    .listOfKeys = c(),
    # boolean to tell, if last entry was a Key
    .listOfALLKeys = c(),
    # configuration table use to build Object
    .configTable = NULL,
    # Identifier observed data use to build Object
    .dataObserved = c(),
    # boolean, if true export is suppressed
    .suppressExport = FALSE,
    # plotfunction function to createPlotList
    .plotFunction = NULL,
    # function to readConfigtable
    .validateConfigTableFunction = NULL,
    # function to initialize rmdLines
    .startRMD = function(rmdfolder) {
      return(c(
        startRmd(),
        "  ",
        paste0("```{r setup_", private$.rmdName, ", include=FALSE}"),
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
        paste0('            subfolder = "', private$.rmdName, '",'),
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
        con = file.path(private$.rmdfolder, private$.rmdName, paste0(key, extension))
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
    },
    # adjust height if necessary
    .checkAndAdjustExportArguments = function(plotObject, exportArguments) {
      if (is.null(exportArguments)) {
        return(exportArguments)
      }

      if (!is.null(exportArguments[["heightToWidth"]]) & is.null(exportArguments[["height"]])) {
        if (is.null(exportArguments[["width"]])) {
          exportArguments[["width"]] <- getOspsuite.plots.option(optionKey = OptionKeys$export.width)
        }
        width <- exportArguments[["width"]]

        if ("CombinedPlot" %in% class(plotObject)) {
          dimensions <- calculatePlotDimensions(plotObject$plotObject, width)
        } else {
          dimensions <- calculatePlotDimensions(plotObject, width)
        }

        exportArguments[["height"]] <- exportArguments[["heightToWidth"]] * width + dimensions$heightOffset
        exportArguments[["heightToWidth"]] <- NULL
      }

      return(exportArguments)
    }
  )
)
