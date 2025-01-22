#' @title RmdPlotManager
#' @docType class
#' @description An object to create a .Rmd file during a plot function
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
    #' @param suppressExport A logical value indicating whether to suppress export. Default is FALSE.
    #' @return An instance of the RmdPlotManager object.
    initialize = function(rmdfolder, suppressExport = FALSE) {
      private$.rmdfolder <- rmdfolder
      self$suppressExport <- suppressExport
      self$validateConfigTableFunction  <- validateConfigTableForPlots

      return(self)
    },
    #' @description
    #' Evaluate and select a plotting function based on the provided
    #' `functionKey` or `plotFunction`. Constructs a subfolder for output
    #' based on the provided `subfolder` and `configTableSheet`.
    #' @param subfolder A character string representing the name of the subfolder
    #'   where output files will be stored. If not provided, it will be constructed
    #'   from `configTableSheet`.
    #' @param functionKey A character string that specifies which plotting function
    #'   to use. Valid options are "TimeProfiles", "PK_Boxwhisker_Absolute",
    #'   "PK_RatioForestByAggregation", and "PK_RatioForestByAggregation".
    #' @param plotFunction A function to be used for plotting if `functionKey` is
    #'   not provided. Must be a valid R function.
    #' @param configTableSheet A character string representing the name of the
    #'   configuration table sheet. Required if `subfolder` is not provided.
    evaluateFunctionkey = function( subfolder,functionKey = NULL,plotFunction = NULL,configTableSheet = NULL){

      if (is.null(plotFunction) & is.null(functionKey)){
        stop('Please provide either plotFunction or valid functionKey')
      }

      if (is.null(configTableSheet) & is.null(subfolder)){
        stop('Please provide name of subfolder')
      }

      # Select the plot function based on the functionKey
      subfolderOffset = ''
      if (is.null(functionKey)) {
        checkmate::assertFunction(plotFunction)
        self$plotFunction =plotFunction
      } else {
        switch(functionKey,
               TimeProfiles = {
                 self$plotFunction = plotTimeProfilePanels
                 self$validateConfigTableFunction = validateConfigTableForTimeProfiles
               },
               PK_Boxwhisker = {
                 self$plotFunction = plotPKBoxwhisker
                 self$validateConfigTableFunction = validatePKBoxwhiskerConfigTable
               },
               PK_RatioForestByAggregation = {
                 self$plotFunction = plotPKRatioForestPlotByRatioAggregation
               },
               PK_RatioForestByAggregation = {
                 self$plotFunction = plotPKRatioForestPlotByBoostrapping
               },
               stop('functionKey is unknown. Must be one of "TimeProfiles", "PK_Boxwhisker_Absolute",
                    "PK_RatioForestByAggregation", PK_RatioForestByAggregation')

        )
      }

      # construct and create subfolder
      if (is.null(subfolder)) subfolder <- configTableSheet

      private$.subfolder <- subfolder

      checkmate::assert_path_for_output(file.path(private$.rmdfolder, private$.subfolder),
                                        overwrite = TRUE)

      if (!dir.exists(file.path(private$.rmdfolder, private$.subfolder))) {
        dir.create(file.path(private$.rmdfolder, private$.subfolder), recursive = TRUE)
      }

      # add start of rmd
      private$.rmdLines <- private$.startRMD(rmdfolder)

    },
    #' @description
    #' Write the .Rmd file.
    #' @param fileName Name of the .Rmd file. If NULL, the file name will default to the subfolder name.
    #' @return NULL. The function writes the .Rmd file to the specified location.
    writeRmd = function(fileName = NULL) {
      if (private$.suppressExport) return(invisible())

      if (is.null(fileName)) fileName = paste0(private$.subfolder, ".Rmd")

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
    exportPlotList = function(plotList){

      if (private$.suppressExport) {
        return(invisible())
      }

      for (key in names(plotList)){
        obj <- plotList[[key]]
        caption <- attr(obj,'caption')
        if (is.null(caption)){
          warning(paste('Caption is missing for key',caption))
          caption <- 'Missing'
        }

        if (is.ggplot(obj)){
          self$addAndExportFigure(plotObject = obj,
                                  caption = caption,
                                  figureKey = key,
                                  footNoteLines = attr(obj,'footNoteLines'),
                                  exportArguments = attr(obj,'exportArguments'))
        } else if (is.data.table(obj)){
          self$addAndExportTable(table = obj,
                                  caption = caption,
                                  tableKey = key,
                                  footNoteLines = attr(obj,'footNoteLines'))

        }
      }


    },
    #' @description
    #' Add and export a figure with caption and footnote.
    #' @param plotObject A ggplot object to export.
    #' @param figureKey A key used to generate file names; it should be unique for the folder.
    #' @param caption A character string for the caption text.
    #' @param footNoteLines A character string for figure footnotes.
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

      do.call(what = ospsuite.plots::exportPlot,
              args = c(list(
                plotObject = plotObject,
                filepath = file.path(private$.rmdfolder, private$.subfolder),
                filename = figureKey),
                exportArguments)
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

      if (private$.suppressExport) return(invisible())
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
    #' @return An integer representing the number of significant digits.
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
    #' @return A data.table representing the configuration table.
    configTable = function(value) {
      if (missing(value)) {
        value <- private$.configTable
      } else {
        checkmate::assertDataTable(value,null.ok = TRUE)
      }
      private$.configTable <- value
    },
    #' @field dataObserved Observed data used for this Rmd.
    #' @return A data.table containing observed data.
    dataObserved = function(value) {
      if (missing(value)) {
        value <- private$.dataObserved
      } else {
        value <- unique(rbind(private$.dataObserved,
                       value))
      }
      private$.dataObserved <- value
    },
    #' @field plotFunction Function to create plot list.
    #' @return A function used to generate plots.
    plotFunction = function(value) {
      if (missing(value)) {
        return(private$.plotFunction)  # Return the stored function
      } else {
        checkmate::assertFunction(value)  # Ensure the value is a function
        private$.plotFunction <- value  # Store the function directly
      }
    },
    #' @field validateConfigTableFunction Function to read config table.
    #' @return A function used to validate the configuration table.
    validateConfigTableFunction = function(value) {
      if (missing(value)) {
        return(private$.validateConfigTableFunction)  # Return the stored function
      } else {
        checkmate::assertFunction(value)  # Ensure the value is a function
        private$.validateConfigTableFunction <- value  # Store the function directly
      }
    },
    #' @field suppressExport A logical value indicating whether to suppress export.
    #' @return A logical value indicating if export is suppressed.
    suppressExport = function(value) {
      if (missing(value)) {
        return(private$.suppressExport)  # Return the stored function
      } else {
        checkmate::assertFlag(value)  # Ensure the value is a function
        private$.suppressExport <- value  # Store the function directly
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
    .subfolder = NULL,
    # digits for table display
    .digitsOfSignificance = 3,
    # boolean to tell, if last entry was a Key
    .keyCollectionIsOpen = FALSE,
    # boolean to tell, if last entry was a Key
    .listOfKeys = c(),
    # boolean to tell, if last entry was a Key
    .listOfALLKeys = c(),
    # ConfigurationTable use to build Object
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
