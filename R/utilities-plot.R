#' Title
#'
#' @template projectConfig
#' @param functionKey keyword which select plot function, if NULL plotfunction is needed
#' @param plotFunction function which is used for plotting, if not null, it overwrites functionKey selection
#' @param subfolder subfolder where results are filed, if functionKey is used, as default value the sheetname of the plotconfiguartion is used
#' @param inputs additionally inputs for the
#'
#' @export
runPlot <- function(projectConfiguration,
                    functionKey = c("TimeProfile_Panel"),
                    plotFunction = NULL,
                    subfolder = NULL,
                    inputs = list()) {
  # validate inputs
  if (is.null(plotFunction)) {
    functionKey <- match.arg(functionKey)
    plotFunction <- getFunctionByKey(functionKey)
    if (is.null(subfolder)) subfolder <- inputs$configTableSheet
  }
  checkmate::assertFunction(plotFunction)
  checkmate::assertCharacter(subfolder, null.ok = FALSE)

  message(paste0('Start plotting "', subfolder, '"'))

  resultDirectory <- file.path(projectConfiguration$outputFolder, subfolder)
  if (!dir.exists(resultDirectory)) {
    dir.create(resultDirectory, recursive = TRUE)
  }

  # execute plotfunction
  rmdContainer <- do.call(
    what = plotFunction,
    args = c(list(
      projectConfiguration = projectConfiguration,
      subfolder = subfolder
    ), inputs)
  )

  # create rmd
  rmdContainer$writeRmd(fileName = paste0(subfolder, ".Rmd"))

  return(invisible())
}


#' select function by key
#'
#' @param key character for function selection
#'
#' @return selected `function` for key
#' @export
getFunctionByKey <- function(key) {
  plotFunction <-
    switch(key,
      TimeProfile_Panel = plotTimeProfilePanels,
      stop("unkown function key")
    )
}


#' adds the default configuration to the template configuration
#'
#' if the template does not exist in the plotconfigurationfile in the project directory
#' it is taken from the plotconfigurationfile of the package installation.
#' In this case formats are not preserved
#'
#' @param wb Plotconfiguration file
#' @param templateSheet name of the template sheet
#' @param sheetName name of new sheet
#' @param dtNewConfig `data.table` with default configuration
#'
#' @return Plotconfiguration file
#' @export
addConfigToTemplate <- function(wb, templateSheet, sheetName, dtNewConfig) {
  # get template
  if (templateSheet %in% wb$sheet_names) {
    templateConfiguration <- xlsxReadData(wb = wb, sheetName = templateSheet)
  } else {
    templateConfiguration <-
      xlsxReadData(
        wb = system.file(
          "templates",
          "templateProject",
          "Parameters",
          "Plots.xlsx",
          package = "ospsuite.reportingframework",
          mustWork = TRUE
        ),
        sheetName = templateSheet
      )
  }

  dtNewConfig <- rbind(templateConfiguration[1, ],
    dtNewConfig,
    fill = TRUE
  )

  if (templateSheet != sheetName) {
    xlsxCloneAndSet(wb = wb, clonedSheet = templateSheet, sheetName = sheetName, dt = dtNewConfig)
  } else {
    xlsxWriteData(wb = wb, sheetName = sheetName, dt = dtNewConfig)
  }

  return(wb)
}

# auxiliaries ----
#' returns a scalevector usable for manaul scaling in ggplot
#'
#' @param namesOfScaleVector names of the vector
#' @param listOfValues list of possible entry, take the first where all values are not NA
#'
#' @return scaleVector
#' @export
getScalevector <- function(namesOfScaleVector,
                           listOfValues) {
  checkmate::assertCharacter(namesOfScaleVector, any.missing = FALSE, min.len = 1, unique = TRUE)
  checkmate::assertList(listOfValues)

  scaleVector <- NULL
  for (values in listOfValues) {
    if (!is.null(values) && !any(is.na(values))) {
      scaleVector <- values
      break
    }
  }

  if (is.null(scaleVector)) {
    stop(paste("no valid values for scalevector for", paste0(namesOfScaleVector, collapse = ", ")))
  }

  names(scaleVector) <- namesOfScaleVector

  return(scaleVector)
}


#' creates default color Vector
#'
#' @param shade for n < 10 differentiation between dark and light is possible
#' @param n
#'
#' @return named vector with default colors
#' @export
getDefaultColorsForScaleVector <- function(shade = c("dark", "light"), n) {
  checkmate::assertIntegerish(n, lower = 1, len = 1)
  shade <- match.arg(shade)
  if (n <= 10) {
    colorVector <-
      switch(shade,
        dark = ggsci::pal_d3("category20c")(20)[1:n],
        light = ggsci::pal_d3("category20c")(20)[(10 + 1):(10 + n)]
      )
  } else {
    if (n > length(colorMaps[["ospDefault"]])) {
      stop(paste("To many colors for colorVector, maximal", length(colorMaps[["ospDefault"]]), "allowed"))
    }
    colorVector <- colorMaps[["ospDefault"]][1:n]
  }

  return(colorVector)
}

#' returns default shapes
#'
#' @param n
#'
#' @return named vector with default shapes
#' @export
getDefaultShapesForScaleVector <- function(n) {
  shapes <- getOspsuite.plots.option(optionKey = OptionKeys$shapeValues)
  if (is.null(shapes)) {
    stop("no default shape sets for ospsuite.plots. Please use ospsuite.plots::setDefaults")
  }

  if (n > length(shapes)) {
    stop("not enough shapes available")
  }

  return(shapes[1:n])
}



#' generates named color vectors usable for sclae_color_manual
#'
#' @param dt `data.table` with aesthetic an dindex column
#' @param aesthetic  named `list`, names correspond to aesthic columns,
#'          entries are either 'dark' or 'light'
#' @param index name of index column
#'
#' @return named list of color vectors
#' @export
generateColorScaleVectors <- function(dt,
                                      aesthetic = list(
                                        color = "dark",
                                        fill = "light"
                                      ),
                                      index = "colorIndex") {
  n <- nrow(dt)
  scaleVectors <- list()
  for (col in names(aesthetic)) {
    for (col2 in c(col, setdiff(names(aesthetic), col))) {
      if (!all(is.na(scaleVectors[[col]]))) {
        scaleVectors[[col]] <- dt[[col2]]
        break
      }
    }

    if (is.null(scaleVectors[[col]])) {
      if (n <= 10) {
        if (aesthetic[[col]] == "dark") {
          scaleVectors[[col]] <- ggsci::pal_d3("category20c")(20)[1:n]
        } else {
          scaleVectors[[col]] <- ggsci::pal_d3("category20c")(20)[(10 + 1):(10 + n)]
        }
      } else {
        scaleVectors[[col]] <- colorMaps[["ospDefault"]][1:n]
      }
    }
    names(scaleVectors[[col]]) <- dt[[index]]
  }
  return(scaleVectors)
}


#' Title
#'
#' @param dtCaption `data.table` with caption information must have column PlotTag
#' @param captionColumn `character`column names which should be sorted to Tags
#'
#' @return `character` text for cpation
pasteFigureTags <- function(dtCaption, captionColumn) {
  if (dplyr::n_distinct(dtCaption[[captionColumn]]) == 1) {
    captionText <- unique(dtCaption[[captionColumn]])
  } else {
    captionTextVector <- dtCaption[, .(tags = paste0(
      get(captionColumn),
      " (", paste(unique(PlotTag), collapse = ", "), ")"
    )),
    by = captionColumn
    ]$tags

    allTags <- dtCaption[, .(tags = paste0(" \\(", paste(unique(PlotTag), collapse = ", "), "\\)"))]$tags

    captionTextVector <- gsub(allTags, "", captionTextVector)
    captionTextVector <- gsub("\\.$", "", captionTextVector)

    captionText <-
      paste(
        c(
          paste(captionTextVector[seq(1, length(captionTextVector) - 1)],
            collapse = ", "
          ),
          utils::tail(captionTextVector, 1)
        ),
        collapse = " and "
      )

    if (any(grepl("\\.$", captionTextVector))) {
      captionText <- paste0(captionText, ".")
    }
  }

  return(captionText)
}

# validation ----------------


#' checks if config table header and plot rows are strict separated
#'
#' @param configTable `data.table` configuration table to check
#'
#' @return configuration table without header lines
#' @export
validateHeaders <- function(configTable) {
  configTableHeader <- configTable[!is.na(Level)]
  checkmate::assertIntegerish(configTableHeader$Level, lower = 1, any.missing = FALSE)
  checkmate::assertCharacter(configTableHeader$Header, any.missing = FALSE)

  if (any(!is.na(configTableHeader %>% dplyr::select(setdiff(
    names(configTableHeader), c("Level", "Header")
  ))))) {
    stop(
      "Invalid plot configuration table. For Rows with headers all other columns must be empty."
    )
  }

  configTablePlots <- configTable[is.na(Level)]
  if (!all(configTablePlots[, lapply(.SD, function(x) all(is.na(x))),
    .SDcols = "Header"
  ])) {
    stop("Invalid plot configuration table. Missing header for level")
  }

  return(configTablePlots)
}

#' validate types of plot configuration tables
#'
#' @template configTablePlots
#' @param charactersWithoutMissing vector with character columns, where no missing value is allowed
#' @param charactersWithMissing  vector with character column, where values may missing
#' @param numericColumns  vector with numeric columns
#' @param logicalColumns vector with booleans
#' @param numericRangeColumns vector with columns where entries must be evaluate to a numeric of length 2
#' @param subsetList  list where each entry is a list:
#'  list(cols = 'vector with columns',
#'  allowedValues = vector with allowed values)
#'
validateConfigTablePlots <- function(configTablePlots,
                                     charactersWithoutMissing = NULL,
                                     charactersWithMissing = NULL,
                                     numericColumns = NULL,
                                     numericColumnsWithMissing = NULL,
                                     logicalColumns = NULL,
                                     numericRangeColumns = NULL,
                                     subsetList = list()) {
  # character columns without missing values
  if (!is.null(charactersWithoutMissing)) {
    invisible(lapply(
      charactersWithoutMissing,
      function(col) {
        checkmate::assertCharacter(
          configTablePlots[[col]],
          any.missing = FALSE,
          .var.name = paste("Plotconfiguration column", col)
        )
      }
    ))
  }

  # character columns with missing values
  if (!is.null(charactersWithMissing)) {
    invisible(lapply(
      charactersWithMissing,
      function(col) {
        checkmate::assertCharacter(
          configTablePlots[[col]],
          any.missing = TRUE,
          .var.name = paste("Plotconfiguration column", col)
        )
      }
    ))
  }

  # numeric columns
  if (!is.null(numericColumns)) {
    invisible(lapply(
      numericColumns,
      function(col) {
        checkmate::assertNumeric(
          configTablePlots[[col]],
          .var.name = paste("Plotconfiguration column", col)
        )
      }
    ))
  }

  # columns is a logical
  if (!is.null(logicalColumns)) {
    invisible(lapply(
      logicalColumns,
      function(col) {
        checkmate::assertLogical(
          as.logical(configTablePlots[!is.na(get(col))][[col]]),
          any.missing = FALSE,
          .var.name = paste("Plotconfiguration column", col)
        )
      }
    ))
  }

  # valid selection
  checkmate::assertList(subsetList, types = "list")
  for (subsetCheck in subsetList) {
    checkmate::assertList(subsetCheck, types = c("character", "factor"), names = "named")
    checkmate::assertNames(names(subsetCheck), permutation.of = c("cols", "allowedValues"))
    invisible(lapply(
      subsetCheck$cols,
      function(col) {
        if (any(!is.na(configTablePlots[[col]]))) {
          checkmate::assertNames(
            gsub("[()]", "", splitInputs(configTablePlots[!is.na(get(col))][[col]])),
            subset.of = subsetCheck$allowedValues,
            .var.name = paste("Plotconfiguration column", col)
          )
        }
      }
    ))
  }

  # is numeric range
  if (!is.null(numericRangeColumns)) {
    tryCatch(
      {
        for (col in numericRangeColumns) {
          if (any(!is.na(configTablePlots[[col]]))) {
            x <- configTablePlots[!is.na(get(col)), ][[col]]
            if (length(x) > 0) {
              valid <-
                is.numeric(eval(parse(text = x))) &&
                  length(eval(parse(text = x))) == 2
            }
            if (!all(valid)) {
              stop(paste("invalid inputs in plot configuration column", col))
            }
          }
        }
      },
      error = function(err) {
        stop(paste("invalid inputs in plot configuration column", col))
      }
    )
  }

  return(invisible())
}

#' check if at least one of the following columns is selected
#'
#' @template configTablePlots
#' @param columnVector vector of columns to check
#'
validateAtleastOneEntry <- function(configTablePlots, columnVector) {
  if (nrow(configTablePlots[rowSums(is.na(configTablePlots)) == length(columnVector), ]) > 0) {
    stop(paste(
      "Invalid configTable, each plot row needs at least one entry in one of the columns",
      paste(columnVector, collapse = ", ")
    ))
  }

  return(invisible())
}


#' Validation of data.table with outputPath Ids
#'
#' @param dtOutputPaths data.table with outputPath Ids
validateOutputIdsForPlot <- function(dtOutputPaths) {
  checkmate::assertFactor(dtOutputPaths$outputPathId, any.missing = FALSE)
  checkmate::assertCharacter(dtOutputPaths$OutputPath, any.missing = FALSE)
  checkmate::assertCharacter(dtOutputPaths$DisplayName, any.missing = FALSE)

  if (any(!is.na(dtOutputPaths$color))) {
    checkmate::assertCharacter(dtOutputPaths$color, any.missing = FALSE)
  }

  # Check for unique values for outputpathids
  uniqueColumns <- c("DisplayName", "DisplayUnit")
  uniqueIDValues <-
    dtOutputPaths[, lapply(.SD, function(x) {
      length(unique(x))
    }), by = outputPathId, .SDcols = uniqueColumns]
  tmp <- lapply(uniqueColumns, function(col) {
    if (any(uniqueIDValues[[col]] > 1)) stop(paste("values for", col, "should be the same within outputPathId"))
  })

  # check validity of units
  invisible(lapply(
    unique(dtOutputPaths$DisplayUnit),
    function(unit) {
      tryCatch(
        {
          suppressMessages(getDimensionForUnit(unit))
        },
        error = function(e) {
          stop(paste0('Please check sheet Outputs in plotconfiguration file. Unit "', unit, '" is not valid'))
        }
      )
    }
  ))

  return(invisible())
}
