#' Title
#'
#' @template projectConfig
#' @param functionKey keyword which select plot function, if NULL plotfunction is needed
#' @param plotFunction function which is used for plotting, if not null, it overwrites `functionKey` selection
#' @param subfolder subfolder where results are filed, if `functionKey` is used, as default value the sheetname of the plotconfiguration is used
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
           TimeProfile_Panel = plotTimeProfilePanels, # nolint indentation_linter
           stop("unkown function key")
    )

  return(plotFunction)
}


#' adds the default configuration to the template configuration
#'
#' if the template does not exist in the plot-configuration file in the project directory
#' it is taken from the plot-configuration file of the package installation.
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
          "scripts",
          "ReportingFramework",
          "Plots.xlsx",
          package = "ospsuite.reportingframework",
          mustWork = TRUE
        ),
        sheetName = templateSheet
      )
  }

  dtNewConfig <- rbind(templateConfiguration[1, ],
                       dtNewConfig, # nolint indentation_linter
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
#' returns a scalevector usable for manual scaling in ggplot
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


#' Get Default Colors for Scale Vector
#'
#' This function generates a vector of default colors based on the specified shade and number of colors required.
#' It provides colors from the ggsci package for small numbers and a custom color map for larger requests.
#'
#' @param shade A character string indicating the shade of colors to return.
#'   Must be either "dark" or "light". Default is "dark".
#'
#' @param n An integer specifying the number of colors to return. Must be greater than or equal to 1.
#'
#' @details
#' - For `n` values less than or equal to 10, the function uses the ggsci package's "category20c" palette.
#' - For `n` values greater than 10, it retrieves colors from the predefined color map of the package `ospsuite.plots` named "ospDefault".
#' - If `n` exceeds the maximum number of colors available in "ospDefault", an error is raised.
#'
#' @return A character vector of color values in hexadecimal format.
#'
#' @keywords internal
getDefaultColorsForScaleVector <- function(shade = c("dark", "light"), n) {
  checkmate::assertIntegerish(n, lower = 1, len = 1)
  shade <- match.arg(shade)
  if (n <= 10) {
    colorVector <-
      switch(shade,
             dark = ggsci::pal_d3("category20c")(20)[1:n], # nolint indentation_linter
             light = ggsci::pal_d3("category20c")(20)[(10 + 1):(10 + n)]
      )
  } else {
    if (n > length(ospsuite.plots::colorMaps[["ospDefault"]])) {
      stop(paste("To many colors for colorVector, maximal", length(ospsuite.plots::colorMaps[["ospDefault"]]), "allowed"))
    }
    colorVector <- ospsuite.plots::colorMaps[["ospDefault"]][1:n]
  }

  return(colorVector)
}


#' Get Default Shapes for Scale Vector
#'
#' This function retrieves a vector of default shapes for plotting based on the specified number of shapes required.
#' It utilizes shape settings from the ospsuite.plots package.
#'
#' @param n An integer specifying the number of shapes to return. Must be greater than or equal to 1.
#'
#' @details
#' - The function calls `getOspsuite.plots.option` to obtain the default shape values.
#' - If no shapes are available, an error is raised, prompting the user to set defaults using `ospsuite.plots::setDefaults()`.
#' - If the requested number of shapes exceeds the available shapes, an error is raised.
#'
#' @return A character vector of shape values.
#'
#' @keywords internal
getDefaultShapesForScaleVector <- function(n) {
  shapes <- ospsuite.plots::getOspsuite.plots.option(optionKey = ospsuite.plots::OptionKeys$shapeValues)
  if (is.null(shapes)) {
    stop("no default shape sets for ospsuite.plots. Please use ospsuite.plots::setDefaults()")
  }

  if (n > length(shapes)) {
    stop("not enough shapes available")
  }

  return(shapes[1:n])
}



#' generates named color vectors usable for scale_color_manual
#'
#' @param dt `data.table` with aesthetic and index column
#' @param aesthetic  named `list`, names correspond to aesthetic columns,
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
        scaleVectors[[col]] <- ospsuite.plots::colorMaps[["ospDefault"]][1:n]
      }
    }
    names(scaleVectors[[col]]) <- dt[[index]]
  }
  return(scaleVectors)
}


#' Paste Figure Tags for Captions
#'
#' This function generates a formatted caption text by combining unique captions with associated plot tags.
#' If all captions are the same, it returns that caption. Otherwise, it creates a string that includes
#' the unique captions and their corresponding tags.
#'
#' @param dtCaption A data.table containing the captions and plot tags. It must have at least the following columns:
#'   - `captionColumn`: The column name containing the captions.
#'   - `plotTag`: A column containing the plot tags associated with each caption.
#'
#' @param captionColumn A string specifying the name of the column in `dtCaption` that contains the captions.
#'
#' @param endWithDot A logical value indicating whether to append a period at the end of the caption text. Default is FALSE.
#'
#' @return A character string representing the formatted caption text, which includes the captions and associated plot tags.
#'
pasteFigureTags <- function(dtCaption, captionColumn, endWithDot = FALSE) {
  # avoid warning for global variable
  plotTag <- NULL

  if (dplyr::n_distinct(dtCaption[[captionColumn]]) == 1) {
    captionText <- unique(dtCaption[[captionColumn]])
  } else {
    captionTextVector <- dtCaption[, .(tags = paste0(
      get(captionColumn),
      " (", paste(unique(plotTag), collapse = ", "), ")"
    )),
    by = captionColumn
    ]$tags

    allTags <- dtCaption[, .(tags = paste0(" \\(", paste(unique(plotTag), collapse = ", "), "\\)"))]$tags

    captionTextVector <- gsub(allTags, "", captionTextVector)

    captionText <-
      paste(
        c(
          paste(captionTextVector[seq(1, length(captionTextVector) - 1)],
                collapse = ", " # nolint indentation_linter
          ),
          utils::tail(captionTextVector, 1)
        ),
        collapse = " and "
      )

    if (endWithDot & length(trimws(captionText)) > 0) {
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
  # avoid warning for global variable
  level <- NULL

  configTableHeader <- configTable[!is.na(level)]
  checkmate::assertIntegerish(configTableHeader$level, lower = 1, any.missing = FALSE)
  checkmate::assertCharacter(configTableHeader$header, any.missing = FALSE)

  if (any(!is.na(configTableHeader %>% dplyr::select(setdiff(
    names(configTableHeader), c("level", "header")
  ))))) {
    stop(
      "Invalid plot configuration table. For Rows with headers all other columns must be empty."
    )
  }

  configTablePlots <- configTable[is.na(level)]
  if (!all(configTablePlots[,
                            lapply(.SD, function(x)
                              all(is.na(x))),   # nolint indentation_linter
                            .SDcols = "header"])) {
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
                                     logicalColumns = NULL,
                                     numericRangeColumns = NULL,
                                     subsetList = list()) {

  # Validate character columns
  if (!is.null(charactersWithoutMissing)) {
    invisible(lapply(charactersWithoutMissing, function(col) validateColumn(col, configTablePlots, "character", FALSE)))
  }

  if (!is.null(charactersWithMissing)) {
    invisible(lapply(charactersWithMissing, function(col) validateColumn(col, configTablePlots, "character", TRUE)))
  }

  # Validate numeric columns
  if (!is.null(numericColumns)) {
    invisible(lapply(numericColumns, function(col) validateColumn(col, configTablePlots, "numeric")))
  }

  # Validate logical columns
  if (!is.null(logicalColumns)) {
    invisible(lapply(logicalColumns, function(col) validateColumn(col, configTablePlots, "logical")))
  }

  # Validate subset list
  checkmate::assertList(subsetList, types = "list")
  validateSubsetList(subsetList, configTablePlots)

  if (!is.null(numericRangeColumns)) {
    validateNumericRangeColumns(numericRangeColumns, configTablePlots)
  }

  return(invisible())
}

#' validate a Column
#'
#' This function validates a specified column in a data frame based on the
#' provided type. It checks for character, numeric, or logical types and
#' can enforce the presence or absence of missing values.
#'
#' @param col A string representing the name of the column to validate.
#' @param data A data frame containing the column to be validated.
#' @param type A string specifying the type of validation to perform.
#'             Options are "character", "numeric", or "logical".
#' @param anyMissing A logical value indicating whether missing values are allowed.
#'                   Default is FALSE.
#'
#' @return NULL. The function will throw an error if the validation fails.
#' @keywords internal
validateColumn <- function(col, data, type, anyMissing = FALSE) {
  switch(
    type,
    character = checkmate::assertCharacter(
      data[[col]],
      any.missing = anyMissing,
      .var.name = paste("Plot configuration column", col)
    ),
    numeric = checkmate::assertNumeric(data[[col]], .var.name = paste("Plot configuration column", col)),
    logical = checkmate::assertLogical(
      as.logical(data[[col]]),
      any.missing = FALSE,
      .var.name = paste("Plot configuration column", col)
    )
  )
}


#' validate Subset List
#'
#' Validates the subset list against the provided data frame.
#'
#' @param subsetList A list containing subsets to validate.
#' @param data A data frame containing the columns to validate.
validateSubsetList <- function(subsetList, data) {
  for (subsetCheck in subsetList) {
    checkmate::assertList(subsetCheck, types = c("character", "factor"), names = "named")
    checkmate::assertNames(names(subsetCheck), permutation.of = c("cols", "allowedValues"))
    invisible(lapply(subsetCheck$cols, function(col) {
      if (any(!is.na(data[[col]]))) {
        checkmate::assertNames(
          gsub("[()]", "", splitInputs(data[!is.na(get(col))][[col]])),
          subset.of = subsetCheck$allowedValues,
          .var.name = paste("Plot configuration column", col)
        )
      }
    }))
  }
}



#' validate Numeric Range Columns
#'
#' Validates numeric range columns in the provided data frame.
#'
#' @param columns A vector of column names to validate.
#' @param data A data frame containing the columns to validate.
validateNumericRangeColumns <- function(columns, data) {
  for (col in columns) {
    if (any(!is.na(data[[col]]))) {
      x <- data[!is.na(get(col)), ][[col]]
      if (length(x) > 0) {
        valid <- is.numeric(eval(parse(text = x))) && length(eval(parse(text = x))) == 2
        if (!valid) {
          stop(paste("Invalid inputs in plot configuration column", col))
        }
      }
    }
  }
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
  # avoid warning for global variable
  outputPathId <- NULL

  checkmate::assertFactor(dtOutputPaths$outputPathId, any.missing = FALSE,
                          .var.name = 'Outputs column outputPathId')
  checkmate::assertCharacter(dtOutputPaths$outputPath, any.missing = FALSE,
                             .var.name = 'Outputs column outputPath')
  checkmate::assertCharacter(dtOutputPaths$displayName, any.missing = FALSE,
                             .var.name = 'Outputs column displayName')

  if (any(!is.na(dtOutputPaths$color))) {
    checkmate::assertCharacter(dtOutputPaths$color, any.missing = FALSE,
                               .var.name = 'Outputs column color')
  }

  # Check for unique values for outputpathids
  uniqueColumns <- c("displayName", "displayUnit")
  uniqueIDValues <-
    dtOutputPaths[, lapply(.SD, function(x) {
      length(unique(x))
    }), by = outputPathId, .SDcols = uniqueColumns]
  tmp <- lapply(uniqueColumns, function(col) { # nolint object_usage
    if (any(uniqueIDValues[[col]] > 1)) stop(paste("values for", col, "should be the same within outputPathId"))
  })

  # check validity of units
  invisible(lapply(
    unique(dtOutputPaths$displayUnit),
    function(unit) {
      tryCatch(
        {
          suppressMessages(ospsuite::getDimensionForUnit(unit))
        },
        error = function(e) {
          stop(paste0('Please check sheet Outputs in plotconfiguration file. Unit "', unit, '" is not valid'))
        }
      )
    }
  ))

  return(invisible())
}

#' Validation of data.table with outputPath Ids
#'
#' @param projectConfiguration ProjectConfiguration object
validateDataGroupIdsForPlot <- function(projectConfiguration) {
  # avoid warning for global variable
  outputPathId <- NULL

  dtDataGroupIds <<- getDataGroups(projectConfiguration)

  checkmate::assertFactor(
    dtDataGroupIds$group,
    any.missing = FALSE,
    unique = TRUE,
    .var.name = 'DataGroups column group'
  )
  checkmate::assertCharacter(dtDataGroupIds$displayName,
                             any.missing = FALSE,
                             .var.name = 'DataGroups column displayName')

  if (any(!is.na(dtDataGroupIds$color))) {
    checkmate::assertCharacter(dtOutputPaths$color, any.missing = FALSE,
                               .var.name = 'DataGroups column color')
  }

  return(invisible())
}
