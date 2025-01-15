# file with function called by different plot functions

#' run plot function
#'
#' creates .Rmd file for selected plotfile and inputs
#'
#' @template projectConfig
#' @param functionKey keyword which select plot function, if NULL plotfunction is needed
#' @param plotFunction function which is used for plotting, if not null, it overwrites `functionKey` selection
#' @param subfolder subfolder where results are filed, if `functionKey` is used, as default value the sheetname of the plotconfiguration is used
#' @param inputs additionally inputs for the
#'
#' @export
runPlot <- function(projectConfiguration,
                    functionKey = c("TimeProfile_Panel",'PK_RatioForestPlot','PK_BoxPlot_absolute','PK_BoxPlot_relative'),
                    plotFunction = NULL,
                    subfolder = NULL,
                    inputs = list()) {

  loadConfigTables(projectConfiguration)

  functionKeys <-  getFunctionKeys()

  # validate inputs
  if (is.null(plotFunction)) {
    functionKey <- checkmate::assertChoice(functionKey,choices = names(functionKeys))
    plotFunction <- functionKeys[[functionKey]]$fun

    if (is.null(subfolder)) {
      subfolder <- paste0(inputs$configTableSheet,functionKeys[[functionKey]]$subfolderOffset)
    }
  } else{
    if (is.null(subfolder)) {
      browser()
      subfolder <-  gsub('^plot','',deparse(substitute(plotFunction)))
    }
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

  if (getOption('OSPSuite.RF.withEPackage') &
      (!is.null(rmdContainer$configTable))){
    addScenariosToEPackage(projectConfiguration = projectConfiguration,
                           configTable = rmdContainer$configTable,subfolder = subfolder)

    if (nrow(rmdContainer$dataObserved) > 0 & exists(x = 'dataObserved')){
      exportTimeProfileDataForEPackage(
        projectConfiguration = projectConfiguration,
        dataToExport = merge(rmdContainer$dataObserved, dataObserved, by = '.Id')
      )
    }
  }

  # create rmd
  rmdContainer$writeRmd(fileName = paste0(subfolder, ".Rmd"))

  return(invisible())
}

#' Get Function Keys
#'
#' This function returns a list of function keys associated with specific plotting functions and offset for subfolders
#'
#' @keywords internal
getFunctionKeys <- function(){


  list(TimeProfile_Panel = list(fun = plotTimeProfilePanels,
                                subfolderOffset = ''),
       PK_Boxwhisker_Absolute = list(fun = plotPKBoxwhisker,
                                          subfolderOffset = '_abs'),
       PK_RatioForestByAggregation = list(fun = plotPKRatioForestPlotByRatioAggregation,
                                        subfolderOffset = '_ratio_aggregation'),
       PK_RatioForestByBootstrap = list(fun = plotPKRatioForestPlotByBoostrapping,
                                        subfolderOffset = '_ratio_bootstrap'),
       Sensitivity = list(fun = plotSensitivity,
                          subfolderOffset = '')
  )


}



#' Generate R Markdown Container for Plotting
#'
#' This function initializes an R Markdown container and iterates through a configuration table to generate plots based on the provided plotting function.
#'
#' @param projectConfiguration An object of class `ProjectConfiguration`, which contains project-specific settings.
#' @param subfolder A string specifying the subfolder within the output directory where the R Markdown files will be stored.
#' @param configTable A data frame or data table containing the configuration settings for the plots, including headers and levels.
#' @param plotFunction A function that takes a plot configuration and the R Markdown container as arguments and generates the corresponding plot.
#' @param ... Additional arguments passed to the `plotFunction`.
#'
#' @return An instance of `RmdContainer` populated with the generated plots based on the configuration table.
#'
#' @keywords internal
generateRmdContainer <- function(projectConfiguration, subfolder, configTable, plotFunction, ...) {
  # Check common arguments
  checkmate::assertClass(projectConfiguration, classes = 'ProjectConfiguration')
  checkmate::assertString(subfolder)

  # Initialize Container for RMD generation
  rmdContainer <- RmdContainer$new(
    rmdfolder = file.path(projectConfiguration$outputFolder),
    subfolder = subfolder
  )

  iRow <- 1
  levelLines <- which(!is.na(configTable$level))
  while (iRow <= nrow(configTable)) {
    if (!is.na(configTable$level[iRow])) {
      # Add section headers
      rmdContainer$addHeader(configTable$header[iRow], level = configTable$level[iRow])
      iRow <- iRow + 1
    } else {
      # Execute plot section
      iEndX <- utils::head(which(levelLines > iRow), 1)
      iEnd <- if (length(iEndX) == 0) nrow(configTable) else levelLines[iEndX] - 1

      for (onePlotConfig in split(configTable[seq(iRow, iEnd)], by = "plotName")) {
        tryCatch({
          rmdContainer <- plotFunction(onePlotConfig = onePlotConfig, rmdContainer = rmdContainer, ...)
        }, error = function(err) {
          if (!getOption("OSPSuite.RF.skipFailingPlots", default = FALSE)) {
            stop(err)
          } else {
            warning(paste("Error during creation of:", onePlotConfig$plotName[1], "Message:", conditionMessage(err)))
          }
        })
      }
      iRow <- iEnd + 1
    }
  }

  return(rmdContainer)
}

# plotAddons ------------
#' Add facets to a ggplot object
#'
#' This function adds facets to a given ggplot object, allowing for better visualization of data subsets.
#' Faceting is done by variable plotTag
#'
#' @param plotObject A ggplot object to which the facets should be added.
#' @param facetScale A character string indicating the scale of the facets. Options are "free", "fixed", "free_x", or "free_y".
#' @param facetAspectRatio A numeric value specifying the aspect ratio of the facets. Default is 0.5.
#' @param nFacetColumns An integer specifying the number of columns to use for the facet layout. If NULL no faceting is done
#'
#' @return An updated ggplot object with facets added.
#' @keywords internal
addFacets <- function(plotObject,
                      facetScale,
                      facetAspectRatio = 0.5,
                      nFacetColumns) {
  # avoid warnings for global variables during check
  plotTag <- NULL

  if (!is.null(nFacetColumns)) {
    plotObject <- plotObject +
      ggplot2::facet_wrap(
        facets = ggplot2::vars(plotTag),
        scales = facetScale,
        ncol = nFacetColumns
      ) +
      ggplot2::theme(aspect.ratio = facetAspectRatio,
                     strip.background = element_rect(fill = NA,color = NA),
                     strip.text = element_text(hjust = 0,vjust = 1))

  }

  return(plotObject)
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

    captionText <- concatWithAnd(captionTextVector)

    if (endWithDot & length(trimws(captionText)) > 0) {
      captionText <- paste0(captionText, ".")
    }
  }

  return(captionText)
}

#' Concatenate Text Vector with Commas and "and"
#'
#' This function takes a vector of text strings and concatenates them into a single string.
#' For vectors with more than two elements, it separates them with commas and uses "and"
#' before the last element.
#'
#' @param textVector A character vector of text strings to be concatenated.
#' @return A single character string representing the concatenated text.
#'
#' @keywords internal
concatWithAnd <- function(textVector) {

  textVector <- trimws(textVector)
  textVector <- textVector[textVector != '']

  n <- length(textVector)

  if (n == 0) {
    return("")
  } else if (n == 1) {
    return(textVector)
  } else if (n == 2) {
    return(paste(textVector, collapse = " and "))
  } else {
    return(concatWithAnd(c(paste(textVector[1:(n - 1)], collapse = ", "), tail(textVector, 1))))
  }
}


' Process Percentiles
#'
#' This function takes a numeric vector of percentiles and maps specific values to their corresponding labels.
#' It also formats other numeric values based on whether they are integers or not.
#'
#' @param percentiles A numeric vector of percentiles (0, 50, 100, and other values).
#' @param suffix A character string to append to formatted percentile values.
#'
#' @return A vector containing the mapped labels for specific percentiles and formatted strings for others.
formatPercentiles <- function(percentiles,suffix = '',allAsPercentiles = FALSE){

  lapply(percentiles*100, function(p) {
    if (p == 0 & !allAsPercentiles){
      'min'
    } else if (p == 50 & !allAsPercentiles){
      'median'
    } else if (p == 100 & !allAsPercentiles){
      'max'
    } else if (p %% 1 == 0) {
      paste0(scales::label_ordinal()(x = p), suffix)
    } else {
      paste0(p, 'th', suffix)
    }

  }) %>% unlist()
}

#' Generate a Plot Tag
#'
#' This function generates a plot tag based on the provided index.
#' The function takes an index and returns the corresponding letter
#' from the alphabet in uppercase.
#'
#' @param index An integer representing the position in the alphabet
#'
#' @return A character string representing the uppercase letter
#' corresponding to the given index.
#' @keywords internal
generatePlotTag <- function(index){
  toupper(letters[index])
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


#' Validate Consistency of Values Within Groups
#'
#' This function checks whether specified columns in a data table have consistent values
#' within groups defined by one or more grouping columns. If any column contains more than
#' one unique value within a group, an error is raised.
#'
#' @param dt A data.table or data.frame containing the data to be validated.
#' @param valueColumns A character vector of column names to check for consistency.
#' @param groupingColumns A character vector of column names that define the groups.
#'        Default is 'plotName'.
#'
#' @return Returns NULL (invisible) if all checks pass. Raises an error if any value
#'         column contains inconsistent values within a group.
#'
#' @export
validateGroupConsistency <- function(
    dt,
    valueColumns,
    groupingColumns = 'plotName') {

  # Check for unique values of value columns for each group defined by groupingColumns
  uniqueValueCounts <-
    dt[, lapply(.SD, function(x) {
      length(unique(x))
    }), by = groupingColumns,
    .SDcols = valueColumns]

  # Check if any value column has more than one unique value within each group
  lapply(valueColumns, function(col) {
    if (any(uniqueValueCounts[[col]] > 1)) {
      stop(paste("Values for", col, "should be the same within each group defined by", paste(groupingColumns, collapse = ", ")))
    }
  })

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
validateOutputIdsForPlot <- function() {
  # avoid warning for global variable
  outputPathId <- NULL

  checkmate::assertFactor(configEnv$outputPaths$outputPathId, any.missing = FALSE,
                          .var.name = 'Outputs column outputPathId')
  checkmate::assertCharacter(configEnv$outputPaths$outputPath, any.missing = FALSE,
                             .var.name = 'Outputs column outputPath')
  checkmate::assertCharacter(configEnv$outputPaths$displayNameOutputs, any.missing = FALSE,
                             .var.name = 'Outputs column displayName')

  if (any(!is.na(configEnv$outputPaths$color))) {
    checkmate::assertCharacter(configEnv$outputPaths$color, any.missing = FALSE,
                               .var.name = 'Outputs column color')
  }

  # Check for unique values for outputpathids
  uniqueColumns <- c("displayNameOutputs", "displayUnit")
  uniqueIDValues <-
    configEnv$outputPaths[, lapply(.SD, function(x) {
      length(unique(x))
    }), by = outputPathId, .SDcols = uniqueColumns]
  tmp <- lapply(uniqueColumns, function(col) { # nolint object_usage
    if (any(uniqueIDValues[[col]] > 1)) stop(paste("values for", col, "should be the same within outputPathId"))
  })

  # check validity of units
  invisible(lapply(
    unique(configEnv$outputPaths$displayUnit),
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
validateDataGroupIdsForPlot <- function() {
  # avoid warning for global variable
  outputPathId <- NULL

  checkmate::assertFactor(
    configEnv$dataGroupIds$group,
    any.missing = FALSE,
    unique = TRUE,
    .var.name = 'DataGroups column group'
  )
  checkmate::assertCharacter(configEnv$dataGroupIds$displayNameData,
                             any.missing = FALSE,
                             .var.name = 'DataGroups column displayName')

  if (any(!is.na(configEnv$dataGroupIds$color))) {
    checkmate::assertCharacter(dtOutputPaths$color, any.missing = FALSE,
                               .var.name = 'DataGroups column color')
  }

  return(invisible())
}
