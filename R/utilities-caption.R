#' Caption Prefix for Simulated Data
#'
#' @return Prefix text used at the start of generated captions.
#' @keywords internal
#' @noRd
.captionPrefixSimulated <- function() {
  return("Simulated")
}

#' Caption Scale Text
#'
#' @param scale Axis scale type (linear or log).
#' @param axisLabel Axis label text, for example x, y, or x and y.
#'
#' @return A sentence fragment describing the axis scale.
#' @keywords internal
#' @noRd
.captionScaleText <- function(scale, axisLabel) {
  return(paste(
    "on a",
    ifelse(scale == "linear", "linear", "logarithmic"),
    paste0(axisLabel, "-scale")
  ))
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
#' @param captionColumn A string specifying the name of the column in `dtCaption` that contains the captions.
#' @param endWithDot A logical value indicating whether to append a period at the end of the caption text. Default is FALSE.
#' @param startWithBlank boolean if TRUE adds as prefix a blank
#'
#' @return A character string representing the formatted caption text, which includes the captions and associated plot tags.
#'
#' @keywords internal
#' @noRd
.pasteFigureTags <- function(
  dtCaption,
  captionColumn,
  endWithDot = FALSE,
  startWithBlank = FALSE
) {
  checkmate::assertDataFrame(dtCaption)
  checkmate::assertNames(names(dtCaption), must.include = captionColumn)

  if (dplyr::n_distinct(dtCaption[[captionColumn]]) == 1) {
    captionText <- as.character(unique(dtCaption[[captionColumn]]))
  } else {
    captionGroups <- split(dtCaption, dtCaption[[captionColumn]])
    captionTextVector <- vapply(
      captionGroups,
      function(groupData) {
        paste0(
          groupData[[captionColumn]][1],
          " (",
          paste(unique(groupData[["plotTag"]]), collapse = ", "),
          ")"
        )
      },
      FUN.VALUE = character(1)
    )

    allTags <- paste0(
      " \\(",
      paste(unique(dtCaption[["plotTag"]]), collapse = ", "),
      "\\)"
    )

    captionTextVector <- gsub(allTags, "", captionTextVector)

    captionText <- .concatWithAnd(captionTextVector)
  }
  if (endWithDot && trimws(captionText) != "" && !grepl("\\.$", captionText)) {
    captionText <- paste0(captionText, ".")
  }
  if (startWithBlank & trimws(captionText) != "") {
    captionText <- paste0(" ", trimws(captionText))
  }

  return(captionText)
}

#' Add Text to caption text
#'
#' This function modifies a caption text by ensuring it ends with a period
#' and appending an additional caption if provided. Ensure the additional caption
#' ends also with a period before adding the additional text.
#'
#' @param captiontext A character string representing the main caption text.
#' @param plotCaptionAddon A character string representing the additional caption
#' text to be added. If NULL or NA, it will not be added.
#' @return A modified caption text with the additional caption added if applicable.
#' @keywords internal
#' @noRd
.addCaptionTextAddon <- function(captiontext, plotCaptionAddon) {
  captiontext <- trimws(captiontext)
  if (!grepl("\\.$", captiontext)) {
    captiontext <- paste0(captiontext, ".")
  }

  if (!is.na(plotCaptionAddon) && plotCaptionAddon != "") {
    if (!grepl("\\.$", plotCaptionAddon)) {
      plotCaptionAddon <- paste0(plotCaptionAddon, ".")
    }
    captiontext <- paste0(captiontext, " ", plotCaptionAddon)
  }

  return(captiontext)
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
#' @noRd
.concatWithAnd <- function(textVector) {
  textVector <- trimws(textVector)
  textVector <- textVector[textVector != ""]

  n <- length(textVector)

  if (n == 0) {
    return("")
  }
  if (n == 1) {
    return(textVector)
  }
  if (n == 2) {
    return(paste(textVector, collapse = " and "))
  }

  return(.concatWithAnd(c(
    paste(textVector[1:(n - 1)], collapse = ", "),
    utils::tail(textVector, 1)
  )))
}


#' This function takes a numeric vector of percentiles and maps specific values to their corresponding labels.
#' It also formats other numeric values based on whether they are integers or not.
#'
#' @param percentiles A numeric vector of percentiles (0, 50, 100, and other values).
#' @param suffix A character string to append to formatted percentile values.
#' @param allAsPercentiles boolean, if FALSE for percentiles = 0,50,100 min, median and max is returned
#'  otherwise 0th 50th 100th
#'
#' @return A vector containing the mapped labels for specific percentiles and formatted strings for others.
#' @examples
#' formatPercentiles(c(0, 0.5, 1))
#' formatPercentiles(c(0.05, 0.25, 0.75, 0.95), suffix = " CI")
#'
#' @export
formatPercentiles <- function(
  percentiles,
  suffix = "",
  allAsPercentiles = FALSE
) {
  return(lapply(percentiles * 100, function(p) {
    if (p == 0 & !allAsPercentiles) {
      "min"
    } else if (p == 50 & !allAsPercentiles) {
      "median"
    } else if (p == 100 & !allAsPercentiles) {
      "max"
    } else if (p %% 1 == 0) {
      paste0(scales::label_ordinal()(x = p), suffix)
    } else {
      paste0(p, "th", suffix)
    }
  }) |>
    unlist())
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
#' @noRd
.generatePlotTag <- function(index) {
  checkmate::assertIntegerish(index, lower = 1, upper = length(letters))
  return(toupper(letters[index]))
}
