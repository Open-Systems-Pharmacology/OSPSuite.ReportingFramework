#' Get Aggregation Function
#'
#' This function retrieves the appropriate aggregation function based on the specified aggregation flag.
#'
#' @param aggregationFlag A character string indicating the aggregation method.
#' @param percentiles A numeric vector of percentiles to calculate if aggregationFlag is "Percentiles".
#' @param customFunction A custom function for aggregation if aggregationFlag is "Custom".
#'
#' @return the aggregation function
#' @export
getAggregationFunction <- function(aggregationFlag,
                                   percentiles,
                                   customFunction) {
  checkmate::assertChoice(
    aggregationFlag,
    c(
      unname(unlist(ospsuite::DataErrorType)),
      "Percentiles", "Custom"
    )
  )

  if (aggregationFlag == ospsuite::DataErrorType$GeometricStdDev) {
    aggregationFunction <-
      function(y) {
        y <- y[y > 0 & !is.na(y)]
        return(list(
          yValues = exp(mean(log(y))),
          yErrorValues = exp(sqrt(var(log(y)))),
          yErrorType = ospsuite::DataErrorType$GeometricStdDev
        ))
      }
  } else if (aggregationFlag == ospsuite::DataErrorType$ArithmeticStdDev) {
    aggregationFunction <-
      function(y) {
        y <- y[!is.na(y)]
        return(list(
          yValues = mean(y),
          yErrorValues = sqrt(var(y)),
          yErrorType = ospsuite::DataErrorType$ArithmeticStdDev
        ))
      }
  } else if (aggregationFlag == "Percentiles") {
    checkmate::assertDouble(
      percentiles,
      null.ok = FALSE,
      lower = 0,
      upper = 1,
      any.missing = FALSE,
      unique = TRUE,
      len = 3,
      sorted = TRUE
    )
    aggregationFunction <-
      function(y) {
        y <- y[!is.na(y)]
        return(list(
          yMin = quantile(y, probs = percentiles[1]),
          yValues = quantile(y, probs = percentiles[2]),
          yMax = quantile(y, probs = percentiles[3]),
          yErrorType = getErrorTypeForPercentiles(percentiles)
        ))
      }
  } else {
    checkmate::assertFunction(customFunction, null.ok = FALSE)
    aggregationFunction <- customFunction
  }

  return(aggregationFunction)
}

#' creates text for percentiles
#'
#' @param percentiles numeric vectors with percentiles
#'
#' @return  character with with errortype for given precentiles
getErrorTypeForPercentiles <- function(percentiles) {
  mName <- ifelse(percentiles[2] == 0.5, "median",
    paste(scales::label_ordinal()(x = percentiles[2] * 100), "percentile")
  )
  yMinTxt <- ifelse(percentiles[1] == 0, "min",
    paste(
      scales::label_ordinal()(x = percentiles[1] * 100),
      ifelse(percentiles[3] == 1, "percentile", "")
    )
  )
  yMaxTxt <- ifelse(percentiles[3] == 1, "max",
    paste(scales::label_ordinal()(x = percentiles[3] * 100), "percentile")
  )
  return(paste(mName, "|", trimws(yMinTxt), "-", trimws(yMaxTxt)))
}


#' Perform Aggregation
#'
#' This function performs the aggregation of observed data based on the specified criteria.
#'
#' @param aggregationFun aggregation function
#' @param dataToAggregate A data.table containing the data to be aggregated.
#' @param aggrCriteria A character vector specifying the columns to group
#'
#' @return A data.table containing aggregated results.
#' @export
performAggregation <- function(dataToAggregate,
                               aggregationFun,
                               aggrCriteria) {
  checkmate::assertNames(aggrCriteria, subset.of = names(dataToAggregate))

  aggregatedData <- dataToAggregate[, .(
    numberOfPatients = .N
  ), by = aggrCriteria]

  # Count number of measurements below LLOQ
  if (!("lloq" %in% names(dataToAggregate))) dataToAggregate[, lloq := NA]
  dataToAggregate[, LLOQFlag := ifelse(is.na(lloq), 0, ifelse(yValues < lloq, 1, 0))]
  tmp <- dataToAggregate[, .(nBelowLLOQ = sum(LLOQFlag)), by = aggrCriteria]
  aggregatedData <- merge(aggregatedData, tmp, by = aggrCriteria, all = TRUE)

  # Perform the aggregation
  tmp <- dataToAggregate[, as.list(aggregationFun(yValues)), by = aggrCriteria]

  aggregatedData <- merge(aggregatedData, tmp, by = aggrCriteria, all = TRUE)

  return(aggregatedData)
}

#' extracts error labels
#'
#' @param yErrorType descriptor saved in data
#'
#' @return character vector length 2 with descriptor for mean and range
getErrorLabels <- function(yErrorType) {
  errorLabels <-
    switch(yErrorType,
      GeometricStdDev = c("geometric mean", "geometric standard deviation"),
      ArithmeticStdDev = c("mean", "standard deviation"),
      unlist(lapply(strsplit(yErrorType, "|", fixed = TRUE), trimws))
    )

  return(errorLabels)
}
