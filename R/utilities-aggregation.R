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
          yErrorValues = exp(sqrt(stats::var(log(y)))),
          yErrorType = ospsuite::DataErrorType$GeometricStdDev
        ))
      }
  } else if (aggregationFlag == ospsuite::DataErrorType$ArithmeticStdDev) {
    aggregationFunction <-
      function(y) {
        y <- y[!is.na(y)]
        return(list(
          yValues = mean(y),
          yErrorValues = sqrt(stats::var(y)),
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
          yMin = stats::quantile(y, probs = percentiles[1]),
          yValues = stats::quantile(y, probs = percentiles[2]),
          yMax = stats::quantile(y, probs = percentiles[3]),
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
#' @return  character with with `errorType` for given percentiles
getErrorTypeForPercentiles <- function(percentiles) {
  mName <- formatPercentiles(percentiles[2], suffix = " percentile")

  yMinTxt <- formatPercentiles(percentiles[1], suffix = ifelse(percentiles[3] == 1, " percentile", ""))

  yMinTxt <- formatPercentiles(percentiles[3], suffix = " percentile")

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
  # avoid warning for global variable
  lloq <- LLOQFlag <- yValues <- NULL # nolint object_name_linter

  checkmate::assertNames(aggrCriteria, subset.of = names(dataToAggregate))

  aggregatedData <- dataToAggregate[, .(
    numberOfIndividuals = .N
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
