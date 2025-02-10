#' Get Aggregation Function
#'
#' This function retrieves the appropriate aggregation function based on the specified aggregation flag.
#'
#' @param aggregationFlag A character string indicating the aggregation method. Must be one of the options from `ospsuite::DataErrorType` or "Percentiles" or "Custom".
#' @param percentiles A numeric vector of percentiles to calculate if aggregationFlag is "Percentiles". Must have a length of 3, sorted, and within the range [0, 1].
#' @param customFunction A custom function for aggregation if aggregationFlag is "Custom". Must be a valid function.
#'
#' @return A function that performs the specified aggregation.
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
    yErroryType <- getErrorTypeForPercentiles(percentiles)
    aggregationFunction <-
      function(y) {
        y <- y[!is.na(y)]
        return(list(
          yMin = stats::quantile(y, probs = percentiles[1]),
          yValues = stats::quantile(y, probs = percentiles[2]),
          yMax = stats::quantile(y, probs = percentiles[3]),
          yErrorType = yErroryType
        ))
      }
  } else {
    checkmate::assertFunction(customFunction, null.ok = FALSE)
    aggregationFunction <- customFunction
  }

  return(aggregationFunction)
}

#' Creates text for percentiles
#'
#' This function generates a descriptive text based on the provided percentiles.
#'
#' @param percentiles A numeric vector containing percentiles.
#'
#' @return A character string with the error type for the given percentiles.
#' @keywords internal
getErrorTypeForPercentiles <- function(percentiles) {
  mName <- formatPercentiles(percentiles[2], suffix = " percentile")

  yMinTxt <- formatPercentiles(percentiles[1], suffix = ifelse(percentiles[3] == 1, " percentile", ""))

  yMaxTxt <- formatPercentiles(percentiles[3], suffix = " percentile")

  return(paste(mName, "|", trimws(yMinTxt), "-", trimws(yMaxTxt)))
}

#' Perform Aggregation
#'
#' This function performs the aggregation of observed data based on the specified criteria.
#'
#' @param dataToAggregate A data.table containing the data to be aggregated.
#' @param aggregationFun A function to aggregate the data.
#' @param aggrCriteria A character vector specifying the columns to group by.
#'
#' @return A data.table containing aggregated results with counts and aggregated values.
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

#' Extracts error labels
#'
#' This function retrieves error labels based on the provided error type descriptor.
#'
#' @param yErrorType A string descriptor indicating the type of error.
#'
#' @return A character vector of length 2 containing descriptors for mean and range.
#' @keywords internal
getErrorLabels <- function(yErrorType) {
  errorLabels <-
    switch(yErrorType,
      GeometricStdDev = c("geometric mean", "geometric standard deviation"),
      ArithmeticStdDev = c("mean", "standard deviation"),
      unlist(lapply(strsplit(yErrorType, "|", fixed = TRUE), trimws))
    )

  return(errorLabels)
}

#' Get Aggregated Variance
#'
#' This function calculates the aggregated variance based on the specified aggregation function and identifier.
#'
#' @param dt A data.table containing the data to aggregate.
#' @param aggregationFun A function to aggregate the data.
#' @param valueColumn A string indicating the column name containing the values to aggregate.
#' @param identifier A character vector specifying the columns to group by.
#' @param direction A character string specifying the direction of aggregation, either 'y' or 'x'.
#'
#' @return A data.table containing the aggregated variance results.
#' @keywords internal
getAggregatedVariance <- function(dt,
                                  aggregationFun,
                                  valueColumn,
                                  identifier,
                                  direction = c('y','x')) {
  direction <-  match.arg(direction)

  dtAggregated <- dt[, as.list(aggregationFun(get(valueColumn))), by = identifier]

  if (!is.null(dtAggregated$yErrorValues)){
    if (dtAggregated$yErrorType[1] == ospsuite::DataErrorType$ArithmeticStdDev) {
      dtAggregated[,yMin := yValues - yErrorValues]
      dtAggregated[,yMax := yValues + yErrorValues]
    }
    if (dtAggregated$yErrorType[1] == ospsuite::DataErrorType$GeometricStdDev) {
      dtAggregated[,yMin := yValues/yErrorValues]
      dtAggregated[,yMax := yValues*yErrorValues]
    }
  }

  if (direction == 'x'){
    data.table::setnames(dtAggregated,old = c('yMin','yValues','yMax','yErrorValues','yErrorType'),
                         new = c('xMin', 'x', 'xMax','xErrorValues','xErrorType'),
                         skip_absent = TRUE)
  }


  return(dtAggregated)
}


#' Aggregate Ratios by Bootstrapping
#'
#' This function calculates statistics the results based on bootstrapping.
#'
#' @param nBootstrap An integer specifying the number of bootstrap samples to generate.
#' @param sampleSize number of samples to draw per bootstrap
#' @param value A numeric vector containing the values to aggregate.
#' @param valueReference A numeric vector containing the reference values for aggregation.
#' @param aggregationFunction A function to aggregate the bootstrapped samples.
#' @param confLevel A numeric value indicating the confidence level for the confidence intervals (optional).
#' @param percentiles additional parameter passed to aggregationFunction
#'
#' @return A data frame containing the aggregated ratios and their confidence intervals.
#' @keywords internal
aggregateRatiosByBootstrapping <- function(nBootstrap,value,valueReference,aggregationFun,confLevel = NULL,sampleSize = NULL){

  n1 <- length(value)
  n2 <- length(valueReference)
  if (is.null(sampleSize)){
    sampleSize <- min(n1, n2)
  } else {
    if (sampleSize > min(n1, n2)){
      warning(paste('not enough simulated values for boostrapping with sampleSize',sampleSize, 'for',
                    scenarioName, pkParameter, outputPathId))

    }
  }

  bstrap <-   replicate(nBootstrap,
                        aggregationFun(y = sample(value, size = sampleSize, replace = FALSE)/
                                         sample(valueReference, size = sampleSize, replace = FALSE)),
                        simplify = FALSE
  )

  if (is.list(bstrap[[1]])){
    bstrap <- rbindlist(bstrap)
  } else {
    bstrap <- list(value = unlist(bstrap))
  }

  # Initialize a list to store results
  results <- list()

  # Calculate median and confidence intervals for each numeric column
  for (col in names(bstrap)) {
    if (is.numeric(bstrap[[col]])){
      medianValue <- median(bstrap[[col]], na.rm = TRUE)
      # Store results in a named list
      results[[col]] <- medianValue

      if (!is.null(confLevel)){
        alpha <- 1-confLevel  # For 90% confidence interval
        lowerBound <- quantile(bstrap[[col]], probs = alpha / 2, na.rm = TRUE,names = FALSE)
        upperBound <- quantile(bstrap[[col]], probs = 1 - alpha / 2, na.rm = TRUE,names = FALSE)
        results[[paste0(col, "_lower")]] <- lowerBound
        results[[paste0(col, "_upper")]] <- upperBound
      }
    } else {
      if (dplyr::n_distinct(bstrap[[col]]) > 1){
        warning('aggrgeationFlag not unique')
      }
      results[[col]] <- bstrap[[col]][1]
    }
  }
  return(results)


}



