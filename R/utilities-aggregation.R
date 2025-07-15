#' Get Aggregation Function
#'
#' This function retrieves the appropriate aggregation function based on the specified aggregation flag.
#'
#' @param aggregationFlag A character string indicating the aggregation method.
#'       Must be one of the options from `ospsuite::DataErrorType`, "Percentiles", or "Custom".
#' @param percentiles A numeric vector of percentiles to calculate
#'      if `aggregationFlag` is "Percentiles".
#'      Must have a length of 3, be sorted, and within the range [0, 1].
#' @param legendsize An integer indicating the size of the legend vector.
#'        Supported values are 2 or 3, which correspond to
#'        different formats for displaying percentile results.
#' @param customFunction A custom function for aggregation if `aggregationFlag` is "Custom".
#' A custom function should take a numeric vector `y` as input and return a list containing:
#' - `yValues`: The aggregated value (e.g., mean).
#' - `yMin`: The lower value of the aggregated data, (e.g. mean - sd).
#' - `yMax`: The upper value of the aggregated data, (e.g. mean + sd).
#' - `yErrorType`: A string indicating the type of error associated with the aggregation,
#' it is used in plot legends and captions.
#' It must be a concatenation of the descriptor of yValues and the descriptor of yMin - yMax range
#' separated by "|" (e.g., "mean | standard deviation" or "median | 5th - 95th percentile").
#' If legendsize 3 is needed should contain 3 elements, e.g. "median | 5th percentile | 95th percentile"
#'
#' @return A function that performs the specified aggregation. The returned function accepts a numeric vector and returns a list containing the aggregated values and error types.
#' @export
getAggregationFunction <- function(aggregationFlag,
                                   percentiles,
                                   customFunction,
                                   legendsize = 2) {
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
    yErroryType <- getErrorTypeForPercentiles(percentiles, legendsize = legendsize)
    aggregationFunction <-
      function(y) {
        y <- y[!is.na(y)]
        return(list(
          yMin = stats::quantile(y, probs = percentiles[1], names = FALSE),
          yValues = stats::quantile(y, probs = percentiles[2], names = FALSE),
          yMax = stats::quantile(y, probs = percentiles[3], names = FALSE),
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
#' @param percentiles A numeric vector containing percentiles. Must be of length 3.
#' @param legendsize An integer indicating the size of the legend vector. Supported values are 2 or 3, which affect the format of the output string.
#'
#' @return A character string with the error type for the given percentiles,
#' formatted according to the specified legendsize.
#' @keywords internal
getErrorTypeForPercentiles <- function(percentiles, legendsize) {
  mName <- formatPercentiles(percentiles[2], suffix = " percentile")

  yMaxTxt <- formatPercentiles(percentiles[3], suffix = " percentile")

  result <- switch(as.character(legendsize),
    "2" = {
      yMinTxt <- formatPercentiles(percentiles[1], suffix = ifelse(percentiles[3] == 1, " percentile", ""))
      paste(mName, "|", trimws(yMinTxt), "-", trimws(yMaxTxt))
    },
    "3" = {
      yMinTxt <- formatPercentiles(percentiles[1], suffix = " percentile")
      paste(mName, "|", trimws(yMinTxt), "|", trimws(yMaxTxt))
    },
    stop("Error: legendsize not covered. Please provide a legendsize of 2 or 3.")
  )
  return(result)
}

#' Perform Aggregation
#'
#' This function performs the aggregation of observed data based on the specified criteria.
#'
#' @param dataToAggregate A data.table containing the data to be aggregated.
#' It must include the column 'yValues' and optionally 'lloq'.
#' @param aggregationFun A function to aggregate the data.
#' This function should accept a numeric vector and return a list with aggregated values.
#' @param aggrCriteria A character vector specifying the columns to group by.
#'
#' @return A data.table containing aggregated results with counts (`numberOfIndividuals`),
#' aggregated values, and the number of measurements below the lower limit of quantification (`nBelowLLOQ`).
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
#' @return A character vector of length 2 containing descriptors for mean and range,
#' based on the specified error type.
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
#' It must include the column specified in `valueColumn`.
#' @param aggregationFun A function to aggregate the data.
#' This function should accept a numeric vector and return a list with aggregated values.
#' @param valueColumn A string indicating the column name containing the values to aggregate.
#' @param identifier A character vector specifying the columns to group by.
#' @param direction A character string specifying the direction of aggregation,
#' either 'y' or 'x'.
#'
#' @return A data.table containing the aggregated variance results, including the aggregated values and error types.
#' @keywords internal
getAggregatedVariance <- function(dt,
                                  aggregationFun,
                                  valueColumn,
                                  identifier,
                                  direction = c("y", "x")) {
  # initialize to avoid linter messages
  yMin <- yValues <- NULL

  direction <- match.arg(direction)

  dtAggregated <- dt[, as.list(c(
    list(N = length(!is.na(get(valueColumn)))),
    aggregationFun(get(valueColumn))
  )), by = identifier]

  # if (!is.null(dtAggregated$yErrorValues)) {
  #   if (dtAggregated$yErrorType[1] == ospsuite::DataErrorType$ArithmeticStdDev) {
  #     dtAggregated[, yMin := yValues - yErrorValues]
  #     dtAggregated[, yMax := yValues + yErrorValues]
  #   }
  #   if (dtAggregated$yErrorType[1] == ospsuite::DataErrorType$GeometricStdDev) {
  #     dtAggregated[, yMin := yValues / yErrorValues]
  #     dtAggregated[, yMax := yValues * yErrorValues]
  #   }
  # }

  if (direction == "x") {
    data.table::setnames(dtAggregated,
      old = c("yMin", "yValues", "yMax", "yErrorValues", "yErrorType"),
      new = c("xMin", "xValues", "xMax", "xErrorValues", "xErrorType"),
      skip_absent = TRUE
    )
  }


  return(dtAggregated)
}


#' Calculate Aggregation and Confidence Interval by Group
#'
#' This function calculates a specified aggregation function (e.g., geometric mean)
#' and the associated confidence interval using bootstrapping, grouped by specified identifiers.
#' The function allows for flexible definitions of the aggregation function and can handle
#' different value columns and output formats based on the specified direction.
#' A unique seed for bootstrapping is generated for each group of identifiers to ensure
#' reproducibility of the bootstrap samples.
#'
#' @param dt A data.table containing the data. It must include the columns specified in
#'   `identifier` and the `valueColumn`.
#' @param aggregationFun A function to calculate the aggregation (e.g., geometric mean).
#'   This function should accept a numeric vector and return a single numeric value.
#' @param confLevel A numeric value representing the confidence level for the confidence interval.
#'   The default value is 0.9, corresponding to a 90percent confidence interval. Must be between 0 and 1.
#' @param identifier A character vector of column names in the data.table to group by.
#'   The function will calculate the aggregation and confidence interval for each unique combination
#'   of these identifiers.
#' @param nBootstrap An integer specifying the number of bootstrap samples to use. The default is 100.
#' @param valueColumn The name of the column containing the values to aggregate. The default is 'value'.
#' @param direction A character string indicating the direction of the results. It can be either
#'   'y' (default) or 'x'. This affects how the results are named in the output data.table.
#'
#' @return A data.table containing the following columns:
#'   - `yValues`: The estimated value from the aggregation function.
#'   - `yMin`: The lower bound of the confidence interval.
#'   - `yMax`: The upper bound of the confidence interval.
#'   - `seed`: The seed used for bootstrapping, derived from the identifiers.
#'   - `yErrorType`: A descriptive string indicating the aggregation function and confidence interval bounds.
#'
#'
#' @export
calculateAggregationWithCIBYGroup <- function(dt,
                                              aggregationFun,
                                              confLevel = 0.9,
                                              identifier,
                                              nBootstrap = 100,
                                              valueColumn = "value",
                                              direction = "y") {
  # Check if the identifiers are present in the data.table
  checkmate::assertNames(c(identifier, valueColumn), subset.of = names(dt))

  # Create a function for bootstrapping
  bootFun <- function(data, indices) {
    sampleData <- data[indices]
    return(aggregationFun[[1]](sampleData))
  }

  # get rid of nas
  dt <- copy(dt)[!is.na(valueColumn)]

  # Group by the identifiers and calculate the aggregation and confidence interval
  results <- dt[,
    {
      # us for this group of identifier always the same seed
      set.seed(sum(utf8ToInt(paste(.BY, collapse = "_"))))

      # Perform bootstrapping
      bootResults <- boot::boot(data = get(valueColumn), statistic = bootFun, R = nBootstrap)

      # Calculate confidence interval
      ci <- boot::boot.ci(bootResults, type = "basic", conf = confLevel)

      # Store the results in a list
      list(
        N = length(bootResults$data),
        yValues = ci$t0,
        yMin = ci$basic[4],
        yMax = ci$basic[5]
      )
    },
    by = identifier
  ]

  results[, yErrorType := paste(names(aggregationFun),
    paste0(confLevel * 100, "% CI lower"),
    paste0(confLevel * 100, "% CI upper"),
    sep = "|"
  )]

  if (direction == "x") {
    data.table::setnames(results,
      old = c("yMin", "yValues", "yMax", "yErrorValues", "yErrorType"),
      new = c("xMin", "xValues", "xMax", "xErrorValues", "xErrorType"),
      skip_absent = TRUE
    )
  }

  # Return the results
  return(results)
}
