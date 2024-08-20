#' Interpolate
#'
#' This function performs linear extrapolation for increasing yValues and
#' logarithmic interpolation for decreasing yValues based on the specified
#' grouping identifiers. It modifies the input dtObserved data.table by
#' adding a new column with the predicted values.
#'
#' @param dtObserved A data.table containing observed data with xValues and
#'                   grouping identifiers.
#' @param dtSimulated A data.table containing simulated data with xValues
#'                    and corresponding yValues.
#' @param identifiers A character vector of column names used for grouping
#'                    in the extrapolation and interpolation process.
#'
#' @return A data.table with an additional column named 'predicted' containing
#'         the extrapolated and interpolated values.
#' @export
addPredictedValues <- function(dtObserved,dtSimulated,identifier){
  checkmate::assertCharacter(identifier,any.missing = FALSE)
  checkmate::assertDataTable(dtObserved)
  checkmate::assertNames(names(dtObserved),
                         must.include = c('xValues',identifier))
  checkmate::assertDataTable(dtSimulated)
  checkmate::assertNames(names(dtSimulated),
                         must.include = c('xValues','yValues',identifier))

  # make sure to exclude nas and sorting is correct
  dtSimulated <- data.table::copy(dtSimulated) %>%
    dplyr::select(c('xValues','yValues',identifier)) %>%
    data.table::setorderv(c('xValues',identifier))
  dtSimulated <- dtSimulated[!is.nan(xValues) & !is.nan(yValues)]

  dtObserved[, predicted := {
    # Filter the simulated data based on the current group
    filterConditions <- lapply(identifier, function(id) dtSimulated[[id]] == .BY[[id]])
    filteredX <- dtSimulated[Reduce(`&`, filterConditions)]$xValues
    filteredY <- dtSimulated[Reduce(`&`, filterConditions)]$yValues

    # Get the current xValues from dtObserved
    currentXValues <- xValues  # This refers to the xValues column in dtObserved

    # Initialize an empty vector for predictions
    predicted <- rep(NA, length(currentXValues))

    # Check if there are enough filtered points
    if (length(filteredX) < 2) {
      warning("Not enough data points for ", paste(identifier, .BY, collapse = ", "))
    } else {
      # Create a data.table for easier manipulation
      dtSimulatedGroup <- data.table(x = filteredX, y = filteredY)

      # Calculate differences
      dtSimulatedGroup[, diffY := c(NA, diff(y))]
      dtSimulatedGroup$diffY[1] <- dtSimulatedGroup$diffY[2]
      dtSimulatedGroup[y <= 0,diffY := 0]
      dtSimulatedGroup[data.table::shift(y,-1) <= 0,diffY := 0]

      maxX <- max(dtSimulatedGroup$x)

      # Loop through each xValue in the current group
      for (i in seq_along(currentXValues)) {
        xVal <- currentXValues[i]

        # Find the closest x in the filtered data
        closestIdx <- which(filteredX >= xVal)[1]

        # Check if the slope is positive or negative
        if (length(closestIdx) < 1 | xVal > maxX){
          predicted[i] <- NA  # Handle values outside limits
        } else {
          if (dtSimulatedGroup$diffY[closestIdx] >= 0 ) {
            # Linear extrapolation for increasing slope
            predicted[i] <- approx(x = dtSimulatedGroup$x,
                                   y = dtSimulatedGroup$y,
                                   xout = xVal,
                                   rule = 1)$y
          } else {
            predicted[i] <- exp(approx(x = dtSimulatedGroup$x,
                                       y = log(dtSimulatedGroup$y),
                                       xout = xVal,
                                       rule = 1)$y)
          }
        }
      }
    }

    predicted  # Return the predictions to be assigned to the new column
  }, by = identifier]

  return(dtObserved)  # Return the modified dtObserved
}
