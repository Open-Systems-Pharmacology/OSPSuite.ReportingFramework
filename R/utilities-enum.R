# nolint start: object_name_linter

#' @title enumeration keys for Time range shortcut
#' @export
#' @family enumerations
TIMERANGE <- ospsuite.utils::enum(c(
  total = "total",
  firstApplication = "firstApplication",
  lastApplication = "lastApplication"
))

#' @title enumeration keys for `DataClass`
#' @export
#' @family enumerations
DATACLASS <- ospsuite.utils::enum(c(
  tpIndividual = "tp Individual",
  tpAggregated = "tp Aggregated",
  pkIndividual = "pk Individual",
  pkAggregated = "pk Aggregated",
  tpTwinPop = "tp Virtual Twin Population",
  numericValues = "numeric Values"
))


#' @title enumeration keys for exportDirectories
#' @export
#' @family enumerations
EXPORTDIR <- ospsuite.utils::enum(c(
  simulationResult = "SimulationResults",
  sensitivityResults = "SensitivityResults",
  pKAnalysisResults = "PKAnalysisResults"
))


#' @title enumeration biometric units
#' @export
#' @family enumerations
BIOMETRICUNITS <- ospsuite.utils::enum(c(
  age = "year(s)",
  weight = "kg",
  height = "cm"
))

# nolint end
