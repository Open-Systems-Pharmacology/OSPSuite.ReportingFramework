#' @title enumeration keys for Timerange shortcut
#' @export
TIMERANGE <- ospsuite.utils::enum(c( # nolint
  total = "total",
  firstApplication = "firstApplication",
  lastApplication = "lastApplication"
))

#' @title enumeration keys for DataClass
#' @export
DATACLASS <- ospsuite.utils::enum(c( # nolint
  tpIndividual = "tp Individual",
  tpAggregated = "tp Aggregated",
  pkIndividual = "pk Individual",
  pkAggregated = "pk Aggregated",
  tpTwinPop = "tp Virtual Twin Population"
))

#' @title enumeration keys for exportDirectories
#' @export
EXPORTDIR <- ospsuite.utils::enum(c( # nolint
  simulationResult = "SimulationResults",
  sensitivityResults = "SensitivityResults",
  pKAnalysisResults = "PKAnalysisResults"
))


#' @title enumeration biomrtric units
BIOMETRICUNITS <- ospsuite.utils::enum(c( # nolint
  age = "year(s)",
  weight = "kg",
  height = "cm"
))
