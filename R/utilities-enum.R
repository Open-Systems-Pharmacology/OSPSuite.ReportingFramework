#' @title enumeration keys for type of facetting
#' @export
FACETTYPE <- ospsuite.utils::enum(c( # nolint
  byOrder = "by order",
  vsOutput = "Scenario vs Output",
  vsTimeRange = "Scenario vs TimeRange",
  byIndividual = "by Individual"
))


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
  pKAnalysisResults = "PKAnalysisResults",
  analysisProgram = "AnalysisProgram"
))

