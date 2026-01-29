#' @title enumeration keys for Time range shortcut
#' @export
#' @family enumerations
TIMERANGE <- ospsuite.utils::enum(c( # nolint
  total = "total",
  firstApplication = "firstApplication",
  lastApplication = "lastApplication"
))

#' @title enumeration keys for `DataClass`
#' @export
#' @family enumerations
DATACLASS <- ospsuite.utils::enum(c( # nolint
  tpIndividual = "tp Individual",
  tpAggregated = "tp Aggregated",
  pkIndividual = "pk Individual",
  pkAggregated = "pk Aggregated",
  tpTwinPop = "tp Virtual Twin Population"
))

#' @title enumeration keys for exportDirectories
#' @export
#' @family enumerations
EXPORTDIR <- ospsuite.utils::enum(c( # nolint
  simulationResult = "SimulationResults",
  sensitivityResults = "SensitivityResults",
  pKAnalysisResults = "PKAnalysisResults"
))


#' @title enumeration biometric units
#' @export
#' @family enumerations
BIOMETRICUNITS <- ospsuite.utils::enum(c( # nolint
  age = "year(s)",
  weight = "kg",
  height = "cm"
))

#' @title Default values for time profiles configuration columns
#' @export
#' @family enumerations
TIMEPROFILES_CONFIG_DEFAULTS <- list( # nolint
  timeUnit = "h",
  timeOffset = 0,
  timeOffset_Reference = 0,
  splitPlotsPerTimeRange = 1,
  nFacetColumns = 3,
  yScale = "linear, log",
  facetScale = "fixed",
  plot_TimeProfiles = 1,
  plot_PredictedVsObserved = 0,
  plot_ResidualsAsHistogram = 0,
  plot_ResidualsVsTime = 0,
  plot_ResidualsVsObserved = 0,
  plot_QQ = 0
)
