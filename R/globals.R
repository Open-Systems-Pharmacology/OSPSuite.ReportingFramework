# defining global variables and functions to appease R CMD Check

utils::globalVariables(
  names = c(
    "configEnv"
  ),
  package = "ospsuite.reportingframework",
  add = FALSE
)

#' Standard property names stored by esqlabsR::ProjectConfiguration
#' @keywords internal
STANDARD_ESQLABSR_PROJECT_CONFIGURATION_PROPERTIES <- c(
  "modelFolder",
  "configurationsFolder",
  "modelParamsFile",
  "individualsFile",
  "populationsFile",
  "populationsFolder",
  "scenariosFile",
  "applicationsFile",
  "plotsFile",
  "parameterIdentificationFile",
  "dataFolder",
  "dataFile",
  "dataImporterConfigurationFile",
  "outputFolder",
  "esqlabsRVersion"
)

#' Property name for the reporting framework version in ProjectConfiguration.xlsx
#' @keywords internal
REPORTING_FRAMEWORK_VERSION_PROPERTY <- "ospsuiteReportingFrameworkVersion"
