# defining global variables and functions to appease R CMD Check

utils::globalVariables(
  names = c(
    "configEnv"
  ),
  package = "ospsuite.reportingframework",
  add = FALSE
)
