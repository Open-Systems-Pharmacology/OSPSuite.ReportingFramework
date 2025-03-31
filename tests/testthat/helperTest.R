#' Setup Test Directory for Testing
#'
#' This function sets up a test directory specifically for use in unit tests with
#' the `testthat` framework.
#'
#'
#' @return A list containing:
#'   - `projectConfiguration`: The configuration of the initialized project.
#'   - `scenarioList`: A list of scenarios initialized for the project.
#'   - `scenarioResults`: The results from running the scenarios.
#'
#' @export
setupTestDirectoryForTests <- function() {
  message('load test project')
  buildTestData(rootDirectory = NULL, verbose = FALSE, writeTestData = FALSE)
}
