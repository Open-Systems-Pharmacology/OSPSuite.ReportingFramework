#' Run Example 1 of the Reporting Framework
#'
#' This function sets up the environment for running Example 1 of the
#' Reporting Framework, following the workflow outlined in the tutorial 'Tutorial-timeprofiles'.
#' It creates a directory structure if it does not exist, and sources
#' the necessary R scripts to execute the workflow.
#'
#' @param rootDirectory A character string specifying the root directory
#'                      where the example files will be created. If NULL,
#'                      a temporary directory will be used.
#'
#' @return A character string indicating the path to the root directory
#'         where the example files were created.
#' @export
runExample1 <- function(rootDirectory = NULL){

  library(data.table)
  library(tidyr)

  if (is.null(rootDirectory)){
    rootDirectory <- tempdir()
  }
  message(rootDirectory)
  if (!dir.exists(file.path(rootDirectory,'Scripts','ReportingFramework')))
    dir.create(file.path(rootDirectory,'Scripts','ReportingFramework'),recursive = TRUE)

  setwd(file.path(rootDirectory,'Scripts','ReportingFramework'))

  source(system.file(
    "extdata", "example_1", "workflow.R",
    package = "ospsuite.reportingframework",
    mustWork = TRUE
  ))

  return(rootDirectory)
}
#' Run Example 1 of the Reporting Framework
#'
#' This function sets up the environment for running Example 1 of the
#' Reporting Framework, following the workflow outlined in the tutorial 'Tutorial-timeprofiles'.
#' It creates a directory structure if it does not exist, and sources
#' the necessary R scripts to execute the workflow.
#'
#' @param rootDirectory A character string specifying the root directory
#'                      where the example files will be created. If NULL,
#'                      a temporary directory will be used.
#'
#' @return A character string indicating the path to the root directory
#'         where the example files were created.
#' @export
runExample2 <- function(rootDirectory = NULL){

  if (is.null(rootDirectory)){
    rootDirectory <- tempdir()
  }
  message(rootDirectory)
  if (!dir.exists(file.path(rootDirectory,'Scripts','ReportingFramework')))
    dir.create(file.path(rootDirectory,'Scripts','ReportingFramework'),recursive = TRUE)

  setwd(file.path(rootDirectory,'Scripts','ReportingFramework'))

  source(system.file(
    "extdata", "example_2", "workflow.R",
    package = "ospsuite.reportingframework",
    mustWork = TRUE
  ))

  return(rootDirectory)
}
