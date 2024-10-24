#' Checks if scenarioName + extension .xml fulfills conditions for file names for electronic package
#'
#' @template projectConfig
#'
#' @export
checkScenarioNameValidity <- function(scenarioNames) {
  files <- paste0(scenarioNames, ".xml")

  inValidFileNames <- gsub(".xml", "", checkFileNameValidity(files))

  if (length(inValidFileNames) > 0) {
    stop(paste(
      "Invalid scenarionames: ",
      paste(inValidFileNames, collapse = ", "),
      "\n Scenario names are used to generate filenames for the electronic package.
               Use only alphanumerical letters and _ , use lower case
               and maximal length including extension must be less than 64"
    ))
  }
}


#' Check on directory for valid file names
#'
#' Checks if all files and all files of data sets of a directory fulfill conditions for file names for electronic package
#'
#' @param directoryPath path to the directory where the electronic package is filed
#'
#' @export
checkFileNameValidityForDirectory <- function(directoryPath) {
  # List all files in the directory
  files <- list.files(directoryPath)

  dataSets <- list.files(directoryPath, pattern = ".txt")

  inValidFileNames <- checkFileNameValidity(files, dataSets)

  # Print the result
  if (length(inValidFileNames) == 0) {
    message("All files meet the specified conditions.")
  } else {
    message("Not all files meet the conditions;")
    message(paste(inValidFileNames, collapse = ", "))
  }
}

#' Check File Name Validity
#'
#' This function checks the validity of file names and dataset names based on specified criteria,
#' including allowed file extensions and maximum length restrictions.
#'
#' @param files A character vector of file names to validate. Each file name should have
#'   a valid extension (xml, txt, or csv) and must not exceed 64 characters in length.
#'
#' @param dataSets A character vector of dataset names to validate. Each dataset name must
#'   not exceed 32 characters in length. This parameter is optional.
#'
#' @details
#' The function checks the following conditions for file names:
#' - Must match the regular expression pattern allowing only lowercase letters, numbers, and underscores.
#' - Must end with one of the allowed file extensions.
#' - Must not exceed the maximum length of 64 characters.
#'
#' For dataset names, the function checks that they do not exceed the maximum length of 32 characters.
#'
#' @return A character vector containing the names of invalid files and datasets that do not meet the specified criteria.
#'
#' @examples
#' # Example usage:
#' files <- c("valid_file.xml", "invalid_file.txt", "too_long_file_name_exceeding_the_limit.csv")
#' dataSets <- c("validDataset", "invalidDatasetNameThatIsWayTooLong")
#' invalidNames <- checkFileNameValidity(files, dataSets)
#' print(invalidNames)
#'
#' @export
checkFileNameValidity <- function(files, dataSets = NULL) {
  maxLengthForFiles <- 64
  maxLengthForDataSet <- 32

  files <- unique(files, dataSets)

  # Define the allowed file extensions
  allowedExtensions <- c("xml", "txt", "csv")

  # Define the regular expression pattern for the file names
  pattern <- paste0(
    "^[a-z0-9_]+\\.(",
    paste(allowedExtensions, collapse = "|"),
    ")$"
  )

  # Check if all files meet the conditions
  doesFileMeetCondition <-
    grepl(pattern, files) &
    sapply(strsplit(files, "\\."), function(x) tolower(x[length(x)]) %in% allowedExtensions) &
    sapply(files, function(x) {
      nchar(x) <= maxLengthForFiles
    })
  inValidFileNames <- files[!doesFileMeetCondition]

  if (!is.null(dataSets)) {
    doesFileMeetDataSetConditions <-
      sapply(dataSets, function(x) {
        nchar(x) <= maxLengthForDataSet
      })
    inValidFileNames <- c(inValidFileNames, dataSets[!doesFileMeetDataSetConditions])
  }

  return(inValidFileNames)
}
