#' Checks if scenarioName + extension .xml fullfils conditions for filenames for electronic package
#'
#' @template projectConfig
#'
#' @export
checkScenarioNameValidity <- function(projectConfiguration) {
  scenarios <- getScenarioDefinitions(projectConfiguration)
  files <- paste0(scenarios$Scenario_name, ".xml")

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


#' Check on directory for valid filenams
#'
#' Checks if all files and all files of datasets of a directory fullfill conditions for file names for electronic package
#'
#' @param directory_path path to the driectory where the electronic package is filed
#'
#' @export
checkFileNameValidityForDirectory <- function(directory_path) {
  # List all files in the directory
  files <- list.files(directory_path)

  dataSets <- list.files(directory_path, pattern = ".txt")

  inValidFileNames <- checkFileNameValidity(files, dataSets)

  # Print the result
  if (length(inValidFileNames) == 0) {
    message("All files meet the specified conditions.")
  } else {
    message("Not all files meet the conditions;")
    message(paste(inValidFileNames, collapse = ", "))
  }
}

#' Checks if files given as a character vector fullfills conditions for filen ames for electronic package
#'
#' @param files vector wtih file names
#' @param dataSets  ector with data set names (may be NULL)
#'
#' @return list with inValid file names
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
