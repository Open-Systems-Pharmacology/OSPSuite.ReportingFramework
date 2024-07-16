iniLogFileForTest <- function(){
  # initialize logging. Is always needed
  projectPath <- tempdir()
  if (!dir.exists(projectPath)) dir.create(projectPath)
  initLogfunction(projectPath = projectPath, verbose = FALSE)

  return(projectPath)
}


cleanupLogFileForTest <- function(projectPath){

  unlink(projectPath, recursive = TRUE)
  options(OSPSuite.REF.logFileFolder = NULL)
  options(OSPSuite.REF.warningsNotDisplayed = NULL)
  options(OSPSuite.REF.messagesNotDisplayed = NULL)
  options(OSPSuite.REF.verbose = NULL)

}
