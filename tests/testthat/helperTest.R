# initialize logging. and create test directory
iniLogFileForTest <- function() {
  projectPath <- tempdir()
  if (!dir.exists(projectPath)) dir.create(projectPath)
  initLogfunction(projectPath = projectPath, verbose = FALSE)

  return(projectPath)
}


# Cleanup
cleanupLogFileForTest <- function(projectPath) {
  unlink(projectPath, recursive = TRUE)

  optionsToNull <- grep("ospsuite",
    names(options()),
    ignore.case = TRUE,
    value = TRUE
  )
  for (opt in optionsToNull) {
    eval(parse(text = paste0("options(", opt, " = NULL)")))
  }
}

