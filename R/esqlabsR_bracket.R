#' Initialize Project
#'
#' @description
#'
#' Creates the default project folder structure with excels file templates in
#' the working directory.
#'
#' @param projectPath A string defining the path where to initialize the project.
#'  default to current working directory.
#' @param sourceFolder path of tempalte directory available is
#'    templateDirectory() default path of OSPSuite.ReportingFramework
#'    esqlabsR:::example_directory("TestProject")  defaultPath fro Esqlabs Projects
#' @param overwrite A boolean, if TRUE existing files will be overwritten
#'
#' @returns path of project
#'
#' @export
initProject <- function(projectPath = ".",
                        sourceFolder,
                        overwrite = FALSE) {
  projectPath <- fs::path_abs(projectPath)

  checkmate::assertDirectoryExists(sourceFolder)


  dirsToCopy <- fs::path_rel(path = list.dirs(file.path(sourceFolder)), start = sourceFolder)

  for (d in dirsToCopy) {
    if (!dir.exists(file.path(projectPath, d))) {
      dir.create(file.path(projectPath, d), recursive = TRUE, showWarnings = FALSE)
    }

    fileList <- fs::path_rel(path = fs::dir_ls(file.path(sourceFolder, d), type = "file"), start = sourceFolder)

    for (f in fileList) {
      if (!file.exists(file.path(projectPath, f)) | overwrite) {
        file.copy(
          from = file.path(sourceFolder, f),
          to = file.path(projectPath, f),
          overwrite = overwrite
        )
      }
    }
  }

  return(projectPath)
}




#' Create a default `ProjectConfiguration`
#'
#' wraps (esqlabsR::createDefaultProjectConfiguration())
#'
#' @param path Full path of an XLS/XLSX file
#'
#' @return Object of type ProjectConfiguration
#'
#' @export
createProjectConfiguration <- function(path) {
  logCatch({
    projectConfig <- esqlabsR::createDefaultProjectConfiguration(path = path)
    message(projectConfig)
  }  )

  return(projectConfig)
}
