#' @title ProjectConfiguration
#' @docType class
#' @description An object storing configuration used project-wide
#' @format NULL
#' @export
ProjectConfigurationRF <- R6::R6Class(   # nolint object_name_linter
  "ProjectConfiguration",
  inherit = esqlabsR::ProjectConfiguration,
  cloneable = TRUE,
  active = list(
    #' @field pKParameterFile Path to the file containing BMLM Identification
    pKParameterFile = function(value) {
      if (!missing(value)) {
        private$.projectConfigurationData$pKParameterFile$value <- value
      }
      private$.clean_path(
        private$.projectConfigurationData$pKParameterFile$value,
        self$configurationsFolder
      )
    }
  ),
  public = list(
    #' Initialize
    #'
    #' @param projectConfigurationFilePath A string representing the path to the
    #' project configuration file.
    initialize = function(projectConfigurationFilePath = character()) {
      super$initialize(
        projectConfigurationFilePath = projectConfigurationFilePath
      )
    },
    #' Print
    #' @description print prints a summary of the Project Configuration.
    print = function() {
      super$print()
      private$printLine("PK-Parameter Configuration File", fs::path_rel(as.character(self$pKParameterFile)))
      invisible(self)
    }
  )
)
