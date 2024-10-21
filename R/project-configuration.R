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
    addOns = function() {
      stats::setNames(lapply(names(private$.projectConfigurationDataAddOns),function(property){
      private$.clean_path(
        private$.projectConfigurationDataAddOns[[property]],
        self$configurationsFolder
      )}),
      names(private$.projectConfigurationDataAddOns))
    }
  ),
  private = list(
    .projectConfigurationDataAddOns = list(),
    .addOnFile = function(property,value) {
      if (!missing(value)) {
        private$.projectConfigurationDataAddOns[[property]]$value <- value
      }
    },
    .read_config = function(file_path) {
      path <- private$.clean_path(file_path)
      # Update private values
      private$.projectConfigurationFilePath <- path
      private$.projectConfigurationDirPath <- dirname(path)
      data <- readExcel(path = path)
      for (property in intersect(data$Property,names(self))) {
        # Update each private property
        self[[property]] <- data[data$Property == property, ]$Value
      }
      for (property in setdiff(data$Property,names(private$.projectConfigurationDataAddOns))){
        private$.addOnFile(property = property,
                           value = data[data$Property == property, ]$Value)
      }
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
      for (property in names(private$.projectConfigurationDataAddOns)){
        private$printLine(property, fs::path_rel(as.character(private$.projectConfigurationDataAddOns[[property]]$value )))
      }
      invisible(self)
    },
    #' @param projectConfigurationFilePath A string representing the path to the
    #' @param property Property to add
    #' @param value Value of added property
    #' @param templatePath path of file template
    addAddOnfileToConfiguration = function(property,value,description,templatePath){
      checkmate::assertString(property)
      checkmate::assertString(value)


      if (!file.exists(file.path(self$configurationsFolder, value))) {
        checkmate::assertFileExists(templatePath)
        invisible(file.copy(
          from = templatePath,
          to = file.path(self$configurationsFolder, value)
        ))
      }

      if (!('bMLMConfigurationFile' %in% names(self))){
        wb = openxlsx::loadWorkbook(self$projectConfigurationFilePath)
        dtConfiguration <- xlsxReadData(wb = wb,sheetName =  wb$sheet_names[1])
        dtConfiguration <- rbind(dtConfiguration,
                                 data.table(property = property,
                                            value = value,
                                            description = description))
        xlsxWriteData(wb = wb,sheetName =  wb$sheet_names[1],dt = dtConfiguration)
        openxlsx::saveWorkbook(wb,self$projectConfigurationFilePath,overwrite = TRUE)

      }

      private$.addOnFile(property = property,
                         value = value)

      invisible(self)
    }
  )
)
