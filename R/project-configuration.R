#' @title ProjectConfiguration
#' @docType class
#' @description An object storing configuration used project-wide
#' @format NULL
#' @export
ProjectConfigurationRF <- R6::R6Class( # nolint object_name_linter
  "ProjectConfigurationRF",
  inherit = esqlabsR::ProjectConfiguration,
  cloneable = TRUE,
  active = list(
    #' @field addOns list with non default configurations
    addOns = function() {
      if (length(private$.projectConfigurationDataAddOns) == 0) {
        return(list())
      } else {
        return(stats::setNames(
          lapply(names(private$.projectConfigurationDataAddOns), function(property) {
            private$.clean_path(
              private$.projectConfigurationDataAddOns[[property]],
              self$configurationsFolder
            )
          }),
          names(private$.projectConfigurationDataAddOns)
        ))
      }
    }
  ),
  private = list(
    .projectConfigurationDataAddOns = list(),
    #' @description Initializes the ProjectConfiguration object with a specified configuration file path.
    .addOnFile = function(property, value) {
      if (!missing(value)) {
        private$.projectConfigurationDataAddOns[[property]] <- value
      }
    },
    #' @description Adds new line to configuration xlsx
    .writeToConfigXlsx = function(propertyToSet, value, description) {
      wb <- openxlsx::loadWorkbook(self$projectConfigurationFilePath)
      dtConfiguration <- xlsxReadData(wb = wb, sheetName = wb$sheet_names[1])
      if (!(propertyToSet %in% dtConfiguration$property)) {
        dtConfiguration <- rbind(
          dtConfiguration,
          data.table(
            property = propertyToSet,
            value = value,
            description = description
          )
        )
      } else {
        dtConfiguration[property == propertyToSet, `:=`(
          value = value,
          description = description
        )]
      }
      xlsxWriteData(wb = wb, sheetName = wb$sheet_names[1], dt = dtConfiguration)
      openxlsx::saveWorkbook(wb, self$projectConfigurationFilePath, overwrite = TRUE)
    },
    #' @description Read configuration from file
    .read_config = function(file_path) { # nolint
      path <- private$.clean_path(file_path)
      # Update private values
      private$.projectConfigurationFilePath <- path
      private$.projectConfigurationDirPath <- dirname(path)
      data <- readExcel(path = path)
      for (property in intersect(data$Property, names(self))) {
        # Update each private property
        self[[property]] <- data[data$Property == property, ]$Value
      }
      for (property in setdiff(
        data$Property,
        c(names(private$.projectConfigurationDataAddOns), names(self))
      )) {
        private$.addOnFile(
          property = property,
          value = data[data$Property == property, ]$Value
        )
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

      ospsuite.utils::ospPrintItems(
        x = private$.projectConfigurationDataAddOns,
        title = "AddOns (non esqlabR)"
      )

      for (property in names(private$.projectConfigurationDataAddOns)) {
        print(paste(property, fs::path_rel(as.character(private$.projectConfigurationDataAddOns[[property]]))))
      }
      invisible(self)
    },
    #' @description Adds an add-on file to the project configuration.
    #'
    #' @param property A string representing the name of the property to add.
    #' @param value A string representing the basename of the file to add.
    #' @param description A string providing a description of the property.
    #' @param templatePath A string representing the path of the file to add.
    addAddOnFileToConfiguration = function(property, value, description, templatePath) {
      checkmate::assertString(property)
      checkmate::assertString(value)
      checkmate::assertString(description)

      if (!file.exists(file.path(self$configurationsFolder, value))) {
        checkmate::assertFileExists(templatePath)
        invisible(file.copy(
          from = templatePath,
          to = file.path(self$configurationsFolder, value)
        ))
      }

      private$.writeToConfigXlsx(property, value, description)

      private$.addOnFile(
        property = property,
        value = value
      )

      invisible(self)
    },
    #' @description Adds an add-on file to the project configuration.
    #'
    #' @param property A string representing the name of the property to add.
    #' @param value A string representing the path of the value to add.
    #' @param description A string providing a description of the property.
    #' @param templatePath A string representing the path of the template file.
    addAddOnFolderToConfiguration = function(property, value, description) {
      checkmate::assertString(property)
      checkmate::assertString(value)
      checkmate::assertString(description)

      value <- as.character(fs::path_rel(value, start = self$configurationsFolder))

      dirPath <- fs::path_abs(value, start = self$configurationsFolder)
      if (!dir.exists(dirPath)) {
        dir.create(dirPath, recursive = TRUE)
      }

      private$.writeToConfigXlsx(property, value, description)

      private$.addOnFile(
        property = property,
        value = value
      )

      invisible(self)
    }
  )
)
