#' @title ProjectConfiguration
#' @docType class
#' @description An object storing configuration used project-wide
#' @format NULL
#' @export
#' @family project initialization
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
            suppressWarnings(private$.clean_path(
              path = private$.projectConfigurationDataAddOns[[property]],
              parent = self$configurationsFolder,
              replace_env_vars = FALSE
            ))
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
    #' @description Adds new line to RFAddons sheet in configuration xlsx
    .writeToConfigXlsx = function(propertyToSet, value, description) {
      wb <- openxlsx::loadWorkbook(self$projectConfigurationFilePath)

      # Create RFAddons sheet if it does not exist yet
      if (!("RFAddons" %in% wb$sheet_names)) {
        openxlsx::addWorksheet(wb, "RFAddons")
        openxlsx::writeData(
          wb = wb,
          sheet = "RFAddons",
          x = data.frame(
            property = character(),
            value = character(),
            description = character(),
            stringsAsFactors = FALSE
          )
        )
      }

      dtAddOns <- xlsxReadData(wb = wb, sheetName = "RFAddons")
      if (!(propertyToSet %in% dtAddOns$property)) {
        dtAddOns <- rbind(
          dtAddOns,
          data.table(
            property = propertyToSet,
            value = value,
            description = description
          )
        )
      } else {
        dtAddOns[property == propertyToSet, `:=`(
          value = value,
          description = description
        )]
      }
      xlsxWriteData(wb = wb, sheetName = "RFAddons", dt = dtAddOns)
      openxlsx::saveWorkbook(wb, self$projectConfigurationFilePath, overwrite = TRUE)
    },
    #' @description Read configuration from file
    .read_config = function(file_path) { # nolint
      path <- private$.clean_path(file_path, replace_env_var = FALSE)

      # Update private values
      private$.projectConfigurationFilePath <- path
      private$.projectConfigurationDirPath <- dirname(path)

      inputData <- readExcel(path = path)

      # Reset private variables
      private$.replaced_env_vars <- list()
      private$.projectConfigurationData <- list()
      private$.projectConfigurationDataAddOns <- list()

      for (property in intersect(inputData$Property, names(self))) {
        private$.projectConfigurationData[[property]] <- list(
          value = inputData$Value[inputData$Property == property],
          description = inputData$Description[inputData$Property == property]
        )
      }

      private$.checkProjectConfigurationFile()

      # Mark as not modified after loading from file
      private$.modified <- FALSE

      # Read RFAddons sheet for RF-specific addon properties (new format)
      wb <- openxlsx::loadWorkbook(path)
      if ("RFAddons" %in% wb$sheet_names) {
        rfAddOnsData <- xlsxReadData(wb = wb, sheetName = "RFAddons")
        for (i in seq_len(nrow(rfAddOnsData))) {
          private$.addOnFile(
            property = rfAddOnsData$property[i],
            value = rfAddOnsData$value[i]
          )
        }
      }

      # Backward compatibility: read leftover properties from main sheet as addons
      # (for project files that still have RF-specific properties in the main sheet)
      for (property in setdiff(
        inputData$Property,
        c(names(private$.projectConfigurationDataAddOns), names(self))
      )) {
        private$.addOnFile(
          property = property,
          value = inputData[inputData$Property == property, ]$Value
        )
      }
    }
  ),
  public = list(
    #' Initialize
    #'
    #' @param projectConfigurationFilePath A string representing the path to the
    #' project configuration file.
    #' @param ignoreVersionCheck If `TRUE`, skip the esqlabsR version mismatch
    #' check when loading the configuration file. Defaults to `FALSE`.
    initialize = function(projectConfigurationFilePath = character(),
                          ignoreVersionCheck = FALSE) {
      super$initialize(
        projectConfigurationFilePath = projectConfigurationFilePath,
        ignoreVersionCheck = ignoreVersionCheck
      )
    },
    #' Print
    #' @description print prints a summary of the Project Configuration.
    #' @param className Whether to print the name of the class at the beginning. default to TRUE.
    print = function(className = TRUE) {
      super$print(className = className)

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


      dirPath <- fs::path_abs(value, start = self$configurationsFolder)
      if (!dir.exists(dirPath)) {
        success <- dir.create(dirPath, recursive = TRUE)
      }

      value <- as.character(fs::path_rel(value, start = self$configurationsFolder))

      private$.writeToConfigXlsx(property, value, description)

      private$.addOnFile(
        property = property,
        value = value
      )

      invisible(self)
    }
  )
)
