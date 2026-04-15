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
      path <- private$.clean_path(file_path, replace_env_vars = FALSE)

      # Convert legacy main-sheet RF properties to the RFAddons sheet before
      # delegating to the parent reader, which validates the main sheet strictly.
      .convertLegacyConfigSheet(path)

      super$.__enclos_env__$private$.read_config(path)

      private$.projectConfigurationDataAddOns <- list()

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
                          ignoreVersionCheck = FALSE,
                          ...) {
      super$initialize(
        projectConfigurationFilePath = projectConfigurationFilePath,
        ignoreVersionCheck = ignoreVersionCheck,
        ...
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

#' Convert a legacy ProjectConfiguration main sheet to RF format
#'
#' Moves RF-specific properties (those not present in the RF template's allowed
#' list) from the main sheet to the `RFAddons` sheet of the given workbook file,
#' saving the modified workbook in place. This converter runs automatically
#' before the parent `ProjectConfiguration` reader validates the main sheet.
#'
#' @param path Character. Path to the `ProjectConfiguration.xlsx` to convert.
#' @keywords internal
#' @noRd
.convertLegacyConfigSheet <- function(path) {
  wb <- openxlsx::loadWorkbook(path)
  mainSheet <- wb$sheet_names[1]
  dtMain <- xlsxReadData(wb = wb, sheetName = mainSheet)

  allowedProperties <- tryCatch(
    xlsxReadData(
      system.file("templates", "ProjectConfiguration.xlsx",
        package = "ospsuite.reportingframework"
      )
    )$property,
    error = function(...) dtMain$property
  )
  leftoverProperties <- setdiff(dtMain$property, allowedProperties)

  if (length(leftoverProperties) == 0) {
    return(invisible())
  }

  if (!("RFAddons" %in% wb$sheet_names)) {
    openxlsx::addWorksheet(wb, "RFAddons")
    xlsxWriteData(
      wb = wb,
      sheetName = "RFAddons",
      dt = data.table(
        property = character(),
        value = character(),
        description = character()
      )
    )
  }

  dtAddOns <- xlsxReadData(wb = wb, sheetName = "RFAddons")
  dtAddOns <- rbind(
    dtAddOns,
    dtMain[property %in% leftoverProperties, c("property", "value", "description")]
  )
  dtMain <- dtMain[!property %in% leftoverProperties]

  xlsxWriteData(wb = wb, sheetName = mainSheet, dt = dtMain)
  xlsxWriteData(wb = wb, sheetName = "RFAddons", dt = dtAddOns)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)

  return(invisible())
}
