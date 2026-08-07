# auxiliaries ------------
#' Reporting framework version description
#' @return Description text for the stored reporting framework version.
#' @keywords internal
#' @noRd
.reportingFrameworkVersionDescription <- function() {
  return(
    "Version of the ospsuite.reportingframework package used to create this configuration"
  )
}

#' Current reporting framework version
#' @return Installed ospsuite.reportingframework package version as a character string.
#' @keywords internal
#' @noRd
.currentReportingFrameworkVersion <- function() {
  return(as.character(utils::packageVersion("ospsuite.reportingframework")))
}

#' Stamp reporting framework version into ProjectConfiguration.xlsx
#' @param path Path to ProjectConfiguration.xlsx
#' @return Invisible `NULL`.
#' @keywords internal
#' @noRd
.stampReportingFrameworkVersion <- function(path) {
  if (!file.exists(path)) {
    return(invisible(NULL))
  }

  wb <- openxlsx::loadWorkbook(path)
  targetSheet <- if ("addons" %in% wb$sheet_names) {
    "addons"
  } else {
    wb$sheet_names[1]
  }
  dtConfiguration <- xlsxReadData(wb = wb, sheetName = targetSheet)
  if (REPORTING_FRAMEWORK_VERSION_PROPERTY %in% dtConfiguration$property) {
    # nolint: object_usage_linter.
    return(invisible(NULL))
  }

  dtConfiguration <- rbind(
    dtConfiguration,
    data.table(
      property = REPORTING_FRAMEWORK_VERSION_PROPERTY, # nolint: object_usage_linter.
      value = .currentReportingFrameworkVersion(),
      description = .reportingFrameworkVersionDescription()
    )
  )
  xlsxWriteData(wb = wb, sheetName = targetSheet, dt = dtConfiguration)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  return(invisible(NULL))
}

#' Delegate active binding to the base project configuration
#' @param field Name of the delegated field.
#' @param readonly Logical flag indicating whether the binding is read-only.
#' @return A function implementing the active binding.
#' @keywords internal
#' @noRd
.delegateProjectConfigurationBinding <- function(field, readonly = FALSE) {
  checkmate::assertString(field)

  binding <- if (readonly) {
    eval(substitute(
      function(value) {
        if (!missing(value)) {
          stop(paste0(FIELD, " is readonly"))
        }
        self$baseProjectconfiguration[[FIELD]]
      },
      list(FIELD = field)
    ))
  } else {
    eval(substitute(
      function(value) {
        if (missing(value)) {
          self$baseProjectconfiguration[[FIELD]]
        } else {
          self$baseProjectconfiguration[[FIELD]] <- value
          self
        }
      },
      list(FIELD = field)
    ))
  }

  return(binding)
}

.readonlyProjectConfigurationFields <- c(
  "projectConfigurationDirPath",
  "modified",
  "esqlabsRVersion"
)

.delegatedProjectConfigurationFields <- unique(c(
  STANDARD_ESQLABSR_PROJECT_CONFIGURATION_PROPERTIES,
  .readonlyProjectConfigurationFields
))

.delegatedProjectConfigurationBindings <- stats::setNames(
  lapply(
    .delegatedProjectConfigurationFields,
    function(field) {
      .delegateProjectConfigurationBinding(
        field,
        readonly = field %in% .readonlyProjectConfigurationFields
      )
    }
  ),
  .delegatedProjectConfigurationFields
)
# class -------------
#' @title ProjectConfigurationRF
#' @docType class
#' @description An object storing configuration used project-wide, composed of
#'   an esqlabsR project configuration and reporting-framework add-ons.
#' @format NULL
#' @export
#' @family project initialization
ProjectConfigurationRF <- # nolint object_name_linter
  R6::R6Class(
    "ProjectConfigurationRF",
    cloneable = FALSE,
    active = c(
      .delegatedProjectConfigurationBindings,
      list(
        #' @field projectConfigurationFilePath Path to the project configuration file.
        projectConfigurationFilePath = function(value) {
          if (missing(value)) {
            self$baseProjectconfiguration$projectConfigurationFilePath
          } else {
            private$.loadBaseProjectConfiguration(
              projectConfigurationFilePath = value,
              ignoreVersionCheck = private$.ignoreVersionCheck
            )
            private$.readAddOns()
            self
          }
        },
        #' @field addOns list with non default configurations
        addOns = function() {
          if (length(private$.projectConfigurationDataAddOns) == 0) {
            return(list())
          }

          stats::setNames(
            lapply(
              names(private$.projectConfigurationDataAddOns),
              function(property) {
                suppressWarnings(fs::path_abs(
                  file.path(
                    self$baseProjectconfiguration$configurationsFolder,
                    private$.projectConfigurationDataAddOns[[property]]
                  )
                ))
              }
            ),
            names(private$.projectConfigurationDataAddOns)
          )
        },
        #' @field ospsuiteReportingFrameworkVersion Version of the reporting framework
        #'   package stored in the project configuration. Read-only.
        ospsuiteReportingFrameworkVersion = function(value) {
          if (!missing(value)) {
            stop("ospsuiteReportingFrameworkVersion is readonly")
          }
          private$.reportingFrameworkVersion
        }
      )
    ),
    private = list(
      .projectConfigurationDataAddOns = list(),
      .addOnDescriptions = list(),
      .reportingFrameworkVersion = NULL,
      .ignoreVersionCheck = FALSE,
      .addOnFile = function(property, value) {
        if (!missing(value)) {
          private$.projectConfigurationDataAddOns[[property]] <- value
        }
      },
      .writeStandardConfigToXlsx = function(standardData, path) {
        openxlsx::write.xlsx(
          x = standardData[,
            c("Property", "Value", "Description"),
            drop = FALSE
          ],
          file = path,
          overwrite = TRUE
        )
        invisible(NULL)
      },
      .loadBaseProjectConfiguration = function(
        projectConfigurationFilePath,
        ignoreVersionCheck = FALSE
      ) {
        if (
          missing(projectConfigurationFilePath) ||
            length(projectConfigurationFilePath) == 0 ||
            !nzchar(projectConfigurationFilePath[[1]])
        ) {
          self$baseProjectconfiguration <- esqlabsR::ProjectConfiguration$new(
            ignoreVersionCheck = ignoreVersionCheck
          )
          return(invisible(NULL))
        }

        if (!file.exists(projectConfigurationFilePath)) {
          self$baseProjectconfiguration <- esqlabsR::ProjectConfiguration$new(
            projectConfigurationFilePath = projectConfigurationFilePath,
            ignoreVersionCheck = ignoreVersionCheck
          )
          return(invisible(NULL))
        }

        inputData <- esqlabsR::readExcel(path = projectConfigurationFilePath)
        hasAddOns <- any(
          !inputData$Property %in%
            c(
              STANDARD_ESQLABSR_PROJECT_CONFIGURATION_PROPERTIES,
              REPORTING_FRAMEWORK_VERSION_PROPERTY
            )
        )

        if (hasAddOns) {
          standardData <- inputData[
            inputData$Property %in%
              STANDARD_ESQLABSR_PROJECT_CONFIGURATION_PROPERTIES,
            ,
            drop = FALSE
          ]
          backupPath <- tempfile(fileext = ".xlsx")
          file.copy(projectConfigurationFilePath, backupPath, overwrite = TRUE)
          private$.writeStandardConfigToXlsx(
            standardData,
            projectConfigurationFilePath
          )
        }

        self$baseProjectconfiguration <- esqlabsR::ProjectConfiguration$new(
          projectConfigurationFilePath = projectConfigurationFilePath,
          ignoreVersionCheck = ignoreVersionCheck
        )

        if (hasAddOns) {
          file.copy(backupPath, projectConfigurationFilePath, overwrite = TRUE)
          unlink(backupPath)
        }

        invisible(NULL)
      },
      .readReportingFrameworkVersion = function(inputData) {
        if (REPORTING_FRAMEWORK_VERSION_PROPERTY %in% inputData$Property) {
          private$.reportingFrameworkVersion <- inputData$Value[
            inputData$Property == REPORTING_FRAMEWORK_VERSION_PROPERTY
          ]
        } else {
          private$.reportingFrameworkVersion <- NULL
        }
        invisible(NULL)
      },
      .writeReportingFrameworkVersion = function() {
        currentVersion <- .currentReportingFrameworkVersion()
        private$.reportingFrameworkVersion <- currentVersion
        private$.writeToConfigXlsx(
          propertyToSet = REPORTING_FRAMEWORK_VERSION_PROPERTY,
          value = currentVersion,
          description = .reportingFrameworkVersionDescription()
        )
        invisible(NULL)
      },
      .checkReportingFrameworkVersion = function() {
        if (private$.ignoreVersionCheck) {
          return(invisible(NULL))
        }

        storedVersion <- private$.reportingFrameworkVersion
        currentVersion <- .currentReportingFrameworkVersion()
        versionIssue <- if (
          is.null(storedVersion) ||
            length(storedVersion) == 0 ||
            is.na(storedVersion)
        ) {
          "notStored"
        } else if (!identical(as.character(storedVersion), currentVersion)) {
          "mismatch"
        } else {
          NULL
        }

        if (is.null(versionIssue)) {
          return(invisible(NULL))
        }

        if (versionIssue == "notStored") {
          message(
            "No ospsuite.reportingframework version is stored in the project configuration. ",
            "The configuration may have been created with an older version of the package."
          )
        } else {
          message(
            "The ospsuite.reportingframework version stored in the project configuration (",
            storedVersion,
            ") does not match the currently installed version (",
            currentVersion,
            ")."
          )
        }

        if (interactive()) {
          qs <- sample(c("Absolutely not", "Yes", "No way"))
          out <- utils::menu(
            title = "Do you want to update the version in the project configuration and continue?",
            choices = qs
          )
          if (out == 0L || qs[[out]] != "Yes") {
            stop("Aborted by user.")
          }
        } else {
          stop(
            "ospsuite.reportingframework version mismatch. ",
            "You must update the version in the project configuration before continuing.",
            call. = FALSE
          )
        }

        private$.writeReportingFrameworkVersion()
        invisible(NULL)
      },
      .writeToConfigXlsx = function(propertyToSet, value, description) {
        configPath <- self$baseProjectconfiguration$projectConfigurationFilePath
        wb <- openxlsx::loadWorkbook(configPath)
        targetSheet <- if ("addons" %in% wb$sheet_names) {
          "addons"
        } else {
          wb$sheet_names[1]
        }
        if (!(targetSheet %in% wb$sheet_names)) {
          openxlsx::addWorksheet(wb = wb, sheetName = targetSheet)
          openxlsx::writeData(
            wb = wb,
            sheet = targetSheet,
            x = data.frame(
              Property = character(),
              Value = character(),
              Description = character(),
              stringsAsFactors = FALSE
            )
          )
        }
        dtConfiguration <- xlsxReadData(wb = wb, sheetName = targetSheet)
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
          dtConfiguration[
            property == propertyToSet,
            `:=`(
              value = value,
              description = description
            )
          ]
        }
        xlsxWriteData(
          wb = wb,
          sheetName = targetSheet,
          dt = dtConfiguration
        )
        openxlsx::saveWorkbook(wb, configPath, overwrite = TRUE)
      },
      .readAddOns = function() {
        private$.projectConfigurationDataAddOns <- list()
        private$.addOnDescriptions <- list()
        private$.reportingFrameworkVersion <- NULL

        configPath <- self$baseProjectconfiguration$projectConfigurationFilePath
        if (is.null(configPath) || !file.exists(configPath)) {
          return(invisible(NULL))
        }

        wb <- openxlsx::loadWorkbook(configPath)
        inputData <- if ("addons" %in% wb$sheet_names) {
          xlsxReadData(
            wb = wb,
            sheetName = "addons",
            convertHeaders = FALSE,
            emptyAsNA = FALSE
          )
        } else {
          esqlabsR::readExcel(path = configPath)
        }
        private$.readReportingFrameworkVersion(inputData)
        addOnProperties <- setdiff(
          inputData$Property,
          c(
            STANDARD_ESQLABSR_PROJECT_CONFIGURATION_PROPERTIES,
            REPORTING_FRAMEWORK_VERSION_PROPERTY
          )
        )

        for (property in addOnProperties) {
          private$.addOnFile(
            property = property,
            value = inputData$Value[inputData$Property == property]
          )
          private$.addOnDescriptions[[property]] <- inputData$Description[
            inputData$Property == property
          ]
        }

        private$.checkReportingFrameworkVersion()
        invisible(NULL)
      },
      .getAddOnRowsFromXlsx = function() {
        configPath <- self$baseProjectconfiguration$projectConfigurationFilePath
        if (is.null(configPath) || !file.exists(configPath)) {
          return(data.frame(
            Property = character(),
            Value = character(),
            Description = character(),
            stringsAsFactors = FALSE
          ))
        }

        wb <- openxlsx::loadWorkbook(configPath)
        inputData <- if ("addons" %in% wb$sheet_names) {
          xlsxReadData(
            wb = wb,
            sheetName = "addons",
            convertHeaders = FALSE,
            emptyAsNA = FALSE
          )
        } else {
          esqlabsR::readExcel(path = configPath)
        }
        addOnProperties <- setdiff(
          inputData$Property,
          c(
            STANDARD_ESQLABSR_PROJECT_CONFIGURATION_PROPERTIES,
            REPORTING_FRAMEWORK_VERSION_PROPERTY
          )
        )
        inputData[inputData$Property %in% addOnProperties, , drop = FALSE]
      },
      .writeAddOnRowsToXlsx = function(addOnRows) {
        if (nrow(addOnRows) == 0) {
          return(invisible(NULL))
        }
        configPath <- self$baseProjectconfiguration$projectConfigurationFilePath
        private$.writeAddOnRowsToPath(addOnRows, configPath)
      },
      .writeAddOnRowsToPath = function(addOnRows, configPath) {
        if (nrow(addOnRows) == 0) {
          return(invisible(NULL))
        }
        wb <- openxlsx::loadWorkbook(configPath)
        targetSheet <- "addons"
        if (!(targetSheet %in% wb$sheet_names)) {
          openxlsx::addWorksheet(wb = wb, sheetName = targetSheet)
          openxlsx::writeData(
            wb = wb,
            sheet = targetSheet,
            x = data.frame(
              Property = character(),
              Value = character(),
              Description = character(),
              stringsAsFactors = FALSE
            )
          )
        }
        dtConfiguration <- xlsxReadData(wb = wb, sheetName = targetSheet)
        for (i in seq_len(nrow(addOnRows))) {
          propertyToSet <- addOnRows$Property[[i]]
          value <- addOnRows$Value[[i]]
          description <- addOnRows$Description[[i]]
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
            dtConfiguration[
              property == propertyToSet,
              `:=`(
                value = value,
                description = description
              )
            ]
          }
        }
        xlsxWriteData(
          wb = wb,
          sheetName = targetSheet,
          dt = dtConfiguration
        )
        openxlsx::saveWorkbook(wb, configPath, overwrite = TRUE)
        invisible(NULL)
      },
      .writeRfVersionToPath = function(configPath) {
        currentVersion <- .currentReportingFrameworkVersion()
        private$.reportingFrameworkVersion <- currentVersion
        wb <- openxlsx::loadWorkbook(configPath)
        targetSheet <- if ("addons" %in% wb$sheet_names) {
          "addons"
        } else {
          wb$sheet_names[1]
        }
        if (!(targetSheet %in% wb$sheet_names)) {
          openxlsx::addWorksheet(wb = wb, sheetName = targetSheet)
          openxlsx::writeData(
            wb = wb,
            sheet = targetSheet,
            x = data.frame(
              Property = character(),
              Value = character(),
              Description = character(),
              stringsAsFactors = FALSE
            )
          )
        }
        dtConfiguration <- xlsxReadData(wb = wb, sheetName = targetSheet)
        if (
          !(REPORTING_FRAMEWORK_VERSION_PROPERTY %in% dtConfiguration$property)
        ) {
          dtConfiguration <- rbind(
            dtConfiguration,
            data.table(
              property = REPORTING_FRAMEWORK_VERSION_PROPERTY,
              value = currentVersion,
              description = .reportingFrameworkVersionDescription()
            )
          )
        } else {
          dtConfiguration[
            property == REPORTING_FRAMEWORK_VERSION_PROPERTY,
            `:=`(
              value = currentVersion,
              description = .reportingFrameworkVersionDescription()
            )
          ]
        }
        xlsxWriteData(wb = wb, sheetName = targetSheet, dt = dtConfiguration)
        openxlsx::saveWorkbook(wb, configPath, overwrite = TRUE)
        invisible(NULL)
      }
    ),
    public = list(
      #' @field baseProjectconfiguration esqlabsR project configuration object
      baseProjectconfiguration = NULL,
      #' Initialize
      #'
      #' @param projectConfigurationFilePath A string representing the path to the
      #' project configuration file.
      #' @param ignoreVersionCheck If `TRUE`, skip the version mismatch check when
      #'   loading the configuration file. Defaults to `FALSE`.
      initialize = function(
        projectConfigurationFilePath = character(),
        ignoreVersionCheck = FALSE
      ) {
        private$.ignoreVersionCheck <- isTRUE(ignoreVersionCheck)
        private$.loadBaseProjectConfiguration(
          projectConfigurationFilePath = projectConfigurationFilePath,
          ignoreVersionCheck = private$.ignoreVersionCheck
        )
        private$.readAddOns()
      },
      #' Print
      #' @description print prints a summary of the Project Configuration.
      #' @param className Whether to print the name of the class at the beginning. default to TRUE.
      print = function(className = TRUE) {
        if (className) {
          ospsuite.utils::ospPrintClass(self)
        }

        self$baseProjectconfiguration$print(className = FALSE)

        ospsuite.utils::ospPrintItems(list(
          "ospsuite.reportingframework version" = self$ospsuiteReportingFrameworkVersion
        ))

        ospsuite.utils::ospPrintItems(
          x = private$.projectConfigurationDataAddOns,
          title = "AddOns (non esqlabR)"
        )

        for (property in names(private$.projectConfigurationDataAddOns)) {
          print(paste(
            property,
            fs::path_rel(as.character(private$.projectConfigurationDataAddOns[[
              property
            ]]))
          ))
        }
        invisible(self)
      },
      #' @description Export ProjectConfiguration object to ProjectConfiguration.xlsx
      #' @param path a string representing the path or file name where to save the file.
      save = function(path = NULL) {
        addOnRows <- private$.getAddOnRowsFromXlsx()
        inMemoryAddOns <- private$.projectConfigurationDataAddOns
        inMemoryDescriptions <- private$.addOnDescriptions

        if (length(inMemoryAddOns) > 0) {
          missingFromFile <- setdiff(names(inMemoryAddOns), addOnRows$Property)
          if (length(missingFromFile) > 0) {
            addOnRows <- rbind(
              addOnRows,
              data.frame(
                Property = missingFromFile,
                Value = unlist(
                  inMemoryAddOns[missingFromFile],
                  use.names = FALSE
                ),
                Description = vapply(
                  missingFromFile,
                  function(property) {
                    inMemoryDescriptions[[property]] %||% ""
                  },
                  character(1)
                ),
                stringsAsFactors = FALSE
              )
            )
          }
        }

        self$baseProjectconfiguration$save(path)

        # Write addons to the saved file (baseProjectconfiguration$save writes to
        # 'path' but does not update projectConfigurationFilePath, so we write
        # addons directly to 'path' rather than through the normal helpers).
        savedPath <- if (is.null(path)) {
          self$baseProjectconfiguration$projectConfigurationFilePath
        } else {
          path
        }
        private$.writeAddOnRowsToPath(addOnRows, savedPath)
        private$.writeRfVersionToPath(savedPath)
        invisible(self)
      },
      #' @description Adds an add-on file to the project configuration.
      #'
      #' @param property A string representing the name of the property to add.
      #' @param value A string representing the basename of the file to add.
      #' @param description A string providing a description of the property.
      #' @param templatePath A string representing the path of the file to add.
      addAddOnFileToConfiguration = function(
        property,
        value,
        description,
        templatePath
      ) {
        checkmate::assertString(property)
        checkmate::assertString(value)
        checkmate::assertString(description)

        configurationsFolder <- self$baseProjectconfiguration$configurationsFolder
        if (!file.exists(file.path(configurationsFolder, value))) {
          checkmate::assertFileExists(templatePath)
          invisible(file.copy(
            from = templatePath,
            to = file.path(configurationsFolder, value)
          ))
        }

        private$.writeToConfigXlsx(property, value, description)

        private$.addOnFile(
          property = property,
          value = value
        )
        private$.addOnDescriptions[[property]] <- description

        invisible(self)
      },
      #' @description Adds an add-on folder to the project configuration.
      #'
      #' @param property A string representing the name of the property to add.
      #' @param value A string representing the path of the value to add.
      #' @param description A string providing a description of the property.
      addAddOnFolderToConfiguration = function(property, value, description) {
        checkmate::assertString(property)
        checkmate::assertString(value)
        checkmate::assertString(description)

        configurationsFolder <- self$baseProjectconfiguration$configurationsFolder
        dirPath <- fs::path_abs(value, start = configurationsFolder)
        if (!dir.exists(dirPath)) {
          dir.create(dirPath, recursive = TRUE)
        }

        value <- as.character(fs::path_rel(value, start = configurationsFolder))

        private$.writeToConfigXlsx(property, value, description)

        private$.addOnFile(
          property = property,
          value = value
        )
        private$.addOnDescriptions[[property]] <- description

        invisible(self)
      },
      #' @description Creates a copy of this object.
      #' @param deep Whether to make a deep clone.
      copy = function(deep = FALSE) {
        cloned <- ProjectConfigurationRF$new(
          projectConfigurationFilePath = character(0),
          ignoreVersionCheck = TRUE
        )
        if (deep) {
          cloned$baseProjectconfiguration <- self$baseProjectconfiguration$clone(
            deep = TRUE
          )
        } else {
          cloned$baseProjectconfiguration <- self$baseProjectconfiguration
        }
        clonedPrivate <- cloned$.__enclos_env__$private
        clonedPrivate$.projectConfigurationDataAddOns <- rlang::duplicate(
          private$.projectConfigurationDataAddOns,
          shallow = !deep
        )
        clonedPrivate$.addOnDescriptions <- rlang::duplicate(
          private$.addOnDescriptions,
          shallow = !deep
        )
        clonedPrivate$.reportingFrameworkVersion <- private$.reportingFrameworkVersion
        clonedPrivate$.ignoreVersionCheck <- private$.ignoreVersionCheck
        cloned
      }
    )
  )
