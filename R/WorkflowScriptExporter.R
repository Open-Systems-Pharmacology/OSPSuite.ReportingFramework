#' @title WorkflowScriptExporter
#' @docType class
#' @description Manages the export of an ePackage workflow.
#' @keywords internal
WorkflowScriptExporter <- R6::R6Class( # nolint
  "WorkflowScriptExporter",
  inherit = ospsuite.utils::Printable,
  cloneable = TRUE,
  # public ----
  public = list(
    #' @description
    #' Initialize a new instance of the class.
    #' @param projectConfiguration An object of class projectConfiguration containing information like paths.
    #' @param wfIdentifier A unique identifier for the workflow.
    #' @param scenarioNames A vector of scenario names to be included in the workflow (optional).
    #' @param workflowRmd Path to the input markdown file for the workflow (optional).
    #' @param fileNameReplacements A vector of file name replacements (optional).
    initialize = function(projectConfiguration,
                          wfIdentifier,
                          scenarioNames = NULL,
                          workflowRmd = NULL,
                          fileNameReplacements = c()) {
      self$wfIdentifier <- wfIdentifier
      self$scenarioNames <- scenarioNames
      self$workflowRmd <- workflowRmd
      self$fileNameReplacements <- fileNameReplacements

      if (is.null(scenarioNames) & is.null(workflowRmd)) {
        stop("Error: Please provide either scenarioNames or workflowRmd. Only one of the two is required.")
      }
      if (!is.null(scenarioNames) & !is.null(workflowRmd)) {
        stop("Error: Please provide either scenarioNames or workflowRmd. At least one of the two is required for initialization.")
      }

      electronicPackageFolder <- suppressWarnings(projectConfiguration$addOns$electronicPackageFolder)
      if (!dir.exists(electronicPackageFolder)) dir.create(electronicPackageFolder, recursive = TRUE)
      private$electronicPackageFolder <- electronicPackageFolder

      self$inputFiles <- data.table()
      self$changedInputFiles <- data.table(source = character(0), fileName = character(0), fileType = character(0))

      private$initializeconfigurationSheets(projectConfiguration)

      self$codeChunks <- list()


      return(self)
    },
    #' @description
    #' Extract code chunks from the specified R Markdown file.
    #' This method reads the R Markdown file provided in the `workflowRmd` field and extracts code chunks
    #' for further processing. It also ensures that necessary code chunks are present and replaces any
    #' placeholders with the corresponding values from the template.
    #'
    #' @return Invisible NULL. The function does not return any value but populates the `codeChunks`
    #'         field with the extracted code chunks.
    #' @details
    #' The method will raise an error if required code chunks are missing from the R Markdown file.
    #' It also handles the temporary creation of a script file to read the code chunks.
    #' A timeout mechanism is implemented to ensure that the script file is available for reading.
    extractCodeChunks = function() {
      .extract <- function(rmdfile) {
        knitr::knit_code$restore()

        tmpScript <- tempfile()
        knitr::purl(rmdfile, output = tmpScript, quiet = TRUE)
        # Set the timeout duration (in seconds)
        timeout <- 60
        startTime <- Sys.time()
        codeChunks <- knitr::read_chunk(tmpScript)
        # Wait until the file exists and is not empty, or timeout after 1 minute
        while (is.null(unlist(tail(codeChunks, 1)))) {
          if (as.numeric(difftime(Sys.time(), startTime, units = "secs")) >= timeout) {
            stop("Error: The file did not become available within 1 minute.")
          }
          Sys.sleep(0.1) # Check every 0.1 seconds
          codeChunks <- knitr::read_chunk(tmpScript)
        }
        return(codeChunks)
      }
      codeChunksTemplate <- .extract(system.file("templates", "template_ePackageWorkflow.Rmd",
        package = "ospsuite.reportingframework"
      ))

      chunkLabels <- list()
      for (suffix in c("copy", "eval")) {
        chunkLabels[[suffix]] <- grep(paste0("-", suffix, "$"), names(codeChunksTemplate), value = TRUE)
      }

      codeChunks <- .extract(self$workflowRmd)

      if (!all(unlist(chunkLabels) %in% names(codeChunks))) {
        tmp <- setdiff(unlist(chunkLabels), names(codeChunks))
        stop(paste(
          "Chunks are missing in workflowRmd check:",
          paste(tmp, collapse = ", ")
        ))
      }

      codeChunks <- codeChunks[unlist(chunkLabels)]

      codeChunks <- utils::modifyList(codeChunks, codeChunksTemplate[chunkLabels[["replace"]]])

      self$codeChunks <- codeChunks

      return(invisible())
    },
    #' @description
    #' This function evaluates the code chunks extracted from the R Markdown file associated with the workflow.
    #' It iterates through the code chunks, validating and executing them to set up the necessary variables
    #' for the workflow export process. The evaluation includes handling specific chunks related to custom functions,
    #' scenario names, and data import.
    #'
    #' @return Invisible NULL. The function does not return any value but performs evaluations that update the
    #'         internal state of the `WorkflowScriptExporter` object. If successful, the function will complete
    #'         without errors; otherwise, it will raise an informative error if any evaluation fails.
    evalCodeChunks = function() {
      for (chunkName in grep("-eval$", names(self$codeChunks), value = TRUE)) {
        variableValue <- private$evalChunkAndValidate(
          chunkName = chunkName,
          expectedVarName = gsub("-eval$", "", chunkName)
        )

        switch(chunkName,
          "pathsCustomfunctions-eval" =
            private$evalChunkCustomFunctions(pathsCustomfunctions = variableValue),
          "scenarioNames-eval" =
            private$evalChunkForScenarioNames(scenarioNames = variableValue),
          "dataObserved-eval" =
            private$evalChunkDataImport(
              dataObserved = variableValue,
              chunkName = chunkName
            ),
          "dataObservedPK-eval" =
            private$evalChunkDataImport(
              dataObserved = variableValue,
              chunkName = chunkName
            )
        )
      }
      return(invisible())
    },
    #' @description
    #' Add referenced plotting sheets to Configuration
    #' @param projectConfiguration An object of class projectConfiguration containing information like paths.
    #' @return Invisible NULL.
    addEPackageConfigurationForPlotting = function(projectConfiguration) {
      chunkText <- self$codeChunks[["runPlot-copy"]]

      # get list of all referenced plot sheets
      plotSheets <- regmatches(
        chunkText,
        gregexpr("configTableSheet\\s*=\\s*['\"](.*?)['\"]",
          chunkText,
          perl = TRUE
        )
      ) %>%
        unlist() %>%
        sub(pattern = "configTableSheet\\s*=\\s*['\"](.*?)['\"]", replacement = "\\1") %>%
        unique()

      # return if no sheets are found
      if (length(plotSheets) == 0) {
        return(invisible())
      }

      # check if plotSheets exist and collect entries of common sheets,
      wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)
      checkmate::assertNames(plotSheets, subset.of = wb$sheet_names)

      configurationSheets <- list()
      for (sheet in plotSheets) {
        configurationSheets[["Plots"]][[sheet]] <-
          excelToListStructure(xlsxReadData(
            wb = wb,
            sheetName = sheet,
            convertHeaders = FALSE,
            skipDescriptionRow = FALSE
          ))
      }

      commonConfigs <- list(
        Scenarios = list(
          idcol = "Scenario",
          colsToSearch = c("scenario", "scenarios", "referenceScenario")
        ),
        Outputs = list(
          idcol = "OutputPathId",
          colsToSearch = c("outputPathIds")
        ),
        DataGroups = list(
          idcol = "Group",
          colsToSearch = c("dataGroupIds", "dataGroupId")
        ),
        ModelParameter = list(
          idcol = "ParameterId",
          colsToSearch = c("parameterIds", "parameterId_Bin")
        ),
        TimeRange = list(idcol = "Tag")
      )

      for (commonConfigTable in names(commonConfigs)) {
        entries <- c()

        for (sheet in plotSheets) {
          dt <- xlsxReadData(wb = wb, sheetName = sheet, skipDescriptionRow = TRUE)

          if (commonConfigTable == "TimeRange") {
            timeRangeColumns <- grep(pattern = "^timeRange_", names(dt), value = TRUE)
            if (length(timeRangeColumns) > 0) {
              entries <- unique(c(
                entries,
                sub(pattern = "^timeRange_", replacement = "", timeRangeColumns)
              ))
            }
          } else {
            for (col in commonConfigs[[commonConfigTable]][["colsToSearch"]]) {
              if (col %in% names(dt)) {
                entries <-
                  unique(c(
                    entries,
                    gsub("[()]", "", splitInputs(dt[!is.na(get(col))][[col]]))
                  ))
              }
            }
          }
        }
        if (length(entries) > 1) {
          dtConfig <- xlsxReadData(
            wb = wb, sheetName = commonConfigTable,
            skipDescriptionRow = FALSE, convertHeaders = FALSE
          )
          dtConfig <- rbind(
            dtConfig[1],
            dtConfig[get(commonConfigs[[commonConfigTable]][["idcol"]]) %in% entries]
          )
          configurationSheets[["Plots"]][[commonConfigTable]] <-
            excelToListStructure(dtConfig)
        }
      }

      self$configurationSheets <- configurationSheets

      return(invisible())
    },
    #' @description
    #' Export workflow text to a file.
    #' @param projectConfiguration An object of class projectConfiguration containing information like paths.
    #' @return Invisible NULL.
    exportWorkflowText = function(projectConfiguration) {
      workflowTextLines <-
        readLines(system.file("templates", "template_ePackageWorkflow.R",
          package = "ospsuite.reportingframework"
        ))

      workflowTextLines <- private$replaceChunkPlaceholderChunks(workflowTextLines)

      workflowText <-
        paste(workflowTextLines, collapse = "\n")

      workflowText <- gsub(
        "XXwfIdentifierXX",
        self$wfIdentifier,
        workflowText
      )
      workflowText <- gsub(
        "XXscenarioNamesXX",
        paste(self$scenarioNames, collapse = "', '"),
        workflowText
      )
      projectDirectory <- getProjectDirectory(projectConfiguration)
      workflowText <- gsub(
        "XXprojectDirectoryXX",
        fs::path_rel(projectDirectory,
          start = private$electronicPackageFolder
        ),
        workflowText
      )

      workflowText <- styler::style_text(workflowText)

      writeLines(workflowText, file.path(
        private$electronicPackageFolder,
        paste0("w", self$wfIdentifier, "_workflow_r.txt")
      ))

      return(invisible())
    },
    #' @description
    #' Retrieve input files for the ePackage.
    #' @param projectConfiguration An object of class projectConfiguration containing information like paths and filenames.
    #' @return Invisible NULL.
    addEPackageInputfilesForScenarioNames = function(projectConfiguration) {
      inputFiles <- data.table()

      if (is.null(self$scenarioNames) | length(self$scenarioNames) == 0) {
        stop("no scenarios for export available")
      }

      wb <- openxlsx::loadWorkbook(projectConfiguration$scenariosFile)
      dtScenarios <- xlsxReadData(wb, "Scenarios")
      checkmate::assertNames(self$scenarioNames, subset.of = dtScenarios$scenario_name)
      dtScenarios <- dtScenarios[scenario_name %in% self$scenarioNames]

      # Check if population are always used as readPopulationFromCsv
      if (any(!is.na(dtScenarios$populationId))) {
        readPopulationFromCSV <-
          as.logical(dtScenarios[!is.na(populationId)]$readPopulationFromCSV)
        if (any(!(readPopulationFromCSV), na.rm = TRUE) |
          any(is.na(readPopulationFromCSV))) {
          stop(paste(
            "Error: Please use only population scenarios that have exported populations",
            "in workflows intended for an electronic package."
          ))
        }

        inputFiles <-
          rbind(
            inputFiles,
            data.table(
              source = file.path(
                projectConfiguration$populationsFolder,
                paste0(unique(dtScenarios[!is.na(populationId)]$populationId), ".csv")
              ),
              fileType = "population"
            )
          )
      }

      inputFiles <-
        rbind(
          inputFiles,
          data.table(
            source = file.path(
              projectConfiguration$modelFolder,
              unique(dtScenarios$modelFile)
            ),
            fileType = "model"
          )
        )

      self$inputFiles <- inputFiles

      return(invisible())
    },
    #' @description
    #' Get E-Package Configuration for Scenario Names.
    #' This function retrieves the configuration data for specified scenario names from various Excel sheets
    #' related to a project configuration.
    #' @param projectConfiguration A list containing project configuration details, including file paths.
    #' @return Invisible NULL.
    addEPackageConfigurationForScenarioNames = function(projectConfiguration) {
      configurationSheets <- list()

      # read and filter sheets of scenario table
      dtScenarios <- private$addScenarioConfiguration(projectConfiguration)

      # Filter Individuals
      if (any(!is.na(dtScenarios$IndividualId))) {
        dtIndividuals <- xlsxReadData(projectConfiguration$individualsFile, "IndividualBiometrics", convertHeaders = FALSE)
        dtIndividuals <- dtIndividuals[IndividualId %in% dtScenarios$IndividualId]
        private$addSelectedSheets(
          xlsfile = projectConfiguration$individualsFile,
          xlsLabel = "Individuals",
          selectedSheets = dtIndividuals$IndividualId
        )

        self$configurationSheets[["Individuals"]][["IndividualBiometrics"]] <-
          excelToListStructure(dtIndividuals)
      }

      private$addSelectedSheets(
        xlsfile = projectConfiguration$modelParamsFile,
        xlsLabel = "ModelParameters",
        selectedSheets = dtScenarios$modelParameterSheets
      )

      private$addSelectedSheets(
        xlsfile = projectConfiguration$applicationsFile,
        xlsLabel = "Applications",
        selectedSheets = dtScenarios$applicationProtocol
      )

      return(invisible())
    },
    #' @description
    #' Export input files to the electronic package folder.
    #' @return Invisible NULL.
    exportInputFiles = function() {
      inputFiles <- self$inputFiles

      if (any(duplicated(inputFiles$fileName))) {
        stop(paste(
          "Error: File names must be unique. Duplicate file names found:",
          paste(inputFiles$fileName[duplicated(inputFiles$fileName)], collapse = ", "),
          ". Please ensure all file names are unique."
        ))
      }

      success <- file.copy(
        from = inputFiles[!is.na(source)]$source,
        to = file.path(private$electronicPackageFolder, inputFiles[!is.na(source)]$fileName),
        overwrite = TRUE
      )

      if (!all(success)) {
        stop(paste(
          "Error: File copy to the ePackage folder failed for the following files:",
          paste(inputFiles[!success]$fileName, collapse = ", "),
          ". Please check the source paths and ensure the files exist."
        ))
      }

      fwrite(
        x = inputFiles[, c("fileName", "fileType")],
        file = file.path(private$electronicPackageFolder, paste0("w", self$wfIdentifier, "_input_files.csv"))
      )

      return(invisible())
    },
    #' @description
    #' Export configuration sheets to the electronic package folder.
    #' @return Invisible NULL.
    exportConfigSheets = function() {
      # Convert Configuration to JSON
      jsonData <- jsonlite::toJSON(
        self$configurationSheets,
        pretty = TRUE,
        auto_unbox = TRUE,
        digits = NA
      )

      # Write to file
      writeLines(jsonData, file.path(
        private$electronicPackageFolder,
        paste0("w", self$wfIdentifier, "_config_json.txt")
      ))

      return(invisible())
    }
  ),
  # active ----
  active = list(
    #' @field wfIdentifier Identifier of the workflow.
    wfIdentifier = function(value) {
      if (missing(value)) {
        value <- private$.wfIdentifier
      } else {
        checkmate::assertIntegerish(value,
          lower = 1,
          len = 1,
          any.missing = FALSE
        )
      }
      private$.wfIdentifier <- value
    },
    #' @field scenarioNames Relevant scenarios to export.
    scenarioNames = function(value) {
      if (missing(value)) {
        value <- private$.scenarioNames
      } else {
        checkmate::assertCharacter(value,
          any.missing = FALSE,
          unique = TRUE,
          null.ok = TRUE
        )
      }
      private$.scenarioNames <- value
    },
    #' @field workflowRmd Input markdown file for TLF workflow.
    workflowRmd = function(value) {
      if (missing(value)) {
        value <- private$.workflowRmd
      } else {
        checkmate::assertCharacter(value,
          any.missing = FALSE,
          len = 1,
          null.ok = TRUE
        )
      }
      private$.workflowRmd <- value
    },
    #' @field fileNameReplacements List of file name replacements.
    fileNameReplacements = function(value) {
      if (missing(value)) {
        value <- private$.fileNameReplacements
      } else {
        checkmate::assertCharacter(value,
          any.missing = FALSE,
          unique = TRUE,
          null.ok = TRUE
        )
        checkmate::assertIntegerish(length(value) / 2)
      }
      private$.fileNameReplacements <- value
    },
    #' @field inputFiles Table with all input files.
    inputFiles = function(value) {
      if (missing(value)) {
        value <- private$.inputFiles
      } else {
        if (is.data.table(private$.inputFiles)) {
          value <- rbind(
            private$.inputFiles,
            private$addValidFileNames(value)
          ) %>%
            unique()
        }
      }
      private$.inputFiles <- value
    },
    #' @field changedInputFiles Table with all changed input files.
    changedInputFiles = function(value) {
      if (missing(value)) {
        value <- private$.changedInputFiles
      } else {
        if (is.data.table(private$.changedInputFiles)) {
          value <- rbind(
            private$.changedInputFiles,
            value
          ) %>%
            unique()
        }
      }
      private$.changedInputFiles <- value
    },
    #' @field configurationSheets  A list containing configuration sheets .
    configurationSheets = function(value) {
      if (missing(value)) {
        value <- private$.configurationSheets
      } else {
        if (is.list(private$.configurationSheets)) {
          value <- utils::modifyList(
            private$.configurationSheets,
            value
          )
        }
      }
      private$.configurationSheets <- value
    },
    #' @field codeChunks # A list with code read in from workflowRmd .
    codeChunks = function(value) {
      if (missing(value)) {
        value <- private$.codeChunks
      } else {
        if (is.list(private$.codeChunks)) {
          value <- utils::modifyList(
            private$.codeChunks,
            value
          )
        }
      }
      private$.codeChunks <- value
    }
  ),
  # private ----
  private = list(
    # identifier of workflow
    .wfIdentifier = NA,
    # relevant scenarios to export
    .scenarioNames = c(),
    # input markdown file for TLF workflow
    .workflowRmd = "",
    # list of file name replacements
    .fileNameReplacements = c(),
    # directory of export
    electronicPackageFolder = "",
    # table with all input files
    .inputFiles = NA,
    # table with all changed input files
    .changedInputFiles = NA,
    # list with configuration sheets
    .configurationSheets = NA,
    # list with code read in from workflowRmd
    .codeChunks = NA,
    # Function to add validated filenames to the list of input files
    addValidFileNames = function(inputFiles) {
      inputFiles[, fileName := basename(source)]
      # replace filename
      if (length(self$fileNameReplacements) > 0) {
        inputFiles[fileName %in% self$fileNameReplacements[seq(1, length(self$fileNameReplacements), 2)],
          fileName := self$fileNameReplacements[which(fileName == self$fileNameReplacements) + 1],
          by = .I
        ]
      }
      # change extension
      inputFiles[fileType == "model", fileName := fs::path_ext_set(path = fileName, ext = "xml")]
      inputFiles[fileType == "script", fileName := fs::path_ext_set(path = fileName, ext = "txt")]

      # adjust filenames to fulfill naming requirements
      inputFiles[, fileName := validateAndAdjustFilenames(fileName, fileType),
        by = .I
      ]

      # list all files with changed names
      ixChanged <- which(fs::path_ext_remove(basename(inputFiles$source)) !=
        fs::path_ext_remove(inputFiles$fileName))

      # return the change names for replacement in config sheets
      changedInputFiles <- inputFiles[ixChanged, c("source", "fileName", "fileType")]
      changedInputFiles[, fileName := as.character(fs::path_ext_set(path = fileName, ext = fs::path_ext(source)))]
      changedInputFiles[, source := basename(source)]
      # population files are refernces with ID
      changedInputFiles[fileType == "population", source := fs::path_ext_remove(source)]
      setkey(changedInputFiles, source)

      self$changedInputFiles <- changedInputFiles

      # skip manually changed filenames for warning message
      if (length(self$fileNameReplacements) > 0) {
        ixChanged <- setdiff(
          ixChanged,
          which(changedInputFiles$fileName %in%
            self$fileNameReplacements[seq(2, length(self$fileNameReplacements), 2)])
        )
      }
      if (length(ixChanged > 0)) {
        warning(paste0(
          "Warning: Adjusted filenames due to naming requirements:\n",
          paste(basename(changedInputFiles$source), "->", changedInputFiles$fileName,
            collapse = "\n"
          ),
          "\nYou may use the input variable `fileNameReplacements` of the workflow export function",
          " to configure file names more appropriately."
        ))
      }

      # set source of data files to NA, as copy is not needed
      inputFiles[fileType == "data", source := NA_character_]

      return(inputFiles)
    },
    # Function to add scenario configuration
    addScenarioConfiguration = function(projectConfiguration) {
      configurationSheets <- list()

      wb <- openxlsx::loadWorkbook(projectConfiguration$scenariosFile)
      dtScenarios <- xlsxReadData(wb, "Scenarios", convertHeaders = FALSE)
      checkmate::assertNames(self$scenarioNames, subset.of = dtScenarios$Scenario_name)

      dtScenarios <- dtScenarios[Scenario_name %in% self$scenarioNames]
      dtScenarios[self$changedInputFiles, ModelFile := i.fileName, on = .(ModelFile = source)]
      dtScenarios[self$changedInputFiles, PopulationId := fs::path_ext_remove(i.fileName),
        on = .(PopulationId = source)
      ]
      configurationSheets[["Scenarios"]] <-
        list(Scenarios = excelToListStructure(dtScenarios))

      dtPKarameter <- xlsxReadData(wb, "PKParameter", convertHeaders = FALSE)
      dtPKarameter <- dtPKarameter[Scenario_name %in% self$scenarioNames]
      if (nrow(dtPKarameter) > 0) {
        configurationSheets[["Scenarios"]][["PKParameter"]] <-
          excelToListStructure(dtPKarameter)

        private$addSelectedSheets(
          xlsfile = projectConfiguration$addOns$pKParameterFile,
          xlsLabel = "PKParameter"
        )
      }

      dtOutputPaths <- xlsxReadData(wb, "OutputPaths", convertHeaders = FALSE)
      configurationSheets[["Scenarios"]][["OutputPaths"]] <-
        excelToListStructure(dtOutputPaths)

      self$configurationSheets <- configurationSheets

      return(dtScenarios)
    },
    # Function to add selected sheets
    addSelectedSheets = function(xlsfile,
                                 xlsLabel,
                                 selectedSheets = NULL) {
      wb <- openxlsx::loadWorkbook(xlsfile)
      if (is.null(selectedSheets)) {
        selectedSheets <- wb$sheet_names
      }
      selectedSheets <- selectedSheets[!is.na(selectedSheets)]

      configurationSheets <- list()

      for (sheet in intersect(wb$sheet_names, selectedSheets)) {
        configurationSheets[[xlsLabel]][[sheet]] <-
          excelToListStructure(xlsxReadData(wb, sheet, convertHeaders = FALSE))
      }

      self$configurationSheets <- configurationSheets
      return(invisible())
    },
    # Replace placeholder chunks in workflow text lines
    replaceChunkPlaceholderChunks = function(workflowTextLines) {
      if (length(self$codeChunks) > 0) {
        for (chunkName in c(grep("-copy$", names(self$codeChunks), value = TRUE))) {
          ixStart <- grep(
            paste0("^XXCHUNKstart-", gsub("-copy$", "", chunkName), "XXX"),
            workflowTextLines
          )
          ixEnd <- grep(
            paste0("^XXCHUNKend-", gsub("-copy$", "", chunkName), "XXX"),
            workflowTextLines
          )
          if (length(ixStart) * length(ixEnd) == 0) {
            stop(paste(
              "Error: Inconsisten placeholders in workflow script template and chunk Names.",
              "Placeholder for chunk", chunkName, "is missing",
              "\nThat should not happen. Please ask package administrator for help."
            ))
          }
          workflowTextLines <-
            c(
              workflowTextLines[seq_len(ixStart - 1)],
              self$codeChunks[[chunkName]],
              " ", # add empty line after each chunk insert,
              workflowTextLines[seq(ixEnd + 1, length(workflowTextLines))]
            )
        }
      }
      # delete remaining chunk placeholders
      workflowTextLines <- workflowTextLines[setdiff(
        seq_len(length(workflowTextLines)),
        grep("^XXCHUNK", workflowTextLines)
      )]

      return(workflowTextLines)
    },
    # Evaluate a chunk and validate the expected variable
    evalChunkAndValidate = function(chunkName, expectedVarName) {
      eval(parse(text = self$codeChunks[[chunkName]]))

      # Check if the expected variable exists
      if (!exists(expectedVarName)) {
        stop(paste0(
          "The chunk `", chunkName, "` of the workflowRmd does not evaluate to a variable `", expectedVarName, "`. ",
          "Please adjust chunk code."
        ))
      }

      # Check if the variable is NULL or empty
      variableValue <- get(expectedVarName, envir = environment())
      if (is.null(variableValue) || length(variableValue) == 0) {
        self$codeChunks <- setNames(list(NULL), chunkName)
      }

      return(variableValue)
    },
    # Evaluate the chunk for custom functions
    evalChunkCustomFunctions = function(pathsCustomfunctions) {
      # check if anything to do
      if (is.null(pathsCustomfunctions) || length(pathsCustomfunctions) == 0) {
        return(invisible())
      }

      checkmate::assertCharacter(pathsCustomfunctions)
      checkmate::assertFileExists(pathsCustomfunctions)

      self$inputFiles <- data.table(
        source = pathsCustomfunctions,
        fileType = "script"
      )

      fileNames <- self$inputFiles[fileType == "script"]$fileName
      contentLines <- paste0(
        "", # source custom functions",
        "source(file.path(projectConfiguration$configurationsFolder,'",
        fs::path_ext_set(path = fileNames, ext = "R"),
        "'))"
      )
      self$codeChunks <- list("pathsCustomfunctions-copy" = contentLines)
      return(invisible())
    },
    # Evaluate the chunk for scenario names
    evalChunkForScenarioNames = function(scenarioNames) {
      # check if anything to do
      if (is.null(scenarioNames) || length(scenarioNames) == 0) {
        return(invisible())
      }

      checkmate::assertCharacter(scenarioNames)
      self$scenarioNames <- scenarioNames

      self$codeChunks <-
        list(
          "scenarioNames-copy" =
            paste0(
              "", # initialize variable with list of scenarios",
              "scenarioNames = c('",
              paste(scenarioNames, collapse = "', '"),
              "')"
            )
        )

      return(invisible())
    },
    # Evaluate the chunk for data importEvaluate the chunk for data import
    evalChunkDataImport = function(dataObserved, chunkName) {
      checkmate::assertDataTable(dataObserved, null.ok = TRUE)

      # check if anything to do
      if (is.null(dataObserved) || nrow(dataObserved) == 0) {
        return(invisible())
      }

      for (dt in split(dataObserved, by = "dataClass")) {
        dataClass <- dt$dataClass[1]
        xUnit <- dt$xUnit[1]
        dt <- dt[, !c("dataClass", "dataType", "xUnit"), with = FALSE]

        filename <- paste0("data_", tolower(gsub(" ", "", dataClass)), ".csv")
        dictionaryName <- paste0(gsub(" ", "", dataClass), "Dictionary")

        private$addDataFilesConfig(
          dataClass = dataClass,
          filename = filename,
          dictionaryName = dictionaryName
        )

        private$addDictionaryToConfig(
          dt = dt,
          dataClass = dataClass,
          xUnit = xUnit,
          dictionaryName = dictionaryName
        )

        write.csv(dt,
          file = file.path(private$electronicPackageFolder, filename),
          na = "",
          row.names = FALSE,
          fileEncoding = "UTF-8"
        )

        self$inputFiles <- data.table(
          source = filename,
          fileType = "data"
        )
      }
      self$codeChunks <-
        switch(chunkName,
          "dataObserved-eval" =
            list("dataObserved-copy" = c(
              "", # import observed time profile data",
              "dataObserved <- readObservedDataByDictionary(projectConfiguration = projectConfiguration,",
              "                                             dataClassType = 'timeprofile',",
              "                                             fileIds = NULL,",
              "                                             spreadData = FALSE)"
            )),
          "dataObservedPK-eval" =
            list("dataObservedPK-copy" = c(
              "", # import observed pk-parameter",
              "dataObservedPK <- readObservedDataByDictionary(projectConfiguration = projectConfiguration,",
              "                                             dataClassType = 'pkParameter',",
              "                                             fileIds = NULL,",
              "                                             spreadData = FALSE)"
            ))
        )

      invisible(NULL)
    },
    # Add configuration for data files
    addDataFilesConfig = function(dataClass, filename, dictionaryName) {
      xlsLabel <- "DataImportConfiguration"
      sheetName <- "DataFiles"
      # Initialize configsheet if necessary
      if (is.null(self$configurationSheets) ||
        !(xlsLabel %in% names(self$configurationSheets))) {
        self$configurationSheets <- setNames(list(NULL), xlsLabel)
      }
      if (is.null(self$configurationSheets[[xlsLabel]]) ||
        !(sheetName %in% names(self$configurationSheets[[xlsLabel]]))) {
        configSheet <- xlsxReadData(
          wb = system.file("templates", "DataImportConfiguration.xlsx",
            package = "ospsuite.reportingframework"
          ),
          sheetName = sheetName,
          skipDescriptionRow = FALSE,
          convertHeaders = FALSE
        )
        configSheet <- configSheet[1, ]
      } else {
        tmp <- convertSheet(self$configurationSheets[[xlsLabel]])
        configSheet <- setDT(tmp[[sheetName]])
      }

      # add new row to config sheet
      configSheet <- rbind(
        configSheet,
        data.table(
          FileIdentifier = dataClass,
          DataFile = file.path("..", paste0("Data_w", self$wfIdentifier), filename),
          Dictionary = dictionaryName,
          DataFilter = NA_character_,
          DataClass = dataClass
        )
      )

      private$.configurationSheets[[xlsLabel]][[sheetName]] <- excelToListStructure(configSheet)

      return(invisible())
    },
    # Add dictionary configuration
    addDictionaryToConfig = function(dt, xUnit, dataClass, dictionaryName) {
      columnsWithAttributes <- unlist(lapply(dt, attr, "columnType"))

      dict <- data.table(
        TargetColumn = names(columnsWithAttributes),
        Type = unname(columnsWithAttributes),
        SourceColumn = names(columnsWithAttributes),
        SourceUnit = ""
      )

      # adjust Unit
      dict[
        TargetColumn %in% c("xValues", names(BIOMETRICUNITS)),
        SourceUnit := c(xUnit, unlist(BIOMETRICUNITS))[match(TargetColumn, c("xValues", names(BIOMETRICUNITS)))]
      ]

      # adjust format
      dictTemplate <- xlsxReadData(
        wb = system.file("templates", "DataImportConfiguration.xlsx",
          package = "ospsuite.reportingframework"
        ),
        sheetName = ifelse(dataClass %in% DATACLASS[c("tpIndividual", "tpAggregated")],
          "tpDictionary", "pkDictionary"
        ),
        skipDescriptionRow = FALSE,
        convertHeaders = FALSE
      )

      dict <- merge(dict,
        dictTemplate[, c("TargetColumn", "Description")],
        by = "TargetColumn",
        all.x = TRUE,
        sort = FALSE
      )

      dict <- rbind(dictTemplate[1],
        dict,
        fill = TRUE
      )

      self$configurationSheets <- setNames(
        list(
          setNames(list(excelToListStructure(dict)), dictionaryName)
        ),
        "DataImportConfiguration"
      )

      return(invisible())
    },
    # initializes configuration sheets a
    initializeconfigurationSheets = function(projectConfiguration) {
      wb <- openxlsx::loadWorkbook(
        file = system.file("templates", "ProjectConfiguration.xlsx",
          package = "ospsuite.reportingframework"
        )
      )
      dtConfig <-
        xlsxReadData(
          wb = wb,
          sheetName = wb$sheet_names[1], convertHeaders = FALSE
        )

      projectDirectory <- getProjectDirectory(projectConfiguration)
      configDirW <- file.path(projectDirectory, getConfigDirectoryForWorkflow(self$wfIdentifier))
      dtConfig[
        Property == "modelFolder",
        Value := paste0("../Models", "_w", self$wfIdentifier)
      ]
      dtConfig[
        Property == "populationsFolder",
        Value := paste0("../Models", "_w", self$wfIdentifier, "/Populations")
      ]
      dtConfig[
        Property == "outputFolder",
        Value := fs::path_rel(projectConfiguration$outputFolder,
          start = configDirW
        )
      ]
      dtConfig[
        Property == "electronicPackageFolder",
        Value := fs::path_rel(projectConfiguration$addOns$electronicPackageFolder,
          start = configDirW
        )
      ]

      configurationSheets <- list(
        "ProjectConfiguration" =
          list(Scenarios = excelToListStructure(dtConfig))
      )
      self$configurationSheets <- configurationSheets

      return(invisible())
    }
  )
)
# calling function ------------
#' @title Export Simulation Workflow
#' @description
#' This function facilitates the export of a simulation workflow to an electronic package (ePackage).
#' It initializes a `WorkflowScriptExporter` object and executes a series of operations to prepare
#' the workflow for export, including generating workflow text, retrieving input files,
#' configuring scenario details, and exporting necessary files.
#'
#' @param projectConfiguration An object of class `projectConfiguration` that contains various configuration details,
#'                             including paths for input files, output directories, and any other relevant settings
#'                             required for the workflow export.
#' @param wfIdentifier A unique identifier (integer) for the workflow. This identifier is used to distinguish
#'                     this workflow from others and is included in the names of the exported files.
#' @param scenarioNames A character vector of scenario names that should be included in the workflow export.
#'                      These scenarios determine which specific configurations and input files will be processed
#'                      and exported as part of the ePackage.
#' @param fileNameReplacements An optional character vector that specifies replacements for file names.
#'                              The vector should contain pairs of values, where the first value in each
#'                              pair represents the original file name, and the second value represents
#'                              the desired replacement name. This allows for customization of exported file names
#'                              to meet specific naming conventions or requirements. For Files not listed in this
#'                              variable the source filename is converted to a valid filename.
#' @return Invisible NULL. The function does not return any value but performs a series of operations
#'         that result in the creation of exported files and configurations necessary for the ePackage.
#'         If successful, the function will complete without errors; otherwise, it will raise an informative error.
#' @examples
#' \dontrun{
#' # Example usage of exportSimulationWorkflow function
#' exportSimulationWorkflow(
#'   projectConfiguration = projectConfiguration,
#'   wfIdentifier = 1,
#'   scenarioNames = c("Scenario1", "Scenario2"),
#'   fileNameReplacements = c("oldName.pkml", "new_name.pkml")
#' )
#' }
#' @export
#' @family electronic package
exportSimulationWorkflowToEPackage <- function(projectConfiguration,
                                               wfIdentifier,
                                               scenarioNames,
                                               fileNameReplacements = c()) {
  exporter <- WorkflowScriptExporter$new(projectConfiguration,
    wfIdentifier = wfIdentifier,
    scenarioNames = scenarioNames,
    fileNameReplacements = fileNameReplacements
  )

  exporter$exportWorkflowText(projectConfiguration = projectConfiguration)

  exporter$addEPackageInputfilesForScenarioNames(projectConfiguration = projectConfiguration)

  exporter$addEPackageConfigurationForScenarioNames(projectConfiguration = projectConfiguration)

  exporter$exportInputFiles()

  exporter$exportConfigSheets()

  return(invisible())
}
#' @title Export TLF Workflow to E-Package
#' @description
#' This function facilitates the export of a TLF (Table-Linked Format) workflow to an electronic package (ePackage).
#' It initializes a `WorkflowScriptExporter` object, extracts code chunks from the provided R Markdown file,
#' evaluates the code chunks, and executes a series of operations to prepare the workflow for export.
#' This includes generating workflow text, retrieving input files, configuring scenario details, and exporting necessary files.
#'
#' @param projectConfiguration An object of class `projectConfiguration` that contains various configuration details,
#'                             including paths for input files, output directories, and any other relevant settings
#'                             required for the workflow export.
#' @param wfIdentifier A unique identifier (integer) for the workflow. This identifier is used to distinguish
#'                     this workflow from others and is included in the names of the exported files.
#' @param workflowRmd Path to the input markdown file for the TLF workflow. This file should contain the necessary
#'                     code chunks that are to be extracted and evaluated for the export process.
#' @param fileNameReplacements An optional character vector that specifies replacements for file names.
#'                              The vector should contain pairs of values, where the first value in each
#'                              pair represents the original file name, and the second value represents
#'                              the desired replacement name. This allows for customization of exported file names
#'                              to meet specific naming conventions or requirements. For files not listed in this
#'                              variable, the source filename is converted to a valid filename.
#' @return Invisible NULL. The function does not return any value but performs a series of operations
#'         that result in the creation of exported files and configurations necessary for the ePackage.
#'         If successful, the function will complete without errors; otherwise, it will raise an informative error.
#' @examples
#' \dontrun{
#' # Example usage of exportTLFWorkflowToEPackage function
#' exportTLFWorkflowToEPackage(
#'   projectConfiguration = projectConfiguration,
#'   wfIdentifier = 1,
#'   workflowRmd = "path/to/workflow.Rmd",
#'   fileNameReplacements = c("oldName.pkml", "new_name.pkml")
#' )
#' }
#' @export
#' @family electronic package
exportTLFWorkflowToEPackage <- function(projectConfiguration,
                                        wfIdentifier,
                                        workflowRmd,
                                        fileNameReplacements = c()) {
  exporter <- WorkflowScriptExporter$new(projectConfiguration,
    wfIdentifier = wfIdentifier,
    workflowRmd = workflowRmd,
    fileNameReplacements = fileNameReplacements
  )

  exporter$extractCodeChunks()

  exporter$evalCodeChunks()

  exporter$exportWorkflowText(projectConfiguration = projectConfiguration)

  exporter$addEPackageInputfilesForScenarioNames(projectConfiguration = projectConfiguration)

  exporter$addEPackageConfigurationForScenarioNames(projectConfiguration = projectConfiguration)

  exporter$addEPackageConfigurationForPlotting(projectConfiguration = projectConfiguration)

  exporter$exportInputFiles()

  exporter$exportConfigSheets()

  return(invisible())
}
#' @title Import Workflow
#' @description
#' This function imports a workflow from an electronic package (ePackage) into a project directory.
#' It initializes the project configuration, retrieves relevant project files, and synchronizes
#' scenarios with plots. The function ensures that the necessary directories exist and that the
#' configuration is set up correctly for further processing.
#'
#' @param projectDirectory A character string representing the path to the project directory where
#'                         the workflow will be imported.
#' @param wfIdentifier An integer identifier for the workflow being imported. This identifier is
#'                     used to distinguish between different workflows.
#' @param ePackageFolder A character string representing the path to the electronic package folder
#'                       that contains the workflow files and configurations to be imported.
#' @param configurationDirectory A character string representing the directory where the configuration
#'                               files will be stored.
#' @return An object containing the updated project configuration after the workflow has been imported.
#' @export
#' @family electronic package
importWorkflow <- function(projectDirectory,
                           wfIdentifier,
                           ePackageFolder,
                           configurationDirectory) {
  checkmate::assertDirectoryExists(projectDirectory)
  checkmate::assertDirectoryExists(ePackageFolder)
  checkmate::assertIntegerish(wfIdentifier)

  configurationDirectory <- fs::path_abs(getConfigDirectoryForWorkflow(wfIdentifier = wfIdentifier),
    start = projectDirectory
  )

  if (!dir.exists(configurationDirectory)) {
    dir.create(configurationDirectory, recursive = TRUE)
  }

  directionOfSynchronisation <- importProjectConfiguration(
    ePackageFolder = ePackageFolder,
    configurationDirectory = configurationDirectory,
    wfIdentifier = wfIdentifier
  )

  # get paths of all relevant project files
  projectConfigurationNew <-
    ospsuite.reportingframework::createProjectConfiguration(
      path = fs::path_rel(file.path(configurationDirectory, "ProjectConfiguration.xlsx"))
    )

  # add data folder
  projectConfigurationNew$addAddOnFolderToConfiguration(
    property = "dataFileFolder",
    value = file.path(projectDirectory, paste0("Data_w", wfIdentifier)),
    description = "Path to the folder where data files are saved"
  )

  initLogfunction(projectConfigurationNew)

  importWorkflowFiles(projectConfigurationNew, ePackageFolder, wfIdentifier)

  synchronizeScenariosOutputsWithPlots(
    projectConfiguration = projectConfigurationNew,
    direction = directionOfSynchronisation
  )
  synchronizeScenariosWithPlots(projectConfigurationNew)

  return(projectConfigurationNew)
}


# auxiliaries ------
#' @title Validate and Adjust Filenames
#' @description
#' This function validates and adjusts filenames to ensure they meet specific criteria for naming conventions.
#' It checks for valid file extensions, converts filenames to lowercase, replaces spaces with underscores,
#' removes non-alphanumeric characters (except underscores), and ensures that filenames do not start with a number.
#' Additionally, it checks the length of the filename against specified limits based on the file type.
#'
#' @param fileName A character string representing the original filename that needs to be validated and adjusted.
#' @param fileType A character string indicating the type of the file (e.g., "data", "model", "script").
#'                 This is used to determine the length limit for the filename.
#' @return A character string representing the adjusted filename that meets the specified naming conventions.
#'         If the filename does not meet the criteria, an error will be raised with an informative message.
#'
#' @keywords internal
validateAndAdjustFilenames <- function(fileName, fileType) {
  # Check if the file has a valid extension
  extension <- fs::path_ext(fileName)
  if (!(extension %in% c("txt", "csv", "xml"))) {
    stop(paste("Error: Invalid file extension for", fileName))
  }

  # use only lowercase
  fileName <- tolower(fileName)

  # Replace all spaces with underscores
  fileName <- gsub(" ", "_", fileName)

  # Remove all non-alphanumeric characters except underscores
  fileName <- fs::path_ext_set(
    gsub("[^a-zA-Z0-9_]", "", fs::path_ext_remove(fileName)),
    extension
  )

  # Check if the first character is numeric
  if (grepl("^[0-9]", fileName)) {
    stop(paste(
      "Error: Filename cannot start with a number:", fileName,
      "\nPlease use valid file names that do not start with a numeric character.",
      "\nThe workflow export function provides the input variable `fileNameReplacements` to configure file names."
    ))
  }

  # Check if the length of the filename is below the limit
  limitLength <- ifelse(fileType == "data", 32, 64)
  if (nchar(fileName) > limitLength) {
    stop(paste(
      "Error: Filename is too long (greater than", limitLength, "characters):", fileName,
      "\nPlease shorten the filename to meet the length requirement.",
      "\nThe workflow export function provides the input variable `fileNameReplacements` to configure file names."
    ))
  }

  return(fileName)
}
#' @title Convert Excel file to List Structure
#' @description
#' This function converts the content of an Excel sheet into a list structure suitable for JSON serialization.
#'
#' @param df A data frame representing the content of an Excel sheet.
#' @return A list structure ready for JSON serialization, containing column names and rows.
#' @keywords internal
excelToListStructure <- function(df) {
  # Convert to simple list format
  sheetData <- list(
    column_names = names(df),
    rows = list()
  )

  # Store each row
  if (nrow(df) > 0) {
    for (i in seq_len(nrow(df))) {
      # Extract row as a character vector to avoid type issues during serialization
      rowValues <- sapply(df[i, ], as.character)
      sheetData$rows[[i]] <- as.list(rowValues)
    }
  }

  return(sheetData)
}
#' @title Import Project Configuration
#' @description
#' This function imports the project configuration from an electronic package (ePackage) into a specified directory.
#' It reads the configuration data from a JSON file and creates the necessary Excel sheets.
#'
#' @param configurationDirectory A character string representing the directory where the configuration files will be stored.
#' @param ePackageFolder A character string representing the path to the electronic package folder containing the configuration.
#' @param wfIdentifier An integer identifier for the workflow being imported.
#' @return A character string indicating the direction of synchronization (e.g., "plotToScenario", "bothways").
#' @keywords internal
importProjectConfiguration <- function(
    configurationDirectory,
    ePackageFolder,
    wfIdentifier) {
  # Check if JSON file exists
  jsonPath <- file.path(ePackageFolder, paste0("w", wfIdentifier, "_config_json.txt"))
  if (!file.exists(jsonPath)) {
    stop("JSON file does not exist: ", jsonPath)
  }

  # Load JSON data
  configData <- jsonlite::fromJSON(jsonPath, simplifyVector = FALSE)

  # export first ProjectConfiguration.xlsx as base for project structure
  sheetsData <- configData[["ProjectConfiguration"]]
  excelSheets <- convertSheet(sheetsData)
  excelPath <- file.path(configurationDirectory, "ProjectConfiguration.xlsx")
  openxlsx::write.xlsx(excelSheets[[1]], file = excelPath)

  initProject(
    configurationDirectory = configurationDirectory,
    sourceConfigurationXlsx = excelPath
  )


  for (name in setdiff(names(configData), "ProjectConfiguration")) {
    # Get the sheets for this file
    sheetsData <- configData[[name]]

    excelSheets <- convertSheet(sheetsData)
    # Write the Excel file if we have data
    if (length(excelSheets) > 0) {
      excelPath <- file.path(configurationDirectory, paste0(name, ".xlsx"))
      wb <- openxlsx::loadWorkbook(excelPath)

      for (sheet in names(excelSheets)) {
        if (sheet %in% wb$sheet_names) {
          xlsxWriteData(wb = wb, sheetName = sheet, dt = excelSheets[[sheet]])
        } else {
          xlsxAddSheet(wb = wb, sheetName = sheet, dt = excelSheets[[sheet]])
        }
      }
      openxlsx::saveWorkbook(wb = wb, file = excelPath, overwrite = TRUE)
    }
  }

  directionOfSynchronisation <- ifelse("Plots" %in% names(configData), "plotToScenario", "bothways")

  return(directionOfSynchronisation)
}
#' @title Convert Sheet Data to Data Frame
#' @description
#' This function converts the sheet data from a JSON structure into a data frame format suitable for Excel.
#'
#' @param sheetsData A list containing the sheet data in a structured format.
#' @return A list of data frames representing the sheets ready for Excel writing.
#' @keywords internal
convertSheet <- function(sheetsData) {
  excelSheets <- list()

  # Process each sheet
  for (sheetName in names(sheetsData)) {
    sheetInfo <- sheetsData[[sheetName]]

    # Get column names
    columnNames <- sheetInfo$column_names

    # Create an empty data frame with the correct structure
    df <- data.frame(matrix(
      ncol = length(columnNames),
      nrow = length(sheetInfo$rows)
    ))
    colnames(df) <- columnNames

    # Fill in data if we have rows
    if (length(sheetInfo$rows) > 0) {
      for (i in seq_along(sheetInfo$rows)) {
        rowData <- sheetInfo$rows[[i]]

        # Fill in each column
        for (j in seq_along(columnNames)) {
          if (j <= length(rowData)) {
            df[i, j] <- rowData[[j]] %||% ""
          }
        }
      }
      # Convert data types to match the originals
      df <- .convertDataTypes(df)
    }

    # Add to the list of sheets
    excelSheets[[sheetName]] <- df
  }

  return(excelSheets)
}
#' @title Import Workflow Files
#' @description
#' This function imports workflow files from an electronic package (ePackage) into a new project configuration.
#' It handles different file types and copies them to the appropriate directories.
#'
#' @param projectConfigurationNew An object representing the new project configuration where files will be imported.
#' @param ePackageFolder A character string representing the path to the electronic package folder containing the workflow files.
#' @param wfIdentifier An integer identifier for the workflow being imported.
#' @return Invisible NULL.
#' @keywords internal
importWorkflowFiles <- function(projectConfigurationNew,
                                ePackageFolder,
                                wfIdentifier) {
  inputFiles <- fread(file = file.path(
    ePackageFolder,
    paste0("w", wfIdentifier, "_input_files.csv")
  ))

  fileGroup <- split(inputFiles, by = "fileType")
  for (fileType in names(fileGroup)) {
    fileNames <- fileGroup[[fileType]]$fileName
    for (f in fileNames) {
      newFile <-
        switch(fileType,
          "population" = file.path(projectConfigurationNew$populationsFolder, f),
          "model" = file.path(projectConfigurationNew$modelFolder, fs::path_ext_set(f, ".pkml")),
          "data" = file.path(projectConfigurationNew$addOns$dataFileFolder, f),
          "script" = file.path(projectConfigurationNew$dataFolder, fs::path_ext_set(f, ".R")),
        )

      file.copy(
        from = file.path(ePackageFolder, f),
        to = newFile
      )
    }
  }

  return(invisible())
}

#' Convert data frame columns to appropriate types
#'
#' @param df Data frame with columns that need type conversion
#' @return Data frame with columns converted to appropriate types
#' @keywords internal
.convertDataTypes <- function(df) {
  if (ncol(df) == 0 || nrow(df) == 0) {
    return(df)
  }

  for (colName in names(df)) {
    # Get non-NA values for type checking
    values <- df[[colName]]
    nonNaValues <- values[!is.na(values) & values != ""]

    # Skip if all values are NA or empty
    if (length(nonNaValues) == 0) {
      next
    }

    # Check if column can be converted to logical (TRUE/FALSE values)
    if (all(nonNaValues %in% c("TRUE", "FALSE"))) {
      df[[colName]] <- as.logical(values)
      next
    }

    # Check if column can be converted to numeric
    # Use suppressWarnings to handle NAs gracefully
    numericConversion <- suppressWarnings(as.numeric(values))
    # If no NAs were introduced by conversion (except those that were already NA)
    if (!any(is.na(numericConversion) & !is.na(values) & values != "")) {
      df[[colName]] <- numericConversion
    }
  }

  return(df)
}
#' @title Get Project Directory
#' @description
#' This function retrieves the common project directory based on the configuration settings.
#'
#' @param projectConfiguration An object containing the project configuration details.
#' @return A character string representing the common project directory.
#' @keywords internal
getProjectDirectory <- function(projectConfiguration) {
  fs::path_common(path = c(
    projectConfiguration$configurationsFolder,
    projectConfiguration$outputFolder
  ))
}
#' @title Get Configuration Directory for Workflow
#' @description
#' This function generates the configuration directory path for a specific workflow based on its identifier.
#'
#' @param wfIdentifier An integer identifier for the workflow.
#' @return A character string representing the configuration directory path for the specified workflow.
#' @keywords internal
getConfigDirectoryForWorkflow <- function(wfIdentifier) {
  paste0("Scripts_w", wfIdentifier)
}
