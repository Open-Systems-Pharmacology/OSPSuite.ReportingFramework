#' @title WorkflowScriptExporter
#' @docType class
#' @description Manages the export of an ePackage workflow.
#' @export
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
      self$changedInputFiles <- data.table()

      wb <- openxlsx::loadWorkbook(system.file("templates", "projectConfiguration.xlsx", package = "ospsuite.reportingframework"))
      dtConfig <- xlsxReadData(wb, sheetName = wb$sheet_names[1])
      dtConfig[setdiff(grep("Folder$", property), c(grep("Outputs", value), which(value == "."))), value := paste0(value, "_w", wfIdentifier)]
      configurationSheets <- list(
        "projectConfiguration" =
          list(Scenarios = excelToListStructure(dtConfig))
      )
      self$configurationSheets <- configurationSheets

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
          if (as.numeric(difftime(Sys.time(),  startTime, units = "secs")) >= timeout) {
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
      workflowText <- gsub(
        "XXprojectDirectoryXX",
        fs::path_rel(
          fs::path_common(path = c(
            projectConfiguration$configurationsFolder,
            projectConfiguration$outputFolder
          )),
          start = private$electronicPackageFolder
        ),
        workflowText
      )

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
      if (any(!is.na(dtScenarios$individualId))) {
        dtIndividuals <- xlsxReadData(projectConfiguration$individualsFile, "IndividualBiometrics")
        dtIndividuals[individualId %in% dtScenarios$individualId]
        configurationSheets[["Individuals"]] <-
          list(IndividualBiometrics = excelToListStructure(dtIndividuals))

        private$addSelectedSheets(
          xlsfile = projectConfiguration$individualsFile,
          xlsLabel = "Individuals",
          selectedSheets = dtIndividuals$individualId
        )
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
      inputFiles[, fileNameUnchanged := fileName]
      inputFiles[, fileName := validateAndAdjustFilenames(fileName, fileType),
        by = .I
      ]

      # list all files with changed names
      ixChanged <- which(inputFiles$fileNameUnchanged != inputFiles$fileName)

      # return the change names for replacement in config sheets
      changedInputFiles <- inputFiles[ixChanged, c("source", "fileName", "fileType")]
      changedInputFiles[, fileName := fs::path_ext_set(path = fileName, ext = fs::path_ext(source))]
      changedInputFiles[, source := basename(source)]
      setkey(changedInputFiles, source)

      self$changedInputFiles <- changedInputFiles

      inputFiles$fileNameUnchanged <- NULL

      if (length(ixChanged > 0)) {
        warning(paste0(
          "Warning: Adjusted filenames due to naming requirements:\n",
          paste(basename(changedInputFiles$source), "->", changedInputFiles$fileName,
            collapse = "\n"
          ),
          "\nYou may use the input variable `fileNameReplacements` of the workflow export function",
          "to configure file names more appropriately."
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
      dtScenarios <- xlsxReadData(wb, "Scenarios")
      checkmate::assertNames(self$scenarioNames, subset.of = dtScenarios$scenario_name)

      dtScenarios <- dtScenarios[scenario_name %in% self$scenarioNames]
      dtScenarios[self$inputFilesChanged, modelFile := i.fileName, on = .(modelFile = source)]
      dtScenarios[self$inputFilesChanged, populationId := i.fileName, on = .(populationId = source)]

      configurationSheets[["Scenarios"]] <-
        list(Scenarios = excelToListStructure(dtScenarios))

      dtOutputPaths <- xlsxReadData(wb, "OutputPaths")
      dtOutputPaths <- dtOutputPaths[outputPathId %in% dtScenarios$outputPathsIds]
      configurationSheets[["Scenarios"]][["OutputPaths"]] <-
        excelToListStructure(dtOutputPaths)

      dtPKarameter <- xlsxReadData(wb, "PKParameter")
      dtPKarameter <- dtPKarameter[scenario_name %in% self$scenarioNames]
      if (nrow(dtPKarameter) > 0) {
        configurationSheets[["Scenarios"]][["PKParameter"]] <-
          excelToListStructure(dtPKarameter)

        private$addSelectedSheets(
          xlsfile = projectConfiguration$addOns$pKParameterFile,
          xlsLabel = "PKParameter"
        )
      }

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
          excelToListStructure(xlsxReadData(wb, sheet))
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

        filename <- paste("data", tolower(gsub(" ", "", dataClass)),
          paste0("w", self$wfIdentifier, ".csv"),
          sep = "_"
        )
        dictionaryName <- paste0(gsub(" ", "", dataClass), "Dictionary")

        private$addToDataFilesConfig(
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
              "                                             fileIds = NULL"
            )),
          "dataObservedPK-eval" =
            list("dataObservedPK-copy" = c(
              "", # import observed pk-parameter",
              "dataObservedPK <- readObservedDataByDictionary(projectConfiguration = projectConfiguration,",
              "                                             dataClassType = 'pkParameter',",
              "                                             fileIds = NULL"
            ))
        )

      invisible(NULL)
    },
    # Add configuration for data files
    addToDataFilesConfig = function(dataClass, filename, dictionaryName) {
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
        configSheet <- self$configurationSheets[[xlsLabel]][[sheetName]]
      }

      # add new row to config sheet
      configSheet <- rbind(configSheet,
        data.table(
          FileIdentifier = dataClass,
          DataFile = file.path("..", "..", "Data", filename),
          Dictionary = dictionaryName,
          DataFilter = NA_character_,
          DataClass = dataClass
        ),
        fill = TRUE
      )

      private$.configurationSheets[[xlsLabel]][[sheetName]] <- configSheet

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

  exporter$exportInputFiles()

  exporter$exportConfigSheets()

  return(invisible())
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
#' @examples
#' # Example usage of validateAndAdjustFilenames function
#' adjustedName <- validateAndAdjustFilenames("My Data File.csv", "data")
#' print(adjustedName) # Outputs: "my_data_file.csv"
#'
#' @export
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
#' Convert Excel file to list structure for JSON serialization
#'
#' @param dat of an Excelsheet
#' @return List structure ready for JSON serialization
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
