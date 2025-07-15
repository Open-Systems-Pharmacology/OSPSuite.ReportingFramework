# export ---------
#' @param projectconfiguration A ProjectConfiguration object containing the
#'   necessary settings and file paths for the project.
#' @param wfIdentifier (integer) identifier of the workflow.
#' @param scenarioNames (Character vector) list of scenario names to be simulated. Necessary for simulation workflow.
#' @param workflowRmd (character) filename of ePackageWorkflowRmd. Necessary for TLF workflows.
#' @export
exportEPackageWorkflow <- function(projectconfiguration,
                                   wfIdentifier,
                                   scenarioNames = NULL,
                                   workflowRmd = NULL,
                                   fileNameReplacements = c()){

  checkmate::assertIntegerish(wfIdentifier,
                              lower = 1,
                              len = 1,
                              any.missing = FALSE)
  checkmate::assertCharacter(scenarioNames,
                             any.missing = FALSE,
                             unique = TRUE,
                             null.ok = !is.null(workflowRmd))
  checkmate::assertCharacter(workflowRmd,
                             any.missing = FALSE,
                             len = 1,
                             null.ok = !is.null(scenarioNames))
  checkmate::assertCharacter(fileNameReplacements,
                             any.missing = FALSE,
                             unique = TRUE,
                             null.ok = TRUE)
  checkmate::assertIntegerish(length(fileNameReplacements)/2)

  if (!is.null(scenarioNames) & !is.null(workflowRmd)){
    stop('scenarioNames and workflowRmd are provided. Only one is needed')
  }

  electronicPackageFolder <- projectConfiguration$addOns$electronicPackageFolder
  if (!dir.exists(electronicPackageFolder)) dir.create(electronicPackageFolder, recursive = TRUE)


  if (!is.null(scenarioNames)) {
    exportSimulationWorkflow(projectconfiguration = projectconfiguration,
                             wfIdentifier = wfIdentifier,
                             scenarioNames = scenarioNames,
                             fileNameReplacements = fileNameReplacements)
  }
  if (!is.null(workflowRmd)) {
    exportTLFWorkflow(projectconfiguration = projectconfiguration,
                      wfIdentifier = wfIdentifier,
                      workflowRmd = workflowRmd,
                      workflowText = workflowText,
                      fileNameReplacements = fileNameReplacements)
  }

  return(invisible())
}

exportSimulationWorkflow = function(projectconfiguration,
                                    wfIdentifier,
                                    scenarioNames,
                                    fileNameReplacements){

  exportWorkflowText(wfIdentifier = wfIdentifier,
                     scenarioNames = scenarioNames,
                     projectconfiguration = projectconfiguration)

  inputFiles <-
    getEPackageInputfilesForScenarioNames(projectconfiguration = projectconfiguration,
                                          scenarioNames = scenarioNames)

  inputFilesChanged <- exportInputFilesForEPackage(inputFiles = inputFiles,
                              wfIdentifier = wfIdentifier,
                              electronicPackageFolder = projectConfiguration$addOns$electronicPackageFolder,
                              fileNameReplacements = fileNameReplacements)

  configurationSheets <-
    getEPackageConfigurationForScenarioNames(projectconfiguration = projectconfiguration,
                                             scenarioNames = scenarioNames,
                                             inputFilesChanged = inputFilesChanged)

  exportConfigSheets(configurationSheets = configurationSheets,
                     electronicPackageFolder = projectConfiguration$addOns$electronicPackageFolder,
                     wfIdentifier = wfIdentifier)

  return(invisible())

}


exportTLFWorkflow = function(projectconfiguration,
                             wfIdentifier,
                             workflowRmd,
                             fileNameReplacements){

  codeChunks <- etxractCodeChunks(workflowRmd)

  pathsCustomfunctions <- NULL
  eval(parse(text = codeChunks$`customfunctions-eval`))
  codeChunks[['customfunctions-replace']] <-
    evalChunkForCustomFunctions(pathsCustomfunctions = pathsCustomfunctions,
                        projectconfiguration = projectconfiguration,
                        wfIdentifier = wfIdentifier)



  exportWorkflowText(wfIdentifier = wfIdentifier,
                     scenarioNames = scenarioNames,
                     projectconfiguration = projectconfiguration)

  inputFiles <-
    getEPackageInputfilesForScenarioNames(projectconfiguration = projectconfiguration,
                                          scenarioNames = scenarioNames,
                                          pathsCustomfunctions = pathsCustomfunctions)

  inputFilesChanged <-
    exportInputFilesForEPackage(inputFiles = inputFiles,
                                wfIdentifier = wfIdentifier,
                                electronicPackageFolder = projectConfiguration$addOns$electronicPackageFolder,
                                fileNameReplacements = fileNameReplacements)

  configurationSheets <-
    getEPackageConfigurationForScenarioNames(projectconfiguration = projectconfiguration,
                                             scenarioNames = scenarioNames,
                                             inputFilesChanged = inputFilesChanged)

  exportConfigSheets(configurationSheets = configurationSheets,
                     electronicPackageFolder = projectConfiguration$addOns$electronicPackageFolder,
                     wfIdentifier = wfIdentifier)

  return(invisible())

}

# exportHelpers ---------

exportWorkflowText <- function(wfIdentifier,scenarioNames,projectconfiguration){
  workflowText <-
    paste(readLines(system.file("templates", "template_ePackageWorkflow.R",
                                package = "ospsuite.reportingframework")),
          collapse = "\n")


  workflowText <- gsub('XXwfIdentifierXX',
                       wfIdentifier,
                       workflowText)
  workflowText <- gsub('XXscenarioNamesXX',
                       paste(scenarioNames,collapse = "', '"),
                       workflowText)
  workflowText <- gsub('XXprojectDirectoryXX',
                       fs::path_rel(fs::path_common(path = c(projectConfiguration$configurationsFolder,
                                                             projectConfiguration$outputFolder)),
                                    start = projectConfiguration$addOns$electronicPackageFolder),
                       workflowText)


  writeLines(workflowText, file.path(projectConfiguration$addOns$electronicPackageFolder,
                                     paste0('w',wfIdentifier,'_workflow_r.txt')))

  return(invisible())
}
#' Get E-Package Configuration for Scenario Names
#'
#' This function retrieves the configuration data for specified scenario names from various Excel sheets
#' related to a project configuration. It loads relevant data from the specified workbook files and
#' organizes them into a list structure.
#'
#' @param projectconfiguration A list containing project configuration details, including file paths
#'                             for scenarios, individuals, model parameters, and applications.
#' @param scenarioNames A character vector of scenario names for which the configuration is to be
#'                      retrieved. These names must exist in the scenarios sheet of the workbook.
#' @param wfIdentifier A unique identifier for the workflow. (Currently not used in the function.)
#'
#' @return A list containing configuration sheets for the specified scenario names, including
#'         scenarios, output paths, PK parameters, individual biometrics, model parameters, and
#'         application protocols.
#' @keywords internal
getEPackageConfigurationForScenarioNames <- function(projectconfiguration,
                                      scenarioNames,
                                      wfIdentifier,
                                      inputFilesChanged){

  configurationSheets = list()

  wb = openxlsx::loadWorkbook(system.file("templates", "ProjectConfiguration.xlsx", package = "ospsuite.reportingframework"))
  dtConfig <- xlsxReadData(wb,sheetName = wb$sheet_names[1])
  dtConfig[setdiff(grep('Folder$',property),c(grep('Outputs',value),which(value == '.'))),value := paste0(value,'_w',wfIdentifier)]
  configurationSheets[["ProjectConfiguration.xlsx"]] <-
    list(Scenarios = excelToListStructure(dtConfig))

  # read and filter sheets of scenario table
  wb = openxlsx::loadWorkbook(projectConfiguration$scenariosFile)
  dtScenarios <- xlsxReadData(wb,'Scenarios')
  checkmate::assertNames(scenarioNames,subset.of = dtScenarios$scenario_name)

  dtScenarios <- dtScenarios[scenario_name %in% scenarioNames]
  dtScenarios[inputFilesChanged, modelFile := i.fileName, on = .(modelFile = source)]
  dtScenarios[inputFilesChanged, populationId := i.fileName, on = .(populationId = source)]

  configurationSheets[[basename(projectConfiguration$scenariosFile)]] <-
    list(Scenarios = excelToListStructure(dtScenarios))

  dtOutputPaths <- xlsxReadData(wb,'OutputPaths')
  dtOutputPaths <- dtOutputPaths[outputPathId %in% dtScenarios$outputPathsIds]
  configurationSheets[[basename(projectConfiguration$scenariosFile)]][['OutputPaths']] <-
    excelToListStructure(dtOutputPaths)

  dtPKarameter <- xlsxReadData(wb,'PKParameter')
  dtPKarameter <- dtPKarameter[scenario_name %in% scenarioNames]
  if (nrow(dtPKarameter) > 0){
    configurationSheets[[basename(projectConfiguration$scenariosFile)]][['PKParameter']] <-
      excelToListStructure(dtPKarameter)

    configurationSheets <-
      addSelectedSheets(configurationSheets = configurationSheets,
                        xlsfile = projectConfiguration$pKParameterFile)
  }

  # Filter Individuals
  if (any(!is.na(dtScenarios$individualId))){
    dtIndividuals <- xlsxReadData(projectConfiguration$individualsFile,'IndividualBiometrics')
    dtIndividuals[individualId %in% dtScenarios$individualId]
    configurationSheets[[basename(projectConfiguration$individualsFile)]] <-
      list(IndividualBiometrics = excelToListStructure(dtIndividuals))

    configurationSheets <-
      addSelectedSheets(configurationSheets = configurationSheets,
                        xlsfile = projectConfiguration$individualsFile,
                        selectedSheets = dtIndividuals$individualId)
  }

  configurationSheets <-
    addSelectedSheets(configurationSheets = configurationSheets,
                      xlsfile = projectConfiguration$modelParamsFile,
                      selectedSheets = dtScenarios$modelParameterSheets)

  configurationSheets <-
    addSelectedSheets(configurationSheets = configurationSheets,
                      xlsfile = projectConfiguration$applicationsFile,
                      selectedSheets = dtScenarios$applicationProtocol)

  return(configurationSheets)

}

getEPackageInputfilesForScenarioNames <- function(projectconfiguration,
                                         scenarioNames,
                                         wfIdentifier){

  inputFiles = data.table()

  wb = openxlsx::loadWorkbook(projectConfiguration$scenariosFile)
  dtScenarios <- xlsxReadData(wb,'Scenarios')
  checkmate::assertNames(scenarioNames,subset.of = dtScenarios$scenario_name)
  dtScenarios <- dtScenarios[scenario_name %in% scenarioNames]

  # Check if population are always used as readPopulationFromCsv
  if (any(!is.na(dtScenarios$populationId))){
    readPopulationFromCSV <-
      as.logical(dtScenarios[!is.na(populationId)]$readPopulationFromCSV)
    if (any(!(readPopulationFromCSV),na.rm = TRUE) |
        any(is.na(readPopulationFromCSV))){
      stop('Please use only poulation scenarios with exported poulation in workflows inteded for an electronic package')
    }

    inputFiles <-
      rbind(inputFiles,
            data.table(source = file.path(projectConfiguration$populationsFolder,
                                            paste0(unique(dtScenarios[!is.na(populationId)]$populationId),'.csv')),
                       fileType = 'population'))
  }


  inputFiles <-
    rbind(inputFiles,
          data.table(source = file.path(projectConfiguration$modelFolder,
                                          unique(dtScenarios$modelFile)),
                     fileType = 'model'))


  return(inputFiles)

}

exportInputFilesForEPackage <- function(inputFiles,
                                        wfIdentifier,
                                        electronicPackageFolder,
                                        fileNameReplacements){

  checkmate::assertFileExists(inputFiles$source)

  inputFiles[,fileName := basename(source)]
  # replace filename
  if (length(fileNameReplacements) > 0){
    inputFiles[fileName %in% fileNameReplacements[seq(1,length(fileNameReplacements),2)],
               fileName := fileNameReplacements[which(fileName == fileNameReplacements)+1]]
  }
  #change extension
  inputFiles[fileType == 'model',fileName := fs::path_ext_set(path = fileName,ext = 'xml')]

  # adjust filenames to fulfill naming requirements
  inputFiles[,fileNameUnchanged := fileName]
  inputFiles[,fileName := validateAndAdjustFilenames(fileName,fileType),by = .I]

  if (any(duplicated(inputFiles$fileName)))
    stop(paste('fileName is not unique',
               paste(inputFiles$fileName[duplicated(inputFiles$fileName)],collapse = ', ')))

  # do the actual copy
  success <- file.copy(from = inputFiles$source,
            to = file.path(electronicPackageFolder,inputFiles$fileName),
            overwrite = TRUE)

  if (!all(success)) stop(paste('filecopy to ePackae folder failed for:',
                                paste(inputFiles[!success]$fileName,collapse = ', ')))

  fwrite(x = inputFiles[,c('fileName','fileType')],
         file = file.path(electronicPackageFolder,paste0('w',wfIdentifier,'_input_files.csv')))

  # list all files with changed names
  ixChanged <- which(inputFiles$fileNameUnchanged != inputFiles$fileName)

  if (length(ixChanged > 0))
    warning(paste0('Adjusted filenames:\n',paste(
      paste(inputFiles[ixChanged]$fileNameUnchanged,'->',inputFiles[ixChanged]$fileName),
      collapse = '\n')))

  # return the change names for replacement in configsheets
  inputFilesChanged <- inputFiles[ixChanged,c('source','fileName','fileType')]
  inputFilesChanged[,fileName := fs::path_ext_set(path = fileName,ext = fs::path_ext(source))]
  inputFilesChanged[,source := basename(source)]
  setkey(inputFilesChanged, source)

  return(inputFilesChanged)

}

exportConfigSheets <- function(configurationSheets,electronicPackageFolder,wfIdentifier){

  # Convert Configuration to JSON
  jsonData <- jsonlite::toJSON(
    configurationSheets,
    pretty = TRUE,
    auto_unbox = TRUE,
    digits = NA
  )

  # Write to file
  writeLines(jsonData, file.path(electronicPackageFolder,paste0('w',wfIdentifier,'_config_json.txt')))

  return(invisible())
}

etxractCodeChunks <- function(workflowRmd){

  .extract = function(rmdfile){
    knitr::knit_code$restore()

    tmpScript <- tempfile()
    knitr::purl(rmdfile, output=tmpScript, quiet = TRUE)
    # Set the timeout duration (in seconds)
    timeout <- 60
    start_time <- Sys.time()
    codeChunks = knitr::read_chunk(tmpScript)
    # Wait until the file exists and is not empty, or timeout after 1 minute
    while (is.null(unlist(tail(codeChunks,1)))) {
      if (as.numeric(difftime(Sys.time(), start_time, units = "secs")) >= timeout) {
        stop("Error: The file did not become available within 1 minute.")
      }
      Sys.sleep(0.1)  # Check every 0.1 seconds
      codeChunks = knitr::read_chunk(tmpScript)
    }
    return(codeChunks)

  }
  codeChunksTemplate <- .extract(system.file("templates","template_ePackageWorkflow.Rmd",
                                             package = "ospsuite.reportingframework"))

  chunkLabels = list()
  for (suffix in c('copy','replace','eval')){
    chunkLabels[[suffix]] = grep(paste0('-',suffix,'$'),names(codeChunksTemplate),value = TRUE)
  }

  codeChunks <- .extract(workflowRmd)

  if (!all(unlist(chunkLabels) %in% names(codeChunks))){
    tmp <- setdiff(unlist(chunkLabels),names(codeChunks))
    stop(paste('Chunks are missing in workflowRmd check:',
               paste(tmp,collapse = ', ')))
  }

  codeChunks <- codeChunks[unlist(chunkLabels)]

  codeChunks <- utils::modifyList(codeChunks,codeChunksTemplate[chunkLabels[['replace']]])

  return(codeChunks)
}


evalChunkForCustomFunctions <- function(pathsCustomfunctions,
                            projectconfiguration,
                            wfIdentifier){
  #check if anything to do
  if (length(pathsCustomfunctions) == 0) return('')

}


# auxiliaries ------
# Function to validate and adjust filenames
validateAndAdjustFilenames <- function(fileName,fileType) {
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
  fileName <- fs::path_ext_set(gsub("[^a-zA-Z0-9_]", "",fs::path_ext_remove(fileName)),
                               extension)

  # Check if the first character is numeric
  if (grepl("^[0-9]", fileName)) {
    stop(paste("Error: Filename cannot start with a number:", fileName))
  }

  # Check if the length of the filename is below the limit
  limitLength <- ifelse(fileType == 'data',32,64)
  if (nchar(fileName) > limitLength) {
    stop(paste("Error: Filename is too long (greater than", limitLength,"characters):", fileName))
  }

  return(fileName)
}


addSelectedSheets <- function(configurationSheets,
                              xlsfile,
                              selectedSheets = NULL){
  if (is.null(selectedSheets) || any(!is.na(selectedSheets))){
    wb = openxlsx::loadWorkbook(xlsfile)
    if (is.null(selectedSheets)){
      selectedSheets <- wb$sheet_names
    }
    for (sheet in intersect(wb$sheet_names,selectedSheets)){
      configurationSheets[[basename(xlsfile)]][[sheet]] <-
        excelToListStructure(xlsxReadData(wb,sheet))
    }

    return(configurationSheets)
  }
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
    for (i in 1:nrow(df)) {
      # Extract row as a character vector to avoid type issues during serialization
      rowValues <- sapply(df[i, ], as.character)
      sheetData$rows[[i]] <- as.list(rowValues)
    }
  }

  return(sheetData)
}


### old ------------

#' Create Summary Overview Table
#'
#' This function creates a summary overview table based on the configuration table.
#'
#' @param configTableToExport A data.table containing the configuration table to export.
#' @param projectConfiguration A ProjectConfiguration object containing project-specific configurations.
#' @keywords internal
createSummaryOverviewTable <- function(configTableToExport, projectConfiguration) {
  summaryOverview <- configTableToExport[, c("scenarioLongName", "pkmlFileValid", "populationCsvValid")] %>%
    tidyr::pivot_longer(cols = c("pkmlFileValid", "populationCsvValid"), names_to = "fileType", values_to = "Report/Document Name") %>%
    data.table::setDT() %>%
    .[, scenarioLongName := ifelse(fileType == "pkmlFileValid", scenarioLongName, "")] %>%
    unique() %>%
    data.table::setnames(
      old = c("scenarioLongName"),
      new = c("Content of Document")
    ) %>%
    .[, ("Document Type") := ifelse(fileType == "pkmlFileValid", "UTF-8 file (xml format)", "UTF-8 file")] %>%
    .[, ("Location in eCTD") := "M5"] %>%
    .[, fileType := NULL] %>%
    data.table::setcolorder(c("Report/Document Name", "Content of Document", "Document Type", "Location in eCTD"))

  filenameOverview <- file.path(projectConfiguration$addOns$electronicPackageFolder, "summaryOverview.csv")
  if (file.exists(filenameOverview)) {
    summaryOverview <- rbind(data.table::fread(filenameOverview),
      summaryOverview,
      fill = TRUE
    ) %>%
      unique()
  }
  data.table::fwrite(summaryOverview, file = filenameOverview)
}
#' Export Time Profile Data for Electronic Package
#'
#' This function exports time profile data for the electronic package by merging
#' the provided data with configuration details and saving it in the specified format.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project-specific configurations.
#' @param dataToExport A data.table containing the data to be exported.
#'
#' @return This function does not return a value. It performs file operations to save the exported data.
#' @keywords internal
exportTimeProfileDataForEPackage <- function(projectConfiguration, dataToExport) {
  dataToExport <- merge(
    dataToExport %>%
      dplyr::select(!any_of(c(
        setdiff(intersect(names(dataToExport), names(configEnv$dataGroupIds)), "group"),
        ".Id", "individualId", "dataType"
      ))),
    configEnv$dataGroupIds %>%
      dplyr::select(!any_of(c("shape", "reference", "defaultScenario"))),
    by = "group"
  )

  data.table::setnames(
    x = dataToExport,
    old = c("dataSection", "scenario", "studyId", "group", "dose", "route", "molecule", "species", "organ", "compartment"),
    new = c("Section", "Simulation", "Study Id", "Group Id", "Dose", "Route", "Molecule", "Species", "Organ", "Compartment"),
    skip_absent = TRUE
  )

  for (dataByClass in split(dataToExport, by = "dataClass")) {
    dataByClass <- setDT(dataByClass)

    if (dataByClass$dataClass[1] == DATACLASS$tpAggregated) {
      dataByErrorList <- split(dataToExport, by = "yErrorType")
    } else {
      dataByErrorList <- list(dataByClass)
    }

    for (dataByError in dataByErrorList) {
      dataByError <- setDT(dataByError)

      if (dplyr::n_distinct(dataByError$xUnit) == 1) {
        data.table::setnames(
          x = dataByError,
          old = "xValues",
          new = paste0("Time [", dataByError$xUnit[1], "]")
        )
        dataByError[, xUnit := NULL]
      }

      if (dataByError$dataClass[1] == DATACLASS$tpIndividual) {
        data.table::setnames(x = dataByError, old = "yValues", "Measurement")
      } else if (dataByError$yErrorType[1] == ospsuite::DataErrorType$ArithmeticStdDev) {
        data.table::setnames(x = dataByError, old = "yValues", "Mean")
        data.table::setnames(x = dataByError, old = "yErrorValues", "Sd")
      } else if (dataByError$yErrorType[1] == ospsuite::DataErrorType$GeometricStdDev) {
        data.table::setnames(x = dataByError, old = "yValues", "GeoMean")
        data.table::setnames(x = dataByError, old = "yErrorValues", "GeoSd")
      }

      suffix <- names(DATACLASS[DATACLASS == dataByError$dataClass[1]])
      if (dataByError$dataClass[1] == DATACLASS$tpAggregated) {
        suffix <- paste0(suffix, "_", dataByError$yErrorType[1])
      }

      filenameData <- file.path(
        projectConfiguration$addOns$electronicPackageFolder,
        paste0("dataObserved_", suffix, ".csv")
      )
      if (file.exists(filenameData)) {
        tmp <- utils::read.csv(
          file = filenameData,
          fileEncoding = "UTF8", check.names = FALSE
        ) %>%
          setDT()

        dataByError <- rbind(tmp,
          dataByError,
          fill = TRUE
        ) %>%
          unique()
      }
      utils::write.csv(
        x = dataByError,
        file = filenameData,
        fileEncoding = "UTF8",
        row.names = FALSE
      )
    }
  }
}


#' Check File Name Validity
#'
#' This function checks if a given filename is valid based on specified criteria,
#' including allowed file extensions, maximum length, and that it does not start
#' with a numerical digit.
#'
#' @param fileName A character string representing the filename to be checked.
#' @param isDataSet A logical value indicating whether the filename is for a dataset.
#'                  If TRUE, only "txt" is allowed as a file extension with a maximum
#'                  length of 32 characters. If FALSE, "xml", "txt", and "csv" are allowed
#'                  with a maximum length of 64 characters.
#'
#' @return A logical value: TRUE if the filename is valid, FALSE otherwise.
#'         If invalid, a warning is issued detailing the specific reason(s).
#'
#' @examples
#' checkFileNameValidity("valid_file.txt")
#' checkFileNameValidity("1invalid_file.txt")
#' checkFileNameValidity("too_long_filename_that_exceeds_the_limit.txt", isDataSet = TRUE)
#'
#' @export
checkFileNameValidity <- function(fileName, isDataSet = FALSE) {
  # Define the allowed file extensions and maximum length
  if (isDataSet) {
    maxLengthForFiles <- 32
    allowedExtensions <- c("txt")
  } else {
    maxLengthForFiles <- 64
    allowedExtensions <- c("xml", "txt", "csv")
  }

  # Define the regular expression pattern for the file names (without extension)
  pattern <- paste0(
    "^[a-z][a-z0-9_]*$"
  )

  # Initialize a list to collect warning messages
  warnings <- character()

  # Check the pattern without the extension
  if (!grepl(pattern, tools::file_path_sans_ext(fileName))) {
    warnings <- c(warnings, paste("Filename '", fileName, "' must start with a letter, contain only lowercase letters, digits, or underscores.", sep = ""))
  }

  if (tolower(tools::file_ext(fileName)) %in% allowedExtensions == FALSE) {
    warnings <- c(warnings, paste("Filename '", fileName, "' must have one of the following extensions:", paste(allowedExtensions, collapse = ", "), ".", sep = ""))
  }

  if (nchar(fileName) > maxLengthForFiles) {
    warnings <- c(warnings, paste("Filename '", fileName, "' must not exceed", maxLengthForFiles, "characters.", sep = " "))
  }

  if (length(warnings) > 0) {
    warning(paste(warnings, collapse = "\n"))
    return(FALSE) # Return FALSE if there are warnings
  }

  return(TRUE) # Return TRUE if all conditions are met
}

#' Export PK Parameters with IOV for Electronic Package
#'
#' This function exports pharmacokinetic (PK) parameters with inter-observation variability (IOV)
#' for the electronic package, saving them in separate CSV files based on scenarios.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project-specific configurations.
#' @param pkParameterDT A data.table containing PK parameter data.
#'
#' @return This function does not return a value. It performs file operations to save the exported PK parameters.
#' @keywords internal
exportPKWithIOVForEPackage <- function(projectConfiguration, pkParameterDT) {
  if (getOption("OSPSuite.RF.withEPackage", default = FALSE)) {
    exportDir <- file.path(projectConfiguration$addOns$electronicPackageFolder, "PKParameterWithIOV")
    checkmate::assertPathForOutput(exportDir)
    if (!dir.exists(exportDir)) dir.create(exportDir)

    dtList <- split(pkParameterDT, by = c("scenario", "referenceScenario", "outputPathId", "pkParameter"))

    invisible(lapply(
      names(dtList),
      function(dtName) {
        fileName <- paste0(dtName, ".csv")
        utils::write.csv(
          x = dtList[[dtName]],
          file = file.path(exportDir, fileName),
          fileEncoding = "UTF8",
          row.names = FALSE
        )
      }
    ))
  }
  return(invisible())
}
