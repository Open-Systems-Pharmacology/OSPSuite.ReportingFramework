#' Add Scenarios to Electronic Package
#'
#' This function processes a configuration table of scenarios, validates filenames,
#' and organizes the necessary files for an electronic package export. It also
#' generates logs and summary files for review.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project-specific configurations,
#'                             including paths for electronic package folders and
#'                             output directories.
#' @param configTable A data.table containing configuration details for scenarios,
#'                    including scenario names, long names, and data group IDs.
#' @param subfolder A character string indicating the subfolder filled by the config table
#'
#' @return This function does not return a value. It performs file operations and
#'         generates log files based on the configuration provided.
#'
#' @export
addScenariosToEPackage <- function(projectConfiguration, configTable, subfolder) {
  checkmate::assertDataTable(configTable)
  checkmate::assertNames(names(configTable), must.include = "scenario")

  message(paste("Electronic Package: Export files for ", subfolder))

  dirAnalysisProgram <- createAnalysisDirectory(projectConfiguration)

  configTableToExport <- prepareConfigTableExport(configTable)

  configTableToExport <- validateAndUpdateFilenames(configTableToExport, fileNameDict, invalidFilenames, projectConfiguration)

  copyFilesToAnalysisProgram(configTableToExport, dirAnalysisProgram)

  configTableToExport[, subfolder := subfolder]

  exportLogFile <- createExportLogFile(configTableToExport, projectConfiguration)

  createReviewerAidTable(configTableToExport, projectConfiguration)

  createSummaryOverviewTable(configTableToExport, projectConfiguration)
}

#' Create Analysis Directory
#'
#' This function creates the analysis directory if it does not already exist.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project-specific configurations.
#'
#' @return The path to the created analysis directory.
#' @keywords internal
createAnalysisDirectory <- function(projectConfiguration) {
  dirAnalysisProgram <- file.path(projectConfiguration$addOns$electronicPackageFolder, EXPORTDIR$analysisProgram)
  if (!dir.exists(dirAnalysisProgram)) dir.create(dirAnalysisProgram, recursive = TRUE)
  return(dirAnalysisProgram)
}

#' Read or Initialize File Name Dictionary
#'
#' This function reads the filename dictionary from the specified path or initializes
#' an empty one if it does not exist.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project-specific configurations.
#'
#' @return A data.table containing the filename dictionary.
#' @keywords internal
readOrInitializeFileNameDictionary <- function(projectConfiguration) {
  filePathDictionary <- file.path(projectConfiguration$addOns$electronicPackageFolder, "filenameDictionary.csv")
  if (file.exists(filePathDictionary)) {
    fileNameDict <- data.table::fread(filePathDictionary)
    checkmate::assertCharacter(fileNameDict$filename, unique = TRUE, null.ok = TRUE)
    fileNameDict <- fileNameDict[!is.na(filenameNew)]
    checkmate::assertCharacter(fileNameDict$filenameNew, unique = TRUE, null.ok = TRUE)
  } else {
    fileNameDict <- data.table(filename = character(), filenameNew = character())
  }
  return(fileNameDict)
}

#' Prepare Configuration Table
#'
#' This function prepares the configuration table for export by merging it with
#' data group and scenario definitions.
#'
#' @param configTable A data.table containing configuration details for scenarios.
#'
#' @return A prepared configuration table for export.
#' @keywords internal
prepareConfigTableExport <- function(configTable) {
  configTableToExport <- rbind(
    configTable[is.na(level), c("scenario", "scenarioLongName", "dataGroupIds")] %>%
      separateAndTrim(columnName = "dataGroupIds") %>%
      merge(configEnv$dataGroupIds[, c("group", "displayNameData", "dataSection")],
        by.x = "dataGroupId",
        by.y = "group",
        all.x = TRUE
      ) %>%
      unique(),
    configTable[!is.na(referenceScenario) & !(referenceScenario %in% configTable$scenario), "referenceScenario"] %>%
      data.table::setnames(old = "referenceScenario", new = "scenario") %>%
      dplyr::mutate(comment = "as referencescenario") %>%
      unique(),
    fill = TRUE
  ) %>%
    merge(
      configEnv$scenarios[, c("scenarioName", "populationId")] %>%
        data.table::setnames(old = "populationId", new = "sourcePouplation"),
      by.x = "scenario",
      by.y = "scenarioName"
    )

  return(configTableToExport)
}

#' Validate and Update Filenames
#'
#' This function validates and updates the filenames in the configuration table.
#'
#' @param configTableToExport A data.table containing the configuration table to export.
#' @param fileNameDict A data.table containing the filename dictionary.
#' @param invalidFilenames A vector of invalid filenames encountered during validation.
#' @param projectConfiguration A ProjectConfiguration object containing project-specific configurations.
#'
#' @return The updated configuration table with valid filenames.
#' @keywords internal
validateAndUpdateFilenames <- function(configTableToExport, fileNameDict, invalidFilenames, projectConfiguration) {
  fileNameDict <- readOrInitializeFileNameDictionary(projectConfiguration)

  invalidFilenames <- c()

  configTableToExport[, pkmlFile := paste0(scenario, ".pkml")]
  checkmate::assertFileExists(file.path(projectConfiguration$outputFolder, EXPORTDIR$simulationResult, unique(configTableToExport$pkmlFile)))
  configTableToExport[, pkmlFileValid := ""]

  for (iRow in seq_len(nrow(configTableToExport))) {
    newFile <- if (configTableToExport$pkmlFile[iRow] %in% fileNameDict$filename) {
      fileNameDict[configTableToExport$pkmlFile[iRow] == filename]$filenameNew
    } else {
      paste0(configTableToExport$scenario[iRow], ".xml")
    }

    if (checkFileNameValidity(newFile, isDataSet = FALSE)) {
      configTableToExport$pkmlFileValid[iRow] <- newFile
    } else {
      invalidFilenames <- c(invalidFilenames, newFile)
    }
  }

  # Evaluate population files and build new simulation file name
  configTableToExport[, populationCsv := ""]
  configTableToExport[, populationCsvValid := ""]

  for (iRow in seq_len(nrow(configTableToExport))) {
    sourcePopulationFile <- file.path(
      projectConfiguration$populationsFolder,
      paste0(configTableToExport$sourcePouplation[iRow], ".csv")
    )
    scenarioPopulationFile <- file.path(
      projectConfiguration$outputFolder,
      EXPORTDIR$simulationResult,
      paste0(configTableToExport$scenario[iRow], "_population.csv")
    )

    checkmate::assertFile(scenarioPopulationFile)

    if (!file.exists(sourcePopulationFile)) {
      configTableToExport$populationCsv[iRow] <- scenarioPopulationFile
    } else {
      popsource <- data.table::fread(sourcePopulationFile)
      popscenario <- data.table::fread(scenarioPopulationFile)
      # Remove unit information from the column names
      data.table::setnames(popsource, gsub("\\s*\\[.*\\]", "", names(popsource)))
      data.table::setnames(popscenario, gsub("\\s*\\[.*\\]", "", names(popscenario)))

      comparisonResult <- all.equal(popsource, popscenario)
      if (is.logical(comparisonResult) && comparisonResult) {
        configTableToExport$populationCsv[iRow] <- sourcePopulationFile
      } else {
        configTableToExport$populationCsv[iRow] <- scenarioPopulationFile
      }
    }

    newFile <- basename(configTableToExport$populationCsv[iRow])
    if (newFile %in% fileNameDict$filename) {
      newFile <- fileNameDict[newFile == filename]$filenameNew
    }

    if (checkFileNameValidity(newFile, isDataSet = FALSE)) {
      configTableToExport$populationCsvValid[iRow] <- newFile
    } else {
      invalidFilenames <- unique(c(invalidFilenames, newFile))
    }
  }


  if (length(invalidFilenames) > 0) {
    fileNameDict <- updateFileNameDictionary(fileNameDict, invalidFilenames, projectConfiguration)
  }

  return(configTableToExport)
}

#' Update File Name Dictionary
#'
#' This function updates the filename dictionary with invalid filenames encountered.
#'
#' @param fileNameDict A data.table containing the filename dictionary.
#' @param invalidFilenames A vector of invalid filenames encountered during validation.
#' @param projectConfiguration A ProjectConfiguration object containing project-specific configurations.
#'
#' @return The updated filename dictionary.
#' @keywords internal
updateFileNameDictionary <- function(fileNameDict, invalidFilenames, projectConfiguration) {
  fileNameDict <- rbind(
    fileNameDict,
    data.table(
      filename = invalidFilenames,
      filenameNew = ""
    )
  )
  filePathDictionary <- file.path(projectConfiguration$addOns$electronicPackageFolder, "filenameDictionary.csv")
  data.table::fwrite(fileNameDict, file = filePathDictionary)
  return(fileNameDict)
}

#' Copy Files to Analysis Program
#'
#' This function copies valid files to the designated analysis program directory#'
#' @param configTableToExport A data.table containing the configuration table to export.
#' @param dirAnalysisProgram The directory where files should be copied.
#' @keywords internal
copyFilesToAnalysisProgram <- function(configTableToExport, dirAnalysisProgram) {
  tmp <- rbind(
    configTableToExport[, c("pkmlFile", "pkmlFileValid")] %>%
      unique() %>%
      .[, pkmlFile := file.path(projectConfiguration$outputFolder, EXPORTDIR$simulationResult, pkmlFile)] %>%
      data.table::setnames(
        old = c("pkmlFile", "pkmlFileValid"),
        new = c("from", "to")
      ),
    configTableToExport[, c("populationCsv", "populationCsvValid")] %>%
      unique() %>%
      data.table::setnames(
        old = c("populationCsv", "populationCsvValid"),
        new = c("from", "to")
      )
  )

  tmp <- tmp[to != ""]
  for (iRow in seq_len(nrow(tmp))) {
    file.copy(
      from = tmp$from[iRow],
      to = file.path(dirAnalysisProgram, tmp$to[iRow]),
      overwrite = TRUE,
      copy.mode = TRUE,
      copy.date = TRUE
    )
  }
}

#' Create Export Log File
#'
#' This function creates the export log file based on the configuration table.
#'
#' @param configTableToExport A data.table containing the configuration table to export.
#' @param projectConfiguration A ProjectConfiguration object containing project-specific configurations.
#'
#' @return The path to the created export log file.
#' @keywords internal
createExportLogFile <- function(configTableToExport, projectConfiguration) {
  exportLogFile <- file.path(
    getOption("OSPSuite.RF.logFileFolder", default = projectConfiguration$addOns$electronicPackageFolder),
    "ePackageFileExport.csv"
  )
  if (file.exists(exportLogFile)) {
    configTableToExport <- rbind(data.table::fread(exportLogFile),
      configTableToExport,
      fill = TRUE
    ) %>%
      unique()
  }
  data.table::fwrite(configTableToExport, file = exportLogFile)
}

#' Create Reviewer Aid Table
#'
#' This function creates a reviewer aid table based on the configuration table.
#'
#' @param configTableToExport A data.table containing the configuration table to export.
#' @param projectConfiguration A ProjectConfiguration object containing project-specific configurations.
#' @keywords internal
createReviewerAidTable <- function(configTableToExport, projectConfiguration) {
  tableForReviewerAidPdfFile <- file.path(projectConfiguration$addOns$electronicPackageFolder, "templateReviewerAid.csv")

  configTableToExport[, pkmlFileValid := ifelse(pkmlFileValid == "", "!!!invalidFileName", pkmlFileValid)]
  configTableToExport[, populationCsvValid := ifelse(populationCsvValid == "" & populationCsv != "",
    "!!!invalidFileName",
    basename(populationCsvValid)
  )]

  tableForReviewerAidPdf <- configTableToExport[, c("scenarioLongName", "pkmlFileValid", "populationCsvValid", "dataSection")] %>%
    data.table::setnames(
      old = c("scenarioLongName", "pkmlFileValid", "populationCsvValid", "dataSection"),
      new = c("Report name", "Model name (*.xml)", "Population file (*.csv)", "Observed data building block subfolder")
    ) %>%
    unique()

  filenameReviewerAid <- file.path(projectConfiguration$addOns$electronicPackageFolder, "reviewerAid.csv")
  if (file.exists(filenameReviewerAid)) {
    tableForReviewerAidPdf <- rbind(data.table::fread(filenameReviewerAid),
      tableForReviewerAidPdf,
      fill = TRUE
    ) %>%
      unique()
  }
  data.table::fwrite(tableForReviewerAidPdf, file = filenameReviewerAid)
}

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
