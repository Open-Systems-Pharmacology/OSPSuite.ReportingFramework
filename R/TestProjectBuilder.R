#' @title TestProjectBuilder
#' @docType class
#' @description Manages the creation and writing test project, which can be used for this package or
#' packages which use the reporting framework as base to build a test project
#' @export
TestProjectBuilder <- R6::R6Class( # nolint
  "TestProjectBuilder",
  inherit = ospsuite.utils::Printable,
  cloneable = TRUE,
  # public ----
  public = list(
    #' @description
    #' Initialize a new instance of the class.
    initialize = function() {
      private$packageDirFramework <- system.file(
        package = "ospsuite.reportingframework",
        "extdata",
        mustWork = TRUE
      )

      return(self)
    },
    #' @description
    #' A short description...
    #'
    #' This function initializes the project structure and configuration for the test project.
    #'
    #' @param rootDirectory A character string specifying the root directory for the test project.
    #' @param verbose A logical value indicating whether to print messages about the process.
    #'
    #' @return A ProjectConfiguration object containing the details of the initialized project.
    iniTestProject = function(rootDirectory,verbose = FALSE) {
      if (is.null(rootDirectory)) {
        rootDirectory <- file.path(tempdir(), "testProject")
      }
      if (!dir.exists(file.path(rootDirectory, "Scripts", "ReportingFramework"))) {
        dir.create(file.path(rootDirectory, "Scripts", "ReportingFramework"), recursive = TRUE)
      }
      self$rootDirectory <- rootDirectory

      setWorkflowOptions(isValidRun = FALSE)

      # Setup project
      initProject(configurationDirectory = file.path(self$rootDirectory, "Scripts", "ReportingFramework")) # Initialize the project structure

      # Get paths of all relevant project files
      projectConfiguration <- ospsuite.reportingframework::createProjectConfiguration(
        path = file.path(file.path(self$rootDirectory, "Scripts", "ReportingFramework", "ProjectConfiguration.xlsx"))
      )

      initLogfunction(projectConfiguration = projectConfiguration,verbose = verbose)

      private$synchronizeDirectories(
        fromDir = file.path(private$packageDirFramework, "Models"),
        toDir = file.path(projectConfiguration$modelFolder)
      )

      return(projectConfiguration)
    },
    #' @description
    #' This function sets up populations for the test project by copying necessary files
    #' and exporting defined populations.
    #'
    #' @param projectConfiguration A ProjectConfiguration object containing the project configuration details.
    #' @param writeTestData A logical value indicating whether newly created test data should be filed to the inst directory of the package.
    #' @param populationNames A character vector of population names to be copied.
    #' @param templateDir  character string specifying the installation directory for the package.
    #'
    #' @return NULL
    setupRandomPopulations = function(projectConfiguration,
                                      populationNames,
                                      writeTestData = FALSE,
                                      templateDir = NULL) {
      if (is.null(templateDir)) templateDir <- private$packageDirFramework

      # Copy files needed for example to the correct folders
      private$synchronizeDirectories(
        fromDir = file.path(templateDir, "Populations"),
        toDir = file.path(projectConfiguration$populationsFolder),
        filePrefixes = populationNames
      )

      # Exports all populations defined in the population.xlsx sheet "Demographics"
      suppressMessages(exportRandomPopulations(
        projectConfiguration = projectConfiguration,
        populationNames = NULL,
        overwrite = FALSE
      ))

      if (writeTestData) {
        if (!dir.exists(file.path(templateDir, "Populations"))) {
          dir.create(file.path(templateDir, "Populations"))
        }

        private$synchronizeDirectories(
          fromDir = file.path(projectConfiguration$populationsFolder),
          toDir = file.path(templateDir, "Populations")
        )
      }
    },
    #' @description
    #' This function sets up simulations for the test project by synchronizing necessary directories
    #' and running initialized scenarios to calculate pharmacokinetic (PK) parameters.
    #'
    #' @param projectConfiguration A ProjectConfiguration object containing the project configuration details.
    #' @param scenarioList A list of scenarios initialized for the project.
    #' @param templateDir A character string specifying the installation directory for the package.
    #' @param writeTestData A logical value indicating whether newly created test data should be filed to the inst directory of the package.
    #'
    #' @return A list containing the results from running the scenarios.
    setupSimulations = function(projectConfiguration,
                                scenarioList,
                                writeTestData,
                                templateDir = NULL) {
      if (is.null(templateDir)) templateDir <- private$packageDirFramework

      private$synchronizeDirectories(
        fromDir = file.path(templateDir, EXPORTDIR$simulationResult),
        toDir = file.path(projectConfiguration$outputFolder, EXPORTDIR$simulationResult),
        filePrefixes = names(scenarioList)
      )

      private$synchronizeDirectories(
        fromDir = file.path(templateDir, EXPORTDIR$pKAnalysisResults),
        toDir = file.path(projectConfiguration$outputFolder, EXPORTDIR$pKAnalysisResults),
        filePrefixes = names(scenarioList)
      )

      # Run initialized scenarios and Calculate PK Parameters
      scenarioResults <- runOrLoadScenarios(
        projectConfiguration = projectConfiguration,
        scenarioList = scenarioList,
        simulationRunOptions = ospsuite::SimulationRunOptions$new(
          showProgress = TRUE
        )
      )

      if (writeTestData) {
        if (!dir.exists(file.path(templateDir, EXPORTDIR$simulationResult))) {
          dir.create(file.path(templateDir, EXPORTDIR$simulationResult))
        }

        private$synchronizeDirectories(
          fromDir = file.path(projectConfiguration$outputFolder, EXPORTDIR$simulationResult),
          toDir = file.path(templateDir, EXPORTDIR$simulationResult)
        )

        if (!dir.exists(file.path(templateDir, EXPORTDIR$pKAnalysisResults))) {
          dir.create(file.path(templateDir, EXPORTDIR$pKAnalysisResults))
        }

        private$synchronizeDirectories(
          fromDir = file.path(projectConfiguration$outputFolder, EXPORTDIR$pKAnalysisResults),
          toDir = file.path(templateDir, EXPORTDIR$pKAnalysisResults),
          pattern = ".csv"
        )
      }

      return(scenarioResults)
    },
    #' Set Up Sensitivity Analysis
    #'
    #' This function sets up sensitivity analysis for the test project based on the defined scenarios.
    #'
    #' @param projectConfiguration A ProjectConfiguration object containing project configuration details.
    #' @param scenarioList A list of scenarios initialized for the project.
    #' @param scenarioName The name of the scenario used for sensitivity
    #' @param templateDir A character string specifying the installation directory for the package.
    #' @param parameterList A named list where each name corresponds to a parameter in the
    #'                      Excel sheet, and each value is the new value to set for that
    #'                      parameter.
    #' @param sensitivitySheet Name of sheet with parameter selection
    #' @param writeTestData A logical value indicating whether newly created test data should be filed to the inst directory of the package.
    #'
    #' @return A ProjectConfiguration object containing the updated project configuration.
    setUpSensitivity = function(projectConfiguration,
                                scenarioList,
                                scenarioName,
                                writeTestData,
                                parameterList,
                                sensitivitySheet = "smallSelection",
                                templateDir = NULL) {
      if (is.null(templateDir)) templateDir <- private$packageDirFramework

      projectConfiguration <- addSensitivityTable(
        projectConfiguration,
        scenarioList = scenarioList,
        scenarioName = scenarioName,
        sheetName = sensitivitySheet
      )

      self$mockManualEditingsSensitivity(
        projectConfiguration = projectConfiguration,
        sheetName = sensitivitySheet,
        parameterList = parameterList
      )

      private$synchronizeDirectories(
        fromDir = file.path(templateDir, EXPORTDIR$sensitivityResults),
        toDir = file.path(projectConfiguration$outputFolder, EXPORTDIR$sensitivityResults)
      )


      runSensitivityAnalysisForScenarios(
        projectConfiguration = projectConfiguration,
        scenarioList = scenarioListInd,
        scenarioNames = scenarioName,
        sensitivitysheet = sensitivitySheet,
        overwrite = FALSE
      )

      if (writeTestData) {
        if (!dir.exists(file.path(templateDir, EXPORTDIR$sensitivityResults))) {
          dir.create(file.path(templateDir, EXPORTDIR$sensitivityResults))
        }

        private$synchronizeDirectories(
          fromDir = file.path(projectConfiguration$outputFolder, EXPORTDIR$sensitivityResults),
          toDir = file.path(templateDir, EXPORTDIR$sensitivityResults)
        )
      }

      return(projectConfiguration)
    },
    #' @description
    #' This function adds random time profile data to the test project for selected individuals.
    #'
    #' @param projectConfiguration A ProjectConfiguration object containing project configuration details.
    #' @param scenarioResults List of scenario results.
    #' @param ids A vector of individual IDs to include in the random data.
    #' @param timePoints A numeric vector specifying the time points at which to generate data (default is c(15, 30, 45, 60, 90, c(3, 6, 9, 12, 18, 24) * 60)).
    #' @param outputPathIds A character vector specifying the output path IDs to include in the random data (default is c('Plasma', 'CYP3A4total', 'CYP3A4Liver')).
    #' @param sdShift A numeric value indicating the standard deviation shift applied to the random values (default is 0.05).
    #' @param lloqValue A numeric value representing the lower limit of quantification (default is 0.1).
    #'
    #' @return Invisible NULL.
    addRandomTPData = function(projectConfiguration,
                               scenarioResults,
                               ids,
                               timePoints = c(15, 30, 45, 60, 90, c(3, 6, 9, 12, 18, 24) * 60),
                               outputPathIds = c("Plasma", "CYP3A4total", "CYP3A4Liver"),
                               sdShift = 0.05,
                               lloqValue = 0.1) {
      # Initialize variable to avoid messages
      Time <- STUD <- IndividualId <- Height <- Age <- Weight <- IndividualId <- LLOQ <- NULL # nolint
      dataFolder <- file.path(projectConfiguration$configurationsFolder, "..", "..", "Data")
      if (!dir.exists(dataFolder)) dir.create(dataFolder)

      for (sc in names(scenarioResults)) {
        scenarioResult <- scenarioResults[[sc]]
        randomIndividuals <- private$getRandomIndividualData(
          projectConfiguration = projectConfiguration,
          scenarioResult = scenarioResult,
          timePoints = timePoints,
          outputPathIds = outputPathIds,
          ids = ids
        )
        randomIndividuals <- private$calculateShiftedValues(
          randomIndividuals = randomIndividuals,
          sdShift = sdShift,
          lloqValue = lloqValue
        )
        randomIndividuals <- private$addBiometricPopulationData(
          randomIndividuals = randomIndividuals,
          scenarioResult = scenarioResult
        )

        randomIndividuals[, STUD := 1234]
        randomIndividuals[, route := sub(".*_(.*)", "\\1", sc)]
        setnames(randomIndividuals,
          old = c("IndividualId", "outputPathId", "Time", "shiftedValue", "displayUnit", "Gender", "Age", "Weight", "Height"),
          new = c("SID", "outputPathId", "TIME", "DV", "DVUNIT", "SEX", "AGE", "WGHT0", "HGHT0")
        )
        setcolorder(randomIndividuals, "STUD")

        fwrite(
          x = randomIndividuals,
          file = file.path(dataFolder, paste0("timeprofiles_", sc, ".csv"))
        )
      }

      private$addDataImporterConfigurationFilesTP(
        projectConfiguration = projectConfiguration,
        scenarioNames = names(scenarioResults)
      )

      return(invisible())
    },
    #' This function adds random pharmacokinetic (PK) data to the test project,
    #' including shifting values and calculating statistics for selected individuals.
    #'
    #' @param projectConfiguration A ProjectConfiguration object containing project configuration details.
    #' @param pkParameterDT A data.table containing PK parameters and values.
    #' @param ids A vector of individual IDs to include in the random data.
    #' @param outputPathIds A character vector specifying the output path IDs to include in the random data (default is c('Plasma', 'CYP3A4total', 'CYP3A4Liver')).
    #' @param sdShift A numeric value indicating the standard deviation shift applied to the random values (default is 0.05).
    #' @return Invisible NULL.
    addRandomPKData = function(projectConfiguration,
                               pkParameterDT,
                               ids,
                               outputPathIds = c("Plasma", "CYP3A4total", "CYP3A4Liver"),
                               sdShift = 0.05) {
      # initialize variable to avoid messages
      ratio <- iv <- po <- displayUnitPKParameter <- outputPathId <- route <- value <- NULL
      pkParameter <- population <- shiftedValue <- scenario <- individualId <- unit <- NULL
      STUD <- NULL # nolint

      dataFolder <- file.path(projectConfiguration$configurationsFolder, "..", "..", "Data")
      if (!dir.exists(dataFolder)) dir.create(dataFolder)

      randomIndividuals <- private$getRandomIndividualPKdata(
        pkParameterDT = pkParameterDT,
        ids = ids,
        outputPathIds = outputPathIds,
        sdShift = sdShift
      )

      dt <- private$aggregatePKData(randomIndividuals)
      fwrite(dt, file.path(dataFolder, "PK_absolute_values.csv"))

      # ratios
      dt <- private$aggregatePKDataRatios(randomIndividuals)
      fwrite(dt, file.path(dataFolder, "PK_ratios.csv"))

      private$addDataImporterConfigurationFilesPK(projectConfiguration)

      return(invisible())
    },
    #' @description
    #' This function cleans up the project configuration files
    #' by clearing existing data from relevant Excel sheets.
    #' @param projectConfiguration A ProjectConfiguration object containing project configuration details.
    #' @return NULL
    mockManualEditingsCleanup = function(projectConfiguration) {
      # delete examples
      for (xlsfile in c(
        projectConfiguration$scenariosFile,
        projectConfiguration$applicationsFile,
        projectConfiguration$populationsFile,
        projectConfiguration$individualsFile,
        projectConfiguration$modelParamsFile
      )) {
        # cleanup template lines in workbooks
        wb <- openxlsx::loadWorkbook(xlsfile)

        for (sheetName in wb$sheet_names) {
          dt <- xlsxReadData(wb = wb, sheetName = sheetName)
          dt <- dt[FALSE]
          xlsxWriteData(wb = wb, sheetName = sheetName, dt = dt)
        }
        # save all sheets
        openxlsx::saveWorkbook(wb, xlsfile, overwrite = TRUE)
      }
      # cleanup in data Configuration only the sheet with data files
      wb <- openxlsx::loadWorkbook(projectConfiguration$dataImporterConfigurationFile)
      dt <- xlsxReadData(wb = wb, sheetName = "DataFiles", skipDescriptionRow = FALSE)
      dt <- dt[1]
      xlsxWriteData(wb = wb, sheetName = "DataFiles", dt = dt)
      openxlsx::saveWorkbook(wb, projectConfiguration$dataImporterConfigurationFile, overwrite = TRUE)
    },
    #' @description
    #' This function adds biometric ranges for virtual pediatric and one adult populations to the configuration files
    #' @param projectConfiguration A ProjectConfiguration object containing project configuration details.
    #' @param randomPops data.table with content for sheet Demographics in Population.xlsx
    #' @return NULL
    mockManualEditingsPopulation = function(projectConfiguration,
                                            randomPops = data.table(
                                              populationName = c("adults", "toddler", "children", "school-children", "adolescents"),
                                              species = "Human",
                                              population = "European_ICRP_2002",
                                              numberOfIndividuals = 100,
                                              proportionOfFemales = 50,
                                              ageMin = c(20, 0.5, 2, 6, 12),
                                              ageMax = c(40, 2, 6, 12, 18),
                                              weightUnit = "kg",
                                              heightUnit = "cm",
                                              bMIUnit = "kg/m\u00B2",
                                              protein = "CYP3A4,UGT1A4",
                                              ontogeny = "CYP3A4,UGT1A4"
                                            )) {
      # add virtual population with in bio metric ranges of observed data
      wb <- openxlsx::loadWorkbook(projectConfiguration$populationsFile)

      dtPops <- xlsxReadData(wb = wb, sheetName = "Demographics")
      dtPops <- rbind(dtPops[0],
        randomPops,
        fill = TRUE
      )
      xlsxWriteData(wb = wb, sheetName = "Demographics", dt = dtPops)

      openxlsx::saveWorkbook(wb, projectConfiguration$populationsFile, overwrite = TRUE)
    },
    #' @description
    #' This function sets up scenarios based on the defined populations and adds PK Parameters
    #' and output paths to the project configuration.
    #'
    #' @param projectConfiguration A ProjectConfiguration object containing project configuration details.
    #' @param dtTestScenarios `data.table` with content for scenarios
    #' @param pKParameter vector with pkParameter sheets
    #' @return NULL
    mockManualEditingsScenario = function(projectConfiguration,
                                          dtTestScenarios,
                                          pKParameter = NULL) {
      wb <- openxlsx::loadWorkbook(projectConfiguration$scenariosFile)
      # set scenarios
      dtScenario <- rbind(xlsxReadData(wb = wb, sheetName = "Scenarios"),
        dtTestScenarios[, !c("shortName", "longName"), with = FALSE],
        fill = TRUE
      )

      xlsxWriteData(
        wb = wb, sheetName = "Scenarios",
        dt = dtScenario
      )

      # add PK Parameter sheets
      if (!is.null(pKParameter)) {
        dtPK <- rbind(
          xlsxReadData(wb = wb, sheetName = "PKParameter"),
          data.table(
            scenario_name = dtScenario$scenario_name,
            pKParameter = pKParameter
          )
        )
        xlsxWriteData(wb = wb, sheetName = "PKParameter", dt = dtPK)
      }

      # OutputPaths
      dtOutputs <- rbind(
        data.table(
          outputPathId = "Plasma",
          outputPath = "Organism|PeripheralVenousBlood|DrugX|Plasma (Peripheral Venous Blood)"
        ),
        data.table(
          outputPathId = "CYP3A4total",
          outputPath = "Organism|DrugX-CYP3A4-Optimized Metabolite|Total fraction of dose-DrugX"
        ),
        data.table(
          outputPathId = "CYP3A4Liver",
          outputPath = "Organism|Liver|Periportal|Intracellular|DrugX-CYP3A4-Optimized Metabolite|Fraction of dose-DrugX"
        ),
        fill = TRUE
      )

      xlsxWriteData(wb = wb, sheetName = "OutputPaths", dt = dtOutputs)

      # save all sheets
      openxlsx::saveWorkbook(wb, projectConfiguration$scenariosFile, overwrite = TRUE)

      synchronizeScenariosOutputsWithPlots(projectConfiguration) # Sync outputs with plots
      synchronizeScenariosWithPlots(projectConfiguration) # Sync scenarios with plots

      private$adjustdisplayNames(
        projectConfiguration = projectConfiguration,
        dtTestScenarios = dtTestScenarios
      )
    },
    #' @description
    #' This function adds PK parameters to the project configuration based on templates.
    #'
    #' @param projectConfiguration A ProjectConfiguration object containing project configuration details.
    #' @return NULL
    mockManualEditingsPKParameter = function(projectConfiguration) {
      # initialize variable to avoid messages
      displayName <- name <- outputPathIds <- NULL

      # add all data files which are used in the project
      wb <- openxlsx::loadWorkbook(projectConfiguration$addOns$pKParameterFile)

      dtTemplate <- xlsxReadData(wb = wb, sheetName = "Template")

      dtTemplate <- dtTemplate[c(1, which(dtTemplate$name %in% c("C_max", "AUC_inf")))]
      dtTemplate[name %in% c("C_max", "AUC_inf"), outputPathIds := "Plasma"]

      xlsxCloneAndSet(wb = wb, clonedSheet = "Template", sheetName = "PK_Plasma", dt = dtTemplate)


      dtTemplate <- xlsxReadData(wb = wb, sheetName = "Template")

      dtTemplate <- dtTemplate[c(1, which(dtTemplate$name %in% c("F_tEnd")))]
      dtTemplate[name %in% c("F_tEnd"), outputPathIds := c("CYP3A4total, CYP3A4Liver")]
      dtTemplate[name %in% c("F_tEnd"), displayName := "fraction metabolized"]

      xlsxCloneAndSet(wb = wb, clonedSheet = "Template", sheetName = "PK_Fraction", dt = dtTemplate)

      openxlsx::saveWorkbook(wb, projectConfiguration$addOns$pKParameterFile, overwrite = TRUE)
    },
    #' @description
    #' This function adjust the bio-metrics with in individuals and exports virtual twin populations.
    #'
    #' @param projectConfiguration A `ProjectConfiguration` object containing project configuration details.
    #' @return `data.table` with content of sheet `VirtualTwinPopulation`
    mockManualEditingsIndividuals = function(projectConfiguration) {
      # initialize variable to avoid messages
      scenario_name <- populationName <- NULL # nolint

      # add all data files which are used in the project
      wb <- openxlsx::loadWorkbook(projectConfiguration$individualsFile)

      dt <- xlsxReadData(
        wb = wb, sheetName = "IndividualBiometrics",
        skipDescriptionRow = FALSE, emptyAsNA = FALSE
      )
      dt[, `:=`(
        protein = "CYP3A4,UGT1A4",
        ontogeny = "CYP3A4,UGT1A4"
      )]

      individualIds <- dt$individualId

      xlsxWriteData(wb = wb, sheetName = "IndividualBiometrics", dt = dt)

      dt <- xlsxReadData(
        wb = wb, sheetName = "VirtualTwinPopulation",
        skipDescriptionRow = FALSE,
        emptyAsNA = FALSE
      )
      dt[, populationName := unique(gsub("_po$", "", gsub("_iv$", "", splitInputs(dataGroups))))]

      populationId <- unique(dt$populationName)

      xlsxWriteData(wb = wb, sheetName = "VirtualTwinPopulation", dt = dt)

      openxlsx::saveWorkbook(wb, projectConfiguration$individualsFile, overwrite = TRUE)

      return(dt)
    },
    #' @description
    #' This function modifies specific parameters in a given Excel sheet based on the provided
    #' parameter list and saves the changes back to the workbook.
    #'
    #' @param projectConfiguration A list containing project configuration details, including
    #'                             the path to the sensitivity file.
    #' @param sheetName A string representing the name of the sheet in the Excel workbook
    #'                  that will be edited.
    #' @param parameterList A named list where each name corresponds to a parameter in the
    #'                      Excel sheet, and each value is the new value to set for that
    #'                      parameter.
    #'
    #' @return NULL This function is called for its side effects (i.e., modifying the Excel file).
    mockManualEditingsSensitivity = function(projectConfiguration, sheetName, parameterList) {
      # initialize variable to avoid messages
      parameter <- NULL

      wb <- openxlsx::loadWorkbook(projectConfiguration$addOns$sensitivityFile)

      dt <- xlsxReadData(wb = wb, sheetName = sheetName, skipDescriptionRow = FALSE)

      selectedRows <- 1 # headerline
      for (par in names(parameterList)) {
        iRow <- grep(par, dt$parameter)[1] # is ambiguous take first
        dt$parameter[iRow] <- parameterList[[par]]
        selectedRows <- c(selectedRows, iRow)
      }
      dt <- dt[selectedRows]

      xlsxWriteData(wb = wb, sheetName = sheetName, dt = dt)

      openxlsx::saveWorkbook(wb, projectConfiguration$addOns$sensitivityFile, overwrite = TRUE)
    }
  ),
  # active ----
  active = list(
    #' @field rootDirectory root directory of test project.
    rootDirectory = function(value) {
      if (missing(value)) {
        value <- private$.rootDirectory
      } else {
        checkmate::assertDirectoryExists(value)
      }
      private$.rootDirectory <- value
    }
  ),
  # private ----
  private = list(
    .rootDirectory = "",
    packageDirFramework = "",
    writeTestData = FALSE,
    #' This function synchronizes files from one directory to another, copying only the files that are not already present.
    synchronizeDirectories = function(fromDir, toDir, pattern = NULL, filePrefixes = NULL) {
      if (!dir.exists(toDir)) dir.create(toDir)

      # Get the list of files in the source directory
      allFiles <- list.files(path = fromDir, pattern = pattern)

      # Filter files based on startStrings if it's not NULL
      if (!is.null(filePrefixes)) {
        allFiles <- allFiles[grepl(paste0("^(", paste(filePrefixes, collapse = "|"), ")"), allFiles)]
      }

      filesToCopy <- setdiff(
        allFiles,
        list.files(path = toDir, pattern = pattern)
      )

      invisible(lapply(
        filesToCopy,
        function(f) {
          file.copy(
            from = file.path(fromDir, f),
            to = file.path(toDir, f)
          )
        }
      ))

      return(invisible())
    },
    #' This function updates the display names for scenarios and output paths in the project configuration.
    adjustdisplayNames = function(projectConfiguration, dtTestScenarios) {
      # initialize variable to avoid messages
      outputPathId <- scenario <- longName <- shortName <- NULL

      wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

      # scenarios
      dtScenario <- xlsxReadData(wb = wb, sheetName = "Scenarios", skipDescriptionRow = FALSE)

      dtScenario <- dtScenario %>%
        merge(dtTestScenarios[, c("scenario_name", "shortName", "longName")],
          by.x = "scenario",
          by.y = "scenario_name",
          sort = FALSE,
          all.x = TRUE,
          suffixes = c("", ".new")
        )
      dtScenario[, shortName := as.character(shortName)]
      dtScenario[!is.na(shortName.new), shortName := shortName.new]
      dtScenario[, longName := as.character(longName)]
      dtScenario[!is.na(longName.new), longName := longName.new]

      xlsxWriteData(
        wb = wb,
        sheetName = "Scenarios",
        dt = dtScenario[, !c("shortName.new", "longName.new"), with = FALSE]
      )

      # outputpathids
      dtOutputs <- xlsxReadData(wb = wb, sheetName = "Outputs")

      dtOutputs[outputPathId == "Plasma"]$displayName <- "drugX plasma concentration"
      dtOutputs[outputPathId == "Plasma"]$displayUnit <- "\u00B5g/L"
      dtOutputs[outputPathId == "CYP3A4total"]$displayName <- "drugX metabolized by CYP3A4"
      dtOutputs[outputPathId == "CYP3A4Liver"]$displayName <- "drugX metabolized by CYP3A4 in liver"

      xlsxWriteData(wb = wb, sheetName = "Outputs", dt = dtOutputs)

      # save all sheets
      openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)
    },
    # generate randomIndividualData
    getRandomIndividualData = function(projectConfiguration, scenarioResult, timePoints, outputPathIds, ids) {
      allResults <- setDT(ospsuite::simulationResultsToDataFrame(scenarioResult$results))
      randomIndividuals <- allResults[IndividualId %in% ids & Time %in% timePoints]
      randomIndividuals[, Time := Time / 60]

      outputPaths <- getOutputPathIds(wbPlots = projectConfiguration$plotsFile)
      randomIndividuals <- merge(randomIndividuals, outputPaths, by.x = "paths", by.y = "outputPath")
      randomIndividuals <- randomIndividuals[outputPathId %in% outputPathIds]

      return(randomIndividuals)
    },
    # add error to data
    calculateShiftedValues = function(randomIndividuals, sdShift, lloqValue) {
      setorderv(randomIndividuals, c("outputPathId", "IndividualId", "Time"))
      randomIndividuals[, dtV := simulationValues - shift(simulationValues, fill = 0),
        by = c("outputPathId", "IndividualId")
      ]
      set.seed(123) # Set seed for reproducibility
      randomIndividuals[, shiftedValue := ifelse(dimension == "Fraction",
        cumsum(dtV),
        simulationValues
      ) * stats::rnorm(.N, mean = 1, sd = sdShift),
      by = c("outputPathId", "IndividualId")
      ]
      randomIndividuals[dimension == "Fraction", shiftedValue := min(shiftedValue, 1), by = .I]

      for (og in split(randomIndividuals, by = "outputPathId")) {
        unitfactor <- ospsuite::toUnit(
          quantityOrDimension = og$dimension[1],
          values = 1,
          targetUnit = og$displayUnit[1],
          sourceUnit = og$unit[1],
          molWeight = og$molWeight[1],
          molWeightUnit = "g/mol"
        )
        randomIndividuals[outputPathId == og$outputPathId[1], shiftedValue := shiftedValue * unitfactor]
      }

      randomIndividuals[, LLOQ := ifelse(outputPathId == "Plasma", lloqValue, NA)]
      randomIndividuals[, shiftedValue := ifelse(!is.na(LLOQ) & shiftedValue < LLOQ, LLOQ / 2, shiftedValue)]

      return(randomIndividuals[, c("IndividualId", "outputPathId", "Time", "shiftedValue", "displayUnit", "LLOQ")])
    },
    # select the bio metric data from the population
    addBiometricPopulationData = function(randomIndividuals, scenarioResult) {
      dtPop <- setDT(ospsuite::populationToDataFrame(scenarioResult$population))
      dtPop <- dtPop[, c("IndividualId", "Gender", "Organism|Weight", "Organism|Age", "Organism|Height")]

      names(dtPop) <- gsub("Organism\\|", "", names(dtPop))
      dtPop[, `:=`(Weight = round(Weight, 1), Age = floor(Age), Height = round(Height * 10))]

      randomIndividuals <- merge(randomIndividuals, dtPop, by = "IndividualId")
      return(randomIndividuals)
    },
    addDataImporterConfigurationFilesTP = function(projectConfiguration, scenarioNames) {
      # Add all data files which are used in the project
      wb <- openxlsx::loadWorkbook(projectConfiguration$dataImporterConfigurationFile)
      dtDataFiles <- xlsxReadData(wb = wb, sheetName = "DataFiles")

      dtDataFiles <- rbind(
        dtDataFiles,
        data.table(
          fileIdentifier = scenarioNames,
          dataFile = file.path(
            "..", "..", "Data",
            paste0("timeprofiles_", scenarioNames, ".csv")
          ),
          dictionary = "tpDictionary",
          dataFilter = "",
          dataClass = DATACLASS$tpIndividual
        )
      )

      xlsxWriteData(wb = wb, sheetName = "DataFiles", dt = dtDataFiles)

      # tp dictionary
      tpDictionary <- xlsxReadData(wb = wb, sheetName = "tpDictionary", skipDescriptionRow = FALSE)
      tpDictionaryH <- tpDictionary[1]
      tpDictionary <- tpDictionary[targetColumn %in% c(
        "studyId", "subjectId", "individualId",
        "group", "outputPathId", "xValues", "yValues", "yUnit", "lloq",
        "age", "weight", "height", "gender", "population", "species", "route"
      )]
      tpDictionary <- tpDictionary[!duplicated(tpDictionary$targetColumn)]

      tpDictionary[targetColumn == "group"]$filterValue <- "paste(STUD,'adults',route,sep = '_')"
      tpDictionary[targetColumn == "outputPathId"]$sourceColumn <- "outputPathId"
      tpDictionary[targetColumn == "population"]$filter <- "TRUE"
      tpDictionary[targetColumn == "route"]$sourceColumn <- "route"

      tpDictionary[!is.na(sourceColumn), `:=`(filter = NA, filterValue = NA)]

      xlsxWriteData(wb = wb, sheetName = "tpDictionary", dt = rbind(tpDictionaryH, tpDictionary))

      openxlsx::saveWorkbook(wb, projectConfiguration$dataImporterConfigurationFile, overwrite = TRUE)

      return(invisible())
    },
    getRandomIndividualPKdata = function(pkParameterDT, ids, outputPathIds, sdShift) {
      randomIndividuals <- pkParameterDT[individualId %in% ids &
        outputPathId %in% outputPathIds]

      randomIndividuals[, shiftedValue := value * stats::rnorm(.N, mean = 1, sd = sdShift)]
      randomIndividuals[, population := gsub("_iv", "", (gsub("_po", "", scenario))), by = .I]
      randomIndividuals[, route := gsub("_", "", (gsub(population, "", scenario))), by = .I]

      return(randomIndividuals)
    },
    # aggregate individual PK-data
    aggregatePKData = function(randomIndividuals) {
      dt <- randomIndividuals[, .(
        N = .N,
        geomean = exp(mean(log(shiftedValue))),
        geosd = exp(stats::sd(log(shiftedValue))),
        median = stats::median(shiftedValue),
        percentile_5 = stats::quantile(shiftedValue, 0.05),
        percentile_95 = stats::quantile(shiftedValue, 0.95)
      ), by = .(population, route, pkParameter, outputPathId, displayUnitPKParameter)]

      dt[, STUD := 1234]
      setnames(dt, "displayUnitPKParameter", "unit")

      setcolorder(dt, "STUD")

      return(dt)
    },
    # calculates individual PK-ratios and returns aggregates
    aggregatePKDataRatios = function(randomIndividuals) {
      ratiosDT <- dcast(randomIndividuals,
        individualId + pkParameter + outputPathId + population ~ route,
        value.var = "shiftedValue"
      )
      ratiosDT[, ratio := po / iv]

      geomeanCI <- function(x, confLevel = 0.9) {
        n <- length(x)
        if (n == 0) {
          return(c(NA, NA))
        }
        gm <- exp(mean(log(x), na.rm = TRUE))
        se <- stats::sd(log(x), na.rm = TRUE) / sqrt(n)
        cimultiplier <- stats::qnorm((1 + confLevel) / 2)
        cilower <- exp(log(gm) - cimultiplier * se)
        ciupper <- exp(log(gm) + cimultiplier * se)
        return(c(gm, cilower, ciupper))
      }

      dt <- ratiosDT[, .(
        N = .N,
        geomean = geomeanCI(ratio)[1],
        CI_lower = geomeanCI(ratio)[2],
        CI_upper = geomeanCI(ratio)[3]
      ), by = c("pkParameter", "outputPathId", "population")]

      dt[, STUD := 1234]
      dt[, unit := ""]

      setcolorder(dt, "STUD")

      return(dt)
    },
    # sets data importer configuration
    addDataImporterConfigurationFilesPK = function(projectConfiguration) {
      # Add all data files which are used in the project
      wb <- openxlsx::loadWorkbook(projectConfiguration$dataImporterConfigurationFile)
      dtDataFiles <- xlsxReadData(wb = wb, sheetName = "DataFiles")

      dtDataFiles <- rbind(
        dtDataFiles,
        data.table(
          fileIdentifier = c("PK_abs", "PK_ratio"),
          dataFile = file.path(
            "..", "..", "Data",
            c(
              "PK_absolute_values.csv",
              "PK_ratios.csv"
            )
          ),
          dictionary = c(
            "pkDictionary_absValues",
            "pkDictionary_ratios"
          ),
          dataFilter = "",
          dataClass = DATACLASS$pkAggregated
        )
      )

      xlsxWriteData(wb = wb, sheetName = "DataFiles", dt = dtDataFiles)

      # - pkDictionary
      for (suffix in c("absValues", "ratios")) {
        pkDictionary <- xlsxReadData(wb = wb, sheetName = "pkDictionary", skipDescriptionRow = FALSE)

        pkDictionaryH <- pkDictionary[1]

        switch(suffix,
          "absValues" = {
            pkDictionary <- pkDictionary[targetColumn %in% c(
              "studyId", "group", "outputPathId", "pkParameter",
              "values", "unit", "errorType", "errorValues", "numberOfIndividuals"
            )]
            pkDictionary[targetColumn == "group"]$filterValue <- "paste(STUD,population,route,sep = '_')"
            pkDictionary[targetColumn == "errorValues"]$sourceColumn <- "geosd"
          },
          "ratios" = {
            pkDictionary <- pkDictionary[targetColumn %in% c(
              "studyId", "group", "outputPathId", "pkParameter",
              "values", "unit", "errorType", "minValue", "maxValue", "numberOfIndividuals"
            )]
            pkDictionary[targetColumn == "group"]$filterValue <- "paste(STUD,population,'ratio',sep = '_')"
            pkDictionary[targetColumn == "minValue"]$sourceColumn <- "CI_lower"
            pkDictionary[targetColumn == "maxValue"]$sourceColumn <- "CI_upper"
            pkDictionary[targetColumn == "errorType"]$filterValue <- '"geometric mean|90% CI lower|90% CI upper"'
          }
        )
        pkDictionary[targetColumn == "outputPathId"]$sourceColumn <- "outputPathId"
        pkDictionary[targetColumn == "pkParameter"]$sourceColumn <- "pkParameter"
        pkDictionary[targetColumn == "values"]$sourceColumn <- "geomean"
        pkDictionary[targetColumn == "unit"]$sourceColumn <- "unit"
        pkDictionary[targetColumn == "numberOfIndividuals"]$sourceColumn <- "N"
        pkDictionary[!is.na(sourceColumn), `:=`(
          filter = NA,
          filterValue = NA
        )]

        xlsxCloneAndSet(
          wb = wb,
          clonedSheet = "pkDictionary",
          sheetName = paste0("pkDictionary_", suffix),
          dt = rbind(pkDictionaryH, pkDictionary)
        )
      }

      openxlsx::saveWorkbook(wb, projectConfiguration$dataImporterConfigurationFile, overwrite = TRUE)

      return(invisible())
    }
  )
)
