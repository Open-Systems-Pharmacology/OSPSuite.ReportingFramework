#' Prepare Test Project
#'
#' This function sets up the environment for running the test project of the
#' Reporting Framework.
#' It creates a directory structure if it does not exist, and sources
#' the necessary R scripts to execute the workflow.
#'
#' @param rootDirectory A character string specifying the root directory for the test project.
#'
#' @return A list containing:
#'   - `projectConfiguration`: The configuration of the initialized project.
#'   - `scenarioList`: A list of scenarios initialized for the project.
#'   - `scenarioResults`: The results from running the scenarios.
#'
#' @examples
#' \dontrun{
#' prepareTestProject("path/to/my/testDirectory")
#' }
#' @export
prepareTestProject <- function(rootDirectory) {

  checkmate::assertDirectory(rootDirectory)

  buildTestData(rootDirectory, verbose = FALSE, writeTestData = FALSE)
}
#' Build Test Project Directory
#'
#' This function sets up a test project directory for the `ospsuite.reportingframework` package.
#' It initializes the project structure, configures scenarios, and handles necessary file
#' synchronization for testing and development purposes. It can be used in both development
#' mode and testing environments.
#'
#' @param rootDirectory A character string specifying the root directory for the test project.
#'                     If NULL (default), a temporary directory will be created.
#' @param verbose A logical value indicating whether to print messages about the process.
#'                Defaults to FALSE.
#' @param writeTestData A logical value indicating whether newly created testdata should be filed to the inst directory of the package.
#'
#' @return A list containing:
#'   - `projectConfiguration`: The configuration of the initialized project.
#'   - `scenarioList`: A list of scenarios initialized for the project.
#'   - `scenarioResults`: The results from running the scenarios.
#'
#' @details
#' The function performs the following steps:
#' 1. Initializes the project structure.
#' 2. Cleans up existing data and sets up mock manual edits on project configuration.
#' 3. Synchronizes directories for models and populations.
#' 4. Exports defined populations and runs simulations for scenarios.
#' 5. In development mode, it synchronizes simulation results and PK analysis results back
#'    to the development directory.
#'
#' @examples
#' # Example usage in package development
#' if (writeTestData) {
#'   testProject <- buildTestProjectory(verbose = TRUE, writeTestData = TRUE)
#' }
#'
#' # Example usage in setup.R for tests
#' testSetup <- buildTestProjectory(rootDirectory = "path/to/testdirectory", verbose = TRUE)
#' projectdirectory <- testSetup$projectConfiguration
#' scenarioList <- testSetup$scenarioList
#'
#' # Example usage in vignettes
#' \dontrun{
#'   vignetteSetup <- buildTestProjectory(verbose = TRUE)
#'   # Use vignetteSetup$projectConfiguration and vignetteSetup$scenarioList as needed
#' }
#'
#' @keywords internal
buildTestData  <- function(rootDirectory = NULL,
                           verbose = FALSE,
                           writeTestData){

  instDirectory <- system.file(
    package = "ospsuite.reportingframework",
    "extdata",
    mustWork = TRUE)

  modelFiles <- list.files(file.path(instDirectory,'Models'))
  names(modelFiles) <- substr(modelFiles,1,2)

  projectConfiguration <- iniTestProject(rootDirectory = rootDirectory,
                                         verbose = verbose,
                                         modelFiles = modelFiles,
                                         instDirectory = instDirectory)

  updateConfigTablesForTestProject(projectConfiguration,modelFiles)

  setupPopulationsForTest(projectConfiguration = projectConfiguration,
                                    instDirectory = instDirectory,
                                    writeTestData = writeTestData)

  # Initialize all scenarios previously defined in scenario.xlsx
  scenarioList <- createScenarios.wrapped(projectConfiguration = projectConfiguration,
                                          scenarioNames = NULL,
                                          doCheckScenarioNameValidity = TRUE)

  # Run scenarios and calculate PK
  scenarioResults <- setupSimulationsForTest(projectConfiguration = projectConfiguration,
                          scenarioList = scenarioList,
                          instDirectory = instDirectory,
                          writeTestData = writeTestData)

  # generate Random data
  setupRandomDataForTest(projectConfiguration = projectConfiguration,
                         scenarioList = scenarioList,
                         scenarioResults = scenarioResults)

  # generate virtual twin poulations
  scenarioListInd <-
    setupVirtualTwinScenariosForTest(projectConfiguration = projectConfiguration,
                                     modelFiles = modelFiles)


  # Run scenarios and calculate PK
  scenarioResultsInd <- setupSimulationsForTest(projectConfiguration = projectConfiguration,
                                             scenarioList = scenarioListInd,
                                             instDirectory = instDirectory,
                                             writeTestData = writeTestData)


  setUpSensitivity(projectConfiguration = projectConfiguration,
                   scenarioListInd = scenarioListInd,
                   instDirectory = instDirectory,
                   writeTestData = writeTestData)

  return(invisible(list(projectConfiguration = projectConfiguration,
              scenarioList = scenarioList,
              scenarioResults = scenarioResults,
              scenarioListInd = scenarioListInd,
              scenarioResultsInd = scenarioResultsInd)))
}

## auxiliaries --------


iniTestProject <- function(rootDirectory,
                           verbose,
                           modelFiles,
                           instDirectory = system.file(
                             package = "ospsuite.reportingframework",
                             "extdata",
                             mustWork = TRUE)
){

  if (is.null(rootDirectory)){
    rootDirectory <- file.path(tempdir(),'testProject')
    if (verbose) message(rootDirectory)
  }
  if (!dir.exists(file.path(rootDirectory, "Scripts", "ReportingFramework"))) {
    dir.create(file.path(rootDirectory, "Scripts", "ReportingFramework"), recursive = TRUE)
  }

  setWorkflowOptions(isValidRun = FALSE)

  # Setup project
  initProject(configurationDirectory = file.path(rootDirectory, "Scripts", "ReportingFramework"))  # Initialize the project structure

  # Get paths of all relevant project files
  projectConfiguration <- ospsuite.reportingframework::createProjectConfiguration(
    path = file.path(file.path(rootDirectory, "Scripts", "ReportingFramework","ProjectConfiguration.xlsx"))
  )

  initLogfunction(projectConfiguration = projectConfiguration,verbose = verbose)

  synchronizeDirectories(fromDir = file.path(instDirectory,'Models'),
                         toDir = file.path(projectConfiguration$modelFolder))

  return(projectConfiguration)
}
#' Update Tables for Test Projects
#'
#' This function updates the configuration tables for test projects in the
#' Reporting Framework. It performs necessary modifications to the project
#' configuration, including clearing existing data, setting up scenarios,
#' and adding pharmacokinetic (PK) parameters.
#'
#' The function is designed to ensure that the project configuration is
#' correctly set up for running simulations and analyses within the
#' `ospsuite.reportingframework` package.
#'
#' @param projectConfiguration A ProjectConfiguration object containing the
#'   details of the project configuration that needs to be updated.
#' @param modelFiles A named character vector containing paths to the model
#'   files used in the project.
#'
#' @return invisible(NULL)
#' @keywords internal
updateConfigTablesForTestProject <- function(projectConfiguration,modelFiles){
  # Perform mock manual editings on the project configuration tables
  # Clear existing data from relevant Excel sheets
  mockManualEditingsCleanup(projectConfiguration)
  # Define virtual populations within biometric ranges
  mockManualEditingsPopulation(projectConfiguration)
  # Set up scenarios based on the defined populations, add PK Parameter and output paths
  mockManualEditingsScenario(projectConfiguration,modelFiles = modelFiles)
  ospsuite.reportingframework:::synchronizeScenariosOutputsWithPlots(projectConfiguration)  # Sync outputs with plots
  ospsuite.reportingframework:::synchronizeScenariosWithPlots(projectConfiguration)  # Sync scenarios with plots
  # Add PK parameters to the project configuration
  mockManualEditingsPKParameter(projectConfiguration)
  # prepare displaynames for plotting
  mockManualEditingsdisplayNames(projectConfiguration)

  return(invisible())
}

setupPopulationsForTest <- function(projectConfiguration,instDirectory,writeTestData){

  # Copy files needed for example to the correct folders
  # Recreate results
  synchronizeDirectories(fromDir = file.path(instDirectory,'Populations'),
                         toDir = file.path(projectConfiguration$populationsFolder))


  # Exports all populations defined in the population.xlsx sheet "Demographics"
  suppressMessages(exportRandomPopulations(projectConfiguration = projectConfiguration,
                                           populationNames = NULL,
                                           overwrite = FALSE))

  if (writeTestData){
    if (!dir.exists(file.path(instDirectory,'Populations')))
      dir.create(file.path(instDirectory,'Populations'))

    synchronizeDirectories(fromDir = file.path(projectConfiguration$populationsFolder),
                           toDir = file.path(instDirectory,'Populations'))

  }

}
setupSimulationsForTest <- function(projectConfiguration,scenarioList,instDirectory,writeTestData){

  synchronizeDirectories(fromDir = file.path(instDirectory,EXPORTDIR$simulationResult),
                         toDir = file.path(projectConfiguration$outputFolder,EXPORTDIR$simulationResult))

  synchronizeDirectories(fromDir = file.path(instDirectory,EXPORTDIR$pKAnalysisResults),
                         toDir = file.path(projectConfiguration$outputFolder,EXPORTDIR$pKAnalysisResults))

  # Run initialized scenarios and Calculate PK Parameters
  scenarioResults <- runAndSaveScenarios(projectConfiguration = projectConfiguration,
                                         scenarioList = scenarioList,
                                         simulationRunOptions = SimulationRunOptions$new(
                                           showProgress = TRUE
                                         ),
                                         withResimulation = FALSE)



  if (writeTestData){
    if (!dir.exists(file.path(instDirectory,EXPORTDIR$simulationResult)))
      dir.create(file.path(instDirectory,EXPORTDIR$simulationResult))

    synchronizeDirectories(fromDir = file.path(projectConfiguration$outputFolder,EXPORTDIR$simulationResult),
                           toDir = file.path(instDirectory,EXPORTDIR$simulationResult))

    if (!dir.exists(file.path(instDirectory,EXPORTDIR$pKAnalysisResults)))
      dir.create(file.path(instDirectory,EXPORTDIR$pKAnalysisResults))

    synchronizeDirectories(fromDir = file.path(projectConfiguration$outputFolder,EXPORTDIR$pKAnalysisResults),
                           toDir = file.path(instDirectory,EXPORTDIR$pKAnalysisResults),
                           pattern = '.csv')
  }

  return(scenarioResults)
}


setupRandomDataForTest <- function(projectConfiguration,scenarioList,scenarioResults){
  pkParameterDT <- loadPKParameter(projectConfiguration = projectConfiguration,
                                   scenarioList = scenarioList)

  ids <- addRandomPKDataToTestProject(projectConfiguration,pkParameterDT)

  addRandomTPDataToTestProject(projectConfiguration = projectConfiguration,
                               scenarioResult = scenarioResults[['adults_iv']],
                               csvfile = 'timeprofiles_adults_iv.csv',
                               ids = ids)
  addRandomTPDataToTestProject(projectConfiguration = projectConfiguration,
                               scenarioResult = scenarioResults[['adults_po']],
                               csvfile = 'timeprofiles_adults_po.csv',
                               ids)
  mockManualEditingsDataDictionary(projectConfiguration)

  return(invisible())
}
setupVirtualTwinScenariosForTest <- function(projectConfiguration,modelFiles){

  dataObserved <- readObservedDataByDictionary(projectConfiguration)

  newScenarios <- mockManualEditingsIndividuals(projectConfiguration,modelFiles)

  exportVirtualTwinPopulations(projectConfiguration = projectConfiguration,
                               modelFile = modelFiles["po"],
                               overwrite = TRUE)

  scenarioListInd <- createScenarios.wrapped(projectConfiguration = projectConfiguration,
                                          scenarioNames = newScenarios,
                                          doCheckScenarioNameValidity = TRUE)

  return(scenarioListInd)
}

setUpSensitivity <- function(projectConfiguration,scenarioListInd,instDirectory,writeTestData){

  sensitivityScenario = names(scenarioListInd)[2]
  sensitivitySheet = 'smallSelection'

  projectConfiguration = addSensitivityTable(projectConfiguration,
                                             scenarioList = scenarioListInd,
                                             scenarioName =  sensitivityScenario,
                                             sheetName = sensitivitySheet)

  mockManualEditingsSensitivity(projectConfiguration,sheetName= sensitivitySheet)

  synchronizeDirectories(fromDir = file.path(instDirectory,EXPORTDIR$sensitivityResults),
                         toDir = file.path(projectConfiguration$outputFolder,EXPORTDIR$sensitivityResults))


  runSensitivityAnalysisForScenarios(projectConfiguration = projectConfiguration,
                                     scenarioList = scenarioListInd,
                                     scenarioNames = sensitivityScenario,
                                     sensitivitysheet = sensitivitySheet,
                                     overwrite = FALSE)

  if (writeTestData){
    if (!dir.exists(file.path(instDirectory,EXPORTDIR$sensitivityResults)))
      dir.create(file.path(instDirectory,EXPORTDIR$sensitivityResults))

    synchronizeDirectories(fromDir = file.path(projectConfiguration$outputFolder,EXPORTDIR$sensitivityResults),
                           toDir = file.path(instDirectory,EXPORTDIR$sensitivityResults))
  }


  return(projectConfiguration)
}

synchronizeDirectories <- function(fromDir,toDir,pattern = NULL){

  if (!dir.exists(toDir)) dir.create(toDir)

  filesToCopy = setdiff(
    list.files(path = fromDir,pattern = pattern),
    list.files(path = toDir,pattern = pattern))

  invisible(lapply(filesToCopy,
                   function(f){
                     file.copy(from = file.path(fromDir,f),
                               to = file.path(toDir,f))
                   }))

  return(invisible())
}

## random data ---------
#' Add Random PK Data to Test Project
#'
#' This function adds random pharmacokinetic (PK) data to the test project,
#' including shifting values and calculating statistics for selected individuals.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project configuration details.
#' @param pkParameterDT A data.table containing PK parameters and values.
#' @return Invisible NULL.
#' @keywords internal
addRandomPKDataToTestProject <- function(projectConfiguration,pkParameterDT){

  dataFolder <- file.path(projectConfiguration$configurationsFolder,'..','..','Data')
  if (!dir.exists(dataFolder)) dir.create(dataFolder)

  set.seed(123)  # Set seed for reproducibility
  ids <- sample(unique(pkParameterDT$individualId),6,replace = FALSE)
  randomIndividuals <- pkParameterDT[individualId %in% ids]

  randomIndividuals[, shiftedValue := value * rnorm(.N, mean = 1, sd = 0.05)]
  randomIndividuals[,population := gsub('_iv','',(gsub('_po','',scenario))),by =.I]
  randomIndividuals[,route := gsub('_','',(gsub(population,'',scenario))), by = .I]

  dt <- randomIndividuals[, .(
    N = .N,
    geomean = exp(mean(log(shiftedValue))),
    geosd = exp(sd(log(shiftedValue))),
    median = median(shiftedValue),
    percentile_5 = quantile(shiftedValue, 0.05),
    percentile_95 = quantile(shiftedValue, 0.95)
  ), by = .(population,route, pkParameter, outputPathId,displayUnitPKParameter)]

  dt[ , STUD  := 1234]
  setnames(dt,'displayUnitPKParameter','unit')

  setcolorder(dt,'STUD')

  fwrite(dt,file.path(dataFolder,'PK_absolute_values.csv'))


  # ratios
  ratiosDT <- dcast(randomIndividuals, individualId + pkParameter + outputPathId + population ~ route,
                    value.var = "shiftedValue")
  ratiosDT[, ratio := po / iv]

  geomeanCI <- function(x, conf.level = 0.9) {
    n <- length(x)
    if (n == 0) return(c(NA, NA))
    gm <- exp(mean(log(x), na.rm = TRUE))
    se <- sd(log(x), na.rm = TRUE) / sqrt(n)
    cimultiplier <- qnorm((1 + conf.level) / 2)
    cilower <- exp(log(gm) - cimultiplier * se)
    ciupper <- exp(log(gm) + cimultiplier * se)
    return(c(gm, cilower, ciupper))
  }

  dt <- ratiosDT[, .(
    N = .N,
    geomean = geomeanCI(ratio)[1],
    CI_lower = geomeanCI(ratio)[2],
    CI_upper = geomeanCI(ratio)[3]
  ), by = c('pkParameter','outputPathId','population')]

  dt[ , STUD  := 1234]
  dt[ , unit  := '']

  setcolorder(dt,'STUD')

  fwrite(dt,file.path(dataFolder,'PK_po_iv_ratios.csv'))

  return(ids)

}

addRandomTPDataToTestProject <- function(projectConfiguration,scenarioResult,csvfile,ids){

  dataFolder <- file.path(projectConfiguration$configurationsFolder,'..','..','Data')
  if (!dir.exists(dataFolder)) dir.create(dataFolder)

  allResults <- setDT(ospsuite::simulationResultsToDataFrame(scenarioResult$results))

  randomIndividuals <- allResults[IndividualId %in% ids]
  randomIndividuals <- randomIndividuals[Time %in% c(15,30,45,60,90,c(3,6,9,12,18,24)*60)]
  randomIndividuals[,Time := Time/60]

  outputPaths <- getOutputPathIds(wbPlots = projectConfiguration$plotsFile)
  randomIndividuals <- merge(randomIndividuals,
                             outputPaths,
                             by.x = 'paths',
                             by.y = 'outputPath')

  setorderv(randomIndividuals,c('outputPathId','IndividualId','Time'))
  randomIndividuals[,dtV := simulationValues - shift(simulationValues,fill = 0),by = c('outputPathId','IndividualId')]
  set.seed(123)  # Set seed for reproducibility
  randomIndividuals[, shiftedValue := ifelse(dimension == "Fraction",
                                             cumsum(dtV),
                                             simulationValues) * rnorm(.N, mean = 1, sd = 0.05),
                    by = c('outputPathId','IndividualId')]
  randomIndividuals[dimension == 'Fraction' ,shiftedValue := min(shiftedValue,1),by = .I]
  for (og in split(randomIndividuals, by = 'outputPathId')){
    unitfactor = ospsuite::toUnit(quantityOrDimension = og$dimension[1],
                                  values = 1,
                                  targetUnit = og$displayUnit[1],
                                  sourceUnit = og$unit[1],
                                  molWeight = og$molWeight[1],
                                  molWeightUnit = "g/mol")
    randomIndividuals[outputPathId == og$outputPathId[1], shiftedValue := shiftedValue*unitfactor]

  }

  randomIndividuals <- randomIndividuals[,c('IndividualId','outputPathId','Time','shiftedValue','displayUnit')]

  randomIndividuals[,LLOQ := ifelse(outputPathId == 'Plasma',0.1,NA)]
  randomIndividuals[,shiftedValue := ifelse(!is.na(LLOQ) & shiftedValue < LLOQ,LLOQ/2,shiftedValue)]

  dtPop <- setDT(ospsuite::populationToDataFrame(scenarioResult$population))
  dtPop <- dtPop[IndividualId %in% ids,c('IndividualId','Gender','Organism|Weight','Organism|Age','Organism|Height')]

  names(dtPop) <- gsub('Organism\\|','',names(dtPop))
  dtPop[,`:=` (
    Weight = round(Weight,1),
    Age = floor(Age),
    Height = round(Height*10)
  )]

  randomIndividuals <- merge(randomIndividuals,dtPop,by = 'IndividualId')
  randomIndividuals[ , STUD  := 1234]
  randomIndividuals[ , route  := sub(".*_(.*)\\..*", "\\1", csvfile)]
  setnames(randomIndividuals,
           old = c('IndividualId','outputPathId','Time','shiftedValue','displayUnit',
                   'Gender','Age','Weight','Height'),
           new = c('SID','outputPathId','TIME','DV','DVUNIT',
                   'SEX','AGE','WGHT0','HGHT0'))

  setcolorder(randomIndividuals,'STUD')

  fwrite(randomIndividuals,file.path(dataFolder,csvfile))


  return(invisible())

}

## mockmanualEditing -------------
#' Cleanup Mock Manual Editings
#'
#' This function cleans upthe project configuration files
#' by clearing existing data from relevant Excel sheets.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project configuration details.
#' @return NULL
#' @keywords internal
mockManualEditingsCleanup <- function(projectConfiguration){

  # delete examples
  for (xlsfile in c(projectConfiguration$scenariosFile,
                    projectConfiguration$applicationsFile,
                    projectConfiguration$populationsFile,
                    projectConfiguration$individualsFile,
                    projectConfiguration$modelParamsFile)){

    # cleanup templatelines in wokbooks
    wb <- openxlsx::loadWorkbook(xlsfile)

    for (sheetName in wb$sheet_names){
      dt <- xlsxReadData(wb = wb,sheetName  = sheetName)
      dt <- dt[FALSE]
      xlsxWriteData(wb = wb,sheetName  = sheetName,dt = dt)
    }
    # save all sheets
    openxlsx::saveWorkbook(wb, xlsfile, overwrite = TRUE)

  }

}
#' Mock Manual Editings for Population
#'
#' This function adds biometric ranges for virtual pediatric and one adult populations to the configuration files
#'
#' @param projectConfiguration A ProjectConfiguration object containing project configuration details.
#' @return NULL
#' @keywords intenal
mockManualEditingsPopulation <- function(projectConfiguration){

  # add virtual population with in biometric ranges of observed data
  wb <- openxlsx::loadWorkbook(projectConfiguration$populationsFile)

  dtPops <- xlsxReadData(wb = wb,sheetName  = "Demographics" )
  dtPops <-   rbind(dtPops[0],
                    data.table(
                      populationName = c('adults','toddler','children','school-children','adolescents'),
                      species = 'Human',
                      population = 'European_ICRP_2002',
                      numberOfIndividuals = 100,
                      proportionOfFemales = 50,
                      ageMin = c(20,0.5,2,6,12),
                      ageMax = c(40,2,6,12,18),
                      weightUnit = 'kg',
                      heightUnit = 'cm',
                      bMIUnit = 'kg/m²',
                      protein = 'CYP3A4,UGT1A4',
                      ontogeny = 'CYP3A4,UGT1A4'
                    ),
                    fill = TRUE)
  xlsxWriteData(wb = wb, sheetName  = 'Demographics', dt = dtPops)

  openxlsx::saveWorkbook(wb, projectConfiguration$populationsFile, overwrite = TRUE)
}
#' Mock Manual Editings for Scenarios
#'
#' This function sets up scenarios based on the defined populations and adds PK Parameters
#' and output paths to the project configuration.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project configuration details.
#' @return NULL
mockManualEditingsScenario <- function(projectConfiguration,modelFiles){

  wb <- openxlsx::loadWorkbook(projectConfiguration$scenariosFile)
  # set scenarios
  dtScenario <- xlsxReadData(wb = wb,sheetName  = 'Scenarios')
  dtPop <- xlsxReadData(wb = projectConfiguration$populationsFile,sheetName  = 'Demographics')

  dtScenario <- rbind(dtScenario,
                      data.table(
                        scenario_name = paste0(gsub('-','_',dtPop$populationName),'_iv'),
                        populationId =  dtPop$populationName,
                        readPopulationFromCSV = 1,
                        modelFile = modelFiles['iv']),
                      data.table(
                        scenario_name = paste0(gsub('-','_',dtPop$populationName),'_po'),
                        populationId =  dtPop$populationName,
                        readPopulationFromCSV = 1,
                        modelFile = modelFiles['po']),
                      fill = TRUE)

  xlsxWriteData(wb = wb, sheetName  = 'Scenarios', dt = dtScenario)

  # add PK Parameter sheets
  dtPK <- data.table(scenario_name = dtScenario$scenario_name,
                     pKParameter = "PK_Plasma, PK_Fraction" )

  xlsxWriteData(wb = wb,sheetName  = 'PKParameter',dt = dtPK)

  # OutputPaths
  dtOutputs <- xlsxReadData(wb = wb,sheetName = 'OutputPaths')

  dtOutputs <- rbind(data.table(
    outputPathId = 'Plasma',
    outputPath =  "Organism|PeripheralVenousBlood|DrugX|Plasma (Peripheral Venous Blood)"),
    data.table(
      outputPathId = 'CYP3A4total',
      outputPath =  "Organism|DrugX-CYP3A4-Optimized Metabolite|Total fraction of dose-DrugX"),
    data.table(
      outputPathId = 'CYP3A4Liver',
      outputPath =  "Organism|Liver|Periportal|Intracellular|DrugX-CYP3A4-Optimized Metabolite|Fraction of dose-DrugX"),
    fill = TRUE)

  xlsxWriteData(wb = wb,sheetName  = 'OutputPaths',dt = dtOutputs)

  # save all sheets
  openxlsx::saveWorkbook(wb, projectConfiguration$scenariosFile, overwrite = TRUE)


}
#' Mock Manual Editings for Display Names
#'
#' This function updates the display names for scenarios and output paths in the project configuration.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project configuration details.
#' @return NULL
mockManualEditingsdisplayNames <- function(projectConfiguration){


  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  # scenarios

  dtScenario <- xlsxReadData(wb = wb,sheetName = 'Scenarios')
  dtScenario[,shortName := gsub(' iv','',gsub(' po','',shortName))]
  dtScenario[,longName := gsub(' iv','',gsub(' po','',longName))]

  dtScenario[grep('p_123',scenario),`:=`(
    shortName = 'study 1234',
    longName = 'study 1234 iv application'
  )]
  dtScenario[grep('^i123',scenario),`:=`(
             shortName = paste('individual',gsub('i1234','',gsub('_iv','',scenario))),
             longName = paste('individual',gsub('i1234','',gsub('_iv','',scenario)),'iv application')
             )]

  xlsxWriteData(wb = wb, sheetName  = 'Scenarios', dt = dtScenario)

  # outputpathids
  dtOutputs <- xlsxReadData(wb = wb,sheetName = 'Outputs')

  dtOutputs[outputPathId == 'Plasma']$displayName = "drugX plasma concentration"
  dtOutputs[outputPathId == 'Plasma']$displayUnit = "µg/L"
  dtOutputs[outputPathId == 'CYP3A4total']$displayName = "drugX metabolized by CYP3A4"
  dtOutputs[outputPathId == 'CYP3A4Liver']$displayName = "drugX metabolized by CYP3A4 in liver"

  xlsxWriteData(wb = wb, sheetName  = 'Outputs', dt = dtOutputs)

  # save all sheets
  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

}
#' Mock Manual Editings for PK Parameters
#'
#' This function adds PK parameters to the project configuration based on templates.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project configuration details.
#' @return NULL
mockManualEditingsPKParameter <- function(projectConfiguration){


  # add all data files which are used in the project
  wb <- openxlsx::loadWorkbook(projectConfiguration$addOns$pKParameterFile)

  dtTemplate <- xlsxReadData(wb = wb,sheetName = 'Template')

  dtTemplate <- dtTemplate[c(1,which(dtTemplate$name %in% c("C_max","AUC_inf")))]
  dtTemplate[name %in% c("C_max","AUC_inf"),outputPathIds := "Plasma"]

  xlsxCloneAndSet(wb = wb,clonedSheet = 'Template', sheetName  = 'PK_Plasma', dt = dtTemplate)


  dtTemplate <- xlsxReadData(wb = wb,sheetName = 'Template')

  dtTemplate <- dtTemplate[c(1,which(dtTemplate$name %in% c("F_tEnd")))]
  dtTemplate[name %in% c("F_tEnd"),outputPathIds := c('CYP3A4total, CYP3A4Liver')]
  dtTemplate[name %in% c("F_tEnd"),displayName := 'fraction metabolized']

  xlsxCloneAndSet(wb = wb,clonedSheet = 'Template', sheetName  = 'PK_Fraction', dt = dtTemplate)

  dtUserdef <-xlsxReadData(wb,sheetName = "Userdef PK Parameter")


  openxlsx::saveWorkbook(wb, projectConfiguration$addOns$pKParameterFile, overwrite = TRUE)
}
#' Mock Manual Editings for Data Dictionary
#'
#' This function updates the data dictionary for the project configuration with new data files.
#'
#' @param projectConfiguration A ProjectConfiguration object containing project configuration details.
#' @param withPK boolean, if TRUE configuration for pkparameter  are added
#' @return NULL
mockManualEditingsDataDictionary <- function(projectConfiguration,
                                             withPK = TRUE){

  # add all data files which are used in the project
  wb <- openxlsx::loadWorkbook(projectConfiguration$dataImporterConfigurationFile)

  dtDataFiles <- xlsxReadData(wb = wb,sheetName  = 'DataFiles')
  dtDataFiles <- dtDataFiles[c(1)]


  dtDataFiles <- rbind(dtDataFiles,
                       data.table(fileIdentifier = 'tp_iv',
                                  dataFile = file.path('..','..','Data','timeprofiles_adults_iv.csv'),
                                  dictionary = 'tpDictionary',
                                  dataFilter = '',
                                  dataClass = DATACLASS$tpIndividual
                       ),
                       data.table(fileIdentifier = 'tp_po',
                                  dataFile = file.path('..','..','Data','timeprofiles_adults_po.csv'),
                                  dictionary = 'tpDictionary',
                                  dataFilter = '',
                                  dataClass = DATACLASS$tpIndividual
                       ))
  if (withPK == TRUE){
    dtDataFiles <- rbind(dtDataFiles,
                         data.table(fileIdentifier = 'pk_abs',
                                    dataFile = file.path('..','..','Data','PK_absolute_values.csv'),
                                  dictionary = 'pkDictionary_absValues',
                                  dataFilter = '',
                                  dataClass = DATACLASS$pkAggregated
                       ),
                       data.table(fileIdentifier = 'pk_ratios',
                                  dataFile = file.path('..','..','Data','PK_po_iv_ratios.csv'),
                                  dictionary = 'pkDictionary_ratios',
                                  dataFilter = '',
                                  dataClass = DATACLASS$pkAggregated
                       ))
  }

  xlsxWriteData(wb = wb, sheetName  = 'DataFiles', dt = dtDataFiles)

  # tp dictionayr
  tpDictionary <- xlsxReadData(wb = wb,sheetName = 'tpDictionary',skipDescriptionRow = FALSE)
  tpDictionaryH <- tpDictionary[1]
  tpDictionary <- tpDictionary[targetColumn %in% c("studyId", "subjectId", "individualId",
                                                   "group", "outputPathId", "xValues", "yValues", "yUnit", "lloq",
                                                   "age", "weight", "height", "gender", "population", "species", "route")]
  tpDictionary <- tpDictionary[!duplicated(tpDictionary$targetColumn)]

  tpDictionary[targetColumn == 'group']$filterValue <- "paste(STUD,'adults',route,sep = '_')"
  tpDictionary[ targetColumn == 'outputPathId']$sourceColumn = "outputPathId"
  tpDictionary[ targetColumn == 'population']$filter = "TRUE"
  tpDictionary[ targetColumn == 'route']$sourceColumn = "route"

  tpDictionary[!is.na(sourceColumn),`:=`(
    filter = NA,
    filterValue = NA)]

  xlsxWriteData(wb = wb,sheetName = "tpDictionary",dt = rbind(tpDictionaryH,tpDictionary))

  # - pkDictionary absolute
  if (withPK == TRUE){
    pkDictionary <- xlsxReadData(wb = wb,sheetName = 'pkDictionary',skipDescriptionRow = FALSE)
    pkDictionaryH <- pkDictionary[1]
    pkDictionary <- pkDictionary[targetColumn %in% c(
      "studyId","group","outputPathId","pkParameter",
      "values","unit","errorType","errorValues","numberOfIndividuals")]

    pkDictionary[targetColumn == 'group']$filterValue <- "paste(STUD,population,route,sep = '_')"
    pkDictionary[ targetColumn == 'outputPathId']$sourceColumn = "outputPathId"
    pkDictionary[ targetColumn == 'pkParameter']$sourceColumn = "pkParameter"
    pkDictionary[ targetColumn == 'values']$sourceColumn = "geomean"
    pkDictionary[ targetColumn == 'errorValues']$sourceColumn = "geosd"
    pkDictionary[ targetColumn == 'unit']$sourceColumn = "unit"
    pkDictionary[ targetColumn == 'numberOfIndividuals']$sourceColumn = "N"
    pkDictionary[!is.na(sourceColumn),`:=`(
      filter = NA,
      filterValue = NA)]

    xlsxCloneAndSet(wb = wb,
                    clonedSheet = "pkDictionary",
                    sheetName = "pkDictionary_absValues",
                    dt = rbind(pkDictionaryH,pkDictionary))

    # - pkDictionary ratio
    pkDictionary <- xlsxReadData(wb = wb,sheetName = 'pkDictionary',skipDescriptionRow = FALSE)
    pkDictionaryH <- pkDictionary[1]
    pkDictionary <- pkDictionary[targetColumn %in% c(
      "studyId", "group", "outputPathId", "pkParameter",
      "values", "unit", "errorType", "minValue", "maxValue", "numberOfIndividuals"
    )]
    pkDictionary[targetColumn == 'group']$filterValue <- "paste(STUD,population,'ratio',sep = '_')"
    pkDictionary[ targetColumn == 'outputPathId']$sourceColumn = "outputPathId"
    pkDictionary[ targetColumn == 'pkParameter']$sourceColumn = "pkParameter"
    pkDictionary[ targetColumn == 'values']$sourceColumn = "geomean"
    pkDictionary[ targetColumn == 'minValue']$sourceColumn = "CI_lower"
    pkDictionary[ targetColumn == 'maxValue']$sourceColumn = "CI_upper"
    pkDictionary[ targetColumn == 'errorType']$filterValue = '"geometric mean|90% CI lower|90% CI upper"'
    pkDictionary[ targetColumn == 'unit']$sourceColumn = "unit"
    pkDictionary[ targetColumn == 'numberOfIndividuals']$sourceColumn = "N"
    pkDictionary[!is.na(sourceColumn),`:=`(
      filter = NA,
      filterValue = NA)]

    xlsxCloneAndSet(wb = wb,
                    clonedSheet = "pkDictionary",
                    sheetName = "pkDictionary_ratios",
                    dt = rbind(pkDictionaryH,pkDictionary))
  }

  openxlsx::saveWorkbook(wb, projectConfiguration$dataImporterConfigurationFile, overwrite = TRUE)
}

mockManualEditingsIndividuals <- function(projectConfiguration,modelFiles){

  # add all data files which are used in the project
  wb <- openxlsx::loadWorkbook(projectConfiguration$individualsFile)

  dt <- xlsxReadData(wb = wb,sheetName = 'IndividualBiometrics',skipDescriptionRow = FALSE,emptyAsNA = FALSE)
  dt[,`:=`(
    protein = 'CYP3A4,UGT1A4',
    ontogeny = 'CYP3A4,UGT1A4'
  )]

  individualIds <- dt$individualId

    # to avoid bug in eqslabsR (Issue #790) add empty individual tables
  for (ind in setdiff(individualIds,wb$sheet_names)){
    openxlsx::cloneWorksheet(wb = wb, clonedSheet =  "template_Ind", sheetName = ind)
  }


  xlsxWriteData(wb = wb, sheetName  = 'IndividualBiometrics', dt = dt)

  dt <- xlsxReadData(wb = wb,sheetName = 'VirtualTwinPopulation',skipDescriptionRow = FALSE,emptyAsNA = FALSE)
  dt[,populationName := '1234_adults']

  populationId <- unique(dt$populationName)

  xlsxWriteData(wb = wb, sheetName  = 'VirtualTwinPopulation', dt = dt)

  openxlsx::saveWorkbook(wb, projectConfiguration$individualsFile, overwrite = TRUE)

  # add a scenarios

  # set scenarios
  wb <- openxlsx::loadWorkbook(projectConfiguration$scenariosFile)
  dtScenario <- xlsxReadData(wb = wb,sheetName  = 'Scenarios')

  dtNew <- rbind(data.table(scenario_name = paste('p',populationId,'iv',sep = '_'),
                            populationId = populationId,
                            readPopulationFromCSV = 1,
                            modelFile = modelFiles['iv']),
                 data.table(scenario_name = paste(tolower(individualIds),'iv',sep = '_'),
                      individualId = individualIds,
                      modelFile = modelFiles['iv']),
                 fill = TRUE)


  dtScenario <- rbind(dtScenario,
                      dtNew,
                      fill = TRUE)

  dtScenario <- dtScenario[!duplicated(scenario_name,fromLast = TRUE)]

  xlsxWriteData(wb = wb, sheetName  = 'Scenarios', dt = dtScenario)

  openxlsx::saveWorkbook(wb, projectConfiguration$scenariosFile, overwrite = TRUE)

  ospsuite.reportingframework:::synchronizeScenariosWithPlots(projectConfiguration)  # Sync scenarios with plots
  mockManualEditingsdisplayNames(projectConfiguration)

  return(dtNew$scenario_name)
}
mockManualEditingsSensitivity <- function(projectConfiguration,sheetName){

  wb <- openxlsx::loadWorkbook(projectConfiguration$addOns$sensitivityFile)

  dt <- xlsxReadData(wb = wb,sheetName = sheetName,skipDescriptionRow = FALSE)

  dt <- dt[c(1,
             grep('DrugX Lipophilicity',parameter),
             grep('DrugX Fraction unbound',parameter),
             grep('CYP3A4 Ontogeny factor',parameter)
             )]

  dt[grep('DrugX Fraction unbound',parameter),parameter := 'DrugX Fraction unbound']

  xlsxWriteData(wb = wb, sheetName  = sheetName, dt = dt)

  openxlsx::saveWorkbook(wb, projectConfiguration$addOns$sensitivityFile, overwrite = TRUE)

}
