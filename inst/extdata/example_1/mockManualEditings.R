#' Title
#'
#' @param projectConfiguration
#'
#' @return
#' @export
#'
#' @examples
mockManualEditings.DataDictionary <- function(projectConfiguration){

  targetColumn <- NULL

  # add all data files which are used in the project
  wb <- openxlsx::loadWorkbook(projectConfiguration$dataImporterConfigurationFile)

  dtDataFiles <- xlsxReadData(wb = wb,sheetName  = 'DataFiles')
  dtDataFiles <- dtDataFiles[c(1)]
  dtDataFiles <- rbind(dtDataFiles,
                       data.table(dataFile = file.path('..','..','Data','observedData_drugX.csv'),
                                  dictionary = 'tpDictionary',
                                  dataFilter = '',
                                  dataClass = DATACLASS$tpIndividual
                       )
  )

  xlsxWriteData(wb = wb, sheetName  = 'DataFiles', dt = dtDataFiles)

  # - tpDictionary
  tpDictionary <- xlsxReadData(wb = wb,sheetName = 'tpDictionary')

  tpDictionary <- tpDictionary[!(targetColumn  %in% c("yErrorType",'yErrorValues','yMin','yMax','nBelowLLOQ','numberOfIndividuals','population','country'))]

  tpDictionary[ targetColumn == 'subjectId']$sourceColumn <- "SID"
  tpDictionary[ targetColumn == 'individualId']$sourceColumn <- "SID"
  tpDictionary[ targetColumn == 'studyArm']$sourceColumn <- "Route"
  tpDictionary[ targetColumn == 'xValues']$sourceColumn <- "Time"
  tpDictionary[ targetColumn == 'yValues']$sourceColumn <- "values"
  tpDictionary[ targetColumn == 'yUnit']$sourceColumn <- "unit"
  tpDictionary[ targetColumn == 'lloq']$sourceColumn <- "LLOQ"
  tpDictionary[ targetColumn == 'age']$sourceColumn <- "Age"
  tpDictionary[ targetColumn == 'weight']$sourceColumn <- "Weight"
  tpDictionary[ targetColumn == 'height']$sourceColumn <- "Height"
  tpDictionary[ targetColumn == 'gender']$sourceColumn <- "Gender"

  tpDictionary[targetColumn == 'individualId', c("sourceColumn", "filter", "filterValue")] <- list("SID", NA, NA)
  tpDictionary[targetColumn == 'route', c("sourceColumn", "filter", "filterValue")] <- list("Route", NA, NA)
  tpDictionary[targetColumn == 'group', c("sourceColumn", "filter", "filterValue")] <- list("Route", NA, NA)

  tpDictionary[targetColumn == 'height',sourceUnit:= "dm"]
  tpDictionary[targetColumn == 'xValues',sourceUnit:= "min"]

  tpDictionary[targetColumn == 'studyId', c("sourceColumn", "filter", "filterValue")] <- list(NA, "TRUE", '"000"')
  tpDictionary[targetColumn == 'outputPathId', c("sourceColumn", "filter", "filterValue")] <- list(NA, "TRUE", '"Concentration"')
  tpDictionary[targetColumn == 'dose', c("sourceColumn", "filter", "filterValue")] <- list(NA, "TRUE", '"1mg"')


  tpDictionary <- rbind(tpDictionary,
                        tpDictionary[ targetColumn == 'outputPathId'] %>%
                          dplyr::mutate(filter = 'dimension == "Fraction"') %>%
                          dplyr::mutate(filterValue = '"Fraction"'),
                        tpDictionary[ targetColumn == 'dose'] %>%
                          dplyr::mutate(filter = 'Route == "PO"') %>%
                          dplyr::mutate(filterValue = '"5mg"'),
                        tpDictionary[ targetColumn == 'studyId'] %>%
                          dplyr::mutate(targetColumn = 'population') %>%
                          dplyr::mutate(type = 'biometrics') %>%
                          dplyr::mutate(filterValue = '"European_ICRP_2002"'))

  tpDictionary <- unique(tpDictionary)

  xlsxWriteData(wb = wb, sheetName  = 'tpDictionary', dt = tpDictionary)
  openxlsx::saveWorkbook(wb, projectConfiguration$dataImporterConfigurationFile, overwrite = TRUE)
}


mockManualEditings.Population <- function(projectConfiguration,dataObserved,tutorialstep){



  if (tutorialstep == 1){

    # individuals
    wb <- openxlsx::loadWorkbook(projectConfiguration$individualsFile)
    dtInds <- xlsxReadData(wb = wb,sheetName  = "IndividualBiometrics" )
    # delete template rows
    dtInds <- dtInds[individualId != 'MALE']
    dtInds <- dtInds[individualId != 'FEMALE']

    # add ontogeny to individual imported by readObservedDataByDictionary
    dtInds[,protein := as.character(protein)]
    dtInds[,protein := 'CYP3A4,UGT1A4']
    dtInds[,ontogeny := as.character(ontogeny)]
    dtInds[,ontogeny := 'CYP3A4,UGT1A4']

    xlsxWriteData(wb = wb, sheetName  = 'IndividualBiometrics', dt = dtInds)
    openxlsx::saveWorkbook(wb, projectConfiguration$individualsFile, overwrite = TRUE)
  }
  if (tutorialstep == 2){

    # check configuration for virtual twin population settings and adjust population name
    wb <- openxlsx::loadWorkbook(projectConfiguration$individualsFile)

    dtTwinPops <- xlsxReadData(wb, 'VirtualTwinPopulation')

    dtTwinPops$populationName = 'virtual_twin_population'

    xlsxWriteData(wb = wb, sheetName  = 'VirtualTwinPopulation', dt = dtTwinPops)
    openxlsx::saveWorkbook(wb, projectConfiguration$individualsFile, overwrite = TRUE)

  }

  if (tutorialstep == 3){

    # add virtual population with in biometric ranges of observed data
    wb <- openxlsx::loadWorkbook(projectConfiguration$populationsFile)

    dtPops <- xlsxReadData(wb = wb,sheetName  = "Demographics" )
    dtPops <- dtPops[1]

    dtPops$populationName = 'random_population'
    dtPops$species = 'Human'
    dtPops$population = 'European_ICRP_2002'
    dtPops$numberOfIndividuals = 100
    dtPops$proportionOfFemales = 0.5
    dtPops$weightMin = floor(min(dataObserved$weight,na.rm = TRUE))
    dtPops$weightMax = ceiling(max(dataObserved$weight,na.rm = TRUE))
    dtPops$weightUnit = 'kg'
    dtPops$heightMin = floor(min(dataObserved$height,na.rm = TRUE))
    dtPops$heightMax = ceiling(max(dataObserved$height,na.rm = TRUE))
    dtPops$heightUnit = 'cm'
    dtPops$ageMin = floor(min(dataObserved$age,na.rm = TRUE))
    dtPops$ageMax = ceiling(max(dataObserved$age,na.rm = TRUE))
    BMI <- unique(dataObserved$weight/dataObserved$height/dataObserved$height)*10000
    dtPops$bMIMin = floor(min(BMI,na.rm = TRUE))
    dtPops$bMIMax = ceiling(max(BMI,na.rm = TRUE))
    dtPops$protein = 'CYP3A4,UGT1A4'
    dtPops$ontogeny = 'CYP3A4,UGT1A4'

    xlsxWriteData(wb = wb, sheetName  = 'Demographics', dt = dtPops)

    openxlsx::saveWorkbook(wb, projectConfiguration$populationsFile, overwrite = TRUE)
  }
}

#' Title
#'
#' @param projectConfiguration
#' @param dataObserved
#'
#' @return
#' @export
#'
#' @examples
mockManualEditings.Scenario <- function(projectConfiguration,dataObserved,tutorialstep){

  if (tutorialstep == 4){
    # add a sheet with parameters to adjust for an reference population
    wb <- openxlsx::loadWorkbook(projectConfiguration$modelParamsFile)
    dtPar <- xlsxReadData(wb,sheetName = 'Template')
    # delete template rows
    dtPar <- dtPar[FALSE]
    dtPar <- rbind(dtPar,
                   data.table( 'container Path' = 'CYP3A4',
                               'parameter Name' = 'Ontogeny factor',
                               value = 2,
                               units = ''
                   ))

    xlsxCloneAndSet(wb = wb, clonedSheet = 'Template',sheetName = 'CYP3A4_2',dt = dtPar)


    openxlsx::saveWorkbook(wb, projectConfiguration$modelParamsFile, overwrite = TRUE)
  }

  wb <- openxlsx::loadWorkbook(projectConfiguration$scenariosFile)

  if (tutorialstep == 1){
    dtOutputs <- xlsxReadData(wb = wb,sheetName  = 'OutputPaths')
    dtOutputs <- dtOutputs[outputPathId %in% c(dtOutputs$outputPathId[1],'Concentration','Fraction')]
    xlsxWriteData(wb = wb, sheetName  = 'OutputPaths', dt = dtOutputs)

  }



  dtScenario <- xlsxReadData(wb = wb,sheetName  = 'Scenarios')

  if (tutorialstep == 1){
    # delete the template lines
    dtScenario <- dtScenario[FALSE]

    # add scenario for an individual iv scenario
    dtScenario = rbind(dtScenario,
                       data.table(scenario_name = 'individual_1_iv',
                                  individualId = '1',
                                  modelFile = 'iv 1 mg (5 min).pkml'),
                       fill = TRUE)
  }


  if (tutorialstep == 2){
    # add scenario for individual population iv application and po application
    dtScenario = rbind(dtScenario,
                       data.table(scenario_name = 'virtual_twin_population_iv',
                                  populationId = 'virtual_twin_population',
                                  readPopulationFromCSV = TRUE,
                                  modelFile = 'iv 1 mg (5 min).pkml'),
                       data.table(scenario_name = 'virtual_twin_population_po',
                                  populationId = 'virtual_twin_population',
                                  readPopulationFromCSV = TRUE,
                                  modelFile = 'po 3 mg (solution).pkml'),
                       fill = TRUE)
  }

  # add scenario for random population iv application and po application
  if (tutorialstep == 3){

    dtScenario = rbind(dtScenario,
                       data.table(scenario_name = 'random_population_iv',
                                  populationId = 'random_population',
                                  readPopulationFromCSV = TRUE,
                                  modelFile = 'iv 1 mg (5 min).pkml'),
                       data.table(scenario_name = 'random_population_po',
                                  populationId = 'random_population',
                                  readPopulationFromCSV = TRUE,
                                  modelFile = 'po 3 mg (solution).pkml'),
                       fill = TRUE)
  }

  # add scenario for virtual population iv application and po application
  if (tutorialstep == 4){
    dtScenario = rbind(dtScenario,
                       data.table(scenario_name = 'reference_population_iv',
                                  populationId = 'random_population',
                                  modelParameterSheets  = 'CYP3A4_2',
                                  readPopulationFromCSV = TRUE,
                                  modelFile = 'iv 1 mg (5 min).pkml'),
                       data.table(scenario_name = 'reference_population_po',
                                  populationId = 'random_population',
                                  modelParameterSheets  = 'CYP3A4_2',
                                  readPopulationFromCSV = TRUE,
                                  modelFile = 'po 3 mg (solution).pkml'),
                       fill = TRUE)
  }

  xlsxWriteData(wb = wb, sheetName  = 'Scenarios', dt = dtScenario)
  openxlsx::saveWorkbook(wb, projectConfiguration$scenariosFile, overwrite = TRUE)

}


#' Title
#'
#' @param projectConfiguration
#' @param dataObserved
#'
#' @return
#' @export
#'
#' @examples
#'
mockManualEditings.DataGroups <- function(projectConfiguration,dataObserved,tutorialstep){
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  # data Groups
  dtDataGroups <- xlsxReadData(wb = wb,sheetName = 'DataGroups')

  # delete template lines, keeping only the ones added by data import
  if (tutorialstep == 1){
    dtDataGroups <- dtDataGroups[c(1,which(dtDataGroups$studyId == '000')),]

  }


  # add simulation Scenario for matching
  if (tutorialstep == 2){
    dtDataGroups[group == "IV", defaultScenario := "virtual_twin_population_iv"]
    dtDataGroups[group == "PO", defaultScenario := "virtual_twin_population_po"]
  }
  if (tutorialstep == 3){
    dtDataGroups[group == "IV_aggregated", defaultScenario := "random_population_iv"]
    dtDataGroups[group == "PO_aggregated", defaultScenario := "random_population_po"]
  }

  xlsxWriteData(wb = wb, sheetName  = 'DataGroups', dt = dtDataGroups)

  # outputpathids
  if (tutorialstep %in% c(1,5)){
    dtOutputs <- xlsxReadData(wb = wb,sheetName = 'Outputs')

    # delete template lines, keeping only the ones added by data import
    dtOutputs <- dtOutputs[outputPathId %in% c(dtOutputs$outputPathId[1],'Concentration','Fraction')]

    # add properties for concentration
    dtOutputs[outputPathId == 'Concentration', outputPath := "Organism|PeripheralVenousBlood|DrugX|Plasma (Peripheral Venous Blood)"]
    dtOutputs[outputPathId == 'Concentration', displayNameOutputs := "DrugX Plasma"]
    dtOutputs[outputPathId == 'Concentration', displayUnit := "µg/L"]
    # add properties for concentration
    dtOutputs[outputPathId == 'Fraction', outputPath := "Organism|Kidney|Urine|DrugX|Fraction excreted to urine"]
    dtOutputs[outputPathId == 'Fraction', displayNameOutputs := "Fraction excreted to urine"]

    dtOutputs[,displayName := outputPathId]

    xlsxWriteData(wb = wb, sheetName  = 'Outputs', dt = dtOutputs)

  }

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

}

#' Title
#'
#' @param projectConfiguration
#' @param dataObserved
#'
#' @return
#' @export
#'
#' @examples
mockManualEditings.TimePlot <- function(projectConfiguration,dataObserved,tutorialstep){

  # update configurations done by addDefaultConfigForTimeProfilePlots
  addHeaderLineBeforePlot <- function(dtPlots,plotName,level,header){
    iRow <- which(dtPlots$plotName == plotName)
    dtPlots <- rbind(dtPlots[seq(1,iRow-1)],
                     data.table(level = level,header =header),
                     dtPlots[seq(iRow,nrow(dtPlots))],
                     fill = TRUE)
    return(dtPlots)
  }

  copyLine <- function(dtPlots,plotName){
    ix = which(dtPlots$plotName == plotName)
    dtPlots <- dtPlots[c(seq(1,ix),seq(ix,nrow(dtPlots)))]
    return(dtPlots)

  }

  shiftLineToEnd <- function(dtPlots,plotName){
    ix = which(dtPlots$plotName == plotName)
    dtPlots <- dtPlots[c(setdiff(seq(1,nrow(dtPlots)),ix),ix)]
    return(dtPlots)

  }


  sheetName = 'TimeProfiles'

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  # add use case for different time ranges
  if (tutorialstep == 5){
    # create new timerange tags
    dtTimeRange <- xlsxReadData(wb,sheetName = 'TimeRange')
    dtTimeRange <- rbind(dtTimeRange,
                         data.table(tag = 'h0_6',
                                    captionText = 'Zoom on first 6 hours',
                                    timeLabel = 'Time',
                                    timeShift = 0),
                         data.table(tag = 'h6_24',
                                    captionText = 'Zoom on time range 6 to 24 hours',
                                    timeLabel = 'Time after dose',
                                    timeShift = 0)) %>%
      unique()


    xlsxWriteData(wb = wb, sheetName  = 'TimeRange', dt = dtTimeRange)
  }


  # Plotconfig
  #Line one is a header line with additional innformation to help the input and is not edited
  dtPlots <- xlsxReadData(wb = wb,sheetName = sheetName)

  if (tutorialstep == 1){
    dtPlots <- addHeaderLineBeforePlot(dtPlots,'individual_1_iv',2,'Setting Up the Project and Basic Simulations')
    # set plotCaptionAddon to describe application
    dtPlots[plotName == 'individual_1_iv',plotCaptionAddon :=  "DrugX was admistered as a 1mg Iv application with an infusion time of 5 minutes."]
    # single dose, only Time range total makes sense
    dtPlots[plotName == 'individual_1_iv',timeRange_firstApplication:= NA]
    dtPlots[plotName == 'individual_1_iv',timeRange_lastApplication:= NA]
    # set caption description of scenario
    dtPlots[plotName == 'individual_1_iv', scenarioLongName := 'individual 1 of study 000']
    # dataGroupIds is empty and has to be set manually as this scenario was not selected as DefaultScenario in the DataGroups configuration
    dtPlots[plotName == 'individual_1_iv', dataGroupIds := 'IV']
    # Filter the data for the simulated individual
    dtPlots[plotName == 'individual_1_iv', individualIds := '1']


    dtPlots <- copyLine(dtPlots = dtPlots,plotName = 'individual_1_iv')
    dtPlots[plotName == 'individual_1_iv', ]$plotName[2] <- 'individual_1_iv-gof'
    cols = c("plot_PredictedVsObserved", "plot_ResidualsAsHistogram", "plot_ResidualsVsTime", "plot_ResidualsVsObserved",  "plot_QQ" )
    dtPlots[plotName == 'individual_1_iv-gof',(cols) := 1]
    dtPlots[plotName == 'individual_1_iv-gof',plot_TimeProfiles := 0]
    # select only Concentraion, as for gof plots two different dimensions like Concentration and Fraction are not possible
    dtPlots[plotName == 'individual_1_iv-gof',outputPathIds :=  "Concentration"]

    # add use case for simulation only
    dtPlots <- copyLine(dtPlots,plotName = 'individual_1_iv')
    dtPlots[plotName == 'individual_1_iv', ]$plotName[2] <- 'noData'
    # Delete data input
    dtPlots[plotName == 'noData', dataGroupIds := '']
    dtPlots[plotName == 'noData', individualIds := '']
    # example for set ylimit
    dtPlots[plotName == 'noData',ylimit_log := "c(0.01,NA)"]


  } else  if (tutorialstep == 2){
    dtPlots <- addHeaderLineBeforePlot(dtPlots,'virtual_twin_population_iv',2,'Adding Virtual Twin Populations')
    dtPlots <- addHeaderLineBeforePlot(dtPlots,'virtual_twin_population_iv',3,'filtered individual')

    dtPlots[plotName == 'virtual_twin_population_iv',plotCaptionAddon :=  "DrugX was admistered as a 1mg Iv application with an infusion time of 5 minutes."]
    dtPlots[plotName == 'virtual_twin_population_po',plotCaptionAddon :=  "DrugX was admistered as a 5mg oral application of a solution."]

    # single dose, only Time range total makes sense
    dtPlots[plotName %in%  c('virtual_twin_population_iv','virtual_twin_population_po'),timeRange_firstApplication:= NA]
    dtPlots[plotName %in%  c('virtual_twin_population_iv','virtual_twin_population_po'),timeRange_lastApplication:= NA]

    # create one plot for individual 1,2,3,4 and one for 5,6,7
    dtPlots <- copyLine(dtPlots = dtPlots,plotName = 'virtual_twin_population_iv')
    dtPlots[plotName == 'virtual_twin_population_iv', plotName := paste(plotName,.I,sep = '-')]
    # Filter the data for the simulated individual
    dtPlots[plotName == 'virtual_twin_population_iv-1', individualIds := '1,2,3,4']
    dtPlots[plotName == 'virtual_twin_population_iv-2', individualIds := '5,6,7']
    # plot both outputs in one panel
    dtPlots[plotName %in% c('virtual_twin_population_iv-1','virtual_twin_population_iv-2'),outputPathIds :=  "(Concentration, Fraction)"]
    dtPlots[plotName %in% c('virtual_twin_population_iv-1','virtual_twin_population_iv-2'), scenarioLongName := 'individual simualtions of study 000']


    # use Scenario virtual_twin_population_po as use case for shortcut '*'
    dtPlots <- addHeaderLineBeforePlot(dtPlots,'virtual_twin_population_po',3,'shortcut for individual filter')
    # Filter all data for the simulated individual
    dtPlots[plotName == 'virtual_twin_population_po', individualIds := '*']
    dtPlots[plotName == 'virtual_twin_population_po', scenarioLongName := 'individual simualtions of study 000']
    dtPlots[plotName == 'virtual_twin_population_po',facetType := "Scenario vs Output"]

    # add goodness for fit plot in new line
    dtPlots <- copyLine(dtPlots,plotName = 'virtual_twin_population_po')
    dtPlots[plotName == 'virtual_twin_population_po',]$plotName[2] <- 'virtual_twin_population_po-gof'
    cols = c("plot_PredictedVsObserved", "plot_ResidualsAsHistogram", "plot_ResidualsVsTime", "plot_ResidualsVsObserved",  "plot_QQ" )
    dtPlots[plotName == 'virtual_twin_population_po-gof',(cols) := 1]
    dtPlots[plotName == 'virtual_twin_population_po-gof',plot_TimeProfiles := 0]
    # plot all individuals in one panel
    dtPlots[plotName == 'virtual_twin_population_po-gof', individualIds := '(*)']
    # select only Concentration, as for gof plots two different dimensions like Concentration and Fraction are not possible
    dtPlots[plotName == 'virtual_twin_population_po-gof',outputPathIds :=  "Concentration"]

  } else  if (tutorialstep == 3){

    dtPlots <- addHeaderLineBeforePlot(dtPlots, 'random_population_iv',2,'Adding Random Populations and use of aggregated data')
    # merge in one plot
    dtPlots[plotName %in% c('random_population_iv','random_population_po'), plotName := 'random_population']
    dtPlots[scenario == 'random_population_iv', scenarioLongName := 'random population simulation of a 1mg 5min iv administration']
    dtPlots[scenario == 'random_population_po', scenarioLongName := 'random population simulation of a 5mg oral administration']
    dtPlots[plotName == 'random_population',plotCaptionAddon := '']
    # single dose, only Time range total makes sense
    dtPlots[plotName ==  'random_population',timeRange_firstApplication:= NA]
    dtPlots[plotName ==  'random_population',timeRange_lastApplication:= NA]

  } else  if (tutorialstep == 4){

    # add use case for reference population
    dtPlots <- addHeaderLineBeforePlot(dtPlots, 'reference_population_iv',2,'Adding Reference Populations')
    dtPlots[,referenceScenario := as.character(referenceScenario)]
    dtPlots[plotName %in% c('reference_population_iv','reference_population_po'),
            plotName := 'reference']
    # shift reference scenarios in column referenceScenario and ad controll scenarios
    dtPlots[plotName == 'reference',  referenceScenario := scenario]
    dtPlots[plotName == 'reference',  scenario := gsub('reference','random',scenario)]

    dtPlots[referenceScenario == 'reference_population_iv', dataGroupIds := 'IV']
    dtPlots[referenceScenario == 'reference_population_po', dataGroupIds := 'PO']

    dtPlots[referenceScenario == 'reference_population_iv', scenarioLongName := 'random population simulation of a 1mg 5min iv administration']
    dtPlots[referenceScenario == 'reference_population_po', scenarioLongName := 'random population simulation of a 5mg oral administration']
    dtPlots[plotName == 'reference',plotCaptionAddon := 'Reference population has doubled CYP3A4 liver ontogeny.']

    # single dose, only Time range total makes sense
    dtPlots[plotName ==  'reference',timeRange_firstApplication:= NA]
    dtPlots[plotName ==  'reference',timeRange_lastApplication:= NA]

  } else  if (tutorialstep == 5){
    # add use case for different time ranges
    # add timerange columns with newly created timeranges
    dtPlots[,timeRange_h0_6 := as.character(NA)]
    dtPlots[,timeRange_h0_6 := NA]
    dtPlots[,timeRange_h6_24 := as.character(NA)]
    dtPlots[,timeRange_h6_24 := NA]

    # ad a new plot line
    dtPlots <- copyLine(dtPlots,plotName = 'individual_1_iv')
    dtPlots[plotName == 'individual_1_iv', ]$plotName[2] <- 'timeranges'
    dtPlots <- shiftLineToEnd(dtPlots,'timeranges')
    dtPlots <- addHeaderLineBeforePlot(dtPlots, 'timeranges',2,'Adding Plots with different Time Ranges')

    # add the timeranges
    dtPlots[plotName == 'timeranges', timeRange_h0_6 := 'c(0,6)']
    dtPlots[plotName == 'timeranges', timeRange_h6_24 := 'c(6,24)']
    # use facet type vs Time ranges
    dtPlots[plotName == 'timeranges', facetType := "Scenario vs TimeRange"]
    # allow different  scales of x-axis
    dtPlots[plotName == 'timeranges', facetScale := "free_x"]

  }


  xlsxWriteData(wb = wb, sheetName  = sheetName, dt = dtPlots)

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)


}


