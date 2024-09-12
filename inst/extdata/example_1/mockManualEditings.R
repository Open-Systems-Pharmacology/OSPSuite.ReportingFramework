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
                       data.table(DataFile = file.path('..','..','Data','observedData_drugX.csv'),
                                  Dictionary = 'tpDictionary',
                                  DataFilter = '',
                                  DataClass = DATACLASS$tpIndividual
                       )
  )

  xlsxWriteData(wb = wb, sheetName  = 'DataFiles', dt = dtDataFiles)

  # - tpDictionary
  tpDictionary <- xlsxReadData(wb = wb,sheetName = 'tpDictionary')

  tpDictionary <- tpDictionary[!(targetColumn  %in% c("yErrorType",'yErrorValues','yMin','yMax','nBelowLLOQ','numberOfPatients','population','country'))]

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



  if (tutorialstep == 2){

    # individuals
    wb <- openxlsx::loadWorkbook(projectConfiguration$individualsFile)
    dtInds <- xlsxReadData(wb = wb,sheetName  = "IndividualBiometrics" )

    # delete template rows
    dtInds <- dtInds[IndividualId != 'MALE']
    dtInds <- dtInds[IndividualId != 'FEMALE']

    # add ontogeny to individual imported by readObservedDataByDictionary
    dtInds[,Protein := as.character(Protein)]
    dtInds[,Protein := 'CYP3A4,UGT1A4']
    dtInds[,Ontogeny := as.character(Ontogeny)]
    dtInds[,Ontogeny := 'CYP3A4,UGT1A4']

    xlsxWriteData(wb = wb, sheetName  = 'IndividualBiometrics', dt = dtInds)
    openxlsx::saveWorkbook(wb, projectConfiguration$individualsFile, overwrite = TRUE)

    # check configuration for virtual twin population settings and adjust population name
    wb <- openxlsx::loadWorkbook(projectConfiguration$populationsFile)

    dtTwinPops <- xlsxReadData(wb, 'VirtualTwinPopulation')

    dtTwinPops$PopulationName = 'virtual_twin_population'

    xlsxWriteData(wb = wb, sheetName  = 'VirtualTwinPopulation', dt = dtTwinPops)
    openxlsx::saveWorkbook(wb, projectConfiguration$populationsFile, overwrite = TRUE)

  }

  if (tutorialstep == 3){

    # add virtual population with in biometric ranges of observed data
    wb <- openxlsx::loadWorkbook(projectConfiguration$populationsFile)

    dtPops <- xlsxReadData(wb = wb,sheetName  = "Demographics" )
    dtPops <- dtPops[1]

    dtPops$PopulationName = 'random_population'
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
    dtPops$BMIMin = floor(min(BMI,na.rm = TRUE))
    dtPops$BMIMax = ceiling(max(BMI,na.rm = TRUE))
    dtPops$Protein = 'CYP3A4,UGT1A4'
    dtPops$Ontogeny = 'CYP3A4,UGT1A4'

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
                   data.table( 'Container Path' = 'CYP3A4',
                               'Parameter Name' = 'Ontogeny factor',
                               Value = 2,
                               Units = ''
                   ))

    xlsxCloneAndSet(wb = wb, clonedSheet = 'Template',sheetName = 'CYP3A4_2',dt = dtPar)


    openxlsx::saveWorkbook(wb, projectConfiguration$modelParamsFile, overwrite = TRUE)
  }


  wb <- openxlsx::loadWorkbook(projectConfiguration$scenariosFile)

  dtScenario <- xlsxReadData(wb = wb,sheetName  = 'Scenarios')

  if (tutorialstep == 1){
    # delete the template lines
    dtScenario <- dtScenario[FALSE]

    # add scenario for an individual iv scenario
    dtScenario = rbind(dtScenario,
                       data.table(Scenario_name = 'individual_1_iv',
                                  IndividualId = '1',
                                  ModelFile = 'iv 1 mg (5 min).pkml'),
                       fill = TRUE)
  }


  if (tutorialstep == 2){
    # add scenario for individual population iv application and po application
    dtScenario = rbind(dtScenario,
                       data.table(Scenario_name = 'virtual_twin_population_iv',
                                  PopulationId = 'virtual_twin_population',
                                  ReadPopulationFromCSV = TRUE,
                                  ModelFile = 'iv 1 mg (5 min).pkml'),
                       data.table(Scenario_name = 'virtual_twin_population_po',
                                  PopulationId = 'virtual_twin_population',
                                  ReadPopulationFromCSV = TRUE,
                                  ModelFile = 'po 3 mg (solution).pkml'),
                       fill = TRUE)
  }

  # add scenario for random population iv application and po application
  if (tutorialstep == 3){

    dtScenario = rbind(dtScenario,
                       data.table(Scenario_name = 'random_population_iv',
                                  PopulationId = 'random_population',
                                  ReadPopulationFromCSV = TRUE,
                                  ModelFile = 'iv 1 mg (5 min).pkml'),
                       data.table(Scenario_name = 'random_population_po',
                                  PopulationId = 'random_population',
                                  ReadPopulationFromCSV = TRUE,
                                  ModelFile = 'po 3 mg (solution).pkml'),
                       fill = TRUE)
  }

  # add scenario for virtual population iv application and po application
  if (tutorialstep == 4){
    dtScenario = rbind(dtScenario,
                       data.table(Scenario_name = 'reference_population_iv',
                                  PopulationId = 'random_population',
                                  ModelParameterSheets  = 'CYP3A4_2',
                                  ReadPopulationFromCSV = TRUE,
                                  ModelFile = 'iv 1 mg (5 min).pkml'),
                       data.table(Scenario_name = 'reference_population_po',
                                  PopulationId = 'random_population',
                                  ModelParameterSheets  = 'CYP3A4_2',
                                  ReadPopulationFromCSV = TRUE,
                                  ModelFile = 'po 3 mg (solution).pkml'),
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
    dtDataGroups <- dtDataGroups[c(1,which(dtDataGroups$StudyId == '000')),]

  }


  # add simulation Scenario for matching
  if (tutorialstep == 2){
    dtDataGroups[Group == "IV", DefaultScenario := "virtual_twin_population_iv"]
    dtDataGroups[Group == "PO", DefaultScenario := "virtual_twin_population_po"]
  }
  if (tutorialstep == 3){
    dtDataGroups[Group == "IV_aggregated", DefaultScenario := "random_population_iv"]
    dtDataGroups[Group == "PO_aggregated", DefaultScenario := "random_population_po"]
  }

  xlsxWriteData(wb = wb, sheetName  = 'DataGroups', dt = dtDataGroups)

  # outputpathids
  if (tutorialstep == 1){
    dtOutputs <- xlsxReadData(wb = wb,sheetName = 'Outputs')

    # delete template lines, keeping only the ones added by data import
    dtOutputs <- dtOutputs[OutputPathId %in% c(dtOutputs$OutputPathId[1],'Concentration','Fraction')]

    # add properties for concentration
    dtOutputs[OutputPathId == 'Concentration', OutputPath := "Organism|PeripheralVenousBlood|DrugX|Plasma (Peripheral Venous Blood)"]
    dtOutputs[OutputPathId == 'Concentration', DisplayName := "DrugX Plasma"]
    dtOutputs[OutputPathId == 'Concentration', DisplayUnit := "µg/L"]
    # add properties for concentration
    dtOutputs[OutputPathId == 'Fraction', OutputPath := "Organism|Kidney|Urine|DrugX|Fraction excreted to urine"]
    dtOutputs[OutputPathId == 'Fraction', DisplayName := "Fraction excreted to urine"]

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
    iRow <- which(dtPlots$PlotName == plotName)
    dtPlots <- rbind(dtPlots[seq(1,iRow-1)],
                     data.table(Level = level,Header =header),
                     dtPlots[seq(iRow,nrow(dtPlots))],
                     fill = TRUE)
    return(dtPlots)
  }

  copyLine <- function(dtPlots,PlotName){
    ix = which(dtPlots$PlotName == PlotName)
    dtPlots <- dtPlots[c(seq(1,ix),seq(ix,nrow(dtPlots)))]
    return(dtPlots)

  }

  shiftLineToEnd <- function(dtPlots,PlotName){
    ix = which(dtPlots$PlotName == PlotName)
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
                         data.table(Tag = 'h0_6',
                                    CaptionText = 'Zoom on first 6 hours',
                                    TimeLabel = 'Time'),
                         data.table(Tag = 'h6_24',
                                    CaptionText = 'Zoom on time range 6 to 24 hours',
                                    TimeLabel = 'Time after dose')) %>%
      unique()


    xlsxWriteData(wb = wb, sheetName  = 'TimeRange', dt = dtTimeRange)
  }


  # Plotconfig
  #Line one is a header line with additional innformation to help the input and is not edited
  dtPlots <- xlsxReadData(wb = wb,sheetName = sheetName)

  if (tutorialstep == 1){
    dtPlots <- addHeaderLineBeforePlot(dtPlots,'individual_1_iv',2,'Setting Up the Project and Basic Simulations')
    # set PlotCaptionAddon to describe application
    dtPlots[PlotName == 'individual_1_iv',PlotCaptionAddon :=  "DrugX was admistered as a 1mg Iv application with an infusion time of 5 minutes."]
    # single dose, only Time range total makes sense
    dtPlots[PlotName == 'individual_1_iv',TimeRange_firstApplication:= NA]
    dtPlots[PlotName == 'individual_1_iv',TimeRange_lastApplication:= NA]
    # set caption description of scenario
    dtPlots[PlotName == 'individual_1_iv', ScenarioCaptionName := 'individual 1 of study 000']
    # DataGroupIds is empty and has to be set manually as this scenario was not selected as DefaultScenario in the DataGroups configuration
    dtPlots[PlotName == 'individual_1_iv', DataGroupIds := 'IV']
    # Filter the data for the simulated individual
    dtPlots[PlotName == 'individual_1_iv', IndividualIds := '1']


    dtPlots <- copyLine(dtPlots = dtPlots,PlotName = 'individual_1_iv')
    dtPlots[PlotName == 'individual_1_iv', ]$PlotName[2] <- 'individual_1_iv-gof'
    cols = c("Plot_PredictedVsObserved", "Plot_ResidualsAsHistogram", "Plot_ResidualsVsTime", "Plot_ResidualsVsObserved",  "Plot_QQ" )
    dtPlots[PlotName == 'individual_1_iv-gof',(cols) := TRUE]
    dtPlots[PlotName == 'individual_1_iv-gof',Plot_TimeProfiles := FALSE]
    # select only Concentraion, as for gof plots two different dimensions like Concentration and Fraction are not possible
    dtPlots[PlotName == 'individual_1_iv-gof',OutputPathIds :=  "Concentration"]

    # add use case for simulation only
    dtPlots <- copyLine(dtPlots,PlotName = 'individual_1_iv')
    dtPlots[PlotName == 'individual_1_iv', ]$PlotName[2] <- 'noData'
    # Delete data input
    dtPlots[PlotName == 'noData', DataGroupIds := '']
    dtPlots[PlotName == 'noData', IndividualIds := '']
    # example for set ylimit
    dtPlots[PlotName == 'noData',ylimit_log := "c(0.01,NA)"]


  } else  if (tutorialstep == 2){
    dtPlots <- addHeaderLineBeforePlot(dtPlots,'virtual_twin_population_iv',2,'Adding Virtual Twin Populations')
    dtPlots <- addHeaderLineBeforePlot(dtPlots,'virtual_twin_population_iv',3,'filtered individual')

    dtPlots[PlotName == 'virtual_twin_population_iv',PlotCaptionAddon :=  "DrugX was admistered as a 1mg Iv application with an infusion time of 5 minutes."]
    dtPlots[PlotName == 'virtual_twin_population_po',PlotCaptionAddon :=  "DrugX was admistered as a 5mg oral application of a solution."]

    # single dose, only Time range total makes sense
    dtPlots[PlotName %in%  c('virtual_twin_population_iv','virtual_twin_population_po'),TimeRange_firstApplication:= NA]
    dtPlots[PlotName %in%  c('virtual_twin_population_iv','virtual_twin_population_po'),TimeRange_lastApplication:= NA]

    # create one plot for individual 1,2,3,4 and one for 5,6,7
    dtPlots <- copyLine(dtPlots = dtPlots,PlotName = 'virtual_twin_population_iv')
    dtPlots[PlotName == 'virtual_twin_population_iv', PlotName := paste(PlotName,.I,sep = '-')]
    # Filter the data for the simulated individual
    dtPlots[PlotName == 'virtual_twin_population_iv-1', IndividualIds := '1,2,3,4']
    dtPlots[PlotName == 'virtual_twin_population_iv-2', IndividualIds := '5,6,7']
    # plot both outputs in one panel
    dtPlots[PlotName %in% c('virtual_twin_population_iv-1','virtual_twin_population_iv-2'),OutputPathIds :=  "(Concentration, Fraction)"]
    dtPlots[PlotName %in% c('virtual_twin_population_iv-1','virtual_twin_population_iv-2'), ScenarioCaptionName := 'individual simualtions of study 000']


    # use Scenario virtual_twin_population_po as use case for shortcut '*'
    dtPlots <- addHeaderLineBeforePlot(dtPlots,'virtual_twin_population_po',3,'shortcut for individual filter')
    # Filter all data for the simulated individual
    dtPlots[PlotName == 'virtual_twin_population_po', IndividualIds := '*']
    dtPlots[PlotName == 'virtual_twin_population_po', ScenarioCaptionName := 'individual simualtions of study 000']
    dtPlots[PlotName == 'virtual_twin_population_po',FacetType := "Scenario vs Output"]

    # add goodness for fit plot in new line
    dtPlots <- copyLine(dtPlots,PlotName = 'virtual_twin_population_po')
    dtPlots[PlotName == 'virtual_twin_population_po',]$PlotName[2] <- 'virtual_twin_population_po-gof'
    cols = c("Plot_PredictedVsObserved", "Plot_ResidualsAsHistogram", "Plot_ResidualsVsTime", "Plot_ResidualsVsObserved",  "Plot_QQ" )
    dtPlots[PlotName == 'virtual_twin_population_po-gof',(cols) := TRUE]
    dtPlots[PlotName == 'virtual_twin_population_po-gof',Plot_TimeProfiles := FALSE]
    # plot all individuals in one panel
    dtPlots[PlotName == 'virtual_twin_population_po-gof', IndividualIds := '(*)']
    # select only Concentration, as for gof plots two different dimensions like Concentration and Fraction are not possible
    dtPlots[PlotName == 'virtual_twin_population_po-gof',OutputPathIds :=  "Concentration"]

  } else  if (tutorialstep == 3){

    dtPlots <- addHeaderLineBeforePlot(dtPlots, 'random_population_iv',2,'Adding Random Populations and use of aggregated data')
    # merge in one plot
    dtPlots[PlotName %in% c('random_population_iv','random_population_po'), PlotName := 'random_population']
    dtPlots[Scenario == 'random_population_iv', ScenarioCaptionName := 'random population simulation of a 1mg 5min iv administration']
    dtPlots[Scenario == 'random_population_po', ScenarioCaptionName := 'random population simulation of a 5mg oral administration']
    dtPlots[PlotName == 'random_population',PlotCaptionAddon := '']
    # single dose, only Time range total makes sense
    dtPlots[PlotName ==  'random_population',TimeRange_firstApplication:= NA]
    dtPlots[PlotName ==  'random_population',TimeRange_lastApplication:= NA]

  } else  if (tutorialstep == 4){

    # add use case for reference population
    dtPlots <- addHeaderLineBeforePlot(dtPlots, 'reference_population_iv',2,'Adding Reference Populations')
    dtPlots[,ReferenceScenario := as.character(ReferenceScenario)]
    dtPlots[PlotName %in% c('reference_population_iv','reference_population_po'),
            PlotName := 'reference']
    # shift reference scenarios in column ReferenceScenario and ad controll scenarios
    dtPlots[PlotName == 'reference',  ReferenceScenario := Scenario]
    dtPlots[PlotName == 'reference',  Scenario := gsub('reference','random',Scenario)]

    dtPlots[ReferenceScenario == 'reference_population_iv', DataGroupIds := 'IV']
    dtPlots[ReferenceScenario == 'reference_population_po', DataGroupIds := 'PO']

    dtPlots[ReferenceScenario == 'reference_population_iv', ScenarioCaptionName := 'random population simulation of a 1mg 5min iv administration']
    dtPlots[ReferenceScenario == 'reference_population_po', ScenarioCaptionName := 'random population simulation of a 5mg oral administration']
    dtPlots[PlotName == 'reference',PlotCaptionAddon := 'Reference population has doubled CYP3A4 liver ontogeny.']

  } else  if (tutorialstep == 5){
    # add use case for different time ranges
    # add timerange columns with newly created timeranges
    dtPlots[,TimeRange_h0_6 := as.character(NA)]
    dtPlots[,TimeRange_h0_6 := NA]
    dtPlots[,TimeRange_h6_24 := as.character(NA)]
    dtPlots[,TimeRange_h6_24 := NA]

    # ad a new plot line
    dtPlots <- copyLine(dtPlots,PlotName = 'individual_1_iv')
    dtPlots[PlotName == 'individual_1_iv', ]$PlotName[2] <- 'timeranges'
    dtPlots <- shiftLineToEnd(dtPlots,'timeranges')
    dtPlots <- addHeaderLineBeforePlot(dtPlots, 'timeranges',2,'Adding Plots with different Time Ranges')

    # add the timeranges
    dtPlots[PlotName == 'timeranges', TimeRange_h0_6 := 'c(0,6)']
    dtPlots[PlotName == 'timeranges', TimeRange_h6_24 := 'c(6,24)']
    # use facet type vs Time ranges
    dtPlots[PlotName == 'timeranges', FacetType := "Scenario vs TimeRange"]
    # allow different  scales of x- axis
    dtPlots[PlotName == 'timeranges', FacetScale := "free_x"]

  }


  xlsxWriteData(wb = wb, sheetName  = sheetName, dt = dtPlots)

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)


}


