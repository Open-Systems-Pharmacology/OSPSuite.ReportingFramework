
mockManualEditings.Cleanup <- function(projectConfiguration){

  # delete examples
  for (xlsfile in c(projectConfiguration$scenariosFile,
                    projectConfiguration$applicationsFile,
                    projectConfiguration$populationsFile,
                    projectConfiguration$individualsFile,
                    projectConfiguration$modelParamsFile)){

    # cleanup scenarios in Plots
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

mockManualEditings.DataDictionary <- function(projectConfiguration){

  # add all data files which are used in the project
  wb <- openxlsx::loadWorkbook(projectConfiguration$dataImporterConfigurationFile)

  dtDataFiles <- xlsxReadData(wb = wb,sheetName  = 'DataFiles')
  dtDataFiles <- dtDataFiles[c(1)]


  dtDataFiles <- rbind(dtDataFiles,
                       data.table(dataFile = file.path('..','..','Data','Cmax_ratio_Case2.csv'),
                                  dictionary = 'pkDictionary_Cmax_ratio_Case2',
                                  dataFilter = '',
                                  dataClass = DATACLASS$pkAggregated
                       )

  )

  xlsxWriteData(wb = wb, sheetName  = 'DataFiles', dt = dtDataFiles)

  # - pkDictionary
  pkDictionary <- xlsxReadData(wb = wb,sheetName = 'pkDictionary',skipDescriptionRow = FALSE)

  pkDictionary <- rbind(pkDictionary[1],
                        pkDictionary[targetColumn  %in% c("studyId","group","outputPathId","parameter",
                                                          "values","unit","errorType","minValue","maxValue","numberOfIndividuals")])

  pkDictionary[ targetColumn == 'values']$sourceColumn = "Geomean"
  pkDictionary[ targetColumn == 'parameter']$sourceColumn = "Parameter"
  pkDictionary[targetColumn == 'outputPathId', `:=`(
    sourceColumn = "",
    filter = 'TRUE',
    filterValue = '"Concentration"')]
  pkDictionary[targetColumn == 'numberOfIndividuals', `:=`(
    sourceColumn = "N",
    filter = '',
    filterValue = '')]
  pkDictionary[targetColumn == 'minValue', `:=`(
    sourceColumn = "CI90_LowerLimit",
    filter = '',
    filterValue = '')]
  pkDictionary[targetColumn == 'maxValue', `:=`(
    sourceColumn = "CI90_UpperLimit",
    filter = '',
    filterValue = '')]
  pkDictionary[targetColumn == 'unit', `:=`(
    sourceColumn = "",
    filter = 'TRUE',
    filterValue = '')]
  pkDictionary[targetColumn == 'group', `:=`(
    sourceColumn = "",
    filter = 'TRUE',
    filterValue = 'paste("Case2_pkRatio",Group,sep = "_")')]
  pkDictionary[targetColumn == 'errorType', `:=`(
    sourceColumn = "",
    filter = 'TRUE',
    filterValue = '"geomean | 90% CI"')]

  xlsxCloneAndSet(wb = wb,clonedSheet = 'pkDictionary',sheetName = 'pkDictionary_Cmax_ratio_Case2',dt = pkDictionary)

  openxlsx::saveWorkbook(wb, projectConfiguration$dataImporterConfigurationFile, overwrite = TRUE)
}


mockManualEditings.Population <- function(projectConfiguration){

    # add virtual population with in biometric ranges of observed data
    wb <- openxlsx::loadWorkbook(projectConfiguration$populationsFile)

    dtPops <- xlsxReadData(wb = wb,sheetName  = "Demographics" )
    dtPops <-   rbind(dtPops[0],
          data.table(
            populationName = c('adults','toddler','children','school-children','adolescents'),
            species = 'Human',
            population = 'European_ICRP_2002',
            numberOfIndividuals = 100,
            proportionOfFemales = 0.5,
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


mockManualEditings.Scenario <- function(projectConfiguration){

  # set scenarios

  wb <- openxlsx::loadWorkbook(projectConfiguration$scenariosFile)
  dtScenario <- xlsxReadData(wb = wb,sheetName  = 'Scenarios')
  dtPop <- xlsxReadData(wb = projectConfiguration$populationsFile,sheetName  = 'Demographics')

  # delete the template lines
  dtScenario <- dtScenario[FALSE]

  dtScenario <- rbind(dtScenario,
                     data.table(
                       scenario_name = gsub('-','_',dtPop$populationName),
                       populationId =  dtPop$populationName,
                       readPopulationFromCSV = 1,
                       modelFile = 'iv 1 mg (5 min).pkml'),
                     data.table(
                       scenario_name = paste0(gsub('-','_',dtPop$populationName),'_po'),
                       populationId =  dtPop$populationName,
                       readPopulationFromCSV = 1,
                       modelFile = 'po 3 mg (solution).pkml'),
                     fill = TRUE)

  xlsxWriteData(wb = wb, sheetName  = 'Scenarios', dt = dtScenario)

  # add PK Parameter sheets
  dtPK <- data.table(scenario_name = dtScenario$scenario_name,
                     pKParameter = "PK_Plasma, PK_Fraction" )

  xlsxWriteData(wb = wb,sheetName  = 'PKParameter',dt = dtPK)

  # cleanup scenarios in Plots
  dtOutputs <- xlsxReadData(wb = wb,sheetName  = 'OutputPaths')
  dtOutputs <- dtOutputs[outputPathId %in% c('Concentration','Fraction')]

  xlsxWriteData(wb = wb,sheetName  = 'OutputPaths',dt = dtOutputs)

  # save all sheets
  openxlsx::saveWorkbook(wb, projectConfiguration$scenariosFile, overwrite = TRUE)


}

mockManualEditings.outputPath <- function(projectConfiguration){

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  # outputpathids
  dtOutputs <- xlsxReadData(wb = wb,sheetName = 'Outputs')

  dtOutputs <- rbind(dtOutputs[1],
                      data.table(
                        outputPathId = 'Concentration',
                        outputPath =  "Organism|PeripheralVenousBlood|DrugX|Plasma (Peripheral Venous Blood)",
                        displayName = "drugX plasma concentration",
                        displayUnit ="µg/L"),
                     data.table(
                       outputPathId = 'Fraction',
                       outputPath =  "Organism|Kidney|Urine|DrugX|Fraction excreted to urine",
                       displayName = "drugX excreted to urine",
                       displayUnit =""),
                     fill = TRUE)

  xlsxWriteData(wb = wb, sheetName  = 'Outputs', dt = dtOutputs)

  # scenarioName
  dtScenario <- xlsxReadData(wb = wb,sheetName = 'Scenarios')
  dtScenario[,shortName := gsub(' po','',shortName)]
  dtScenario[,longName := gsub(' po','',longName)]

  xlsxWriteData(wb = wb, sheetName  = 'Scenarios', dt = dtScenario)

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

}

#' Title
#'
#' @param projectConfiguration
#'
#' @return
#' @export
#'
#' @examples
mockManualEditings.PKParameter <- function(projectConfiguration){


  # add all data files which are used in the project
  wb <- openxlsx::loadWorkbook(projectConfiguration$addOns$pKParameterFile)

  dtTemplate <- xlsxReadData(wb = wb,sheetName = 'Template')

  dtTemplate <- dtTemplate[c(1,which(dtTemplate$name %in% c("C_max","AUC_inf")))]
  dtTemplate[name %in% c("C_max","AUC_inf"),outputPathIds := "Concentration"]

  xlsxCloneAndSet(wb = wb,clonedSheet = 'Template', sheetName  = 'PK_Plasma', dt = dtTemplate)


  dtTemplate <- xlsxReadData(wb = wb,sheetName = 'Template')

  dtTemplate <- dtTemplate[c(1,which(dtTemplate$name %in% c("F_tEnd")))]
  dtTemplate[name %in% c("F_tEnd"),outputPathIds := "Fraction"]

  xlsxCloneAndSet(wb = wb,clonedSheet = 'Template', sheetName  = 'PK_Fraction', dt = dtTemplate)

  dtUserdef <-xlsxReadData(wb,sheetName = "Userdef PK Parameter")


  openxlsx::saveWorkbook(wb, projectConfiguration$addOns$pKParameterFile, overwrite = TRUE)
}


mockManualEditings.PlotBoxwhsiker1 <- function(projectConfiguration){

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  dt <- xlsxReadData(wb = wb,sheetName = 'PKParameter_Boxplot')
  dt$header[2] <- 'Use Case 1: Pediatric vs adult'

  if (!any(dt[!is.na(level)]$level == 2)){
    dt <- dt[c(1,2,seq(2,nrow(dt)))]
    dt$level[3] <- 2
  }
  dt$header[3] <- 'Boxwhisker plots'

  dt <- dt[!grep('_po$',scenario)]

  dt[!c(1,which(is.na(scenario))),plotCaptionAddon := 'Virtual pediatric simulations of 1mg iv application in comparison to an adult virtual population']
  dt[!c(1,which(is.na(scenario))),plotName := outputPathIds]
  dt[!c(1,which(is.na(scenario))),colorLegend := 'pediatric|adult']
  dt[plotName == outputPathIds &  scenario != 'adults',referenceScenario := 'adults']

  xlsxCloneAndSet(wb = wb,clonedSheet = 'PKParameter_Boxplot', sheetName  = 'PKParameter_Boxplot1', dt = dt)

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

}

mockManualEditings.PlotForest1 <- function(projectConfiguration){


  # PKParameter_ForestAbs
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  dt <- xlsxReadData(wb = wb,sheetName = 'PKParameter_ForestAbs1')
  dt$header[2] <- 'Forest Absolute Values with Variance'
  dt$level[2] <- 2

  dt[grep('_po$',scenario), scenarioGroup := 'PO']
  dt[!c(grep('_po$',scenario),1,2), scenarioGroup := 'IV']

  dt[!is.na(scenario),plotCaptionAddon := 'Pediatric populations simulations in comparison to adults']

  xlsxWriteData(wb = wb, sheetName  = 'PKParameter_ForestAbs1', dt = dt)
  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

  # PKParameter_ForestAbsCI
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  dt <- xlsxReadData(wb = wb,sheetName = 'PKParameter_ForestAbs1')
  dt$header[2] <- 'Forest AbsoulteValues with CI'

  xlsxCloneAndSet(wb,clonedSheet = 'PKParameter_Forest',sheetName = 'PKParameter_ForestAbsCI1',dt = dt)
  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)


  # PKParameter_ForestRatio
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  dt <- xlsxReadData(wb = wb,sheetName = 'PKParameter_ForestAbs1')
  dt$header[2] <- 'Forest Ratio with Variance'
  dt[scenarioGroup == 'PO', referenceScenario := 'adults_po']
  dt[scenarioGroup == 'IV', referenceScenario := 'adults']

  dt <- dt[!grep('adults',scenario)]

  xlsxCloneAndSet(wb,clonedSheet = 'PKParameter_Forest',sheetName = 'PKParameter_ForestRatio1',dt = dt)
  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

  # PKParameter_ForestRatioCI
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  dt <- xlsxReadData(wb = wb,sheetName = 'PKParameter_ForestRatio1')
  dt$header[2] <- 'Forest Ratio with CI'

  xlsxCloneAndSet(wb,clonedSheet = 'PKParameter_Forest',sheetName = 'PKParameter_ForestRatioCI1',dt = dt)
  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)
}





mockManualEditings.PlotBoxwhsiker2 <- function(projectConfiguration){

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  dt <- xlsxReadData(wb = wb,sheetName = 'PKParameter_Boxplot2')

  dt$header[2] <- 'Use Case 2: IV vs PO Crossover Study'

  if (!any(dt[!is.na(level)]$level == 2)){
    dt <- dt[c(1,2,seq(2,nrow(dt)))]
    dt$level[3] <- 2
  }
  dt$header[3] <- 'Boxwhisker plots'

  dt <- dt[!outputPathIds == 'Fraction']

  dt[!c(1,which(is.na(scenario))),plotName := outputPathIds]

  # set IV as reference
  dt[grep('_po$',scenario), referenceScenario := gsub('_po','',scenario)]
  dt[grep('_po$',scenario), plot_Ratio := 1]
  dt[!c(1,which(is.na(scenario))),plotCaptionAddon := 'Virtual population simulations of 3mg po application in comparison to 1mg iv simulation']
  dt[!c(1,which(is.na(scenario))),colorLegend := 'PO administration, 3mg|IV administration, 1mg']
  dt[!c(1,which(is.na(scenario))),pkParameters := 'AUC_inf']


  xlsxWriteData(wb = wb, sheetName  = 'PKParameter_Boxplot2', dt = dt)

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

}


mockManualEditings.PlotForest2 <- function(projectConfiguration){


  # PKParameter_ForestAbs
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  dt <- xlsxReadData(wb = wb,sheetName = 'PKParameter_ForestAbs2')
  dt$header[2] <- 'Forest Absolute Values with Variance'
  dt$level[2] <- 2

  dt[grep('_po$',scenario), referenceScenario := gsub('_po','',scenario)]
  dt[!is.na(scenario),plotCaptionAddon := 'Virtual population simulations of 3mg po application and 1mg iv simulation']

  dt[grep('_po$',scenario), scenarioGroup := 'PO']
  dt[!c(grep('_po$',scenario),1,2), scenarioGroup := 'IV']

  xlsxWriteData(wb = wb, sheetName  = 'PKParameter_ForestAbs2', dt = dt)
  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

  # PKParameter_ForestAbsCI
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  dt <- xlsxReadData(wb = wb,sheetName = 'PKParameter_ForestAbs2')
  dt$header[2] <- 'Forest Absolute Values with CI'

  xlsxCloneAndSet(wb,clonedSheet = 'PKParameter_Forest',sheetName = 'PKParameter_ForestAbsCI2',dt = dt)
  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

  # PKParameter_ForestRatio
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  dt <- xlsxReadData(wb = wb,sheetName = 'PKParameter_ForestAbs2')
  dt$header[2] <- 'Forest Ratio with Variance'
  dt[grep('_po$',scenario), referenceScenario := gsub('_po','',scenario)]

  dt <- dt[!is.na(level) | !is.na(referenceScenario)]
  dt[grep('_po$',scenario), scenarioGroup := 'pediatric']
  dt[grep('adults',scenario), scenarioGroup := '']

  xlsxCloneAndSet(wb,clonedSheet = 'PKParameter_Forest',sheetName = 'PKParameter_ForestRatio2',dt = dt)
  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

  # PKParameter_ForestRatioCI
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  dt <- xlsxReadData(wb = wb,sheetName = 'PKParameter_ForestRatio2')
  dt$header[2] <- 'Forest Ratio with CI'

  dt[plotName == 'PKForest',dataGroupId := paste('Case2_pkRatio',referenceScenario,sep = '_')]

  xlsxCloneAndSet(wb,clonedSheet = 'PKParameter_Forest',sheetName = 'PKParameter_ForestRatioCI2',dt = dt)
  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

}

