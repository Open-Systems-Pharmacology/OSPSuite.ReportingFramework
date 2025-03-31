
mockManualEditings.Cleanup <- function(projectConfiguration){

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

  wb <- openxlsx::loadWorkbook(projectConfiguration$scenariosFile)
  # set scenarios
  dtScenario <- xlsxReadData(wb = wb,sheetName  = 'Scenarios')
  dtPop <- xlsxReadData(wb = projectConfiguration$populationsFile,sheetName  = 'Demographics')

  dtScenario <- rbind(dtScenario,
                     data.table(
                       scenario_name = paste0(gsub('-','_',dtPop$populationName),'_iv'),
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

mockManualEditings.displayNames <- function(projectConfiguration){


  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  # scenarios

  dtScenario <- xlsxReadData(wb = wb,sheetName = 'Scenarios')
  dtScenario[,shortName := gsub(' iv','',gsub(' po','',shortName))]
  dtScenario[,longName := gsub(' iv','',gsub(' po','',longName))]

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

mockManualEditings.PlotForest1 <- function(projectConfiguration,sheetName){

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)
  switch(sheetName,
         'PKParameter_ForestAbs1' = {
           dt <- xlsxReadData(wb = wb,sheetName = sheetName)

           dt$header[2] <- 'Forest aggregated absolute values'
           dt$level[2] <- 2

           dt <- dt[!c(grep('_po$',scenario)),]

           dt[grep('adults',scenario), scenarioGroup := 'Adult']
           dt[!c(grep('adults',scenario),1,2), scenarioGroup := 'Pediatric']

           dt[!is.na(scenario),plotCaptionAddon := 'Pediatric populations simulations in comparison to adults']
           xlsxWriteData(wb = wb, sheetName  = sheetName, dt = dt)

         },
         'PKParameter_ForestAbsPE1' = {
           dt <- xlsxReadData(wb = wb,sheetName = 'PKParameter_ForestAbs1')
           dt$header[2] <- 'Forest point estimates of absolute values'

           xlsxCloneAndSet(wb,clonedSheet = 'PKParameter_Forest',sheetName = sheetName,dt = dt)
         },
         'PKParameter_ForestRatioPE1' = {
           dt <- xlsxReadData(wb = wb,sheetName = 'PKParameter_ForestAbsPE1')
           dt$header[2] <- 'Forest point estimates of ratios'

           dt <- dt[is.na(scenarioGroup) | scenarioGroup != 'Adult']
           dt[scenarioGroup == 'Pediatric', referenceScenario := 'adults']

           xlsxCloneAndSet(wb,clonedSheet = 'PKParameter_Forest',sheetName = sheetName,dt = dt)

         }
  )

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

}

mockManualEditings.Demographics2 <- function(projectConfiguration){

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  dt <- xlsxReadData(wb = wb,sheetName = 'DemographicPlots2')

  dt[plotName == 'demographics', scenarios := gsub('adults, ','',scenarios)]
  dt[plotName == 'demographics', referenceScenario := 'adults']
  dt[plotName == 'demographics', colorLegend := 'pediatrics|adults']

  xlsxWriteData(wb = wb, sheetName  = 'DemographicPlots2', dt = dt)

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


mockManualEditings.PlotForest2 <- function(projectConfiguration,sheetName){

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)
  switch(sheetName,
         'PKParameter_ForestAbs2' = {
           dt <- xlsxReadData(wb = wb,sheetName = sheetName)

           dt$header[2] <- 'Forest aggregated absolute values'
           dt$level[2] <- 2

           dt[grep('_po$',scenario), scenarioGroup := 'PO']
           dt[!c(grep('_po$',scenario),1,2), scenarioGroup := 'IV']

           dt[!is.na(scenario),plotCaptionAddon := 'Virtual population simulations of 3mg po application and 1mg iv simulation']
           xlsxWriteData(wb = wb, sheetName  = sheetName, dt = dt)

         },
         'PKParameter_ForestRatio2' = {
           dt <- xlsxReadData(wb = wb,sheetName = 'PKParameter_ForestAbs2')

           dt$header[2] <- 'Forest aggregated ratios'
           dt$level[2] <- 2

           dt[grep('_po$',scenario), referenceScenario := gsub('_po','',scenario)]

           dt <- dt[!is.na(level) | !is.na(referenceScenario)]
           dt[grep('_po$',scenario), scenarioGroup := 'pediatric']
           dt[grep('adults',scenario), scenarioGroup := '']

           xlsxCloneAndSet(wb,clonedSheet = 'PKParameter_Forest',sheetName = 'PKParameter_ForestRatio2',dt = dt)

         },
         'PKParameter_ForestAbsPE2' = {
           dt <- xlsxReadData(wb = wb,sheetName = 'PKParameter_ForestAbs2')
           dt$header[2] <- 'Forest point estiamtes of absolute values'

           xlsxCloneAndSet(wb,clonedSheet = 'PKParameter_Forest',sheetName = sheetName,dt = dt)
         },
         'PKParameter_ForestRatioPE2' = {
           dt <- xlsxReadData(wb = wb,sheetName = 'PKParameter_ForestRatio2')
           dt$header[2] <- 'Forest point estiamtes of ratios'
           dt[grep('_po$',scenario), dataGroupId := paste0('Case2_pkRatio_',gsub('_po','',scenario))]

           xlsxCloneAndSet(wb,clonedSheet = 'PKParameter_Forest',sheetName = sheetName,dt = dt)

         }
  )

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

}

