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


  # # add a sheet with parameters to adjust application
  # wb <- openxlsx::loadWorkbook(projectConfiguration$applicationsFile)
  # dtPar <- xlsxReadData(wb,sheetName = wb$sheet_names[1])
  # # delete template rows
  # dtPar <- dtPar[FALSE]
  # dtPar <- rbind(dtPar,
  #                data.table( 'container Path' = 'Applications|iv 1 mg (5 min)|Application_1|ProtocolSchemaItem',
  #                            'parameter Name' = 'Dose',
  #                            value = 0,
  #                            units = 'mg'
  #                ),
  #                data.table( 'container Path' = 'Applications|iv 1 mg (5 min)|Application_1|ProtocolSchemaItem',
  #                            'parameter Name' = 'DosePerBodyWeight ',
  #                            value = 0.015,
  #                            units = 'mg/kg'
  #                )
  # )
  #
  # xlsxCloneAndSet(wb = wb, clonedSheet = wb$sheet_names[1],sheetName = '0.015mgPerBW',dt = dtPar)
  #
  #
  # openxlsx::saveWorkbook(wb, projectConfiguration$applicationsFile, overwrite = TRUE)
  #
  # wb <- openxlsx::loadWorkbook(projectConfiguration$applicationsFile)

  # set scenarios

  wb <- openxlsx::loadWorkbook(projectConfiguration$scenariosFile)
  dtScenario <- xlsxReadData(wb = wb,sheetName  = 'Scenarios')
  dtPops <- xlsxReadData(wb = projectConfiguration$populationsFile,sheetName  = 'Demographics')

  # delete the template lines
  dtScenario <- dtScenario[FALSE]

  dtScenario <- rbind(dtScenario,
                     data.table(
                       scenario_name = gsub('-','_',dtPop$populationName),
                       populationId =  dtPop$populationName,
                       readPopulationFromCSV = 1,
                       modelFile = 'iv 1 mg (5 min).pkml'),
                     fill = TRUE)

  xlsxWriteData(wb = wb, sheetName  = 'Scenarios', dt = dtScenario)
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
                        displayNameOutputs = "DrugX Plasma",
                        displayUnit ="µg/L"),
                     data.table(
                       outputPathId = 'Fraction',
                       outputPath =  "Organism|Kidney|Urine|DrugX|Fraction excreted to urine",
                       displayNameOutputs = "Fraction excreted to urine",
                       displayUnit =""),
                     fill = TRUE)

  xlsxWriteData(wb = wb, sheetName  = 'Outputs', dt = dtOutputs)


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


mockManualEditings.PlotBoxwhsiker <- function(projectConfiguration){

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)


  dt <- xlsxReadData(wb = wb,sheetName = 'PKParameter_Boxplot')

  dt[!is.na(scenario),plotCaptionAddon := '1mg iv application']
  dt[!is.na(scenario) & scenario != 'adults',referenceScenario := 'adults']
  dt[!is.na(scenario),plotName := outputPathIds]

  xlsxWriteData(wb = wb, sheetName  = 'PKParameter_Boxplot', dt = dt)


  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)


}



