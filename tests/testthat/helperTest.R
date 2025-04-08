#' Setup Test Directory for Testing
#'
#' This function sets up a test directory specifically for use in unit tests with
#' the `testthat` framework.
#'
#'
#' @return A list containing:
#'   - `projectConfiguration`: The configuration of the initialized project.
#'   - `scenarioList`: A list of scenarios initialized for the project.
#'   - `scenarioResults`: The results from running the scenarios.
#'
#' @export
setupTestDirectoryForTests <- function() {
  message('load test project')
  buildTestData(rootDirectory = NULL, verbose = FALSE, writeTestData = FALSE)
}

mockManualEditingsPlotPkForestTest <- function(projectConfiguration){
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)
  dt <- xlsxReadData(wb = wb,sheetName = 'PKParameter_ForestTest')

  dt <- dt[!c(grep('_iv$',scenario)),]
  dt[plotName == 'PKForest', `:=` (
    plotName ='pediatric',
    scenarioGroup = 'Pediatric',
    referenceScenario = 'adults_iv',
    plotCaptionAddon = 'Pediatric study.'
  )]
  dt[plotName == 'pediatric' & scenario == 'adults_po', `:=` (
    plotName ='pediatric',
    scenarioGroup = 'Adult',
    referenceScenario = ''
  )]

  dtnew = dt[plotName == 'pediatric']
  dtnew[, `:=` (
    plotName = 'pediatric_data',
    dataGroupId = paste0('1234_',scenario),
    facetScale = 'free'
  )]

  dt <- rbind(dt,dtnew)

  dtnew = dt[plotName == 'pediatric']
  dtnew[, `:=` (
    plotName = 'crossover',
    referenceScenario = gsub('_po','_iv',scenario),
    facetScale = 'free'
  )]

  dt <- rbind(dt,dtnew)

  dtnew = dt[plotName == 'crossover']
  dtnew[, `:=` (
    plotName = 'crossover_data',
    dataGroupId = paste0('1234_',gsub('_po','',scenario),'_ratio'),
    facetScale = 'free'
  )]

  dt <- rbind(dt,dtnew)


  xlsxWriteData(wb = wb, sheetName  = "PKParameter_ForestTest", dt = dt)

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

  return(invisible())
}

# adjust configtables in project directory
mockManualEditingsPlotBoxwhsikerTest <- function(projectConfiguration){
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  dt <- xlsxReadData(wb = wb,sheetName = 'PKParameter_BoxplotTest')

  dt <- dt[grep('C_max$',plotName),plotName := 'crossover']
  dt <- dt[!intersect(grep('_iv$',scenario),grep('crossover',plotName))]
  dt[plotName == 'crossover', `:=`(
    referenceScenario = gsub('_po','_iv',scenario),
    colorLegend = 'PO Administration| IV Administration',
    plotCaptionAddon = 'IV Application in comparison to PO administration',
    yScale = 'log',
    ylimit_log = 'c(0.08,NA)',
    plot_Absolute = 0,
    plot_Ratio = 1
  )]

  dt <- dt[grep('F_tEnd$',plotName),plotName := 'pediatric']
  dt <- dt[!intersect(grep('_iv$',scenario),grep('pediatric',plotName))]
  dt[plotName == 'pediatric', `:=`(
    referenceScenario = 'adults_po',
    colorLegend = 'pediatric| adult',
    plotCaptionAddon = 'Pediatric IV Application in comparison to adults',
    facetScale = 'free_y',
    plot_Ratio = 0
  )]
  dt[scenario == 'adults_po' & plotName == 'pediatric', referenceScenario := '']


  xlsxCloneAndSet(wb = wb,clonedSheet = 'PKParameter_Boxplot', sheetName  = 'PKParameter_BoxplotTest', dt = dt)

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)
  return(invisible())
}


mockManualEditingsPlotDemographicsTest <- function(projectConfiguration){
  # prepare configtable
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  dt <- xlsxReadData(wb = wb, sheetName = 'HistogramTest')

  # demographic histogram
  dt <- dt[!(intersect(grep('demographics',plotName),
                       grep('adults',scenario)))]
  dt[plotName == 'demographics', `:=`(
    parameterIds = 'weight,gender',
    referenceScenario = 'adults_iv',
    colorLegend = 'pediatrics|adults',
    xlimit_linear = 'c(0,NA)',
    plotCaptionAddon = 'Pediatric virtual populations in comparison to an adult virtual population'
  )]


  # Add Test for histogram onPKParameter
  dt <- dt[!grep('pkparameter1',plotName)]
  dt <- dt[!(intersect(grep('pkparameter2',plotName),
                         grep('iv',scenario)))]
  dt[plotName == 'pkparameter2', `:=`(
    referenceScenario = gsub('po','iv',scenario),
    colorLegend = 'IV application | PO application',
    plotCaptionAddon = 'Comparison of IV and PO application'
  )]

  xlsxWriteData(wb = wb, sheetName  = 'HistogramTest', dt = dt)

  dt <- xlsxReadData(wb = wb, sheetName = 'RangePlotTest')

  # Update relevant fields for 'demographics' range plot test
  dt[plotName == 'demographics', `:=`(
    scenarios = gsub('adults_iv, ', '', scenarios),
    referenceScenario = 'adults_iv',
    colorLegend = 'pediatrics|adults',
    ylimit_linear = 'c(0,NA)',
    modeOfBinning = BINNINGMODE$breaks,
    numberOfBins = 'seq(0,18)',
    plotCaptionAddon = 'Pediatric virtual populations in comparison to an adult virtual population'
  )]

  # Update relevant fields for 'pk' range plot test
  dt <- dt[!grepl('pkparameter1', plotName)]

  dt[plotName == 'pkparameter2', `:=`(
    scenarios = dt[plotName == 'demographics', scenarios],
    referenceScenario = dt[plotName == 'demographics', referenceScenario],
    colorLegend = dt[plotName == 'demographics', colorLegend],
    facetScale = 'free_y',
    plotCaptionAddon = 'IV Application for pediatric virtual simulations in comparison to an adult virtual simulation'
  )]


  xlsxWriteData(wb = wb, sheetName  = 'RangePlotTest', dt = dt)

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

  return(invisible())
}
mockManualEditingsPlotSensitivityTest <- function(projectConfiguration,
                                                  sensitivityScenario,
                                                  sensitivitySheet){
  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  sheetName = 'SensitivityPlots'
  dt <- xlsxReadData(wb = wb,sheetName = sheetName)
  dt <- dt[1]

  dt <- rbind(dt,
              data.table(level = 1,header = 'Sensitivity Analysis'),
              data.table(plotName = 'sensitivity_all',
                         scenario = sensitivityScenario,
                         outputPathIds = 'Plasma',
                         sensitivityParameterSheet = sensitivitySheet,
                         pKParameters = "C_max, AUC_inf",
                         threshold = 1,
                         plotCaptionAddon = 'Displayed are all model parameters.'),
              fill = TRUE)
  dt <- rbind(dt,
              dt[plotName == 'sensitivity_all'] %>%
                dplyr::mutate(plotName = 'sensitivity_90',
                              threshold = 0.9,
                              plotCaptionAddon = 'Displayed are  model parameters participating to a total of `90` percent of the sensitivity .'))

  xlsxWriteData(wb = wb, sheetName  = sheetName, dt = dt)

  openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)

}
