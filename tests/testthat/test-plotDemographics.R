# testProject was set up by setup.R
pkParameterDT <- loadPKParameter(projectConfiguration = projectConfiguration,
                                 scenarioList = scenarioList)

# prepare configtable
addDefaultDemographicPlots(projectConfiguration = projectConfiguration,
                                    sheetName = "DemographicPlots1",
                                    pkParameterDT = pkParameterDT,
                                    overwrite = TRUE)

wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

dt <- xlsxReadData(wb = wb, sheetName = 'DemographicPlots1')

# Update relevant fields for 'demographics' range plot test
dt[plotName == 'demographics', `:=`(
  scenarios = gsub('adults_iv, ', '', scenarios),
  referenceScenario = 'adults_iv',
  colorLegend = 'pediatrics|adults',
  plotCaptionAddon = 'Pediatric virtual populations in comparison to an adult virtual population'
)]

# Update relevant fields for 'pk' range plot test
dt <- dt[!grepl('pkparameter1', plotName)]

dt[plotName == 'pkparameter2', `:=`(
  scenarios = dt[plotName == 'demographics', scenarios],
  referenceScenario = dt[plotName == 'demographics', referenceScenario],
  colorLegend = dt[plotName == 'demographics', colorLegend],
  plotCaptionAddon = 'IV Application for pediatric virtual simulations in comparison to an adult virtual simulations'
)]


# Add Test for histogram
dtnew <-  dt[plotName == 'pkparameter2'] %>%
  separateAndTrim('scenarios') %>%
  setnames('scenario','scenarios')
dtnew[, `:=`(
  plotName = 'pkhistograms',
  parameterId_Bin = NA,
  modeOfBinning = NA,
  numberOfBins = NA,
  referenceScenario = gsub('iv','po',scenarios),
  colorLegend = 'IV application | PO application',
  plotCaptionAddon = 'comparison of IV and PO application'
  )]

dt <- rbind(dt,dtnew)

dtnew <-  dt[plotName == 'demographics']
dtnew[, `:=`(
  plotName = 'dmhistograms',
  parameterIds = 'weight,gender',
  parameterId_Bin = NA,
  modeOfBinning = NA,
  numberOfBins = NA
)]


dt <- rbind(dt[seq(1,which(dt$plotName == 'demographics'))],
            dtnew,
            dt[seq(which(dt$plotName == 'demographics')+1,nrow(dt))])

xlsxWriteData(wb = wb, sheetName  = 'DemographicPlots1', dt = dt)

openxlsx::saveWorkbook(wb, projectConfiguration$plotsFile, overwrite = TRUE)


test_that("demographic range plots", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  # createPlots
  plotList <- runPlot(
    nameOfplotFunction = "plotDemographics",
    projectConfiguration = projectConfiguration,
    configTableSheet = "DemographicPlots1",
    suppressExport = TRUE,
    plotNames = 'demographics',
    inputs = list(scenarioList = scenarioList,
                  asStepPlot = FALSE,
                  facetAspectRatio = 0.3,
                  colorVector = c(pediatrics = ospsuite.plots::colorMaps[[1]][[1]],
                                  adults = 'grey'))
  )

  expect_equal(length(plotList),6)

  vdiffr::expect_doppelganger(
    title = "weight_linear",
    fig = plotList$demographics_weight_linear
  )
})

test_that("PK range plots", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  plotList <- runPlot(
    nameOfplotFunction = "plotDemographics",
    projectConfiguration = projectConfiguration,
    configTableSheet = "DemographicPlots1",
    suppressExport = TRUE,
    plotNames = 'pkparameter2',
    inputs = list(scenarioList = scenarioList,
                  pkParameterDT = pkParameterDT,
                  asStepPlot = TRUE,
                  aggregationFlag = "GeometricStdDev",
                  colorVector = c(pediatrics = ospsuite.plots::colorMaps[[1]][[1]],
                                  adults = 'grey'))
  )

  expect_equal(length(plotList),4)

  expect_equal(nrow(plotList$pkparameter2_F_tEnd_A),21)

  vdiffr::expect_doppelganger(
    title = "F_tEnd_log",
    fig = plotList$pkparameter2_F_tEnd_log
  )
})


test_that("PK histograms plots", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  # createPlots
  plotList <- runPlot(
    nameOfplotFunction = "plotDemographics",
    projectConfiguration = projectConfiguration,
    configTableSheet = "DemographicPlots1",
    suppressExport = TRUE,
    plotNames = 'pkhistograms',
    inputs = list(scenarioList = scenarioList,
                  pkParameterDT = pkParameterDT,
                  colorVector = c( 'IV application' = 'red',
                                   'PO application' = 'green'),
                  plotAsFrequency = TRUE)
    )

  expect_equal(length(plotList),2)

  vdiffr::expect_doppelganger(
    title = "pkhistograms_F_tEnd_linear",
    fig = plotList$pkhistograms_F_tEnd_linear
  )
})


test_that("demographic histograms plots", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  # createPlots
  plotList <- runPlot(
    nameOfplotFunction = "plotDemographics",
    projectConfiguration = projectConfiguration,
    configTableSheet = "DemographicPlots1",
    suppressExport = TRUE,
    plotNames = 'dmhistograms',
    inputs = list(scenarioList = scenarioList,
                  pkParameterDT = pkParameterDT,
                  colorVector = c(pediatrics = ospsuite.plots::colorMaps[[1]][[1]],
                                  adults = 'grey'))
  )

  expect_equal(length(plotList),2)

  vdiffr::expect_doppelganger(
    title = "dmhistograms_gender_linear",
    fig = plotList$dmhistograms_gender_linear
  )
})







