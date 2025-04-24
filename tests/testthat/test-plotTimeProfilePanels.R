# testProject was set up by setup.R

scenarioResultsInd <- runAndSaveScenarios(projectConfiguration = projectConfiguration,
                                       scenarioList = scenarioListInd,
                                       simulationRunOptions = SimulationRunOptions$new(
                                         showProgress = FALSE
                                       ),
                                       withResimulation = FALSE)

dataObserved <- readObservedDataByDictionary(projectConfiguration)
dataObserved <- rbind(dataObserved,
                      aggregateObservedDataGroups(dataObserved = dataObserved,
                                                   groups = '1234_adults_iv'),
                      aggregateObservedDataGroups(dataObserved = dataObserved,
                                                   groups = '1234_adults_po',
                                                   aggregationFlag = 'Percentile'),
                      fill = TRUE
)


test_that("Default Config For Histograms", {
  addDefaultConfigForTimeProfilePlots(projectConfiguration = projectConfiguration,
                                                 sheetName = "TimeProfileTest",
                                                 dataObserved = dataObserved,
                                                 overwrite = TRUE)

  wb <- openxlsx::loadWorkbook(projectConfiguration$plotsFile)

  expect_contains(wb$sheet_names,'TimeProfileTest')
})

# add configuration for testcases
mockManualEditingsPlotTimeProfileTest(projectConfiguration = projectConfiguration)

test_that("Time profiles of individual scenarios", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  plotList <-
    runPlot(nameOfplotFunction = 'plotTimeProfiles',
            configTableSheet = 'TimeProfileTest',
            projectConfiguration = projectConfiguration,
            suppressExport = TRUE,
            plotNames = c('Individuals_withData','Individuals_withoutData'),
            inputs = list(
              scenarioResults = scenarioResultsInd,
              dataObserved = dataObserved
            )
    )

  expect_equal(length(plotList),2)

  for (pName in names(plotList)){
    vdiffr::expect_doppelganger(
      title = pName,
      fig = plotList[[pName]]
    )
  }
})

test_that("Predicted vs observed of individual scenarios", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  plotList <-
    runPlot(nameOfplotFunction = 'plotTimeProfiles',
            configTableSheet = 'TimeProfileTest',
            projectConfiguration = projectConfiguration,
            suppressExport = TRUE,
            plotNames = c('Individuals_withData_pvo'),
            inputs = list(
              scenarioResults = scenarioResultsInd,
              dataObserved = dataObserved
            )
    )

  for (pName in names(plotList)){
    vdiffr::expect_doppelganger(
      title = pName,
      fig = plotList[[pName]]
    )
  }

})


test_that("Time profiles of virtual twin scenarios", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  plotList <-
    runPlot(nameOfplotFunction = 'plotTimeProfiles',
            configTableSheet = 'TimeProfileTest',
            projectConfiguration = projectConfiguration,
            suppressExport = TRUE,
            plotNames = c('VirtualTwin',
                          'VirtualTwin_withData_all',
                          'VirtualTwin_withData_selected',
                          'VirtualTwin_withReferenceInd'),
            inputs = list(
              scenarioResults = scenarioResultsInd,
              dataObserved = dataObserved
            )
    )

  expect_equal(length(plotList),4)

  for (pName in names(plotList)){
    vdiffr::expect_doppelganger(
      title = pName,
      fig = plotList[[pName]]
    )
  }


  expect_error(
    runPlot(nameOfplotFunction = 'plotTimeProfiles',
            configTableSheet = 'TimeProfileTest',
            projectConfiguration = projectConfiguration,
            suppressExport = TRUE,
            plotNames = 'VirtualTwin_withReferencePop',
            inputs = list(
              scenarioResults = c(scenarioResults,
                                  scenarioResultsInd),
              dataObserved = dataObserved
            )
    )
  )

})

test_that("Predicted vs observed of virtual twin scenarios", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  plotList <-
    runPlot(nameOfplotFunction = 'plotTimeProfiles',
            configTableSheet = 'TimeProfileTest',
            projectConfiguration = projectConfiguration,
            suppressExport = TRUE,
            plotNames = c('VirtualTwin_withData_selected_pvo'),
            inputs = list(
              scenarioResults = scenarioResultsInd,
              dataObserved = dataObserved
            )
    )

  for (pName in names(plotList)){
    vdiffr::expect_doppelganger(
      title = pName,
      fig = plotList[[pName]]
    )
  }

})

test_that("Time profiles with populations", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  plotList <-
    runPlot(nameOfplotFunction = 'plotTimeProfiles',
            configTableSheet = 'TimeProfileTest',
            projectConfiguration = projectConfiguration,
            suppressExport = TRUE,
            plotNames = c('Pop_withoutData',
                          'Pop_withIndividualData',
                          'Pop_withAggregatedData',
                          'Pop_withReference'),
            inputs = list(
              scenarioResults = scenarioResults,
              dataObserved = dataObserved
            )
    )


  for (pName in names(plotList)){
    vdiffr::expect_doppelganger(
      title = pName,
      fig = plotList[[pName]]
    )
  }

  expect_error(runPlot(nameOfplotFunction = 'plotTimeProfiles',
          configTableSheet = 'TimeProfileTest',
          projectConfiguration = projectConfiguration,
          suppressExport = TRUE,
          plotNames = c('Pop_withReference_2'),
          inputs = list(
            scenarioResults = scenarioResults,
            dataObserved = dataObserved
          ))
  )

  expect_error(runPlot(nameOfplotFunction = 'plotTimeProfiles',
                       configTableSheet = 'TimeProfileTest',
                       projectConfiguration = projectConfiguration,
                       suppressExport = TRUE,
                       plotNames = c('Pop_withAggregatedData_Percentiles'),
                       inputs = list(
                         scenarioResults = scenarioResults,
                         dataObserved = dataObserved
                       ))
  )


})


test_that("Predicted vs observed of populations", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  plotList <-
    runPlot(nameOfplotFunction = 'plotTimeProfiles',
            configTableSheet = 'TimeProfileTest',
            projectConfiguration = projectConfiguration,
            suppressExport = TRUE,
            plotNames = 'Pop_withAggregatedData_pvo',
            inputs = list(
              scenarioResults = scenarioResults,
              dataObserved = dataObserved
            )
    )

  for (pName in names(plotList)){
    vdiffr::expect_doppelganger(
      title = pName,
      fig = plotList[[pName]]
    )
  }

})

test_that("Time profiles vs time range", {

  plotList <-
    runPlot(nameOfplotFunction = 'plotTimeProfiles',
            configTableSheet = 'TimeProfileTest',
            projectConfiguration = projectConfiguration,
            suppressExport = TRUE,
            plotNames = c('Pop_withTimeRanges'),
            inputs = list(
              scenarioResults = scenarioResults,
              dataObserved = dataObserved
            )
    )

  for (pName in names(plotList)){
    vdiffr::expect_doppelganger(
      title = pName,
      fig = plotList[[pName]]
    )
  }

})

test_that("Predicted vs observed vs time range", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  plotList <-
    runPlot(nameOfplotFunction = 'plotTimeProfiles',
            configTableSheet = 'TimeProfileTest',
            projectConfiguration = projectConfiguration,
            suppressExport = TRUE,
            plotNames = c('Pop_withTimeRanges_pvo'),
            inputs = list(
              scenarioResults = scenarioResults,
              dataObserved = dataObserved
            )
    )

  for (pName in names(plotList)){
    vdiffr::expect_doppelganger(
      title = pName,
      fig = plotList[[pName]]
    )
  }

})

