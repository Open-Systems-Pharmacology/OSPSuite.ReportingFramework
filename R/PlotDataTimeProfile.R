#' @title plotData for Timeprofile plotData
#' @docType class
#' @description An object to collect plotData and other informations to create a timeprofile plot
#' @export
PlotDataTimeProfile <- R6::R6Class( # nolint
  "RmdContainer",
  inherit = ospsuite.utils::Printable,
  cloneable = TRUE,
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @template projectConfig
    #' @param onePlotConfig plotconfiguration for one plot
    #'
    #' @returns RmdContainer object
    initialize = function(projectConfiguration = projectConfiguration,
                          onePlotConfig = onePlotConfig) {

      private$.dtOutputPaths <- getOutputPathIds(projectConfiguration = projectConfiguration)
      private$.dtDataGroups <- getDataGroups(projectConfiguration = projectConfiguration)

      private$.configTable <- data.table::setDT(onePlotConfig)[,scenarioIndex:= .I]
    },
    #' Collects all observed and simulated data needed for this plots
    #'
    #' @template projectConfig
    #' @template observedDataDT
    #'
    #' @return self
    collectData = function(projectConfiguration,
                                dataObserved) {
      simulatedData <-
        private$loadSimulatedResults(
          projectConfiguration = projectConfiguration
        ) %>%
        dplyr::mutate(dataType = "simulated")

      dataObservedForPlot <-
        private$filterObservedDataForPlot(
          dataObserved = dataObserved
        )

      plotData <- rbind(
        simulatedData,
        dataObservedForPlot,
        fill = TRUE
      )

      return(self)
    }
  ),
  active = list(
    #' @field data data For Plot
    data = function() {
        private$data
    },
    #' @field digitsOfSignificance digits for significance in table display
    configTable = function(value) {
      if (missing(value)) {
        value <- private$.digitsOfSignificance
      }
      private$.configTable <- value
      return(value)
    }
  ),
  private = list(
    # data to plot
    .data = NULL,
    # output path configuration
    .dtOutputPaths = NULL,
    # data group configuration
    .dtDataGroups = NULL,
    # configuration table for this plot
    .configTable = NULL,
    # unit information
    .dtUnit = data.table(),
    # function to load simulated data
    loadSimulatedResultsForTimeProfilePanelPlot = function(projectConfiguration) {
      simulatedData <- data.table()
      dtUnit <- data.table()

      simulatedResults <-
        esqlabsR::loadScenarioResults(
          scenarioNames = c(
            onePlotConfig$Scenario,
            onePlotConfig[!is.na(ReferenceScenario)]$ReferenceScenario
          ),
          resultsFolder = file.path(projectConfiguration$outputFolder, "SimulationResults")
        )

      # use index and not scenario_name,
      # it may be necessary to duplicate scenarios, e.g. to plot with different data,or units
      for (scenarioIndex in seq_len(nrow(self$configTable))) {
        configList <- as.list(onePlotConfig[scenarioIndex, ])

        timeprofile <-
          private$getSimulatedTimeprofile(
            simulatedResult = simulatedResults[[configList$Scenario]],
            targetTimeUnit = configList$TimeUnit,
            outputs = gsub("[()]", "", splitInputs(configList$OutputPathIds)),
            timeOffset = as.double(configList$TimeOffset_Reference) +
              as.double(configList$TimeOffset)
          ) %>%
          dplyr::mutate(scenarioIndex = scenarioIndex)


        simulatedData <- rbind(
          simulatedData,
          timeprofile
        )

        if (!is.na(configList$ReferenceScenario) && as.logical(configList$ReferenceScenario)) {
          simulatedData <- rbind(
            simulatedData,
            getSimulatedTimeprofile(
              projectConfiguration = projectConfiguration,
              simulatedResult = simulatedResults[[configList$ReferenceScenario]],
              targetTimeUnit = configList$TimeUnit,
              outputs = gsub("[()]", "", splitInputs(configList$OutputPathIds)),
              timeOffset = as.double(configList$TimeOffset)
            ) %>%
              dplyr::mutate(scenarioIndex = scenarioIndex)
          )
        }
      }
      return(simulatedData)
    },
    getSimulatedTimeprofile = function(projectConfiguration,
                                        simulatedResult,
                                        outputs,
                                        targetTimeUnit,
                                        timeOffset) {
      dtOutputPaths <- getOutputPathIds(projectConfiguration)
      dt <- ospsuite::simulationResultsToDataFrame(
        simulationResults = simulatedResult$results,
        quantitiesOrPaths = unique(private$.dtOutputPaths[OutputPathId %in% outputs]$OutputPath)
      ) %>%
        data.table::setDT()

      # unit conversion
      dt[, Time := toUnit(
        quantityOrDimension = "Time",
        values = as.double(Time),
        targetUnit = targetTimeUnit,
      )]
      dt[,xUnit:= targetTimeUnit]

      dtUnit <- dt %>%
        dplyr::select("paths", "dimension", "unit", "molWeight") %>%
        unique() %>%
        merge(
          private$.dtOutputPaths %>%
            dplyr::select("OutputPathId", "DisplayUnit", "OutputPath"),
          by.x = "paths",
          by.y = "OutputPath"
        )
      for (iRow in seq_len(nrow(dtUnit))){
        dtUnit$unitFactor[iRow] = toUnit(
          quantityOrDimension = dtUnit$dimension[iRow],
          values = 1,
          sourceUnit = dtUnit$unit[iRow],
          targetUnit = dtUnit$DisplayUnit[iRow],
          molWeight = dtUnit$molWeight[iRow],
          molWeightUnit = "g/mol"
        )
      }

      dt <- dt %>%
        merge(dtUnit[, c("paths", "OutputPathId", "unitFactor",'DisplayUnit')] ,
              by = "paths"
        )
      dt[, simulationValues := simulationValues * unitFactor]

      if (dplyr::n_distinct(dt$IndividualId) > 1) {
        stop("aggregate")
      } else {
        dt <- dt %>%
          dplyr::select(c("OutputPathId", "Time", "xUnit","simulationValues",'DisplayUnit')) %>%
          data.table::setnames(
            old = c("Time", "simulationValues",'DisplayUnit'),
            new = c("xValues", "yValues",'yUnit')
          )
      }

      dt[, xValues := xValues - timeOffset]

      private$upDateDtUnit(dtUnit %>% dplyr::select(c(
        "OutputPathId", "dimension", "DisplayUnit", "molWeight"
      )))

      return(dt)
    },
    # update unit information
    upDateDtUnit = function(dtUnit){
      private$.dtUnit <- rbind(
        private$.dtUnit,
        dtUnit
      ) %>%
        unique()
    },
    #' load observed data used in this panel
    filterObservedDataForPlot = function(dataObserved) {
      observedDataPanel <- data.table()

      for (scenarioIndex in seq_len(nrow(self$configTable))) {
        configList <- as.list(self$configTable[scenarioIndex, ])

        dataGroupIds <- splitInputs(configList$DataGroupIds)
        # load observed data
        if (!is.null(dataGroupIds)) {
          observedDataTmp <-
            dataObserved[group %in% dataGroupIds &
                           OutputPathId %in% gsub("[()]", "", splitInputs(configList$OutputPathIds))] %>%
            dplyr::select(dplyr::any_of(getColumnsForColumnType(dataObserved,
                                                                columnTypes = c("identifier", "timeprofile")))) %>%
            dplyr::mutate(scenarioIndex = scenarioIndex)

          observedDataTmp[, xValues := toUnit(
            quantityOrDimension = "Time",
            sourceUnit = observedDataTmp$xUnit[1],
            values = as.double(xValues),
            targetUnit = configList$TimeUnit,
          )]
          observedDataTmp[,xUnit := configList$TimeUnit]

          dtUnitObserved <- observedDataTmp %>%
            dplyr::select(c("OutputPathId", "yUnit")) %>%
            unique() %>%
            merge(private$.dtUnit,
                  by = c("OutputPathId")
            )
          for (iRow in seq_len(nrow(dtUnitObserved))){
            dtUnitObserved$unitFactor[iRow] = toUnit(
              quantityOrDimension = dtUnitObserved$dimension[iRow],
              values = 1,
              sourceUnit = dtUnitObserved$yUnit[iRow],
              targetUnit = dtUnitObserved$DisplayUnit[iRow],
              molWeight = dtUnitObserved$molWeight[iRow],
              molWeightUnit = "g/mol"
            )
          }

          observedDataTmp <- observedDataTmp %>%
            merge(dtUnitObserved %>%
                    dplyr::select(c('OutputPathId','DisplayUnit','unitFactor')),
                  by = "OutputPathId"
            )
          observedDataTmp[, yValues := yValues * unitFactor]

          observedDataTmp <- observedDataTmp %>%
            dplyr::select(-c('yUnit','unitFactor')) %>%
            data.table::setnames('DisplayUnit','yUnit')

          observedDataPanel <-
            rbind(observedDataPanel,
                  observedDataTmp)
        }
      }

      return(observedDataPanel)
    }


  )
)
