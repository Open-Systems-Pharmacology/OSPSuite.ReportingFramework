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

      private$.data  <- rbind(
        simulatedData,
        dataObservedForPlot,
        fill = TRUE
      )

      return(invisible())
    },
    #' setfactors to plot
    #'
    #' @param plotData `data.table`  with all data
    #' @param identifierColumns identifier columns which define sort order
    #' @template projectConfig
    setOrderAndFactors = function(identifierColumns){

      private$.data$OutputPathId <- factor(private$.data$OutputPathId,
                                      levels = levels(private$.dtOutputPaths$OutputPathId),
                                      ordered = TRUE)


      private$.data$group <- factor(private$.data$group,
                               levels = levels(private$.dtDataGroups$group),
                               ordered = TRUE)

      private$.data %>% data.table::setorderv(intersect(
        unique('dataType',identifierColumns),
        names(private$.data)))

      return(invisible())

    },
    #' Creates PlotId for each scenario outputs group
    #'
    addDtCaption = function() {

      configTable = data.table::copy(self$configTable) %>%
        dplyr::select('scenarioIndex', 'OutputPathIds', 'ScenarioCaptionName')

      dtCaption <-
        tidyr::separate_rows(data.table::setDF(configTable),
                             OutputPathIds,
                             sep = ",\\s*|(?<=\\)),\\s*|\\s(?=\\()")  %>%
        data.table::setDT() %>%
        data.table::setnames('OutputPathIds','OutputPathId')


      # set Plot ID as accumulated sum over difference off number of open and closed brackets
      dtCaption[, nBracketOpen := cumsum(grepl("\\(", OutputPathId)),]
      dtCaption[, nBracketClosed := cumsum(grepl("\\)", OutputPathId)),]
      dtCaption[, countAdd := nBracketClosed-nBracketOpen+1,]
      dtCaption[, PlotId := cumsum(data.table::shift(countAdd,fill = 1)),]
      dtCaption[, PlotTag := toupper(letters[PlotId]),]

      dtCaption$PlotTag <-
        factor(dtCaption$PlotTag,
               ordered = TRUE,
               levels = unique(dtCaption$PlotTag))

      # add display name for caption
      dtCaption[, OutputPathId := trimws(gsub("[()]",'',OutputPathId)),]

      dtCaption <- dtCaption %>%
        merge(private$.dtOutputPaths %>%  dplyr::select(OutputPathId,'DisplayName'),
              by = 'OutputPathId') %>%
        data.table::setnames('DisplayName','outputDisplayName') %>%
        dplyr::select('scenarioIndex','OutputPathId','PlotTag','ScenarioCaptionName','outputDisplayName')

      private$.dtCaption <- dtCaption


      # add plot ID
      private$.data <- private$.data %>%
        merge(dtCaption %>%  dplyr::select('scenarioIndex','OutputPathId','PlotTag'),
              by = c('scenarioIndex','OutputPathId'))


      return(invisible())
    },
    #' add colorIndex column
    #'
    #' @template projectConfig
    #' @template plotDataList
    setColorIndex = function(){

      countGroups <-
        private$.data[, .(
          nOutput = dplyr::n_distinct(OutputPathId),
          nGroup = dplyr::n_distinct(group, na.rm = TRUE)
        ), by = 'PlotTag'][, .(nOutput = max(nOutput),
                               nGroup = max(nGroup))]

      # One data group more the one output
      if (countGroups$nOutput >= 1 & countGroups$nGroup == 1){

        private$.data[,colorIndex := OutputPathId]
        private$.data$colorIndex = factor(private$.data$colorIndex,
                                                  levels = private$.dtOutputPaths$OutputPathId,
                                                  labels = private$.dtOutputPaths$DisplayName,
                                                  ordered = TRUE)

        scaleVectors <- generateColorScaleVectors(
          dt = private$.dtOutputPaths[DisplayName %in% levels(private$.data$colorIndex)],
          index = 'DisplayName')

        private$.tpLabels <- list(colour_ggnewscale_1 = 'simulated timeprofile',
                                  fill_ggnewscale_1 = 'simulated timeprofile',
                                  fill = 'observed data',
                                  colour = 'observed data')


      }

      private$.scaleVectors <- scaleVectors

      return(invisible())

    },
    #' creates a caption text
    #' @param yScale scale of Y axis
    #' @return  `character` with caption text
    getCaptionTimeProfile = function(yScale){

      # generate captiontext
      captiontext = paste('Concentration-time profiles of',
                          pasteFigureTags(private$.dtCaption,captionColumn = 'outputDisplayName'),
                          'for',
                          pasteFigureTags(private$.dtCaption,captionColumn = 'ScenarioCaptionName'),
                          'on a',ifelse(yScale == 'linear','linear','logarithmic'),
                          'y-scale.',
                          ifelse (!is.na(private$.configTable$PlotCaptionAddon[1]),
                                  private$.configTable$PlotCaptionAddon[1],''),
                          'timeRangeTag'
      )

      return(captiontext)

    },
    #' constructs footnote lines for aggregated data and data references
    #'
    #' @template projectConfig
    #' @template observedData
    #'
    #' @return vector of characters, each entry is one footnote line
    #' @export
    getFootNoteLines = function(){
      footnoteLines = c()
      if (any(private$.data$dataType == 'observed')) {
        if (any(private$.data[dataType == 'observed']$DataClass == DATACLASS$tpAggregated)){
          stop('not implemented yet')
        }
      }

      # filter used data
      dtDataReference <- dataObserved %>%
        dplyr::select('group') %>%
        unique() %>%
        merge(private$.dtDataGroups,
              by = c('group'))

      dtDataReference <- dtDataReference[!is.na(Reference)]

      if (nrow(dtDataReference) > 0) {
        footnoteLines = c(footnoteLines,
                          paste0(
                            'Data source: [',
                            paste(dtDataReference$Reference %>%
                                    unique(),
                                  collapse = ', '),
                            ']   '
                          ))
      }
      return(footnoteLines)
    }
  ),
  active = list(
    #' @field data data For Plot
    data = function() {
        private$.data
    },
    #' @field configTable configuration table for one plot
    configTable = function(value) {
      if (missing(value)) {
        value <- private$.configTable
      }
      private$.configTable <- value
      return(value)
    },
    #' @field dtCaption `digits for significance in table display`data.table`with caption information`
    dtCaption = function() {
      private$.dtCaption
    },
    #' @field scaleVectors list with scaling vectors to manually scale aestehtics
    scaleVectors = function() {
      private$.scaleVectors
    },
    #' @field tpLabelSimulatedMean label for simulated mean
    tpLabelSimulatedMean = function() {
      private$.tpLabelSimulatedMean
    },
    #' @field tpLabelSimulatedRange label for simulated range
    tpLabelSimulatedRange = function() {
      private$.tpLabelSimulatedRange
    },
    #' @field tpLabelObserved label for observed data
    tpLabelObserved = function() {
      private$.tpLabelObserved
    },
    #' @field dtCaption list of time profile labels to add on ggplot
    tpLabels = function() {
      private$.tpLabels
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
    # data.table with caption information
    .dtCaption = NULL,
    # list with scaling vectors to manually scale aesthetics
    .scaleVectors = list(),
    # label for simulated mean,
    .tpLabelSimulatedMean = NULL,
    # label for simulated range
    .tpLabelSimulatedRange = NULL,
    # label for observed data
    .tpLabelObserved = 'observed data',
    # list of time profile labels
    .tpLabels = list(),
    # function to load simulated data
    loadSimulatedResults = function(projectConfiguration) {
      simulatedData <- data.table()

      simulatedResults <-
        esqlabsR::loadScenarioResults(
          scenarioNames = c(
            private$.configTable$Scenario,
            private$.configTable[!is.na(ReferenceScenario)]$ReferenceScenario
          ),
          resultsFolder = file.path(projectConfiguration$outputFolder, "SimulationResults")
        )

      # use index and not scenario_name,
      # it may be necessary to duplicate scenarios, e.g. to plot with different data,or units
      for (scenarioIndex in seq_len(nrow(self$configTable))) {
        configList <- as.list(private$.configTable[scenarioIndex, ])

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

      checkmate::assertTRUE(dplyr::n_distinct(simulatedData$DataClass) == 1,
                            .var.name = 'use only one DataClass in one Plot')

      return(simulatedData)
    },
    #' loads simulated result and convert for plotting
    #'
    #' if population scenario, time profiles are aggregated
    #' Units are converted to desired units
    getSimulatedTimeprofile = function(simulatedResult,
                                        outputs,
                                        targetTimeUnit,
                                        timeOffset) {
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
          ) %>%
          dplyr::mutate(DataClass = DATACLASS$tpIndividual)

        private$.tpLabelSimulatedMean = 'simulated timeprofile'
        private$.tpLabelSimulatedRange = ''
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
      observedDataForPlot <- data.table()

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

          observedDataForPlot <-
            rbind(observedDataForPlot,
                  observedDataTmp)
        }
      }
      checkmate::assertTRUE(dplyr::n_distinct(observedDataForPlot$DataClass) == 1,
                            .var.name = 'use only one DataClass per dataType in one plot')


      return(observedDataForPlot)
    }


  )
)
