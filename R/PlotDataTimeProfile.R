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
    #' @param projectConfiguration projectConfiguration
    #' @param observedDataDT 'data.table'  with observed data
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
    #' @param identifierColumns identifier columns which define sort order
    setOrderAndFactors = function(identifierColumns){

      # shorten tables to the ones needed in plot
      private$.dtOutputPaths <-
        private$.dtOutputPaths[outputPathId %in% private$.data$outputPathId]

      private$.dtDataGroups <-
        private$.dtDataGroups[ group %in% private$.data$group]

      private$.data$outputPathId <- factor(private$.data$outputPathId,
                                      levels = levels(private$.dtOutputPaths$outputPathId),
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
    #' @param nFacetColumns  number of maximal facet columns
    addDtCaption = function(nFacetColumns) {

      configTable = data.table::copy(self$configTable) %>%
        dplyr::select('scenarioIndex', 'OutputPathIds', 'ScenarioCaptionName','DataGroupIds')

      dtCaption <-
        tidyr::separate_rows(data.table::setDF(configTable),
                             OutputPathIds,
                             sep = ",\\s*|(?<=\\)),\\s*|\\s(?=\\()")  %>%
        data.table::setDT() %>%
        data.table::setnames(old = c('OutputPathIds'),
                             new = c('outputPathId'))
      dtCaption[,outputPathId := trimws(outputPathId)]



      # set Plot ID as accumulated sum over difference off number of open and closed brackets
      dtCaption[, nBracketOpen := cumsum(grepl("\\(", outputPathId)),]
      dtCaption[, nBracketClosed := cumsum(grepl("\\)", outputPathId)),]
      dtCaption[, countAdd := nBracketClosed-nBracketOpen+1,]
      dtCaption[, PlotId := cumsum(data.table::shift(countAdd,fill = 1)),]
      dtCaption[, PlotTag := toupper(letters[PlotId]),]

      # count color per plotID
      private$.nColorPerPlotID <-
        dtCaption[, .(N = dplyr::n_distinct(outputPathId)),
                  by = 'PlotId']$N %>%
        max()

      # count datagroups per plotID
      private$.nDataGroupPerPlotID <-
        dtCaption[, .(N = length(strsplit(DataGroupIds, ",")[[1]])), by = 'DataGroupIds']$N %>%
        max()


      # get columns for plot Scenario vs Output
      private$.nFacetColumns <- nFacetColumns
      if (dplyr::n_distinct(dtCaption$PlotId) ==1){
        private$.nFacetColumns = NULL
      } else if (private$.configTable$FacetType[1] == FACETTYPE$vsOutput){

        nCol <- dtCaption[, .(nOutputCol = {
          d <- unique(diff(PlotId))
          ifelse(length(d) == 1, d, NA)
        }), by = 'outputPathId']$nOutputCol %>%
          unique
        if (all(!is.na(nCol)) &
          length(nCol) == 1){
          private$.nFacetColumns = nCol
        } else{
          warning(
            paste0(
              'Plot "',
              private$.configTable$PlotName[1],
              '" is not suited for FacetType"',
              FACETTYPE$vsOutput,
              '" use "',
              FACETTYPE$byOrder,
              '" instead.'
            )
          )
        }

      }


      dtCaption$PlotTag <-
        factor(dtCaption$PlotTag,
               ordered = TRUE,
               levels = unique(dtCaption$PlotTag))

      # add display name for caption
      dtCaption[, outputPathId := trimws(gsub("[()]",'',outputPathId)),]

      dtCaption <- dtCaption %>%
        merge(private$.dtOutputPaths %>%  dplyr::select(outputPathId,'DisplayName'),
              by = 'outputPathId') %>%
        data.table::setnames('DisplayName','outputDisplayName') %>%
        dplyr::select('scenarioIndex','outputPathId','PlotTag','ScenarioCaptionName','outputDisplayName')

      private$.dtCaption <- dtCaption


      # add plot ID
      private$.data <- private$.data %>%
        merge(dtCaption %>%  dplyr::select('scenarioIndex','outputPathId','PlotTag'),
              by = c('scenarioIndex','outputPathId'))


      return(invisible())
    },
    #' add colorIndex column
    setColorIndex = function(){

      setColorLegend <- private$.nColorPerPlotID > 1 |
        any(!is.na(private$.dtOutputPaths$color)) |
        any(!is.na(private$.dtOutputPaths$fill))

      setShapeLegend <- private$.nDataGroupPerPlotID > 1 |
        any(!is.na(private$.dtDataGroups$color)) |
        any(!is.na(private$.dtDataGroups$fill))


      if (!setColorLegend & !setShapeLegend){

        private$.data[,colorIndex := ifelse(dataType == 'simulated',private$.tpLabelSimulatedMean,
                                            private$.tpLabelObserved)]
        private$.data$colorIndex = factor(private$.data$colorIndex,
                                          levels = c(private$.tpLabelSimulatedMean,
                                                     private$.tpLabelObserved),
                                          ordered = TRUE)

        if (!is.null(private$.tpLabelSimulatedRange)){
          private$.data[,fillIndex := ifelse(dataType == 'simulated',private$.tpLabelSimulatedRange,
                                             private$.tpLabelObserved)]
          private$.data$fillIndex = factor(private$.data$colorIndex,
                                           levels = c(private$.tpLabelSimulatedRange,
                                                      private$.tpLabelObserved),
                                           ordered = TRUE)
        }

        private$.scaleVectors <- generateColorScaleVectors(
          dt = data.table(index = private$.tpLabelSimulatedMean),
          index = 'index')

        private$.tpLabels <- list(colour_ggnewscale_1 = '',
                                  fill_ggnewscale_1 = '',
                                  colour = '',
                                  fill = '')

      }

      # Create legend for color
      if (setColorLegend){

        private$.data[,colorIndex := outputPathId]
        private$.data$colorIndex = factor(private$.data$colorIndex,
                                                  levels = private$.dtOutputPaths$outputPathId,
                                                  labels = private$.dtOutputPaths$DisplayName,
                                                  ordered = TRUE)

        private$.scaleVectors <- generateColorScaleVectors(
          dt = private$.dtOutputPaths[DisplayName %in% levels(private$.data$colorIndex)],
          index = 'DisplayName')

        private$.tpLabels <- list(colour_ggnewscale_1 = private$.tpLabelSimulatedMean,
                                  fill_ggnewscale_1 = private$.tpLabelSimulatedRange,
                                  colour = private$.tpLabelObserved,
                                  fill = private$.tpLabelObserved)

      }


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
    #' @return vector of characters, each entry is one footnote line
    getFootNoteLines = function(){
      footnoteLines = c()
      if (any(private$.data$dataType == 'observed')) {
        if (any(private$.data[dataType == 'observed']$dataClass == DATACLASS$tpAggregated)){
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
    },
    #' @field dtCaption `digits for significance in table display`data.table`with caption information`
    nFacetColumns = function() {
      private$.nFacetColumns
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
    # vector of facet columns
    .nFacetColumns = NULL,
    # number of colors per panel
    .nColorPerPlotID = NULL,
    # number of data groups per panel
    .nDataGroupPerPlotID = NULL,
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

      checkmate::assertTRUE(dplyr::n_distinct(simulatedData$dataClass) == 1,
                            .var.name = 'use only one "dataClass" in one Plot')

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
        quantitiesOrPaths = unique(private$.dtOutputPaths[outputPathId %in% outputs]$OutputPath)
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
            dplyr::select("outputPathId", "DisplayUnit", "OutputPath"),
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
        merge(dtUnit[, c("paths", "outputPathId", "unitFactor",'DisplayUnit')] ,
              by = "paths"
        )
      dt[, simulationValues := simulationValues * unitFactor]

      if (dplyr::n_distinct(dt$IndividualId) > 1) {
        stop("aggregate")
      } else {
        dt <- dt %>%
          dplyr::select(c("outputPathId", "Time", "xUnit","simulationValues",'DisplayUnit')) %>%
          data.table::setnames(
            old = c("Time", "simulationValues",'DisplayUnit'),
            new = c("xValues", "yValues",'yUnit')
          ) %>%
          dplyr::mutate(dataClass = DATACLASS$tpIndividual)

        private$.tpLabelSimulatedMean = 'simulated timeprofile'
      }

      dt[, xValues := xValues - timeOffset]

      private$upDateDtUnit(dtUnit %>% dplyr::select(c(
        "outputPathId", "dimension", "DisplayUnit", "molWeight"
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
                           outputPathId %in% gsub("[()]", "", splitInputs(configList$OutputPathIds))] %>%
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
            dplyr::select(c("outputPathId", "yUnit")) %>%
            unique() %>%
            merge(private$.dtUnit,
                  by = c("outputPathId")
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
                    dplyr::select(c('outputPathId','DisplayUnit','unitFactor')),
                  by = "outputPathId"
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
      checkmate::assertTRUE(dplyr::n_distinct(observedDataForPlot$dataClass) == 1,
                            .var.name = 'use only one "dataClass" per dataType in one plot')


      return(observedDataForPlot)
    }


  )
)
