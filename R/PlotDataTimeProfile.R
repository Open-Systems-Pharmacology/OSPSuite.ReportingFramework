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
      private$.timeTags <- getTimeRangeTags(projectConfiguration = projectConfiguration)

      private$.configTable <- data.table::setDT(onePlotConfig)[,scenarioIndex:= .I]
    },
    #' Collects all observed and simulated data needed for this plots
    #'
    #' @param projectConfiguration projectConfiguration
    #' @param observedDataDT 'data.table'  with observed data
    collectData = function(projectConfiguration,
                                dataObserved) {

      # helper function to losd simulated results
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

          outputs = gsub("[()]", "", splitInputs(configList$OutputPathIds))
          outputPaths <- private$.dtOutputPaths[outputPathId %in% outputs]$OutputPath %>%
            unique()

          # load control
          timeprofile <-
            getSimulatedTimeprofile(
              simulatedResult = simulatedResults[[configList$Scenario]],
              targetTimeUnit = configList$TimeUnit,
              outputPaths = outputPaths,
              timeOffset = as.double(configList$TimeOffset_Reference) +
                as.double(configList$TimeOffset)
            ) %>%
            dplyr::mutate(scenarioIndex = scenarioIndex)

          simulatedData <- rbind(
            simulatedData,
            timeprofile
          )

          # save application Times
          getApplicationTimes(scenarioIndex = scenarioIndex,
                              simulatedResult = simulatedResults[[configList$Scenario]],
                              targetTimeUnit = configList$TimeUnit,
                              outputPaths = outputPaths,
                              timeOffset = as.double(configList$TimeOffset_Reference) +
                                as.double(configList$TimeOffset))

          # load references
          if (!is.na(configList$ReferenceScenario) && as.logical(configList$ReferenceScenario)) {
            stop('identifier not implemented')
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
      }

      #' helper function to extract time profiles for results
      getSimulatedTimeprofile = function(simulatedResult,
                                         outputPaths,
                                         targetTimeUnit,
                                         timeOffset) {

        dt <- ospsuite::simulationResultsToDataFrame(
          simulationResults = simulatedResult$results,
          quantitiesOrPaths = outputPaths
        ) %>%
          data.table::setDT()
        dt <- convertUnits(dt,targetTimeUnit)
        dt[, Time := Time - timeOffset]


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

        return(dt)
      }

      # helper function to prepare timerange filter
      getApplicationTimes <-function(scenarioIndex,
                                     simulatedResult,
                                     outputPaths,
                                     targetTimeUnit,
                                     timeOffset){

        applicationStartTimes <- lapply(outputPaths,function(pts){
          lapply(simulatedResult$simulation$allApplicationsFor(pts),
                 function(x){x$startTime$value}) %>%
            unlist()
        }) %>%
          unlist() %>%
          unique() %>%
          sort()

        applicationStartTimes <- toUnit(values = applicationStartTimes,
                                        quantityOrDimension = "Time",
                                        targetUnit = targetTimeUnit,
        )
        applicationStartTimes <- applicationStartTimes - timeOffset

        private$.applicationTimes[[scenarioIndex]] <-
          list(startOfFirstApplication = applicationStartTimes[1],
               startOfLastApplication  = utils::tail(applicationStartTimes,1),
               endOfFirstApplication =
                 ifelse(length(applicationStartTimes) > 1,applicationStartTimes[2],Inf))

        return(invisible())

      }

      # helper function for unit conversion
      convertUnits <- function(dt,targetTimeUnit){
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

        private$.dtUnit <- rbind(
          private$.dtUnit,
          dtUnit
        ) %>%
          unique()

        return(dt)
      }


      # helper function to filter observed data
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

      # Main function logic

      private$.dataSimulated <-
        loadSimulatedResults(
          projectConfiguration = projectConfiguration
        ) %>%
        dplyr::mutate(dataType = "simulated")

      private$.dataObserved <-
        filterObservedDataForPlot(
          dataObserved = dataObserved
        )

      return(invisible())
    },
    # replictaes data for each timetag
    addTimeRangeTags = function(){
      timeRangeColumns <-
        names(private$.configTable)[grepl("^TimeRange_", names(private$.configTable))]

      # helper funtion to add the timeRange tag to the data
      addToData = function(timeRangeColumns,dataOld){
        dt = data.table()

        for (col in timeRangeColumns){

          tag = gsub('^TimeRange_','',col)

          for (scenarioIndex in private$.configTable[!is.na(get(col))]$scenarioIndex) {
            configList <- as.list(private$.configTable[scenarioIndex, ])

            if (configList[[col]] == 'total'){
              tRange = c(-Inf,Inf)
            } else if (configList[[col]] == 'firstApplication'){
              tRange = c(private$.applicationTimes[[scenarioIndex]]$startOfFirstApplication,
                         private$.applicationTimes[[scenarioIndex]]$endOfFirstApplication)
            } else if (configList[[col]] == 'firstApplication'){
              tRange = c(private$.applicationTimes[[scenarioIndex]]$startOfLastApplication,
                         Inf)
            } else{
              tRange <- eval(parse(text = configList[[col]]))
            }

            dt = rbind(dt,
                       dataOld[xValues >= tRange[1] &
                                       xValues <= tRange[2] &
                                       scenarioIndex == scenarioIndex] %>%
                         dplyr::mutate(timeRangeTag = tag)
            )

          }
        }
        return(dt)
      }

      private$.dataObserved <- addToData(timeRangeColumns = timeRangeColumns,
                                         dataOld = private$.dataObserved)
      private$.dataSimulated <- addToData(timeRangeColumns = timeRangeColumns,
                                         dataOld = private$.dataSimulated)

    },
    #' setfactors to plot
    #' @param identifierColumns identifier columns which define sort order
    setOrderAndFactors = function(identifierColumns){

      # Helper function to set factors
      setFactorLevels <- function(tableName, identifier, identifierData) {

        # Shorten tables to the ones needed in plot
        private[[tableName]] <-
          private[[tableName]][get(identifier) %in% private$.dataObserved[[identifierData]]]

        identifierLevels <-  private[[tableName]][[identifier]] %>%
                                                   as.character() %>%
                                                   unique()

        private[[tableName]][[identifier]] <-
          factor(private[[tableName]][[identifier]],
                 levels = identifierLevels,
                 ordered = TRUE)

        if (!is.null(private$.dataSimulated[[identifierData]]))
          private$.dataSimulated[[identifierData]] <-
          factor(private$.dataSimulated[[identifierData]],
                 levels = identifierLevels,
                 ordered = TRUE)

        if (!is.null(private$.dataObserved[[identifierData]]))
          private$.dataObserved[[identifierData]] <-
          factor(private$.dataObserved[[identifierData]],
                 levels = identifierLevels,
                 ordered = TRUE)

        return(invisible())
      }

      # Main logic
      setFactorLevels(
        tableName = '.dtOutputPaths',
        identifier = 'outputPathId',
        identifierData = 'outputPathId'
      )

      setFactorLevels(
        tableName = '.dtDataGroups',
        identifier = 'group',
        identifierData = 'group'
      )

      setFactorLevels(
        tableName = '.timeTags',
        identifier = 'Tag',
        identifierData = 'timeRangeTag'
      )

      return(invisible())

    },
    #' Creates PlotId for each scenario outputs group
    #' @param nFacetColumns  number of maximal facet columns
    splitDataToPanels = function(nFacetColumns) {

      # Helper function to prepare the config table
      prepareConfigTable = function() {
        configTable <- data.table::copy(self$configTable) %>%
          dplyr::select('scenarioIndex', 'OutputPathIds', 'ScenarioCaptionName', 'DataGroupIds')
        return(configTable)
      }

      # Helper function to separate and clean outputPathIds
      processOutputPathIds = function(configTable) {
        dtCaption <- tidyr::separate_rows(data.table::setDF(configTable),
                                          OutputPathIds,
                                          sep = ",\\s*|(?<=\\)),\\s*|\\s(?=\\()") %>%
          data.table::setDT() %>%
          data.table::setnames(old = c('OutputPathIds'),
                               new = c('outputPathId'))
        dtCaption[, outputPathId := trimws(outputPathId)]
        return(dtCaption)
      }

      # Helper function to calculate PlotId and related metrics
      calculatePlotMetrics = function(dtCaption) {
        dtCaption[, nBracketOpen := cumsum(grepl("\\(", outputPathId))]
        dtCaption[, nBracketClosed := cumsum(grepl("\\)", outputPathId))]
        dtCaption[, countAdd := nBracketClosed - nBracketOpen + 1,]
        dtCaption[, PlotId := cumsum(data.table::shift(countAdd, fill = 1))]
        dtCaption <-
          data.table::rbindlist(lapply(levels(private$.timeTags$Tag),
                                       function(tag){dtCaption %>%
                                           dplyr::mutate(timeRangeTag = tag)}))
        dtCaption$timeRangeTag <- factor(dtCaption$timeRangeTag,
                                         levels = levels(private$.timeTags$Tag),
                                                         ordered = TRUE)
        if (private$.configTable$FacetType[1] == FACETTYPE$vsTimeRange){
          dtCaption[,PlotId := (PlotId-1)*length(levels(private$.timeTags$Tag)) +
                      as.numeric(timeRangeTag)]
          private$.timeRangeTagFilter <- list(allTimeRanges = "TRUE")
        } else{
          private$.timeRangeTagFilter <-
            lapply(levels(private$.timeTags$Tag),
                   function(x){paste0("timeRangeTag == '",x,"'")})
          names(private$.timeRangeTagFilter) <- levels(private$.timeTags$Tag)
        }

        dtCaption[, PlotTag := toupper(letters[PlotId]),]

        data.table::setorderv(dtCaption,'PlotId')
        dtCaption$PlotTag <- factor(dtCaption$PlotTag,
                                    levels = unique(dtCaption$PlotTag),
                                    ordered = TRUE)

        private$.nColorPerPlotID <- dtCaption[, .(N = dplyr::n_distinct(outputPathId)), by = 'PlotId']$N %>%
          max()

        private$.nDataGroupPerPlotID <- dtCaption[, .(N = length(strsplit(DataGroupIds, ",")[[1]])), by = 'DataGroupIds']$N %>%
          max()

        return(dtCaption)
      }

      # Helper function to determine facet columns
      determineFacetColumns = function(dtCaption, nFacetColumns) {
        private$.nFacetColumns <- nFacetColumns
        if (dplyr::n_distinct(dtCaption$PlotId) == 1) {
          private$.nFacetColumns <- NULL
        } else if (private$.configTable$FacetType[1] == FACETTYPE$vsOutput) {
          nCol <- dtCaption[, .(nOutputCol = {
            d <- unique(diff(PlotId))
            ifelse(length(d) == 1, d, NA)
          }), by = 'outputPathId']$nOutputCol %>%
            unique()

          if (all(!is.na(nCol)) & length(nCol) == 1) {
            private$.nFacetColumns <- nCol
          } else {
            warning(
              paste0(
                'Plot "',
                private$.configTable$PlotName[1],
                '" is not suited for FacetType "',
                FACETTYPE$vsOutput,
                '" use "',
                FACETTYPE$byOrder,
                '" instead.'
              )
            )
          }
        } else if (private$.configTable$FacetType[1] == FACETTYPE$vsTimeRange){
          private$.nFacetColumns <- length(levels(dtCaption$timeRangeTag))
        }
      }

      # Helper function to finalize dtCaption
      finalizeDtCaption = function(dtCaption) {
        dtCaption$PlotTag <- factor(dtCaption$PlotTag, ordered = TRUE, levels = unique(dtCaption$PlotTag))
        dtCaption[, outputPathId := trimws(gsub("[()]", '', outputPathId)),]
        dtCaption$timeRangeCaption <- factor(dtCaption$timeRangeTag,
                                         ordered = TRUE,
                                         levels = private$.timeTags$Tag,
                                         labels = private$.timeTags$CaptionText)

        dtCaption <- dtCaption %>%
          merge(private$.dtOutputPaths %>% dplyr::select(outputPathId, 'DisplayName'),
                by = 'outputPathId') %>%
          data.table::setnames('DisplayName', 'outputDisplayName') %>%
          dplyr::select('scenarioIndex', 'outputPathId', 'PlotTag',
                        'ScenarioCaptionName', 'outputDisplayName',
                        'timeRangeTag','timeRangeCaption')

        return(dtCaption)
      }

      # Main Logic
      configTable <- prepareConfigTable()
      dtCaption <- processOutputPathIds(configTable)
      dtCaption <- calculatePlotMetrics(dtCaption)
      determineFacetColumns(dtCaption, nFacetColumns)
      dtCaption <- finalizeDtCaption(dtCaption)

      private$.dtCaption <- dtCaption
      private$.dataObserved <- private$.dataObserved %>%
        merge(dtCaption %>% dplyr::select('scenarioIndex', 'outputPathId', 'PlotTag','timeRangeTag'),
              by = c('scenarioIndex', 'outputPathId','timeRangeTag'))
      private$.dataSimulated <- private$.dataSimulated %>%
        merge(dtCaption %>% dplyr::select('scenarioIndex', 'outputPathId', 'PlotTag','timeRangeTag'),
              by = c('scenarioIndex', 'outputPathId','timeRangeTag'))

      return(invisible())
    },
    # set color Index and get scale vectors
    prepareLegendDetails = function() {

      # Helper function to determine if color legend is needed
      shouldSetColorLegend <- function() {
        return(private$.nColorPerPlotID > 1 ||
                 any(!is.na(private$.dtOutputPaths$color)) ||
                 any(!is.na(private$.dtOutputPaths$fill)))
      }

      # Helper function to determine if shape legend is needed
      shouldSetShapeLegend <- function() {
        return(private$.nDataGroupPerPlotID > 1 ||
                 any(!is.na(private$.dtDataGroups$color)) ||
                 any(!is.na(private$.dtDataGroups$fill)))
      }

      # Helper function to set default color and fill indices
      setDefaultLegendIndices <- function() {
        for (fieldName in c('.dataSimulated','.dataObserved')){
          private[[fieldName]][, colorIndex := ifelse(dataType == 'simulated',
                                                      private$.tpLabelSimulatedMean,
                                                      private$.tpLabelObserved)]
          private[[fieldName]]$colorIndex <- factor(private[[fieldName]]$colorIndex,
                                                    levels = c(private$.tpLabelSimulatedMean,
                                                               private$.tpLabelObserved),
                                                    ordered = TRUE)

          if (!is.null(private$.tpLabelSimulatedRange)) {
            private[[fieldName]][, fillIndex := ifelse(dataType == 'simulated',
                                                       private$.tpLabelSimulatedRange,
                                                       private$.tpLabelObserved)]
            private[[fieldName]]$fillIndex <- factor(private[[fieldName]]$fillIndex,
                                                     levels = c(private$.tpLabelSimulatedRange,
                                                                private$.tpLabelObserved),
                                                     ordered = TRUE)
          }
        }

        private$.scaleVectors <- generateColorScaleVectors(
          dt = data.table(index = private$.tpLabelSimulatedMean),
          index = 'index')

        private$.tpLabels <- list(colour_ggnewscale_1 = '',
                                  fill_ggnewscale_1 = '',
                                  colour = '',
                                  fill = '')
      }

      # Helper function to set color legend
      setColorLegend <- function() {
        for (fieldName in c('.dataSimulated','.dataObserved')){
          private[[fieldName]][, colorIndex := outputPathId]
          private[[fieldName]]$colorIndex <-
            factor(private[[fieldName]]$colorIndex,
                   levels = private$.dtOutputPaths$outputPathId,
                   labels = private$.dtOutputPaths$DisplayName,
                   ordered = TRUE)
        }

        private$.scaleVectors <- generateColorScaleVectors(
          dt = private$.dtOutputPaths[DisplayName %in% levels(private$.dataSimulated$colorIndex)],
          index = 'DisplayName')

        private$.tpLabels <- list(colour_ggnewscale_1 = private$.tpLabelSimulatedMean,
                                  fill_ggnewscale_1 = private$.tpLabelSimulatedRange,
                                  colour = private$.tpLabelObserved,
                                  fill = private$.tpLabelObserved)
      }

      # Main logic
      setColorLegendFlag <- shouldSetColorLegend()
      setShapeLegendFlag <- shouldSetShapeLegend()

      if (!setColorLegendFlag && !setShapeLegendFlag) {
        setDefaultLegendIndices()
      }

      if (setColorLegendFlag) {
        setColorLegend()
      }

      return(invisible())
    },
    # addPredicted to observed
    addPredictedForObserved = function(identifierColumns){

      if (!(any(as.logical(private$.configTable[1] %>%
                           dplyr::select('Plot_PredictedVsObserved',
                                         'Plot_ResidualsAsHistogram',
                                         'Plot_ResidualsVsTime',
                                         'Plot_ResidualsVsObserved')))))
        return(invisible())

      dtObserved <- private$.dataObserved
      dtSimulated <- private$.dataSimulated

      private$.dataObserved <-
        addPredictedValues(dtObserved = dtObserved,
                           dtSimulated = dtSimulated,
                           identifier = c('PlotTag',
                                          'outputPathId',
                                          'timeRangeTag'))

    },
    # get Mapping for groupby out of column Names
    getGroupbyMapping = function(){
      return(eval(parse(text = paste(
        'aes(groupby =', interaction(intersect(
          names(self$data), c('colorIndex', 'shapeIndex')
        )), ')'
      )))
      )
    },
    # get groupAetsthetics out of column Names
    getGroupAesthetics = function(){
      return(intersect(c('colour','fill','shape'),
                                  ggplot2::standardise_aes_names(names(self$scaleVectors))))

    },
    #' creates a caption text
    #' @param yScale scale of Y axis
    #' @return  `character` with caption text
    getCaptionForPlot = function(yScale,filterName,plotType){

      dtCaption <-
        private$.dtCaption[eval(parse(text = private$.timeRangeTagFilter[[filterName]]))]

      plotTypeTxt <- switch(plotType,
                            TP = 'Concentration-time profiles',
                            PvO = 'Predicted vs Observed',
                            ResvT ='Residuals vs time values',
                            ResvO ='Residuals vs observed values',
                            ResH ='Residuals distribution ',
                            QQ = 'Residuals as quantile-quantile plot')

      # generate captiontext
      captiontext = paste(plotTypeTxt,
                          'for',
                          pasteFigureTags(dtCaption,captionColumn = 'outputDisplayName'),
                          'for',
                          pasteFigureTags(dtCaption,captionColumn = 'ScenarioCaptionName'),
                          'on a',ifelse(yScale == 'linear','linear','logarithmic'),
                          'y-scale.',
                          ifelse (!is.na(private$.configTable$PlotCaptionAddon[1]),
                                  private$.configTable$PlotCaptionAddon[1],''),
                          pasteFigureTags(dtCaption,captionColumn = 'timeRangeCaption')
      )

      return(captiontext)

    },
    #' constructs footnote lines for aggregated data and data references
    #' @return vector of characters, each entry is one footnote line
    getFootNoteLines = function(){
      footnoteLines = c()
      if (nrow(private$.dataObservd)>0) {
        if (any(private$.dataObserved$dataClass == DATACLASS$tpAggregated)){
          stop('not implemented yet')
        }
      }

      # filter used data
      dtDataReference <- private$.dtDataGroups[!is.na(Reference)]

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
    },
    #get the timelabel for the filtered timerange
    getTimeLabelForTimeRange = function(filterName){

      if (filterName == 'allTimeRanges'){
        timeLabel <-
          utils::tail(private$.timeTags$TimeLabel,1)
      } else{
        timeLabel <-
          private$.timeTags[eval(parse(text = paste0('Tag == "', filterName,'"')))]$TimeLabel
      }
        xLabel = paste0(timeLabel,
                        ' [',private$.configTable$TimeUnit[1],']')
    },
    #get the time label for the filtered time range
    getDataForTimeRange = function(filterName){
      self$data[eval(parse(text = private$.timeRangeTagFilter[[filterName]]))]
    }

  ),
  #active ------
  active = list(
    #' @field data data For Plot
    data = function() {
        rbind(private$.dataSimulated,
              private$.dataObserved,
              fill = TRUE)
    },
    #' @field data data For Plot Predicted vs observed residuals
    dataObserved = function() {
      private$.dataObserved
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
    #' @field scaleVectors list with scaling vectors to manually scale aesthetics
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
    #' @field dtCaption `data.table`with caption information`
    nFacetColumns = function() {
      private$.nFacetColumns
    },
    #' @field timeTagfilter list with filters for time range tags
    timeRangeTagFilter = function() {
      private$.timeRangeTagFilter
    }
  ),
  #private ------
  private = list(
    # simulated Datat
    .dataSimulated = NULL,
    # observed Data
    .dataObserved = NULL,
    # output path configuration
    .dtOutputPaths = NULL,
    # data group configuration
    .dtDataGroups = NULL,
    # configuration table for this plot
    .configTable = NULL,
    # unit information
    .timeTags = NULL,
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
    # list with application times per scenariondex
    .applicationTimes = list(),
    # list with timetag filters
    .timeRangeTagFilter = NULL
  )
)
