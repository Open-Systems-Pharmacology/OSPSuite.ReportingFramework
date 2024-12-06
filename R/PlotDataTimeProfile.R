#' @title plotData for Timeprofile plotData
#' @docType class
#' @description An object to collect plotData and other information to create a time profile plot
#' @export
PlotDataTimeProfile <- R6::R6Class( # nolint
  "RmdContainer",
  inherit = ospsuite.utils::Printable,
  cloneable = TRUE,
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #' @template projectConfig
    #' @param onePlotConfig plot configuration for one plot
    #' @param dataObserved `data.table` with observed data
    #' @param aggregationFun function to aggregate simulated data
    #' @returns RmdContainer object
    initialize = function(projectConfiguration = projectConfiguration,
                          onePlotConfig = onePlotConfig) {
      private$.dtOutputPaths <- copy(configEnv$outputPaths)
      private$.dtDataGroups <- copy(configEnv$dataGroupIds)
      private$.timeTags <- copy(configEnv$timeTags)
      private$.configTable <- data.table::setDT(onePlotConfig)[, scenarioIndex := .I]
    },

    #' Load simulated results for the specified project configuration
    #'
    #' @param projectConfiguration Configuration for the project
    #' @param aggregationFun Function to aggregate simulated data
    #' @param scenarioResults list with simulated scenario results
    loadSimulatedResults = function(projectConfiguration, aggregationFun,scenarioResults) {
      # Get scenario names
      scenarioNames <- c(
        private$.configTable$scenario,
        private$.configTable[!is.na(referenceScenario)]$referenceScenario
      )

      # Load simulated results
      if (length(setdiff(scenarioNames,names(scenarioResults)))>0){
        scenarioResults <- utils::modifyList(scenarioResults,
                                              esqlabsR::loadScenarioResults(
                                                scenarioNames = setdiff(scenarioNames,names(scenarioResults)),
                                                resultsFolder = file.path(projectConfiguration$outputFolder, EXPORTDIR$simulationResult)
                                              )
        )
      }

      # Process simulated results
      outputPathsPerScenario <- getOutputPathsPerScenario(
        configTable = self$configTable,
        dtOutputPaths = private$.dtOutputPaths
      )
      dtSimulated <- loadScenarioTimeProfiles(
        projectConfiguration = projectConfiguration,
        simulatedResults = scenarioResults,
        outputPathsPerScenario = outputPathsPerScenario,
        aggregationFun = aggregationFun
      )

      # Get unit conversion data.table
      private$.dtUnit <- getUnitConversionDT(
        dtSimulated = dtSimulated,
        dtOutputs = private$.dtOutputPaths
      )

      # Restructure data by scenario index
      private$.dataSimulated <- private$restructureDataByScenarioIndex(dtSimulated)
      private$.dataSimulated$group <- NA # Mandatory column for plotting

      # Error handling
      if (is.null(private$.dataSimulated) || nrow(private$.dataSimulated) == 0) {
        stop(paste("No simulated data found for", self$configTable$plotName[1]))
      }

      # Get application times
      applicationTimes <-
        getApplicationTimes(
          outputPathsPerScenario = outputPathsPerScenario,
          simulatedResults = scenarioResults
        )
      private$.applicationTimes <-
        restructureApplicationTimeByScenarioIndex(applicationTimes,
                                                  configTable = self$configTable)

      # Set labels
      if (unique(dtSimulated$dataClass) == DATACLASS$tpAggregated) {
        errorLabels <- getErrorLabels(dtSimulated$yErrorType[1])
        private$.tpLabelSimulatedMean <- errorLabels[1]
        private$.tpLabelSimulatedRange <- errorLabels[2]
      } else {
        private$.tpLabelSimulatedMean <- "time profile"
      }

      return(invisible())
    },

    #' Filter observed data for plotting
    #'
    #' @param dataObserved `data.table` with observed data
    #' @return `data.table` with filtered observed data for plotting
    filterObservedDataForPlot = function(dataObserved) {
      if (all(is.na(self$configTable$dataGroupIds))) {
        private$.dataObserved <- data.table()
        return(invisible())
      }

      dtUnit <- getObservedUnitConversionDT(dataObserved, private$.dtUnit)
      observedDataForPlot <- data.table()

      for (scenarioIndex in seq_len(nrow(self$configTable))) {
        configList <- as.list(self$configTable[scenarioIndex, ])
        dataGroupIds <- splitInputs(configList$dataGroupIds)

        if (!is.null(dataGroupIds)) {
          observedDataTmp <- dataObserved[
            group %in% dataGroupIds &
              outputPathId %in% gsub("[()]", "", splitInputs(configList$outputPathIds)),
          ]

          observedDataTmp <- filterIndividualID(
            timeprofile = observedDataTmp,
            individualList = configList$individualIds
          )

          observedDataTmp <- observedDataTmp %>%
            dplyr::select(dplyr::any_of(getColumnsForColumnType(dataObserved, columnTypes = c("identifier", "timeprofile")))) %>%
            dplyr::mutate(scenarioIndex = scenarioIndex)

          # Convert y units and time units
          observedDataTmp <- convertYunit(observedDataTmp, dtUnit)
          observedDataTmp <- convertAndShiftTimeUnits(observedDataTmp, targetTimeUnit = configList$timeUnit)

          observedDataForPlot <- rbind(observedDataForPlot, observedDataTmp)
        }
      }

      checkmate::assertTRUE(dplyr::n_distinct(observedDataForPlot$dataClass) == 1,
                            .var.name = 'Use only one "dataClass" per dataType in one plot' # nolint indentation_linter
      )

      private$.dataObserved <- observedDataForPlot

      return(invisible())
    },
    #' Replicates data for each time tag
    #' @return invisible(NULL)
    addTimeRangeTags = function() {

      timeRangeColumns <- names(private$.configTable)[grepl("^timeRange_", names(private$.configTable))]

      private$.dataSimulated <- addTimeRangeTagsToData(
        timeRangeColumns = timeRangeColumns,
        dataOld = private$.dataSimulated,
        configTable = self$configTable,
        applicationTimes = private$.applicationTimes,
        timeTags = private$.timeTags
      )

      if (nrow(private$.dataSimulated) == 0) {
        stop(glue::glue("All simulated data outside time range for {self$configTable$plotName[1]}"))
      }

      if (nrow(private$.dataObserved) > 0) {
        private$.dataObserved <- addTimeRangeTagsToData(
          timeRangeColumns = timeRangeColumns,
          dataOld = private$.dataObserved,
          configTable = self$configTable,
          applicationTimes = private$.applicationTimes,
          timeTags = private$.timeTags
        )
      }
      return(invisible())
    },
    #' Creates plotId for each scenario outputs group
    #'
    #' @param nFacetColumns default number of facet columns
    #' @param nMaxFacetRows maximal number of facet rows
    splitDataToPanels = function(nFacetColumns, nMaxFacetRows) {
      configTable <- data.table::copy(self$configTable) %>%
        dplyr::select("scenarioIndex", "outputPathIds", "scenarioLongName", "dataGroupIds", "individualIds")

      dtCaption <- getPlotIdForColumns(configTable = configTable, col = "outputPathIds")

      if (private$.dataSimulated$dataClass[1] == DATACLASS$tpTwinPop) {
        dtCaption <- splitCaptionByIndividuals(
          configTable = configTable,
          individualIdVector = sort(unique(self$data$individualId)),
          dtCaption = dtCaption
        )
      }
      dtCaption <- addTimeTagsToCaption(
        dtCaption = dtCaption,
        timeTags = private$.timeTags$tag,
        facetType = self$configTable$facetType[1]
      )

      private$.nFacetColumns <-
        determineFacetColumns(
          dtCaption = dtCaption,
          nFacetColumns = nFacetColumns,
          facetType = self$configTable$facetType[1],
          plotName = self$configTable$plotName[1]
        )


      private$.dtCaption <- finalizeCaptionTable(
        dtCaption = dtCaption,
        timeTags = private$.timeTags,
        dtOutputPaths = private$.dtOutputPaths,
        nFacetColumns = private$.nFacetColumns,
        nMaxFacetRows = nMaxFacetRows,
        facetType = self$configTable$facetType[1]
      )

      private$.timeRangeTagFilter <- setTimeRangeFilter(
        facetType = self$configTable$facetType[1],
        timeTags = private$.timeTags$tag
      )

      private$.nColorPerPlotID <- dtCaption[, .(N = dplyr::n_distinct(outputPathId)), by = c("plotTag","counter")]$N %>%
        max()

      private$.nDataGroupPerPlotID <- dtCaption[, .(N = length(strsplit(dataGroupIds, ",")[[1]])), by = c("dataGroupIds","counter")]$N %>%
        max()


      for (fieldName in c(".dataObserved", ".dataSimulated")) {
        if (nrow(private[[fieldName]]) > 0) {
          identifier <- intersect(
            c("scenarioIndex", "outputPathId", "timeRangeTag", "individualId"),
            names(self$dtCaption)
          )
          private[[fieldName]] <- private[[fieldName]] %>%
            merge(self$dtCaption %>% dplyr::select(all_of(c("plotTag", "counter", identifier))),
                  by = identifier  # nolint indentation_linter
            )
        }
      }

      # clean up individual (*) for legend this has to be done after the merge with the data
      private$.dtCaption <- updateDtCaption(
        dtCaption = self$dtCaption,
        configTable = self$configTable
      )

      return(invisible())
    },

    #'  @title Set factors for plotting
    #' @return invisible(NULL)
    setOrderAndFactors = function() {
      private$setFactorLevels(
        tableName = ".dtOutputPaths",
        identifier = "outputPathId",
        identifierData = "outputPathId",
        dataToMatch = ".dataSimulated"
      )

      private$setFactorLevels(
        tableName = ".dtDataGroups",
        identifier = "group",
        identifierData = "group",
        dataToMatch = ".dataObserved"
      )

      private$setFactorLevels(
        tableName = ".timeTags",
        identifier = "tag",
        identifierData = "timeRangeTag",
        dataToMatch = ".dataSimulated"
      )

      return(invisible())
    },
    #' set columns for aesthetics
    #' @param referenceScaleVector scale vector to scale aesthetic color and fill for scenarios with reference scenario
    setIndexColumns = function(referenceScaleVector) {
      private$addColorIndexColumns(referenceScaleVector = referenceScaleVector)
      private$addShapeIndexColumn()
    },
    #' Get the time label for the filtered time range
    #'
    #' @param filterName character with name of time range filter
    getTimeLabelForTimeRange = function(filterName) {

      if (filterName == "allTimeRanges") {
        timeLabel <- utils::tail(private$.timeTags$TimeLabel, 1)
      } else {
        timeLabel <- private$.timeTags[eval(parse(text = paste0('tag == "', filterName, '"')))]$timeLabel
      }
      xLabel <- paste0(
        timeLabel,
        " [", private$.configTable$timeUnit[1], "]"
      )
    },
    #'  @title Add predicted values to observed data
    #' @return invisible(NULL)
    addPredictedForObserved = function() {
      # Check if predicted data is needed
      plotCols <- setdiff(grep("^plot_", names(private$.configTable), value = TRUE), "plot_TimeProfiles")
      plotCols <- plotCols[unlist(lapply(plotCols, function(x) {
        as.logical(private$.configTable[1, ][[x]])
      }))]
      isNeeded <- length(plotCols) > 0

      if (!self$hasObservedData() & isNeeded) {
        warning(glue::glue("For plot {self$configTable$plotName[1]}, no observed data available for {paste(plotCols, collapse = ', ')}, plots will be omitted"))
        return(invisible())
      }
      if (isNeeded & dplyr::n_distinct(private$.dataObserved$yUnit) > 1) {
        stop(glue::glue("For plot {self$configTable$plotName[1]}, you selected a plotType which is not suited for multiple units. Only Timeprofile can handle a secondary axis with a second unit. Please split outputPathId in the plot configuration xlsx to different rows."))
      }

      if (isNeeded) {
        identifier <- intersect(
          c("plotTag", "outputPathId", "timeRangeTag", "individualId"),
          names(private$.dataSimulated)
        )

        private$.dataObserved <-
          addPredictedValues(
            dtObserved = private$.dataObserved,
            dtSimulated = private$.dataSimulated,
            identifier = identifier
          )
      }
    },
    #' Get the data for the filtered time range
    #'
    #' @param filterName name of time range filter
    #' @param typeFilter filter for data type
    #' @param plotCounter counter for different plots
    #'
    #' @return `data.table` with filtered plot data
    getDataForTimeRange = function(filterName, plotCounter,yScale,typeFilter = NULL) {
      tmp <- self$data[eval(parse(text = private$.timeRangeTagFilter[[filterName]])) &
                         counter == plotCounter]
      if (!is.null(typeFilter)) {
        tmp <- tmp[dataType == typeFilter]
      }
      if (yScale == 'log'){
        tmp <- tmp[yValues > 0]
      }
      return(tmp)
    },

    # Flags -------------
    #' Function to determine if color legend is needed
    #' @return Logical
    useColorIndex = function() {
      return(private$.nColorPerPlotID > 1 ||
               any(!is.na(private$.dtOutputPaths$color)) ||
               any(!is.na(private$.dtOutputPaths$fill)) ||
               self$hasReferenceComparison())
    },

    #' Function to determine if shape legend is needed
    #' @return Logical
    useShapeIndex = function() {
      return(self$hasObservedData() &
               (private$.nDataGroupPerPlotID > 1 ||
                  any(!is.na(private$.dtDataGroups$shape))))
    },

    #' Function to determine if simulation is aggregated
    #' @return Logical
    hasSimulatedPop = function() {
      return(!is.null(private$.tpLabelSimulatedRange))
    },

    #' Function to determine if data contains observed data
    #' @return Logical
    hasObservedData = function() {
      return(nrow(private$.dataObserved) > 0)
    },

    #' Function to determine if data contains observed data range
    #' @return Logical
    hasObservedDataRange = function() {
      return(self$hasObservedData() &&
               !is.null(private$.dataObserved$yErrorType) &&
               !is.na(private$.dataObserved$yErrorType[1]))
    },

    #' Function to determine if a reference population should be plotted
    #' @return Logical
    hasReferenceComparison = function() {
      return(any((private$.dataSimulated$scenarioType == "referenceScenario")))
    }
  ),

  # Active fields ------
  active = list(
    #' @field data Data for Plot
    data = function() {
      tmpData <- rbind(private$.dataSimulated, private$.dataObserved, fill = TRUE)

      # rorder data
      tmpData <- tmpData[order(scenarioIndex,
                               match(outputPathId, private$.dtOutputPaths$outputPathId))]

      return(tmpData)
    },
    #' @field dataReference Data with references for data
    dataReference = function() {
      private$.dtDataGroups[!is.na(reference)]
    },
    #' @field configTable Configuration table for one plot
    configTable = function(value) {
      if (missing(value)) {
        value <- private$.configTable
      }
      private$.configTable <- value
      return(value)
    },
    #' @field plotName plotName of configTable
    plotName = function() {
      private$.configTable$plotName[1]
    },

    #' @field dtCaption Data.table with caption information
    dtCaption = function() {
      private$.dtCaption
    },
    #' @field scaleVectors List with scaling vectors to manually scale aesthetics
    scaleVectors = function() {
      private$.scaleVectors
    },
    #' @field tpLabelSimulatedMean Label for simulated mean
    tpLabelSimulatedMean = function() {
      private$.tpLabelSimulatedMean
    },
    #' @field tpLabelSimulatedRange Label for simulated range
    tpLabelSimulatedRange = function() {
      private$.tpLabelSimulatedRange
    },
    #' @field tpLabelObserved Label for observed data
    tpLabelObserved = function() {
      private$.tpLabelObserved
    },
    #' @field tpLabels List of time profile labels to add on ggplot
    tpLabels = function() {
      private$.tpLabels
    },
    #' @field nFacetColumns Number of facet columns
    nFacetColumns = function() {
      private$.nFacetColumns
    },
    #' @field timeRangeTagFilter List with filters for time range tags
    timeRangeTagFilter = function() {
      private$.timeRangeTagFilter
    }
  ),

  # Private fields ------
  private = list(
    # Simulated Data
    .dataSimulated = NULL,
    # Observed Data
    .dataObserved = NULL,
    # Output path configuration
    .dtOutputPaths = NULL,
    # Data group configuration
    .dtDataGroups = NULL,
    # Configuration table for this plot
    .configTable = NULL,
    # Unit information
    .timeTags = NULL,
    .dtUnit = data.table(),
    # Data.table with caption information
    .dtCaption = NULL,
    # List with scaling vectors to manually scale aesthetics
    .scaleVectors = list(),
    # Label for simulated mean
    .tpLabelSimulatedMean = NULL,
    # Label for simulated range
    .tpLabelSimulatedRange = NULL,
    # Label for observed data
    .tpLabelObserved = "Observed data",
    # List of time profile labels
    .tpLabels = list(),
    # Vector of facet columns
    .nFacetColumns = NULL,
    # Number of colors per panel
    .nColorPerPlotID = NULL,
    # Number of data groups per panel
    .nDataGroupPerPlotID = NULL,
    # List with application times per scenario index
    .applicationTimes = list(),
    # List with time tag filters
    .timeRangeTagFilter = NULL,

    # Helper function to set factors
    setFactorLevels = function(tableName, identifier, identifierData, dataToMatch) {
      # Shorten tables to the ones needed in plot
      private[[tableName]] <- private[[tableName]][get(identifier) %in% private[[dataToMatch]][[identifierData]]]

      identifierLevels <- private[[tableName]][[identifier]] %>%
        as.character() %>%
        unique()

      private[[tableName]][[identifier]] <-
        factor(private[[tableName]][[identifier]],
               levels = identifierLevels,
               ordered = TRUE)

      if (!is.null(private$.dataSimulated[[identifierData]])) {
        private$.dataSimulated[[identifierData]] <-
          factor(private$.dataSimulated[[identifierData]],
                 levels = identifierLevels,
                 ordered = TRUE)
      }

      if (!is.null(private$.dataObserved[[identifierData]])) {
        private$.dataObserved[[identifierData]] <-
          factor(private$.dataObserved[[identifierData]],
                 levels = identifierLevels,
                 ordered = TRUE)
      }

      return(invisible())
    },
    # Helper function to restructure data by scenario index
    restructureDataByScenarioIndex = function(dtSimulated) {
      simulatedData <- data.table()

      for (scenarioIndex in seq_len(nrow(self$configTable))) {
        configList <- as.list(private$.configTable[scenarioIndex, ])

        outputs <- gsub("[()]", "", splitInputs(configList$outputPathIds))
        outputPaths <- private$.dtOutputPaths[outputPathId %in% outputs]$outputPath %>%
          unique()

        for (ScenarioField in c("scenario", "referenceScenario")) {
          if (!is.na(configList[[ScenarioField]])) {
            timeprofile <- dtSimulated[scenario == configList[[ScenarioField]]] %>%
              dplyr::mutate(scenarioIndex = scenarioIndex, scenario = NULL, scenarioType = ScenarioField)

            timeprofile <- filterIndividualID(
              timeprofile = timeprofile,
              individualList = configList$individualIds
            )

            timeprofile <- convertYunit(
              timeprofile = timeprofile,
              dtUnit = private$.dtUnit
            )

            timeprofile <- convertAndShiftTimeUnits(
              timeprofile = timeprofile,
              targetTimeUnit = configList$timeUnit,
              timeOffset = ifelse(ScenarioField == "scenario",
                                  as.double(configList$timeOffset_Reference), # nolint indentation_linter
                                  0
              ) +
                as.double(configList$timeOffset)
            )

            simulatedData <- rbind(simulatedData, timeprofile)
          }
        }

        checkmate::assertTRUE(dplyr::n_distinct(simulatedData$dataClass) == 1,
                              .var.name = 'Use only one "dataClass" in one Plot' # nolint indentation_linter
        )
      }

      return(simulatedData)
    },




    # Add color and fill index columns
    addColorIndexColumns = function(referenceScaleVector) {
      if (self$hasReferenceComparison()) {
        private$addColorIndexByScenarioType(referenceScaleVector = referenceScaleVector)
      } else if (self$useColorIndex()) {
        private$addColorIndexByOutput()
      } else {
        private$addSingleColorIndex()
      }
    },
    # color is defined by output
    addColorIndexByOutput = function() {
      for (fieldName in c(".dataSimulated", ".dataObserved")) {
        if (nrow(private[[fieldName]]) > 0) {
          private[[fieldName]][, colorIndex := outputPathId]
          private[[fieldName]]$colorIndex <-
            factor(private[[fieldName]]$colorIndex,
                   levels = private$.dtOutputPaths$outputPathId, # nolint indentation_linter
                   labels = private$.dtOutputPaths$displayNameOutputs,
                   ordered = TRUE
            )
        }
      }
      private$.scaleVectors[["colour"]] <- getScalevector(
        namesOfScaleVector = levels(private$.dataSimulated$colorIndex),
        listOfValues = list(
          private$.dtOutputPaths$color,
          private$.dtOutputPaths$fill,
          getDefaultColorsForScaleVector(
            shade = "dark",
            n = length(levels(private$.dataSimulated$colorIndex))
          )
        )
      )
      if (self$hasObservedData() | self$hasSimulatedPop()) {
        private$.scaleVectors[["fill"]] <- getScalevector(
          namesOfScaleVector = levels(private$.dataSimulated$colorIndex),
          listOfValues = list(
            private$.dtOutputPaths$fill,
            private$.dtOutputPaths$color,
            getDefaultColorsForScaleVector(
              shade = "light",
              n = length(levels(private$.dataSimulated$colorIndex))
            )
          )
        )
      }
    },
    # color is defined by reference
    addColorIndexByScenarioType = function(referenceScaleVector) {
      namesVector <- names(referenceScaleVector)
      referenceColorScaleVector <-
        setNames(
          sapply(namesVector, function(name) {
            referenceScaleVector[[name]][1]
          }),
          namesVector
        )
      referenceFillScaleVector <-
        setNames(
          sapply(namesVector, function(name) {
            referenceScaleVector[[name]][2]
          }),
          namesVector
        )


      private$.dataSimulated[, colorIndex := scenarioType]
      private$.dataSimulated$colorIndex <-
        factor(private$.dataSimulated$colorIndex,
               levels = c("scenario", "referenceScenario"), # nolint indentation_linter
               labels = names(referenceColorScaleVector),
               ordered = TRUE
        )
      if (nrow(private$.dataObserved) > 0) {
        private$.dataObserved[, colorIndex := names(referenceColorScaleVector)[1]]
        private$.dataObserved$colorIndex <-
          factor(private$.dataObserved$colorIndex,
                 levels = names(referenceColorScaleVector), # nolint indentation_linter
                 ordered = TRUE
          )
      }

      if (any(is.na(referenceColorScaleVector))) {
        private$.scaleVectors[["colour"]] <-
          setNames(
            c(getDefaultColorsForScaleVector(shade = "dark", n = 1), "grey"),
            names(referenceColorScaleVector)
          )
      } else {
        private$.scaleVectors[["colour"]] <- referenceFillScaleVector
      }

      if (self$hasObservedData() | self$hasSimulatedPop()) {
        if (any(is.na(referenceColorScaleVector))) {
          private$.scaleVectors[["fill"]] <-
            setNames(
              c(getDefaultColorsForScaleVector(shade = "light", n = 1), "grey"),
              names(referenceColorScaleVector)
            )
        } else {
          private$.scaleVectors[["fill"]] <- referenceFillScaleVector
        }
      }
    },
    # only one color needed
    addSingleColorIndex = function() {
      colorIndexTxt <- if (self$hasSimulatedPop()) {
        paste(
          "Simulated", private$.tpLabelSimulatedMean,
          "\nwith", private$.tpLabelSimulatedRange
        ) # nolint: line_length
      } else {
        paste("Simulated", private$.tpLabelSimulatedMean)
      }

      for (fieldName in c(".dataSimulated", ".dataObserved")) {
        if (nrow(private[[fieldName]]) > 0) {
          private[[fieldName]][, colorIndex := colorIndexTxt]
          private[[fieldName]]$colorIndex <-
            factor(private[[fieldName]]$colorIndex,
                   levels = colorIndexTxt)
        }
      }
      private$.scaleVectors[["colour"]] <- getScalevector(
        namesOfScaleVector = levels(private$.dataSimulated$colorIndex)[1],
        listOfValues = list(getDefaultColorsForScaleVector("dark", n = 1))
      )
      if (self$hasSimulatedPop() | self$hasObservedData()) {
        private$.scaleVectors[["fill"]] <- getScalevector(
          namesOfScaleVector = levels(private$.dataSimulated$colorIndex)[1],
          listOfValues = list(getDefaultColorsForScaleVector("light", n = 1))
        )
      }
    },

    # Add column for shape index
    addShapeIndexColumn = function() {
      if (!self$useShapeIndex()) {
        return(invisible())
      }
      private$.dataObserved[, shapeIndex := group]
      private$.dataObserved$shapeIndex <-
        factor(private$.dataObserved$shapeIndex,
               levels = private$.dtDataGroups$group, # nolint indentation_linter
               labels = private$.dtDataGroups$displayName,
               ordered = FALSE
        )
      private$.scaleVectors[["shape"]] <- getScalevector(
        namesOfScaleVector = levels(private$.dataObserved$shapeIndex),
        listOfValues = list(
          private$.dtDataGroups$shape,
          getDefaultShapesForScaleVector(n = length(levels(private$.dataObserved$shapeIndex)))
        )
      )
    }
  )
)


# helper function -------------
#' Get Plot ID for Columns
#'
#' This function retrieves the Plot ID for specified columns in the configuration table.
#'
#' @param configTable A data frame containing the configuration data.
#' @param col The column name to be processed.
#' @return A data table with updated Plot IDs.
#' @keywords internal
getPlotIdForColumns <- function(configTable, col) {
  # avoid warning for global variable
  nBracketOpen <- nBracketClosed <- countAdd <- plotId <- NULL

  colNew <- sub("s$", "", col)
  colNew <- paste0(tolower(substring(colNew, 1, 1)), substring(colNew, 2))

  # use a copy of configTable, to make sure class is not changed outside
  dtCaption <-
    tidyr::separate_rows(data.table::setDF(data.table::copy(configTable)), !!col,
                         sep = ",\\s*|(?<=\\)),\\s*|\\s(?=\\()") %>%
    data.table::setDT() %>%
    data.table::setnames(old = col,
                         new = colNew)
  dtCaption[, (colNew) := trimws(get(colNew))]

  dtCaption[, nBracketOpen := cumsum(grepl("\\(", get(colNew)))]
  dtCaption[, nBracketClosed := cumsum(grepl("\\)", get(colNew)))]
  dtCaption[, countAdd := nBracketClosed - nBracketOpen + 1]
  dtCaption[, plotId := cumsum(data.table::shift(countAdd, fill = 1))]
  dtCaption[, (colNew) := trimws(gsub("[()]", "", get(colNew)))]

  return(dtCaption %>% dplyr::select(!c("nBracketOpen", "nBracketClosed", "countAdd")))
}

#' Split Caption by Individuals
#'
#' This function splits the caption data by individual IDs.
#'
#' @param configTable A data frame containing the configuration data.
#' @param individualIdVector A vector of individual IDs.
#' @param dtCaption A data table containing the caption data.
#' @return A data table with merged caption data by individuals.
#' @keywords internal
splitCaptionByIndividuals <- function(configTable, individualIdVector, dtCaption) {
  # avoid warning for global variable
  individualIds <- plotId <- NULL

  tmp <- data.table::copy(configTable) %>%
    data.table::setDT()
  tmp[, individualIds := gsub("\\*", paste(individualIdVector, collapse = ","), individualIds),
      by = "individualIds" # nolint indentation_linter
  ]

  dtCaptionInd <- getPlotIdForColumns(configTable = tmp, col = "individualIds")

  dtCaptionMerge <- merge(
    dtCaption[, -c("individualIds")],
    dtCaptionInd[, c("scenarioIndex", "individualId", "plotId")],
    by = "scenarioIndex",
    suffixes = c(".O", ".Ind"),
    allow.cartesian = TRUE
  )

  tmpCount <- dtCaptionMerge %>%
    dplyr::select(c("plotId.O", "plotId.Ind")) %>%
    unique() %>%
    data.table::setorderv(c("plotId.Ind", "plotId.O"))

  tmpCount[, plotId := .I]

  dtCaptionMerge <- dtCaptionMerge %>%
    merge(tmpCount, by = c("plotId.O", "plotId.Ind"), sort = FALSE) %>%
    dplyr::select(!c("plotId.O", "plotId.Ind")) %>%
    data.table::setorderv(c("plotId"))

  return(dtCaptionMerge)
}

#' Add Time Tags to Caption
#'
#' This function adds time tags to the caption data based on the specified time tags.
#'
#' @param dtCaption A data table containing the caption data.
#' @param timeTags A vector of time tags to be added.
#' @param facetType The type of facet being used.
#' @return A data table with added time tags.
#' @keywords internal
addTimeTagsToCaption <- function(dtCaption, timeTags, facetType) {
  # avoid warnings for global variables
  timeRangeTag <- plotId <- NULL

  dtCaption <-
    data.table::rbindlist(lapply(
      timeTags,
      function(tag) {
        dtCaption %>%
          dplyr::mutate(timeRangeTag = tag)
      }
    ))

  dtCaption$timeRangeTag <-
    factor(dtCaption$timeRangeTag,
           levels = levels(timeTags),
           ordered = TRUE)

  if (facetType == FACETTYPE$vsTimeRange) {
    dtCaption[, plotId := (plotId - 1) * length(levels(timeTags)) +
                as.numeric(timeRangeTag)]
  }

  return(dtCaption)
}
#' Determine Facet Columns
#'
#' This function determines the number of facet columns based on the caption data and facet type.
#'
#' @param dtCaption A data table containing the caption data.
#' @param nFacetColumns The number of facet columns.
#' @param facetType The type of facet being used.
#' @param plotName The name of the plot.
#' @return The number of facet columns to be used.
#' @keywords internal
determineFacetColumns <- function(dtCaption, nFacetColumns, facetType, plotName) {
  # Initialize variables used for data.tables
  rowNumber <- NULL

  if (dplyr::n_distinct(dtCaption$plotId) == 1) {
    return(NULL)
  }
  nCol <- NULL
  data.table::setorderv(dtCaption, c("plotId", "outputPathId"))

  if (facetType == FACETTYPE$vsOutput) {
    tmp <- dtCaption %>%
      dplyr::select('plotId','outputPathId') %>%
      unique()
    tmp <- tmp[, .(rowNumber = .I), by = "outputPathId"] %>%
      .[, .(diff = rowNumber - data.table::shift(rowNumber, 1)),
        by = "outputPathId"
      ]
    uniqueDiff <- unique(tmp[!is.na(diff)]$diff)
    if (length(uniqueDiff) == 0) {
      nCol <- dplyr::n_distinct(dtCaption$outputPathId)
    } else if (length(uniqueDiff) == 1) {
      nCol <- uniqueDiff
    } else {
      warning(
        paste0(
          'Plot "',
          plotName,
          '" is not suited for FacetType "',
          FACETTYPE$vsOutput,
          '" use "',
          FACETTYPE$byOrder,
          '" instead.'
        )
      )
      nCol <- nFacetColumns
    }
  } else if (facetType == FACETTYPE$vsTimeRange) {
    nCol <- length(levels(dtCaption$timeRangeTag))
  } else {
    nCol <- nFacetColumns
  }


  return(nCol)
}

#' Set Time Range Filter
#'
#' This function sets the time range filter based on the facet type and time tags.
#'
#' @param facetType The type of facet being used.
#' @param timeTags A vector of time tags.
#' @return A list of time range filters.
#' @keywords internal
setTimeRangeFilter <- function(facetType, timeTags) {
  if (facetType == FACETTYPE$vsTimeRange) {
    timeRangeTagFilter <- list(allTimeRanges = "TRUE")
  } else {
    timeRangeTagFilter <-
      lapply(
        levels(timeTags),
        function(x) {
          paste0("timeRangeTag == '", x, "'")
        }
      )
    names(timeRangeTagFilter) <- levels(timeTags)
  }

  return(timeRangeTagFilter)
}

#' Restructure Application Time by scenario Index
#'
#' This function restructures application time based on the scenario index.
#'
#' @param applicationTimes A list of application times.
#' @param configTable A data frame containing the configuration data.
#' @return A list of application times by scenario index.
#' @keywords internal
restructureApplicationTimeByScenarioIndex <- function(applicationTimes, configTable) {
  applicationTimesByIndex <- list()

  for (scenarioIndex in seq_len(nrow(configTable))) {
    configList <- as.list(configTable[scenarioIndex, ])

    unitFactor <- ospsuite::toUnit(
      values = 1,
      quantityOrDimension = "Time",
      targetUnit = configList$timeUnit
    )
    timeOffset <- as.double(configList$timeOffset_Reference) +
      as.double(configList$timeOffset)

    applicationTimesByIndex[[scenarioIndex]] <-
      lapply(applicationTimes[[configList$scenario]], function(t) {
        t * unitFactor - timeOffset
      })
  }

  return(applicationTimesByIndex)
}

#' Get Output Paths per scenario
#'
#' This function retrieves output paths for each scenario based on the configuration table.
#'
#' @param configTable A data frame containing the configuration data.
#' @return A list of output paths per scenario.
#' @keywords internal
getOutputPathsPerScenario <- function(configTable, dtOutputPaths) {
  # avoid warning for global variable
  outputPathId <- NULL

  outputPathsPerScenario <- list()
  for (scenarioType in c("scenario", "referenceScenario")) {
    outputPathsPerScenario <- utils::modifyList(
      outputPathsPerScenario,
      lapply(lapply(
        split(configTable[!is.na(get(scenarioType))], by = scenarioType),
        getElement,
        "outputPathIds"
      ), function(x) {
        outputs <- x %>%
          unique() %>%
          gsub(pattern = "[()]", replacement = "") %>%
          splitInputs() %>%
          unique()
        outputPaths <- dtOutputPaths[outputPathId %in% outputs]$outputPath %>%
          unique()
        return(outputPaths)
      })
    )
  }
  return(outputPathsPerScenario)
}

#' Get Observed Unit Conversion Data Table
#'
#' This function retrieves the unit conversion data table for observed data.
#'
#' @param dataObserved A data frame containing observed data.
#' @param dtUnit A data table containing unit information.
#' @return A data table with unit conversions for observed data.
#' @keywords internal
getObservedUnitConversionDT <- function(dataObserved, dtUnit) {
  # avoid warnings for global variables
  unitFactor <- NULL

  dtUnitObserved <- dataObserved %>%
    dplyr::select(c("outputPathId", "yUnit")) %>%
    unique() %>%
    merge(dtUnit %>% dplyr::select(-c("unitFactor", "yUnit")),
          by = "outputPathId" # nolint indentation_linter
    )
  dtUnitObserved[, unitFactor := apply(dtUnitObserved, 1, function(row) {
    ospsuite::toUnit(
      quantityOrDimension = row["dimension"],
      values = 1,
      sourceUnit = row["yUnit"],
      targetUnit = row["displayUnit"],
      molWeight = as.double(row["molWeight"]),
      molWeightUnit = "g/mol"
    )
  })]

  return(dtUnitObserved)
}

#' Add Time Range Tags to Data
#'
#' This function adds time range tags to the provided data based on the configuration table.
#'
#' @param timeRangeColumns A vector of time range column names.
#' @param dataOld A data table containing the old data.
#' @param configTable A data frame containing the configuration data.
#' @param applicationTimes A list of application times.
#' @return A data table with added time range tags.
#' @keywords internal
addTimeRangeTagsToData = function(timeRangeColumns, dataOld, configTable, applicationTimes,timeTags) {
  # avoid warnings for global variables
  xValues <- NULL

  dt <- data.table()
  for (col in timeRangeColumns) {
    tag <- gsub("^timeRange_", "", col)

    for (scenarioIndex in configTable[!is.na(get(col))]$scenarioIndex) {
      configList <- as.list(configTable[scenarioIndex, ])

      if (configList[[col]] == "total") {
        tRange <- c(-Inf, Inf)
      } else if (configList[[col]] == "firstApplication") {
        tRange <- c(
          applicationTimes[[scenarioIndex]]$startOfFirstApplication,
          applicationTimes[[scenarioIndex]]$endOfFirstApplication
        )
      } else if (configList[[col]] == "lastApplication") {
        tRange <- c(
          applicationTimes[[scenarioIndex]]$startOfLastApplication,
          Inf
        )
      } else {
        tRange <- eval(parse(text = configList[[col]]))
      }

      dataNew <- dataOld[xValues >= tRange[1] & xValues <= tRange[2] & scenarioIndex == scenarioIndex]
      dataNew[, timeRangeTag := tag]

      timeshift <- timeTags[tag==gsub("^timeRange_", "", col)]$timeShift
      if (is.na(timeshift) | timeshift == '') timeshift <- tRange[1]
      if(!is.finite(timeshift)) timeshift <- 0
      dataNew[,xValues := xValues - timeshift]

      dt <- rbind(dt,dataNew)
    }
  }
  return(dt)
}

#' Finalize Caption Table
#'
#' This function finalizes the caption table by updating plot IDs, generating plot tags,
#' and merging with output paths to create a comprehensive data table for plotting.
#'
#' @param dtCaption A data.table containing the initial caption data with at least a `plotId` column.
#' @param timeTags A data.table with time tags and corresponding captions, must include `tag` and `captionText` columns.
#' @param dtOutputPaths A data.table containing output paths with at least `outputPathId` and `displayName` columns.
#' @param nFacetColumns An integer specifying the number of facet columns, defaults to NULL.
#' @param nMaxFacetRows An integer specifying the maximum number of facet rows.
#' @param facetType The type of facet being used.
#'
#' @return A data.table that includes updated plot IDs, plot tags, and merged output display names.
#' @keywords internal
finalizeCaptionTable <- function(dtCaption, timeTags, dtOutputPaths, nFacetColumns, nMaxFacetRows,facetType) {
  # Initialize variables used for data.tables
  counter <- plotId <- plotTag <- outputPathId <- NULL

  if (facetType == FACETTYPE$byIndividual){
    dtCaption[, counter := .GRP, by = individualId]
    data.table::setorderv(dtCaption,c('counter','plotId'))
    dtCaption[, plotId := seq_len(.N), by = individualId]
  } else{
    maxPlotId <- ifelse(is.null(nFacetColumns), 1, nFacetColumns) * nMaxFacetRows

    # Update the counter based on the condition
    dtCaption[, counter := ceiling(plotId / maxPlotId)]
    dtCaption[, plotId := plotId - (counter - 1) * maxPlotId]

  }


  dtCaption[, plotTag := generatePlotTag(plotId)]

  dtCaption$plotTag <-
    factor(dtCaption$plotTag,
           ordered = TRUE)

  dtCaption$timeRangeCaption <- factor(
    dtCaption$timeRangeTag,
    ordered = TRUE,
    levels = timeTags$tag,
    labels = timeTags$captionText
  )

  dtCaption <- dtCaption %>%
    merge(dtOutputPaths %>% dplyr::select('outputPathId', "displayNameOutputs"),
          by = "outputPathId" # nolint indentation_linter
    ) %>%
    dplyr::select(dplyr::any_of(c(
      "scenarioIndex", "outputPathId", "plotTag", "counter",
      "scenarioLongName", "displayNameOutputs", "dataGroupIds",
      "timeRangeTag", "timeRangeCaption", "individualId"
    )))

  data.table::setorderv(dtCaption,c('counter','plotTag'))

  return(dtCaption)
}

#' Update Data Table Caption
#'
#' This function updates the provided data table caption by modifying the
#' individual IDs based on a given configuration table. If the configuration
#' table contains any individual IDs marked with '(*)', those IDs will be
#' removed from the data table caption.
#'
#' @param dtCaption A data.table containing the captions to be updated.
#' @param configTable A data.table containing configuration information,
#'                     including individual IDs.
#'
#' @return A data.table with updated captions, where individual IDs marked
#'         with '(*)' are removed.
#' @keywords internal
updateDtCaption <- function(dtCaption, configTable) {
  # avoid warning for global variable
  individualIds <- scenarioIndex <- individualId <- NULL # nolint object_name_linter

  if (!any(configTable[!is.na(individualIds)]$individualIds == "(*)")) {
    return(dtCaption)
  }

  dtCaption[scenarioIndex %in% unique(configTable[individualIds == "(*)"]), individualId := ""]
  dtCaption <- unique(dtCaption)

  return(dtCaption)
}
