#' List of functions and strings used for log, warning, and error messages
#' @keywords internal
messages <- list()

messages$infoVirtualTwinPopulationSheetShifted <- function() {
  "Shift sheet 'VirtualTwinPopulation' from 'Individual.xlsx' to 'Population.xlsx'."
}

messages$infoNoGroupsForVirtualTwinPopulationCreation <- function() {
  "No groups available for virtual twin population creation."
}

messages$infoAddVirtualTwinPopulationConfig <- function() {
  "Add virtual twin population configuration to the Population configuration file:"
}

messages$infoNoNewVirtualTwinPopulationsToGenerate <- function() {
  "No new virtual twin populations to generate; all files already exist."
}

messages$warningSuspiciousProportionOfFemales <- function(suspiciousRows) {
  paste(
    "You have very small values for 'ProportionOfFemales' in the population configurations.\n",
    "The unit is percent, not fraction. Are you sure?\n",
    paste(
      paste(
        suspiciousRows$populationName,
        suspiciousRows$proportionOfFemales,
        sep = ": "
      ),
      collapse = "; "
    )
  )
}

messages$errorInconsistentCustomParameterValues <- function(
  parameterPath,
  populationName
) {
  paste(
    "Inconsistent number of values for",
    parameterPath,
    "in",
    populationName
  )
}

messages$infoNoNewVirtualPopulationsToGenerate <- function() {
  "No new virtual populations to generate; all files already exist."
}

messages$errorInconsistentVirtualPopulationParameters <- function(
  inconsistentParameters,
  observedIndividualId
) {
  paste(
    "Population parameters must be consistent within a virtual population. Check",
    paste(inconsistentParameters, collapse = ", "),
    "for",
    observedIndividualId
  )
}


messages$errorPlotDataTimeProfileNoSimulatedData <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "No simulated data found for",
      self$configTable$plotName[1]
    )),
    envir = env
  )
}

messages$errorPlotDataTimeProfileAllDataOutsideTimeRange <- function(
  env = parent.frame()
) {
  eval(
    quote(glue::glue(
      "All simulated data are outside the time range for {self$configTable$plotName[1]}."
    )),
    envir = env
  )
}

messages$errorPlotDataTimeProfileUnsupportedMultipleUnits <- function(
  env = parent.frame()
) {
  eval(
    quote(glue::glue(
      "Plot {self$configTable$plotName[1]} uses a plotType that is not suited for multiple units. Only TimeProfile can handle a secondary axis with a second unit. Split outputPathId into separate rows in the plot configuration xlsx."
    )),
    envir = env
  )
}

messages$warningPlotDataTimeProfileNoObservedData <- function(
  env = parent.frame()
) {
  eval(
    quote(glue::glue(
      "For plot {self$configTable$plotName[1]}, no observed data are available for {paste(plotCols, collapse = ', ')}. The plots will be omitted."
    )),
    envir = env
  )
}

messages$errorplotDemographicsCategoricalXAxisNotAllowed <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Categorical values are not allowed on the x-axis for range plots. Check plotName",
      onePlotConfig$plotName[1]
    )),
    envir = env
  )
}

messages$errorplotDemographicsMissingParameterPaths <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Parameter path(s)",
      paste(setdiff(modelPaths, names(dtPop)), collapse = ", "),
      "are not available for",
      scenarioName
    )),
    envir = env
  )
}

messages$errorplotDemographicsMissingInputs <- function(
  env = parent.frame()
) {
  eval(
    quote(
      "Inputs are missing. Please provide scenarioList and/or pkParameterDT."
    ),
    envir = env
  )
}

messages$errorplotDemographicsInvalidParameterIds <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "The ParameterIds are not valid model parameters. If they are PK parameters, pkParameterDT must be provided as input.",
      configTablePlots$plotName[1]
    )),
    envir = env
  )
}

messages$errorplotDemographicsMissingConfiguredModelParameters <- function(
  plotName,
  missingColumns
) {
  paste(
    "Configured model parameters are not available in the selected population(s). Check plotName",
    plotName,
    paste(missingColumns, collapse = "; ")
  )
}

messages$errorplotDemographicsCategoricParametersNotSupportedForDistribution <- function(
  plotName,
  invalidColumns
) {
  paste(
    "Categoric model parameters are not supported for plotDistributionVsDemographics. Check plotName",
    plotName,
    paste(invalidColumns, collapse = "; ")
  )
}

messages$warningplotDemographicsCategoricalParametersIgnored <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Categorical parameters are not suitable for this kind of plot and will be ignored:",
      .concatWithAnd(unique(
        plotData[!is.na(categoricValue), ]$parameterId
      ))
    )),
    envir = env
  )
}

messages$errorplotPKBoxwhiskerMultipleCombinationsPerPlot <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Per plot only one combination of scenario, outputPathId and pkParameter is allowed. Please check plot",
      paste(tmp$plotName %>% unique(), collapse = ", ")
    )),
    envir = env
  )
}

messages$errorplotPKBoxwhiskerMissingPlotMode <- function(
  env = parent.frame()
) {
  eval(
    quote("Please select either Plot_Ratio or Plot_Absolute."),
    envir = env
  )
}

messages$errorplotPKBoxwhiskerRatioRequiresSamePopulation <- function(
  env = parent.frame()
) {
  eval(
    quote(
      "Ratio plots are only available if scenario and referenceScenario are based on the same population."
    ),
    envir = env
  )
}

messages$errorplotPKBoxwhiskerMissingReferenceScenario <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "For ratio plots, at least one reference scenario must be selected. Check plotName",
      paste(tmp[isValid == FALSE, ]$plotName, collapse = ", ")
    )),
    envir = env
  )
}

messages$errorplotSensitivityMultipleRowsPerScenario <- function(
  env = parent.frame()
) {
  eval(
    quote(
      "For non-representative sensitivity mode, only one row per scenario is allowed within a plotName."
    ),
    envir = env
  )
}

messages$errorplotSensitivityInvalidReferenceScenario <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Invalid referenceScenario value(s) in sensitivity configuration:",
      paste(invalidReferenceScenarios, collapse = ", "),
      "Allowed values are scenario names from the Scenarios sheet."
    )),
    envir = env
  )
}

messages$errorplotSensitivityMissingResultFile <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Missing sensitivity result file(s) for sensitivity configuration row(s):",
      paste(
        sprintf(
          "%s [scenario=%s, sheet=%s, reference=%s] -> %s",
          missingResultFiles$plotName,
          missingResultFiles$scenario,
          missingResultFiles$sensitivityParameterSheet,
          ifelse(
            is.na(missingResultFiles$referenceScenario) |
              missingResultFiles$referenceScenario == "",
            "<none>",
            missingResultFiles$referenceScenario
          ),
          missingResultFiles$resultFile
        ),
        collapse = "; "
      )
    )),
    envir = env
  )
}

messages$warningplotPKBoxwhiskerNoDataForPlot <- function(
  env = parent.frame()
) {
  eval(
    quote(paste("No data available for", onePlotConfig$plotName[1])),
    envir = env
  )
}

messages$errorplotPKForestMixedBasePopulationComparison <- function(
  env = parent.frame()
) {
  eval(
    quote(
      "Within one plot, compare either only scenarios with the same base population or only scenarios with different base populations."
    ),
    envir = env
  )
}

messages$errorplotPKForestMissingObservedData <- function(
  env = parent.frame()
) {
  eval(
    quote(
      "Please provide aggregated observed PK-Parameter data for this kind of plot."
    ),
    envir = env
  )
}

messages$errorplotTimeProfilePanelsMultiplePlotNames <- function(
  env = parent.frame()
) {
  eval(
    quote("onePlotConfig must contain exactly one plotName."),
    envir = env
  )
}

messages$errorplotTimeProfilePanelsInconsistentAggregationMethods <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Aggregation methods are not consistent:",
      paste(
        paste(tmp$dataType, tmp$yErrorType, sep = ": "),
        collapse = ", "
      )
    )),
    envir = env
  )
}

messages$errorplotTimeProfilePanelsUnknownPlotType <- function(
  env = parent.frame()
) {
  eval(quote(paste("Unknown plot type:", plotType)), envir = env)
}

messages$errorplotTimeProfilePanelsDataOutsideYLimit <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Data outside y limit for",
      plotData$plotName,
      "within time range",
      timeRangeFilter
    )),
    envir = env
  )
}

messages$errorplotTimeProfilePanelsBracketedOutputsWithReferenceScenarios <- function(
  env = parent.frame()
) {
  eval(
    quote(paste0(
      "Do not combine outputs in one panel with brackets () ",
      'if you want to display reference scenarios. Please check "',
      paste0(unique(tmp$plotName), collapse = '", "', '"')
    )),
    envir = env
  )
}

messages$errorplotTimeProfilePanelsReferenceScenariosNeedColorLegend <- function(
  env = parent.frame()
) {
  eval(
    quote(paste0(
      "Plot configurations with reference scenarios must define a color legend. Check ",
      '"',
      paste0(unique(tmp$plotName), collapse = '", "', '"')
    )),
    envir = env
  )
}

messages$errorplotTimeProfilePanelsL2TooManyUnits <- function(
  env = parent.frame()
) {
  eval(quote("Do not combine more than two y units in one panel."), envir = env)
}

messages$errorplotTimeProfilePanelsL2MissingTimeRange <- function(
  env = parent.frame()
) {
  eval(quote("At least one TimeRange column is required."), envir = env)
}

messages$errorplotTimeProfilePanelsL2InvalidTimeRangeFormat <- function(
  env = parent.frame()
) {
  eval(quote('Invalid input in one of the "TimeRange" columns.'), envir = env)
}

messages$errorplotTimeProfilePanelsL2TimeRangeParseError <- function(
  env = parent.frame()
) {
  eval(quote('Failed to parse one of the "TimeRange" columns.'), envir = env)
}

messages$errorplotTimeProfilePanelsL2UnmatchedBrackets <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Please check the brackets in column",
      column,
      paste(tmp[[column]], collapse = "; ")
    )),
    envir = env
  )
}

messages$errorplotTimeProfilePanelsL2VirtualPopMissingIndividualIds <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "For scenarios with virtual twin populations, column individualIds must be filled.",
      'Use "*" or "(*)" if you want to plot all individuals. Brackets are not allowed for time profile plots.',
      "Check scenarios:",
      paste(indScenariosNames, collapse = ", ")
    )),
    envir = env
  )
}

messages$errorplotTimeProfilePanelsL2BracketsDisallowedInIndividualIds <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "For scenarios with virtual twin populations and Plot_TimeProfiles selected, brackets are not allowed in column individualIds.",
      "Check plots:",
      paste(tmp$plotName[errorRows], collapse = ", ")
    )),
    envir = env
  )
}

messages$errorplotTimeProfilePanelsL2ScenarioTypeConflict <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "scenario and referenceScenario must both be populations or both individuals.",
      "Check plots:",
      paste(tmp$plotName, collapse = ", ")
    )),
    envir = env
  )
}

messages$warningplotTimeProfilePanelsIndividualIdsIgnored <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      'Column "individualIds" is filled, but no data group is selected and the scenario is not a virtual twin population scenario. "individualIds" will be ignored.',
      "Check plots:",
      paste(tmp$plotName, collapse = ", ")
    )),
    envir = env
  )
}

messages$errorprojectconfigurationFieldReadOnly <- function(
  env = parent.frame()
) {
  eval(quote(paste0(FIELD, " is read-only")), envir = env)
}

messages$errorprojectconfigurationVersionReadOnly <- function(
  env = parent.frame()
) {
  eval(quote("ospsuiteReportingFrameworkVersion is read-only"), envir = env)
}

messages$errorprojectconfigurationAbortedByUser <- function(
  env = parent.frame()
) {
  eval(quote("Aborted by user."), envir = env)
}

messages$errorQmdPlotManagerInvalidFileOrSubfolderName <- function(
  env = parent.frame()
) {
  eval(
    quote(
      "Please provide a valid name for the .qmd file and its subfolder."
    ),
    envir = env
  )
}

messages$errorQmdPlotManagerMissingPlotFunction <- function(
  env = parent.frame()
) {
  eval(
    quote(paste("Function", nameOfplotFunction, "does not exist.")),
    envir = env
  )
}

messages$errorQmdPlotManagerFileNameMustBeBasename <- function(
  env = parent.frame()
) {
  eval(
    quote(
      "Provide fileName as a base name only. The file will be saved in the folder defined by the class object."
    ),
    envir = env
  )
}

messages$errorQmdPlotManagerDuplicateKey <- function(env = parent.frame()) {
  eval(
    quote(paste0(
      'key "',
      key,
      '" was already added. Figure and table keys must be unique.'
    )),
    envir = env
  )
}

messages$warningQmdPlotManagerMissingCaption <- function(
  env = parent.frame()
) {
  eval(quote(paste("Caption is missing for key", caption)), envir = env)
}

messages$infoQmdPlotManagerNoValidationFunction <- function(
  env = parent.frame()
) {
  eval(
    quote("No specific plot configuration validation function is available."),
    envir = env
  )
}

messages$errorscenariomanagementMissingSimulationResults <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Error: Simulation results for scenario(s)",
      paste(scenarioNames[!file.exists(resultFiles)], collapse = ", "),
      "do not exist."
    )),
    envir = env
  )
}

messages$errorscenariomanagementInvalidOntogenyStructure <- function(
  env = parent.frame()
) {
  eval(
    quote(paste("The ontogeny has the wrong structure:", ontogeny)),
    envir = env
  )
}

messages$errorutilitiesaggregationUnsupportedLegendSize <- function(
  env = parent.frame()
) {
  eval(
    quote(
      "Error: Unsupported legendsize. Please provide a legendsize of 2 or 3."
    ),
    envir = env
  )
}

messages$errorutilitiesdataNoDataFilesDefined <- function(
  env = parent.frame()
) {
  eval(quote(paste("No data files defined for", dataClassType)), envir = env)
}

messages$errorutilitiesdataNumericValuesSheetMissing <- function(
  env = parent.frame()
) {
  eval(
    quote(paste("Numeric values sheet not found:", sheetName)),
    envir = env
  )
}

messages$errorutilitiesdataDuplicateRowsInIdentifierColumns <- function(
  env = parent.frame()
) {
  eval(
    quote(
      paste(
        "Data must be unique in columns",
        paste(colIdentifier, collapse = ", ")
      )
    ),
    envir = env
  )
}

messages$errorutilitiesdataDictionarySourceOrFilterMissing <- function(
  env = parent.frame()
) {
  eval(
    quote(paste0(
      'Either sourceColumn or Filter on sourceColumn has to be filled in dictionary "',
      sheet,
      '" for targetColumn(s) "',
      paste(tmp$targetColumn, collapse = '", "'),
      '"'
    )),
    envir = env
  )
}

messages$errorutilitiesdataFilterEvaluationFailed <- function(
  env = parent.frame()
) {
  eval(quote(conditionMessage(err)), envir = env)
}

messages$errorutilitiesdataL4NonUniqueYUnit <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "DataDT to combinedData: y unit for dataset",
      groupName,
      "is not unique."
    )),
    envir = env
  )
}

messages$errorutilitiesdataL4NonUniqueXUnit <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "DataDT to combinedData: x unit for dataset",
      groupName,
      "is not unique."
    )),
    envir = env
  )
}

messages$errorutilitiesdataL4IndividualDataMissingMetaId <- function(
  env = parent.frame()
) {
  eval(
    quote("IndividualData requires metadata column individualId."),
    envir = env
  )
}

messages$errorutilitiesdataL4AggregationCheckColumnsMissing <- function(
  env = parent.frame()
) {
  eval(
    quote(
      "For custom aggregation, provide lloqCheckColumns2of3 or lloqCheckColumns1of2."
    ),
    envir = env
  )
}

messages$errorutilitiesdataDuplicateVariableNames <- function(
  env = parent.frame()
) {
  eval(
    quote("Duplicate variableName entries found in numeric values sheets"),
    envir = env
  )
}

messages$errorutilitiesdataMissingVariableNames <- function(
  env = parent.frame()
) {
  eval(
    quote("Numeric values sheet contains missing or empty variableName"),
    envir = env
  )
}

messages$errorutilitiesdataNonNumericValues <- function(
  env = parent.frame()
) {
  eval(
    quote("Numeric values sheet contains non-numeric or missing values"),
    envir = env
  )
}

messages$warningutilitiesdataExecutionFailedRetryManually <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Error during execution of",
      call$functionCall,
      "\nMessage:",
      conditionMessage(err),
      "\nAre all relevant xlsx files closed? Retry manually."
    )),
    envir = env
  )
}

messages$warningutilitiesdataMissingColumnAttributes <- function(
  env = parent.frame()
) {
  eval(
    quote(
      paste0(
        'Some data columns have no attribute: "',
        paste(
          setdiff(names(dataDT), columnsWithAttributes),
          collapse = '", "'
        ),
        '"'
      )
    ),
    envir = env
  )
}

messages$warningutilitiesdataColumnContainsMissingValues <- function(
  env = parent.frame()
) {
  eval(
    quote(paste("Data contains NAs or empty values in column", col)),
    envir = env
  )
}

messages$warningutilitiesdataL3AmbiguousUnits <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Ambiguous units:",
      summaryString,
      "\nPlease check whether this is acceptable, for example pkParameter as ratio and absolute values."
    )),
    envir = env
  )
}

messages$warningutilitiesdataDictionaryFilterContext <- function(
  env = parent.frame()
) {
  eval(
    quote(paste0(
      "tpDictionary: '",
      dictionaryName,
      "'; targetColumn: '",
      myFilter$targetColumn,
      "'; filter: '",
      myFilter$filter,
      "'; filterValue: '",
      myFilter$filterValue,
      "'"
    )),
    envir = env
  )
}

messages$warningutilitiesdataUnknownGender <- function(env = parent.frame()) {
  eval(quote("Unknown gender in data set."), envir = env)
}

messages$warningutilitiesdataL4MultipleLloqMinimal <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "DataDT to combinedData: More than one LLOQ for dataset",
      groupName,
      "is set to minimal."
    )),
    envir = env
  )
}

messages$warningutilitiesdataNoGroupsForAggregation <- function(
  env = parent.frame()
) {
  eval(quote("No groups available for aggregation."), envir = env)
}

messages$warningutilitiesdataL4UnusedAggregationCheckColumns <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Input variables lloqCheckColumns2of3 and lloqCheckColumns1of2 are not used for aggregationFlag",
      aggregationFlag
    )),
    envir = env
  )
}

messages$warningutilitiesdataL4ColumnCommasReplaced <- function(
  env = parent.frame()
) {
  eval(
    quote(paste("Column", col, "commas were replaced by _.")),
    envir = env
  )
}

messages$warningutilitiesdataL4UnsuitableGroupsForAggregation <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Groups",
      paste(unsuitableGroups, collapse = ", "),
      "are not suitable for aggregation.",
      "Check whether they are available in the data, have more than",
      minN,
      "individuals, or have data class",
      DATACLASS$tpIndividual
    )),
    envir = env
  )
}

messages$errorutilitiesmarkdownMissingFigureOrTableFile <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "No file exists for key. There should be either",
      figureFile,
      "or",
      tableCsv
    )),
    envir = env
  )
}

messages$errorutilitiesmarkdownInvalidSourceRmdExtension <- function(
  env = parent.frame()
) {
  eval(
    quote(
      "Error: One or more elements of sourceRmds have an extension other than .Rmd."
    ),
    envir = env
  )
}

messages$errorutilitiesmarkdownL4InvalidNewNameExtension <- function(
  env = parent.frame()
) {
  eval(quote("Error: newName has an extension other than .Rmd."), envir = env)
}

messages$errorutilitiespkParameterEmptyDisplayUnitNotAllowed <- function(
  env = parent.frame()
) {
  eval(
    quote(
      "An empty string is not allowed as displayUnit in the sheet 'Userdef PK Parameter'. Workaround: use % and set displayUnit to an empty string in the sheet derived from the template sheet."
    ),
    envir = env
  )
}

messages$errorutilitiespkParameterL1NotDefined <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "PK parameter",
      userPar,
      'is not defined in the "Userdef PK Parameter" sheet.'
    )),
    envir = env
  )
}

messages$errorutilitiespkParameterL1NotUnique <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "PK parameter",
      userPar,
      'is not unique in the "Userdef PK Parameter" sheet.'
    )),
    envir = env
  )
}

messages$errorutilitiespkParameterL1MissingOutputPaths <- function(
  env = parent.frame()
) {
  eval(
    quote("Please define outputPaths in the plot configuration xlsx."),
    envir = env
  )
}

messages$errorutilitiespkParameterL1NotCalculated <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "PK parameter for",
      scenarioName,
      "is not calculated.",
      "Use function calculatePKParameterForCalculation to generate the result."
    )),
    envir = env
  )
}

messages$errorutilitiespkParameterL1NoDataForPlot <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "No PK parameter available for",
      onePlotConfig$plotName[1]
    )),
    envir = env
  )
}

messages$errorutilitiespkParameterL1InconsistentDisplayUnit <- function(
  env = parent.frame()
) {
  eval(
    quote(
      "Please check pkParameterDT. displayUnitPKParameter is not consistent for outputPathId and pkParameter."
    ),
    envir = env
  )
}

messages$errorutilitiespkParameterL1UnitConversionFailed <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Could not convert PK parameter to target unit.",
      paste("\nOutputPathId:", outputPathId),
      paste("\nPK Parameter:", pkParameter),
      paste("\nSource unit:", sourceUnit),
      paste("\nTarget unit:", targetUnit),
      paste("\nMolweight:", molWeight),
      paste("\nReason:", errMsg)
    )),
    envir = env
  )
}

messages$errorutilitiespkParameterL1ConflictingDisplayDefinitions <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "PK parameter names have conflicting display definitions across merged PK parameter sheets:",
      paste(conflictingNames, collapse = ", ")
    )),
    envir = env
  )
}

messages$errorutilitiesplotConditionMessage <- function(env = parent.frame()) {
  eval(quote(err), envir = env)
}

messages$errorutilitiesplotInvalidScaleVectorValues <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "No valid values for scalevector for",
      paste0(namesOfScaleVector, collapse = ", ")
    )),
    envir = env
  )
}

messages$errorutilitiesplotTooManyColorsInVector <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Too many colors for colorVector, maximum",
      length(ospsuite.plots::colorMaps[["ospDefault"]]),
      "allowed."
    )),
    envir = env
  )
}

messages$errorutilitiesplotL2NoDefaultShapes <- function(env = parent.frame()) {
  eval(
    quote(
      "No default shape sets for ospsuite.plots. Please use ospsuite.plots::setDefaults()."
    ),
    envir = env
  )
}

messages$errorutilitiesplotL2InsufficientShapes <- function(
  env = parent.frame()
) {
  eval(quote("Not enough shapes are available."), envir = env)
}

messages$errorutilitiesplotL2HeaderRowsHaveNonEmptyColumns <- function(
  env = parent.frame()
) {
  eval(
    quote(
      "Invalid plot configuration table. For rows with headers, all other columns must be empty."
    ),
    envir = env
  )
}

messages$errorutilitiesplotL2MissingHeaderForLevel <- function(
  env = parent.frame()
) {
  eval(
    quote("Invalid plot configuration table. Missing header for level."),
    envir = env
  )
}

messages$errorutilitiesplotL2ColumnEntriesWithoutAllowedValues <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Plot configuration column",
      col,
      "has entries but no allowed values. Did you forget inputs such as observedData or pkParameterDT?"
    )),
    envir = env
  )
}

messages$errorutilitiesplotL2InvalidInputsInColumn <- function(
  env = parent.frame()
) {
  eval(
    quote(paste("Invalid input in plot configuration column", col)),
    envir = env
  )
}

messages$errorutilitiesplotL2ColumnValuesInconsistentWithinGroup <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Values for",
      col,
      "should be the same within each group defined by",
      paste(groupingColumns, collapse = ", ")
    )),
    envir = env
  )
}

messages$errorutilitiesplotL2NoCoreColumnsPopulated <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Invalid configTable. Each plot row needs at least one entry in one of the columns",
      paste(columnVector, collapse = ", ")
    )),
    envir = env
  )
}

messages$errorutilitiesplotL2ColumnValuesInconsistentWithinOutputPath <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Values for",
      col,
      "should be the same within outputPathId."
    )),
    envir = env
  )
}

messages$errorutilitiesplotL2InvalidUnitInOutputsSheet <- function(
  env = parent.frame()
) {
  eval(
    quote(paste0(
      'Please check sheet Outputs in the plot configuration file. Unit "',
      unit,
      '" is not valid.'
    )),
    envir = env
  )
}

messages$errorutilitiesplotL2InvalidColorLegendFormat <- function(
  env = parent.frame()
) {
  eval(
    quote(
      "colorLegend must be a character string composed of two characters separated by |."
    ),
    envir = env
  )
}

messages$errorutilitiesplotL2InvalidColorNames <- function(
  env = parent.frame()
) {
  eval(
    quote("All values in colorVector must be valid color names."),
    envir = env
  )
}

messages$warningutilitiesplotCreationFailed <- function(
  env = parent.frame()
) {
  eval(
    quote(paste0(
      "Error during creation of plot: '",
      onePlotConfig$plotName[1],
      "':\n ",
      conditionMessage(err)
    )),
    envir = env
  )
}

messages$errorutilitiesreportingQuartoNotInstalled <- function(
  env = parent.frame()
) {
  eval(
    quote("Quarto is not installed, word report was not created."),
    envir = env
  )
}

messages$errorutilitiessensitivityMissingSensitivityParameterConfig <- function(
  env = parent.frame()
) {
  eval(
    quote(
      "SensitivityParameter xlsx is not added to projectConfiguration. Please call 'addSensitivityTable(projectConfiguration)'."
    ),
    envir = env
  )
}

messages$errorutilitiessensitivityNoParameterPaths <- function(
  parameterPathMode,
  env = parent.frame()
) {
  eval(
    quote(paste0(
      "No sensitivity parameter paths available for parameterPathMode '",
      parameterPathMode,
      "'."
    )),
    envir = env
  )
}

messages$errorutilitiessensitivityPopulationScenariosForStandardRunner <- function(
  env = parent.frame()
) {
  eval(
    quote(
      "All selected scenarios include population information. Consider using 'runRepresentativeSensitivityAnalysisForScenarios()' for representative-individual sensitivity runs."
    ),
    envir = env
  )
}

messages$errorutilitiessensitivityIndividualScenariosForRepresentativeRunner <- function(
  env = parent.frame()
) {
  eval(
    quote(
      "Selected scenarios do not include population information. Consider using 'runSensitivityAnalysisForScenarios()' for standard sensitivity runs."
    ),
    envir = env
  )
}

messages$errorutilitiessensitivityReferenceScenarioMustDiffer <- function(
  env = parent.frame()
) {
  eval(
    quote("referenceScenario must differ from the current scenario."),
    envir = env
  )
}

messages$errorutilitiessensitivityNoPKDefinitionsForSensitivity <- function(
  env = parent.frame()
) {
  eval(
    quote("No PK parameter/output path definitions available for sensitivity."),
    envir = env
  )
}

messages$errorutilitiessensitivityMissingParameterPathValues <- function(
  env = parent.frame()
) {
  eval(
    quote("Sensitivity sheet does not contain any parameterPath values."),
    envir = env
  )
}

messages$errorutilitiessensitivityNoCommonColumnsToMergePKAnalysis <- function(
  env = parent.frame()
) {
  eval(
    quote("No common columns to merge the two PK analysis files."),
    envir = env
  )
}

messages$warningutilitiessensitivityReferenceScenarioExcluded <- function(
  referenceScenario,
  env = parent.frame()
) {
  eval(
    quote(paste0(
      "Reference scenario '",
      referenceScenario,
      "' is part of the selected scenarios and will be excluded from sensitivity calculations."
    )),
    envir = env
  )
}

messages$errorutilitiesworkflowOptionsNotInitialized <- function(
  env = parent.frame()
) {
  eval(
    quote(
      "Option stopHelperFunction to control valid runs is not initialized. Please call `setWorkflowOptions(isValidRun)`"
    ),
    envir = env
  )
}

messages$errorutilitiesworkflowHelperFunctionNotAllowed <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "You are using a helper function, which is not allowed during a valid run. Called from:",
      callingFunction
    )),
    envir = env
  )
}

messages$warningutilitiesworkflowQcPassedInvalid <- function(
  env = parent.frame()
) {
  eval(
    quote(
      "Environment variable 'QCpassed' was not found, is empty, or is not logical. Set 'QCpassed' to FALSE."
    ),
    envir = env
  )
}

messages$errorutilitiesxlsxClonedSheetMissing <- function(
  env = parent.frame()
) {
  eval(
    quote(paste("Sheet", clonedSheet, "does not exist in the workbook.")),
    envir = env
  )
}

messages$errorutilitiesxlsxTemplateFileMissing <- function(
  env = parent.frame()
) {
  eval(
    quote(paste("Template file", templatePath, "does not exist.")),
    envir = env
  )
}

messages$errorutilitiesxlsxL1MissingSheet <- function(env = parent.frame()) {
  eval(quote(paste("Sheet", sheetName, "does not exist.")), envir = env)
}

messages$errorutilitiesxlsxL1AmbiguousHeaderNames <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Ambiguous header names in sheet",
      existingData,
      paste(names(existingData)[ix], collapse = ", ")
    )),
    envir = env
  )
}

messages$warningutilitiesxlsxExistingSheetWillBeCleared <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      sheetName,
      "already exists. Existing content will be cleared."
    )),
    envir = env
  )
}

messages$warningutilitiesxlsxOutputDefinitionMismatch <- function(
  env = parent.frame()
) {
  eval(
    quote(
      "Output definition in Scenario.xlsx and Plot.xlsx is inconsistent. Please synchronize manually."
    ),
    envir = env
  )
}

messages$errorWorkflowScriptExporterMutuallyExclusiveInputs <- function(
  env = parent.frame()
) {
  eval(
    quote(
      "Error: Please provide either scenarioNames or workflowRmd. Only one of the two is required."
    ),
    envir = env
  )
}

messages$errorWorkflowScriptExporterL1MissingInitializationInput <- function(
  env = parent.frame()
) {
  eval(
    quote(
      "Error: Please provide either scenarioNames or workflowRmd. At least one of the two is required for initialization."
    ),
    envir = env
  )
}

messages$errorWorkflowScriptExporterL1FileTimeout <- function(
  env = parent.frame()
) {
  eval(
    quote("Error: The file did not become available within 1 minute."),
    envir = env
  )
}

messages$errorWorkflowScriptExporterL1MissingRequiredChunks <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Required chunks are missing in workflowRmd. Check:",
      paste(tmp, collapse = ", ")
    )),
    envir = env
  )
}

messages$errorWorkflowScriptExporterNoScenariosForExport <- function(
  env = parent.frame()
) {
  eval(quote("No scenarios are available for export."), envir = env)
}

messages$errorWorkflowScriptExporterL2PopulationScenariosRequireExportedPopulations <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Error: Please use only population scenarios that have exported populations",
      "in workflows intended for an electronic package."
    )),
    envir = env
  )
}

messages$errorWorkflowScriptExporterL2DuplicateFileNames <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Error: File names must be unique. Duplicate file names found:",
      paste(
        inputFiles$fileName[duplicated(inputFiles$fileName)],
        collapse = ", "
      ),
      ". Please ensure all file names are unique."
    )),
    envir = env
  )
}

messages$errorWorkflowScriptExporterL2FileCopyFailed <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Error: File copy to the ePackage folder failed for the following files:",
      paste(inputFiles[!success]$fileName, collapse = ", "),
      ". Please check the source paths and ensure the files exist."
    )),
    envir = env
  )
}

messages$errorWorkflowScriptExporterMissingChunkPlaceholder <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Error: Inconsistent placeholders in the workflow script template and chunk names.",
      "Placeholder for chunk",
      chunkName,
      "is missing",
      "\nThat should not happen. Please ask package administrator for help."
    )),
    envir = env
  )
}

messages$errorWorkflowScriptExporterMissingExpectedChunkVariable <- function(
  env = parent.frame()
) {
  eval(
    quote(paste0(
      "The chunk `",
      chunkName,
      "` of the workflowRmd does not evaluate to a variable `",
      expectedVarName,
      "`. ",
      "Please adjust chunk code."
    )),
    envir = env
  )
}

messages$errorWorkflowScriptExporterL6InvalidFileExtension <- function(
  env = parent.frame()
) {
  eval(
    quote(paste("Error: Invalid file extension for", fileName)),
    envir = env
  )
}

messages$errorWorkflowScriptExporterL6FilenameBeginsWithNumber <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Error: Filename cannot start with a number:",
      fileName,
      "\nPlease use valid file names that do not start with a numeric character.",
      "\nThe workflow export function provides the input variable `fileNameReplacements` to configure file names."
    )),
    envir = env
  )
}

messages$errorWorkflowScriptExporterFilenameTooLong <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "Error: Filename is too long (greater than",
      limitLength,
      "characters):",
      fileName,
      "\nPlease shorten the filename to meet the length requirement.",
      "\nThe workflow export function provides the input variable `fileNameReplacements` to configure file names."
    )),
    envir = env
  )
}

messages$warningWorkflowScriptExporterAdjustedFilenames <- function(
  env = parent.frame()
) {
  eval(
    quote(paste0(
      "Warning: Adjusted filenames due to naming requirements:\n",
      paste(
        basename(changedInputFiles[ixChanged]$source),
        "->",
        changedInputFiles[ixChanged]$fileName,
        collapse = "\n"
      ),
      "\nYou may use the input variable `fileNameReplacements` of the workflow export function",
      " to configure file names more appropriately."
    )),
    envir = env
  )
}

messages$errorWorkflowScriptExporterL8MissingJsonFile <- function(
  env = parent.frame()
) {
  eval(quote(paste0("JSON file does not exist: ", jsonPath)), envir = env)
}
