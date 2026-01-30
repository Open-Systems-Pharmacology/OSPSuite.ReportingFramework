#' List of functions and strings used to signal error and warning messages
#' 
#' @description
#' This list contains all error, warning, and message strings used throughout the package.
#' It centralizes message management for easier maintenance and testing.
#' 
#' @export
messages <- list()

# Aggregation messages
messages$errorLegendSizeNotCovered <- function() {
  "Error: legendsize not covered. Please provide a legendsize of 2 or 3."
}

# Plot configuration messages
messages$errorNoValidScaleVector <- function(namesOfScaleVector) {
  paste("no valid values for scalevector for", paste0(namesOfScaleVector, collapse = ", "))
}

messages$errorTooManyColors <- function(maxColors) {
  paste("To many colors for colorVector, maximal", maxColors, "allowed")
}

messages$errorNoDefaultShapeSets <- function() {
  "no default shape sets for ospsuite.plots. Please use ospsuite.plots::setDefaults()"
}

messages$errorNotEnoughShapes <- function() {
  "not enough shapes available"
}

messages$errorInvalidPlotConfiguration <- function() {
  "Invalid plot configuration table. Missing header for level"
}

messages$errorInvalidInputsInColumn <- function(col) {
  paste("Invalid inputs in plot configuration column", col)
}

messages$errorValuesNotSameWithinGroup <- function(col, groupingColumns) {
  paste("Values for", col, "should be the same within each group defined by", paste(groupingColumns, collapse = ", "))
}

messages$errorValuesNotSameWithinOutputPathId <- function(col) {
  paste(
    "values for", col, "should be the same within outputPathId.",
    "This results in inconsistencies between the plot configuration in 'Plot.xlsx' and the scenario and output definition in 'Scenario.xlsx'.",
    "Plotting can continue but will cause errors later on, if you proceed anyway."
  )
}

messages$errorInvalidUnit <- function(unit) {
  paste0('Please check sheet Outputs in plotconfiguration file. Unit "', unit, '" is not valid')
}

messages$errorColorLegendFormat <- function() {
  "colorLegend must be a character string concatenated from two characters separated by |."
}

messages$errorInvalidColorNames <- function() {
  "All values in colorVector must be valid color names."
}

messages$errorColumnHasEntriesButNoAllowedValues <- function(col) {
  paste("Plot configuration column", col, "has entries but no allowed values.\nCheck .checkPlotConfigurationStructure if all columns with enumvalues are named.")
}

# PK Parameter messages
messages$errorEmptyDisplayUnit <- function() {
  "empty string is not possible as displayUnit in the sheet 'Userdef PK Parameter',\n use 'default' instead"
}

messages$errorPKParameterNotDefined <- function(userPar) {
  paste("pkParameter", userPar, 'is not defined in "Userdef PK Parameter" sheet.')
}

messages$errorPKParameterNotUnique <- function(userPar) {
  paste("pkParameter", userPar, 'is not unique in "Userdef PK Parameter" sheet.')
}

messages$errorNoPKParameterAvailable <- function(plotName) {
  paste(
    "no PK-Parameter available for", plotName,
    "please consider to calculate pkParameter for all outputs. Set 'calculateAggregatedPKParameter=TRUE' in the scenario definition",
    "or check, whether the sheet 'Userdef PK Parameter' is filled appropriately"
  )
}

messages$errorInconsistentDisplayUnit <- function() {
  "Please check pkParameterDT. It seems that displayUnitPKParameter is not consistent for outputPathId and pkParameter"
}

messages$errorPleaseDefineOutputPaths <- function() {
  "Please define ouputPaths in plot configuration xlsx"
}

# Data validation messages
messages$errorDebugModeInValidRun <- function() {
  "debugMode is not allowed during valid runs. Please set isValidRun to FALSE in setWorkflowOptions()."
}

messages$warningDataQuality <- function(fileName, individualId) {
  paste(
    "Please check data quality for", fileName, "in", individualId,
    "where consecutive identical time values exist"
  )
}

messages$warningNoDatafiles <- function() {
  "No datafiles available.\n Either you did not provide datafiles or the files could not be read."
}

messages$errorDataFiles <- function(msg) {
  msg
}

messages$warningDataContainsNA <- function(col) {
  paste("Data contains NAs or empty values in column", col)
}

messages$warningLLOQValueSmaller <- function(individualId, studyId) {
  paste(
    "Please check values for lloq. At least one value is <= 0 in data set for", individualId,
    "in Study", studyId
  )
}

messages$errorIndividualIdColumnMissing <- function(individualIdColumn) {
  paste0(
    "individualIdColumn '", individualIdColumn, "' is missing in provided data",
    " - you can define a different individualIdColumn with option 'individualIdColumn'\n",
    "in the respective functions `loadScenarioObserved*Data`, `loadReferencePopulationObserved*Data`  or use'defaultIndividualIdColumn'"
  )
}

messages$warningIdentifierColumnConversion <- function(individualId, msg) {
  paste0(
    "Individual column conversion failed in ", individualId, ":\n",
    conditionMessage(msg)
  )
}

messages$errorConversionFailed <- function(err) {
  conditionMessage(err)
}

messages$warningUnknownGender <- function() {
  "Unknown gender in data set"
}

messages$errorNoOuputPathForPlot <- function(plotName) {
  paste(
    "no ouputPath for plot", plotName,
    "available. Please define ouputPaths in plot configuration xlsx"
  )
}

messages$errorUnitNameNotAllowedForOutputPath <- function(outputPath, unit, dataSet) {
  paste(
    "For outputPath", outputPath, "the unit name", unit,
    "is not allowed for dataSet", dataSet,
    ". Please check the definition in the Scenario.xlsx Outputs sheet or column 'yUnit' or 'xUnit' in your data file."
  )
}

messages$warningInconsistentLLOQ <- function(individualId, studyId, lloq) {
  paste(
    "Please check values for lloq. Not all values for lloq are the same in data set for", individualId,
    "in Study", studyId, "Range is", paste(lloq, collapse = " to ")
  )
}

messages$errorIndividualDataNeedsMetaData <- function() {
  "IndividualData needs meta data individualId"
}

messages$warningNoGroupsForAggregation <- function() {
  "No groups available for aggregation"
}

messages$warningLLOQCheckColumnsNotUsed <- function(aggregationFlag) {
  paste("input variable lloqCheckColumns2of3 and lloqCheckColumns1of2 are not used for aggregationFlag", aggregationFlag)
}

messages$errorCustomAggregationNeedsLLOQCheck <- function() {
  "For custom aggregation please provide lloqCheckColumns2of3 or lloqCheckColumns1of2"
}

messages$warningCommasReplacedInColumn <- function(col) {
  paste("Warning: Column", col, "commas were replaced by _")
}

messages$warningIdentifierColumnConversionGeneric <- function(column, msg) {
  paste(
    "Could not convert identifier column", column, "to character:",
    msg
  )
}

# Workflow messages
messages$warningQCPassedNotFound <- function() {
  "Environment Variable 'QCpassed' not found, empty or a non logical, set 'QCpassed' to FALSE"
}

messages$errorStopHelperFunctionNotInitialized <- function() {
  "Option stopHelperFunction to control valid runs is not initialized. Please call `executeAsValidRun(isValidRun)`"
}

messages$errorHelperFunctionNotAllowedInValidRun <- function(callingFunction) {
  paste("You are using a helper function, which is not allowed during a valid run. Called from:", callingFunction)
}

# Time profile messages
messages$errorNoDataForPlot <- function() {
  "No data for this plot available"
}

messages$errorMixedErrorTypes <- function() {
  "Please do not mix different error Types in one plot"
}

messages$errorNoSimulatedDataFound <- function(plotName) {
  paste("No simulated data found for", plotName)
}

messages$errorAllDataOutsideTimeRange <- function(plotName) {
  paste("All simulated data outside time range for", plotName)
}

messages$warningNoObservedDataAvailable <- function(plotName, columns) {
  paste0("For plot ", plotName, ", no observed data available for ", paste(columns, collapse = ', '), ", plots will be omitted")
}

messages$errorPlotTypeNotSuitedForMultipleUnits <- function(plotName) {
  paste0("For plot ", plotName, ", you selected a plotType which is not suited for multiple units. Only Timeprofile can handle a secondary axis with a second unit. Please split outputPathId in the plot configuration xlsx to different rows.")
}

# Time profile panels messages
messages$errorPlotTypeShouldSplit <- function(plotType, plotName) {
  paste(
    "plotType", plotType, "is designed for one unique outputPathID and panel structure per plot.",
    "Please split plot", plotName, "into multiple plots in the configuration file."
  )
}

messages$errorUnknownPlotType <- function(plotType) {
  paste("unknown plottype:", plotType)
}

messages$errorWrongNumberOfRatioPlots <- function(plotType, nRatio) {
  paste(
    "plotType", plotType, "is designed for one ratio plot per plot.",
    "You provided", nRatio, "ratio plots.",
    "Please split into multiple plots in the configuration file."
  )
}

messages$errorMoreThanTwoYUnitsInPanel <- function() {
  "do not combine more than two yUnits in one Panel"
}

messages$errorInvalidTimeRangeColumn <- function() {
  'invalid inputs in one of the "TimeRange" columns'
}

messages$errorBracketsInColumn <- function(column, values) {
  paste("Please check the brackets in column", column, paste(values, collapse = ";"))
}

messages$errorAggregationMethodsNotConsistent <- function(tmp) {
  paste(
    "Aggregation methods are not consistent! ",
    paste(paste(tmp$dataType, tmp$yErrorType, sep = ": "), collapse = ", ")
  )
}

messages$errorDataOutsideYLimit <- function(plotName, timeRangeFilter) {
  paste(
    "data outside ylimit for", plotName,
    "time range", timeRangeFilter
  )
}

messages$errorCombineOutputsWithBracketsAndReferenceScenarios <- function(plotNames) {
  paste0(
    "Do not combine outputs in one panel with brackets () ",
    'if you want to display reference scenarios.
                Please check "', paste0(unique(plotNames), collapse = '", "', '"')
  )
}

messages$errorPlotConfigsWithReferenceNeedColorLegend <- function(plotNames) {
  paste0(
    "Plot configurations with reference scenarios need to have a color legend ",
    'Please check "', paste0(unique(plotNames), collapse = '", "', '"')
  )
}

messages$errorVirtualTwinIndividualIdsMustBeFilled <- function(scenarioNames) {
  paste(
    "For scenarios with virtual twin populations, column individualIds has to be filled.",
    'Use "*" or "(*)", if you want to plot all. (Brackets not allowed for Timeprofile Plots)',
    "Check Scenarios:", paste(scenarioNames, collapse = ", ")
  )
}

messages$errorVirtualTwinBracketsNotAllowed <- function(plotNames) {
  paste(
    "For scenarios with virtual twin populations and selected Plot_TimeProfiles,
                brackets are not allowed in column individualIds.",
    "Check Plots:", paste(plotNames, collapse = ", ")
  )
}

messages$warningIndividualIdsIgnored <- function(plotNames) {
  paste(
    'Column "individualIds" is filled but no data group is selected and
    scenario is not a virtual twin population scenario. "individualIds" will be ignored.',
    "Check Plots:", paste(plotNames, collapse = ", ")
  )
}

messages$errorScenarioAndReferenceMustBothBePopOrInd <- function(plotNames) {
  paste(
    "scenario and referenceScenario must be both populations or both indviduals",
    "Check Plots:", paste(plotNames, collapse = ", ")
  )
}

messages$errorOnePlotConfigContainsMoreThanOnePlotName <- function() {
  "onePlotConfig contains more than one plotName"
}

messages$errorPlotsBasedOnDifferentTimeUnits <- function(outputPaths, unit1, unit2, plotName) {
  paste(
    "For output paths", paste(outputPaths, collapse = ", "),
    "the time units are not consistent:", unit1, "vs.", unit2,
    "in plot", plotName
  )
}

messages$errorInconsistentYUnit <- function(outputPathId, yUnit1, yUnit2, plotName) {
  paste(
    "For outputPathId", outputPathId,
    "the yUnits are not consistent:", yUnit1, "vs.", yUnit2,
    "in plot", plotName
  )
}

messages$warningUnknownLegendPosition <- function(plotlegendPosition, plotName) {
  paste(
    "plotlegendPosition", plotlegendPosition,
    "not implemented. Default 'outsideright' is used instead for", plotName
  )
}

messages$errorOutputIDNotUnique <- function(outputPathId, plotName) {
  paste(
    "outputPathId", outputPathId,
    "exists multiple times with different information in the configuration table for plot", plotName
  )
}

# PK Forest messages
messages$errorScenariosNotUnique <- function() {
  "Scenarios are not unique regarding scenarioShortName and scenarioGroup.\n Possible reason: population scenarios with the same name but different base populations,\n where the first unique scenario is selected"
}

messages$errorScenariosNotUniqueCheckConfig <- function(plotName) {
  paste0("Scenarios are not unique regarding scenarioShortName and scenarioGroup.
           Check configuration for: ", plotName)
}

messages$errorScenariosNotUniqueAggregation <- function(plotName, errorTypes) {
  paste0(
    "Scenarios are not unique regarding aggregation for observed and simulated data.
           Check errorType column for data relevant for plot: ", plotName,
    "errorTypes are: ", paste(errorTypes, collapse = ", ")
  )
}

messages$errorInconsistentBasePopulation <- function() {
  "Within one plot you must either compare always scenarios with the same base population or\ncompare always scenarios with different base populations"
}

messages$errorInconsistentBasePopulationAlways <- function() {
  "Within one plot you must either compare always scenarios with the same base population or
             always scenarios with different base populations"
}

messages$errorNoAggregatedObservedPKParameter <- function() {
  "Please provide aggregated observed PK-Parameter data for this kind of plot "
}

# Demographics messages
messages$warningMeanSEMNotAllowed <- function(plotName) {
  paste(
    "Mean_SEM for aggregation is not allowed for a scatter and range plot",
    "-> plotName:", plotName,
    "\nRangePlots can also displayed with percentiles/quantiles.\nUse percentiles in Scenario.xlsx AggrType"
  )
}

messages$warningCategoricParameterIgnored <- function(parameterIds) {
  paste(
    "Categoric parameter are not suited for this kind of plot and will be ignored:",
    .concatWithAnd(parameterIds)
  )
}

messages$errorParameterPathsNotAvailable <- function(modelPaths, scenarioName) {
  paste(
    "Parameter path(s)", paste(modelPaths, collapse = ", "),
    "is not available for", scenarioName
  )
}

messages$errorParameterIdsNotValidModelParameters <- function(plotName) {
  paste(
    "The ParameterIds are no valid modelparameters!
                 Are they PK-Parameter? But pkParameterDT is missing as input.",
    plotName
  )
}

messages$errorCategoricValuesNotAllowedForXAxis <- function(plotName) {
  paste("Categoric Values are not allowed for x-axis on rangeplots. Check plotName", plotName)
}

messages$errorColumnsNotAvailableForXYAxis <- function(plotName, requiredColumns) {
  paste(
    "For selected plotType, the columns", paste(requiredColumns, collapse = ", "),
    "must be available in the configuration file for plotName", plotName
  )
}

messages$errorMissingInputs <- function() {
  "Inputs are missing, please provide scenarioList and/or pkParameterDT"
}

messages$errorInconsistentYAxisSplit <- function(plotName) {
  paste(
    "For selected plotType (scatter plot with multiple y values and/or",
    "range plot) each combination of y-axis columns must result in its own plot (based on faceting)",
    "but the plot", plotName, "combines different combinations of y-axis columns,",
    "please split this plot into multiple plots by extending column plotName."
  )
}

# PK Boxwhisker messages
messages$warningNoDataForPlot <- function(plotName) {
  paste("No data for", plotName)
}

messages$errorNoPopulationData <- function(plotName) {
  paste(
    "no populationData data for plot", plotName, "available.",
    "Please check consistency of plotName with scenario data"
  )
}

messages$errorSelectPlotTypeAbsoluteOrRatio <- function() {
  "Please select either Plot_Ratio or Plot_Absolute!"
}

messages$errorRatioOnlySamePopulation <- function() {
  "Ratio plots are only available if scenario and referenceScenario is based on the same population"
}

messages$errorOneCombinationPerPlot <- function(plotNames) {
  paste(
    "Per plot only one combination of scenario, outputPathId and pkParameter is allowed. Please check plot",
    paste(plotNames, collapse = ", ")
  )
}

messages$errorRatioNeedsReferenceScenario <- function(plotNames) {
  paste(
    "For ratio plots at lease one reference scenario has to be selected. Check PlotName",
    paste(plotNames, collapse = ", ")
  )
}

messages$errorInconsistentPlotRatioConfig <- function(plotName) {
  paste(
    "PlotRatio is inconsistent within plot", plotName,
    ". Please split plot into multiple plots with consistent PlotRatio definition"
  )
}

# Sensitivity messages
messages$errorInconsistentTimeVectorForSensitivity <- function(runIds) {
  paste(
    "To compare population simulations on time variation,",
    "the time vector should be the same for all runs in population simulation",
    "the following runs have inconsistent time vectors", paste(runIds, collapse = ", ")
  )
}

messages$errorSensitivityFileNotAdded <- function() {
  "SensitivityParameter xlsx is not added to the projectConfiguration Please call 'addSensitivityTable(projectConfiguration)'"
}

# esqlabsR bracket messages
messages$errorOntologyCouldNotBeLoaded <- function(ontogeny) {
  paste("The ontogeny could not be loaded for", ontogeny)
}

messages$errorOntologyFileNotAvailable <- function(ontogeny) {
  paste0(
    "For", ontogeny, ",",
    "there is no file '/Ontogenies/", ontogeny, "/Ontogeny_", ontogeny, ".xls' in 'Data/Physiology'.",
    "Please check the ontogeny naming"
  )
}

messages$errorSimulationNotAvailable <- function(scenarioName, scenario) {
  paste(
    "Simulation is not available. scenario:", scenarioName,
    "in scenarioList:", scenario
  )
}

messages$errorOntogenyWrongStructure <- function(ontogeny) {
  paste("The ontogeny has the wrong structure:", ontogeny)
}

messages$errorWrongXLSStructure <- function() {
  "errorWrongXLSStructure"
}

messages$errorInvalidScenarioConfig <- function(scenarioName, isNull) {
  paste0(
    "Invalid scenario configuration for scenario '", scenarioName, "': ",
    "modelFile is ", if (isNull) "NULL" else "empty", ". ",
    "All scenarios must have a non-empty modelFile with .pkml extension."
  )
}

messages$errorModelFileNotFound <- function(scenarioName, modelFile, correctedFile, modelFolder) {
  paste0(
    "Model file not found for scenario '", scenarioName, "': ",
    "Neither '", modelFile, "' nor '", correctedFile, "' exists in '",
    modelFolder, "'. ",
    "Please check the file name in the scenario configuration."
  )
}

messages$errorSimulationResultsDoNotExist <- function(scenarioNames) {
  paste(
    "Error: Simulation results for scenario(s)",
    paste(scenarioNames, collapse = ", "),
    "do not exist."
  )
}

# Markdown messages
messages$errorNoFileExistsForKey <- function(figureFile, tableCsv) {
  paste("No file exists for key. There should be either", figureFile, "or", tableCsv)
}

messages$errorSourceRmdExtension <- function() {
  "Error: One or more elements of sourceRmds have an extension other than .Rmd."
}

messages$errorNewNameExtension <- function() {
  "Error: NewName has an extension other than .Rmd."
}

# Reporting messages
messages$errorPandocNotInstalled <- function() {
  "Pandoc is not installed, word report was not created."
}

# WorkflowScriptExporter messages
messages$errorProvideOnlyOneScenarioOrWorkflow <- function() {
  "Error: Please provide either scenarioNames or workflowRmd. Only one of the two is required."
}

messages$errorProvideAtLeastOneScenarioOrWorkflow <- function() {
  "Error: Please provide either scenarioNames or workflowRmd. At least one of the two is required for initialization."
}

messages$errorFileNotAvailableWithinTime <- function() {
  "Error: The file did not become available within 1 minute."
}

messages$errorNoScenariosForExport <- function() {
  "no scenarios for export available"
}

messages$errorInvalidFileExtension <- function(fileName) {
  paste("Error: Invalid file extension for", fileName)
}

messages$errorJSONFileDoesNotExist <- function(jsonPath) {
  paste("JSON file does not exist:", jsonPath)
}

messages$errorChunksMissing <- function(missingChunks) {
  paste(
    "Chunks are missing in workflowRmd check:",
    paste(missingChunks, collapse = ", ")
  )
}

messages$errorOnlyPopulationScenariosWithExportedPopulations <- function() {
  paste(
    "Error: Please use only population scenarios that have exported populations",
    "in workflows intended for an electronic package."
  )
}

messages$errorDuplicateFileNames <- function(duplicateNames) {
  paste(
    "Error: File names must be unique. Duplicate file names found:",
    paste(duplicateNames, collapse = ", "),
    ". Please ensure all file names are unique."
  )
}

messages$errorFileCopyFailed <- function(failedFiles) {
  paste(
    "Error: File copy to the ePackage folder failed for the following files:",
    paste(failedFiles, collapse = ", "),
    ". Please check the source paths and ensure the files exist."
  )
}

messages$warningAdjustedFilenames <- function(changedFiles) {
  paste0(
    "Warning: Adjusted filenames due to naming requirements:\n",
    paste(basename(changedFiles$source), "->", changedFiles$fileName,
      collapse = "\n"
    ),
    "\nYou may use the input variable `fileNameReplacements` of the workflow export function",
    " to configure file names more appropriately."
  )
}

messages$errorInconsistentPlaceholders <- function(chunkName) {
  paste(
    "Error: Inconsisten placeholders in workflow script template and chunk Names.",
    "Placeholder for chunk", chunkName, "is missing",
    "\nThat should not happen. Please ask package administrator for help."
  )
}

messages$errorChunkDoesNotEvaluateToVariable <- function(chunkName, expectedVarName) {
  paste0(
    "The chunk `", chunkName, "` of the workflowRmd does not evaluate to a variable `", expectedVarName, "`. ",
    "Please adjust chunk code."
  )
}

messages$errorFilenameStartsWithNumber <- function(fileName) {
  paste(
    "Error: Filename cannot start with a number:", fileName,
    "\nPlease use valid file names that do not start with a numeric character.",
    "\nThe workflow export function provides the input variable `fileNameReplacements` to configure file names."
  )
}

messages$errorFilenameTooLong <- function(limitLength, fileName) {
  paste(
    "Error: Filename is too long (greater than", limitLength, "characters):", fileName,
    "\nPlease shorten the filename to meet the length requirement.",
    "\nThe workflow export function provides the input variable `fileNameReplacements` to configure file names."
  )
}

messages$errorFirstElementNotScenarioName <- function(scenarioName, firstElement) {
  paste(
    "First Element of list in", scenarioName,
    "could not be identified as 'name' (of the input file within the ePackage)",
    "the value of the first element of the list is:", firstElement
  )
}

messages$errorExpectedListForScenario <- function(scenarioName, elementName, elementValue) {
  paste(
    "Expected a list for scenario", scenarioName,
    "for element", elementName,
    "but got value:", elementValue
  )
}

messages$warningNoEPackageInputFiles <- function(scenarioName) {
  paste0(
    "No ePackage input files found for scenario ", scenarioName, ". ",
    "The ePackage will be created without input files for this scenario."
  )
}

messages$errorScenarioDataNotPopulationSimulation <- function(scenarioName) {
  paste(
    "For scenario", scenarioName,
    "scenarioData$simulationResults[[1]] is not of class 'PopulationSimulationResults'"
  )
}

messages$errorScenarioDataMustBeSimulationResults <- function(scenarioName) {
  paste0(
    "For scenario ", scenarioName,
    " scenarioData$simulationResults must be a 'SimulationResults' or 'PopulationSimulationResults' object or list"
  )
}

messages$errorRmdChunkWithoutName <- function(rmdfile, line) {
  paste(
    "Error: In file", rmdfile, "at line", line,
    "a code chunk without a name was found. Please provide a name for all code chunks."
  )
}

# XLSX messages
messages$warningSheetAlreadyExists <- function(sheetName) {
  paste(sheetName, "already exists. Existing content will be cleared.")
}

messages$errorSheetDoesNotExist <- function(sheetName) {
  paste("Sheet", sheetName, "does not exist in the workbook.")
}

messages$errorTemplateFileDoesNotExist <- function(templatePath) {
  paste("Template file", templatePath, "does not exist.")
}

messages$errorInvalidSheetRange <- function(sheetName, startRow, startColumn, dataRange) {
  paste(
    "Invalid range in sheet", sheetName,
    "with startRow =", startRow, ", startColumn =", startColumn,
    ". The data to be written has dimensions:", paste(dataRange, collapse = " x ")
  )
}

messages$warningInconsistentOutputDefinition <- function() {
  "Output definition in Scenario.xlsx and Plot.xlsx is inconsistent. Please synchronize manually"
}

messages$errorAmbiguousHeaderNames <- function(existingData, headerNames) {
  paste(
    "ambiguous header names in sheet", existingData,
    paste(headerNames, collapse = ",")
  )
}

# RmdPlotManager messages
messages$errorProvideValidRmdName <- function() {
  "Please provide a valid name for the .Rmd file and its subfolder."
}

messages$errorFunctionDoesNotExist <- function(nameOfplotFunction) {
  paste("Function", nameOfplotFunction, "does not exist")
}

messages$messageNoValidationFunctionAvailable <- function() {
  "No specific plotconfiguration validation function available."
}

messages$errorProvideFileNameAsBasename <- function() {
  "Please insert fileName as basename, File will be saved in folder defined by class object"
}

messages$warningCaptionMissing <- function(caption) {
  paste("Caption is missing for key", caption)
}

messages$errorKeyAlreadyAdded <- function(key) {
  paste0('key "', key, '" was already added. The figure and table keys must be unique')
}

messages$errorCreatingPlot <- function(plotName, err) {
  paste0("Error during creation of plot: '", plotName, "':\n ", conditionMessage(err))
}

# Logging messages
messages$warningLogfileNotInitialized <- function() {
  "Logfile was not initialized"
}

# Population messages
messages$messageShiftVirtualTwinPopulation <- function() {
  "shift sheet 'VirtualTwinPopulation' from 'Indvidual.xslx' to 'Population.xlsx'"
}

messages$warningSmallProportionOfFemales <- function(populationData) {
  paste(
    "You have very small values for 'ProportionOfFemales' in the population configurations.
    Unit is percent not fraction. Are you sure?\n",
    paste(paste(populationData$populationName, populationData$proportionOfFemales, sep = ": "), collapse = "; ")
  )
}

messages$errorPopulationParameterMustBeConsistent <- function(tmp, observedIndividualId) {
  paste(
    "population parameter must be consistent within a virtual population. Check",
    paste(tmp, collapse = ", "), "for", observedIndividualId
  )
}

messages$warningParameterPathColumnMissing <- function(xslxFile) {
  paste(
    "Column 'parameterPath' not available in Population.xslx,",
    "please use 'parameterPath' instead of 'name'. Check file", xslxFile
  )
}

messages$errorInconsistentNumberOfValues <- function(path, populationName) {
  paste("Inconsistent number of values for", path, "in", populationName)
}

messages$errorPopulationNotUnique <- function(populationFile, duplPopName) {
  paste(
    "Definition of populationName in", populationFile,
    "is not unique. Duplicate population names:",
    paste(duplPopName, collapse = ", ")
  )
}
