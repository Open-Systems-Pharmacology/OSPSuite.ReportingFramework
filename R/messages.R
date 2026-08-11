#' List of functions and strings used for log, warning, and error messages
#' @keywords internal
messages <- list()

messages$infoVirtualTwinPopulationSheetShifted <- function() {
  "shift sheet 'VirtualTwinPopulation' from 'Indvidual.xslx' to 'Population.xlsx'"
}

messages$infoNoGroupsForVirtualTwinPopulationCreation <- function() {
  "No groups available for virtual twin population creation"
}

messages$infoAddVirtualTwinPopulationConfig <- function() {
  "add virtual twin population configuration in Population configuration file:"
}

messages$infoNoNewVirtualTwinPopulationsToGenerate <- function() {
  "No new virtual twin populations to generate; all files already exist."
}

messages$warningSuspiciousProportionOfFemales <- function(suspiciousRows) {
  paste(
    "You have very small values for 'ProportionOfFemales' in the population configurations.\n",
    "Unit is percent not fraction. Are you sure?\n",
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
    "population parameter must be consistent within a virtual population. Check",
    paste(inconsistentParameters, collapse = ", "),
    "for",
    observedIndividualId
  )
}


messages$errorPlotDataTimeProfileL1 <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "No simulated data found for",
      self$configTable$plotName[1]
    )),
    envir = env
  )
}

messages$errorPlotDataTimeProfileL1X <- function(env = parent.frame()) {
  eval(
    quote(glue::glue(
      "All simulated data outside time range for {self$configTable$plotName[1]}"
    )),
    envir = env
  )
}

messages$errorPlotDataTimeProfileL1XX <- function(env = parent.frame()) {
  eval(
    quote(glue::glue(
      "For plot {self$configTable$plotName[1]}, you selected a plotType which is not suited for multiple units. Only Timeprofile can handle a secondary axis with a second unit. Please split outputPathId in the plot configuration xlsx to different rows."
    )),
    envir = env
  )
}

messages$warningPlotDataTimeProfileL1 <- function(env = parent.frame()) {
  eval(
    quote(glue::glue(
      "For plot {self$configTable$plotName[1]}, no observed data available for {paste(plotCols, collapse = ', ')}, plots will be omitted"
    )),
    envir = env
  )
}

messages$errorplotDemographicsL1 <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "Categoric Values are not allowed for x-axis on rangeplots. Check plotName",
      onePlotConfig$plotName[1]
    )),
    envir = env
  )
}

messages$errorplotDemographicsL1X <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "Parameter path(s)",
      paste(setdiff(modelPaths, names(dtPop)), collapse = ", "),
      "is not available for",
      scenarioName
    )),
    envir = env
  )
}

messages$errorplotDemographicsL1XX <- function(env = parent.frame()) {
  eval(
    quote(
      "Inputs are missing, please provide scenarioList and/or pkParameterDT"
    ),
    envir = env
  )
}

messages$errorplotDemographicsL1XXX <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "The ParameterIds are no valid modelparameters!
                 Are they PK-Parameter? But pkParameterDT is missing as input.",
      configTablePlots$plotName[1]
    )),
    envir = env
  )
}

messages$warningplotDemographicsL1 <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "Categoric parameter are not suited for this kind of plot and will be ignored:",
      concatWithAnd(unique(
        plotData[!is.na(categoricValue), ]$parameterId
      ))
    )),
    envir = env
  )
}

messages$errorplotPKBoxwhiskerL1 <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "Per plot only one combination of scenario, outputPathId and pkParameter is allowed. Please check plot",
      paste(tmp$plotName %>% unique(), collapse = ", ")
    )),
    envir = env
  )
}

messages$errorplotPKBoxwhiskerL1X <- function(env = parent.frame()) {
  eval(
    quote("Please select either Plot_Ratio or Plot_Absolute!"),
    envir = env
  )
}

messages$errorplotPKBoxwhiskerL1XX <- function(env = parent.frame()) {
  eval(
    quote(
      "Ratio plots are only available if scenario and referenceScenario is based on the same population"
    ),
    envir = env
  )
}

messages$errorplotPKBoxwhiskerL1XXX <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "For ratio plots at lease one reference scenario has to be selected. Check PlotName",
      paste(tmp[isValid == FALSE, ]$plotName, collapse = ", ")
    )),
    envir = env
  )
}

messages$warningplotPKBoxwhiskerL1 <- function(env = parent.frame()) {
  eval(quote(paste("No data for", onePlotConfig$plotName[1])), envir = env)
}

messages$errorplotPKForestL1 <- function(env = parent.frame()) {
  eval(
    quote(
      "Within one plot you must either compare always scenarios with the same base population or
             always scenarios with different base populations"
    ),
    envir = env
  )
}

messages$errorplotPKForestL1X <- function(env = parent.frame()) {
  eval(
    quote(
      "Please provide aggregated observed PK-Parameter data for this kind of plot "
    ),
    envir = env
  )
}

messages$errorplotTimeProfilePanelsL1 <- function(env = parent.frame()) {
  eval(quote("onePlotConfig conatinas more than one plotName"), envir = env)
}

messages$errorplotTimeProfilePanelsL1X <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "Aggregation methods are not consistent! ",
      paste(
        paste(tmp$dataType, tmp$yErrorType, sep = ": "),
        collapse = ", "
      )
    )),
    envir = env
  )
}

messages$errorplotTimeProfilePanelsL1XX <- function(env = parent.frame()) {
  eval(quote(paste("unknown plottype:", plotType)), envir = env)
}

messages$errorplotTimeProfilePanelsL1XXX <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "data outside ylimit for",
      plotData$plotName,
      "time range",
      timeRangeFilter
    )),
    envir = env
  )
}

messages$errorplotTimeProfilePanelsL2 <- function(env = parent.frame()) {
  eval(
    quote(paste0(
      "Do not combine outputs in one panel with brackets () ",
      'if you want to display reference scenarios.
               Please check "',
      paste0(unique(tmp$plotName), collapse = '", "', '"')
    )),
    envir = env
  )
}

messages$errorplotTimeProfilePanelsL2X <- function(env = parent.frame()) {
  eval(
    quote(paste0(
      "Plot configurations with reference scenarios need to have a color legend ",
      'Please check "',
      paste0(unique(tmp$plotName), collapse = '", "', '"')
    )),
    envir = env
  )
}

messages$errorplotTimeProfilePanelsL2XX <- function(env = parent.frame()) {
  eval(quote("do not combine more than two yUnits in one Panel"), envir = env)
}

messages$errorplotTimeProfilePanelsL2XXX <- function(env = parent.frame()) {
  eval(quote("You need at least one TimeRange Column"), envir = env)
}

messages$errorplotTimeProfilePanelsL2XXXX <- function(env = parent.frame()) {
  eval(quote('invalid inputs in one of the "TimeRange" columns'), envir = env)
}

messages$errorplotTimeProfilePanelsL2XXXXX <- function(env = parent.frame()) {
  eval(quote('invalid inputs in one of the "TimeRange" columns'), envir = env)
}

messages$errorplotTimeProfilePanelsL2XXXXXX <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "Please check the brackets in column",
      column,
      paste(tmp[[column]], collapse = ";")
    )),
    envir = env
  )
}

messages$errorplotTimeProfilePanelsL2XXXXXXX <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "For scenarios with virtual twin populations, column individualIds has to be filled.",
      'Use "*" or "(*)", if you want to plot all. (Brackets not allowed for Timeprofile Plots)',
      "Check Scenarios:",
      paste(indScenariosNames, collapse = ", ")
    )),
    envir = env
  )
}

messages$errorplotTimeProfilePanelsL2XXXXXXXX <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "For scenarios with virtual twin populations and selected Plot_TimeProfiles,
               brackets are not allowed in column individualIds.",
      "Check Plots:",
      paste(tmp$plotName[errorRows], collapse = ", ")
    )),
    envir = env
  )
}

messages$errorplotTimeProfilePanelsL2XXXXXXXXX <- function(
  env = parent.frame()
) {
  eval(
    quote(paste(
      "scenario and referenceScenario must be both populations or both indviduals",
      "Check Plots:",
      paste(tmp$plotName, collapse = ", ")
    )),
    envir = env
  )
}

messages$warningplotTimeProfilePanelsL2 <- function(env = parent.frame()) {
  eval(
    quote(paste(
      'Column "individualIds" is filled but no data group is selected and
    scenario is not a virtual twin population scenario. "individualIds" will be ignored.',
      "Check Plots:",
      paste(tmp$plotName, collapse = ", ")
    )),
    envir = env
  )
}

messages$errorprojectconfigurationL1 <- function(env = parent.frame()) {
  eval(quote(paste0(FIELD, " is readonly")), envir = env)
}

messages$errorprojectconfigurationL1X <- function(env = parent.frame()) {
  eval(quote("ospsuiteReportingFrameworkVersion is readonly"), envir = env)
}

messages$errorprojectconfigurationL1XX <- function(env = parent.frame()) {
  eval(quote("Aborted by user."), envir = env)
}

messages$errorRmdPlotManagerL1 <- function(env = parent.frame()) {
  eval(
    quote(
      "Please provide a valid name for the .Rmd file and its subfolder."
    ),
    envir = env
  )
}

messages$errorRmdPlotManagerL1X <- function(env = parent.frame()) {
  eval(
    quote(paste("Function", nameOfplotFunction, "does not exist")),
    envir = env
  )
}

messages$errorRmdPlotManagerL1XX <- function(env = parent.frame()) {
  eval(
    quote(
      "Please insert fileName as basename, File will be saved in folder defined by class object"
    ),
    envir = env
  )
}

messages$errorRmdPlotManagerL3 <- function(env = parent.frame()) {
  eval(
    quote(paste0(
      'key "',
      key,
      '" was already added. The figure and table keys must be unique'
    )),
    envir = env
  )
}

messages$warningRmdPlotManagerL2 <- function(env = parent.frame()) {
  eval(quote(paste("Caption is missing for key", caption)), envir = env)
}

messages$infoRmdPlotManagerL1 <- function(env = parent.frame()) {
  eval(
    quote("No specific plotconfiguration validation function available."),
    envir = env
  )
}

messages$errorscenariomanagementL1 <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "Error: Simulation results for scenario(s)",
      paste(scenarioNames[!file.exists(resultFiles)], collapse = ", "),
      "do not exist."
    )),
    envir = env
  )
}

messages$errorscenariomanagementL1X <- function(env = parent.frame()) {
  eval(
    quote(paste("The ontogeny has the wrong structure:", ontogeny)),
    envir = env
  )
}

messages$errorutilitiesaggregationL1 <- function(env = parent.frame()) {
  eval(
    quote(
      "Error: legendsize not covered. Please provide a legendsize of 2 or 3."
    ),
    envir = env
  )
}

messages$errorutilitiesdataL1 <- function(env = parent.frame()) {
  eval(quote(paste("no datafiles defined for", dataClassType)), envir = env)
}

messages$errorutilitiesdataL3 <- function(env = parent.frame()) {
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

messages$errorutilitiesdataL4 <- function(env = parent.frame()) {
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

messages$errorutilitiesdataL4X <- function(env = parent.frame()) {
  eval(quote(conditionMessage(err)), envir = env)
}

messages$errorutilitiesdataL4XX <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "DataDT to combinedData: y Unit for dataset",
      groupName,
      "is not unique"
    )),
    envir = env
  )
}

messages$errorutilitiesdataL4XXX <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "DataDT to combinedData: x Unit for dataset",
      groupName,
      "is not unique"
    )),
    envir = env
  )
}

messages$errorutilitiesdataL4XXXX <- function(env = parent.frame()) {
  eval(quote("IndividualData needs meta data individualId"), envir = env)
}

messages$errorutilitiesdataL4XXXXX <- function(env = parent.frame()) {
  eval(
    quote(
      "For custom aggregation please provide lloqCheckColumns2of3 or lloqCheckColumns1of2"
    ),
    envir = env
  )
}

messages$warningutilitiesdataL1 <- function(env = parent.frame()) {
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

messages$warningutilitiesdataL3 <- function(env = parent.frame()) {
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

messages$warningutilitiesdataL3X <- function(env = parent.frame()) {
  eval(
    quote(paste("Data contains NAs or empty values in column", col)),
    envir = env
  )
}

messages$warningutilitiesdataL3XX <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "Ambiguous units:",
      summaryString,
      "\nPlease check if this acceptable, e.g. pkParameter as ratio and absolute values."
    )),
    envir = env
  )
}

messages$warningutilitiesdataL4 <- function(env = parent.frame()) {
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

messages$warningutilitiesdataL4X <- function(env = parent.frame()) {
  eval(quote(paste("Unknown gender in data set")), envir = env)
}

messages$warningutilitiesdataL4XX <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "DataDT to combinedData: More than one LLOQ for dataset",
      groupName,
      "is set to minimal"
    )),
    envir = env
  )
}

messages$warningutilitiesdataL4XXX <- function(env = parent.frame()) {
  eval(quote("No groups available for aggregation"), envir = env)
}

messages$warningutilitiesdataL4XXXX <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "input variable lloqCheckColumns2of3 and lloqCheckColumns1of2 are not used for aggregationFlag",
      aggregationFlag
    )),
    envir = env
  )
}

messages$warningutilitiesdataL4XXXXX <- function(env = parent.frame()) {
  eval(
    quote(paste("Warning: Column", col, "commas were replaced by _")),
    envir = env
  )
}

messages$warningutilitiesdataL4XXXXXX <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "Groups",
      paste(unsuitableGroups, collapse = ", "),
      "are not suited for grouping.",
      "Check if they are available in data, have more then",
      minN,
      "Individuals or
                    if they are have data class",
      DATACLASS$tpIndividual
    )),
    envir = env
  )
}

messages$errorutilitiesloggingL7 <- function(env = parent.frame()) {
  eval(quote(e), envir = env)
}

messages$errorutilitiesloggingL11 <- function(env = parent.frame()) {
  eval(quote(e), envir = env)
}

messages$warningutilitiesloggingL13 <- function(env = parent.frame()) {
  eval(quote("Logfile was not initialized"), envir = env)
}

messages$warningutilitiesloggingL14 <- function(env = parent.frame()) {
  eval(quote("Logfile was not initialized"), envir = env)
}

messages$warningutilitiesloggingL15 <- function(env = parent.frame()) {
  eval(quote("Logfile was not initialized"), envir = env)
}

messages$infoutilitiesloggingL7 <- function(env = parent.frame()) {
  eval(quote(messageText), envir = env)
}

messages$errorutilitiesmarkdownL4 <- function(env = parent.frame()) {
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

messages$errorutilitiesmarkdownL4X <- function(env = parent.frame()) {
  eval(
    quote(
      "Error: One or more elements of sourceRmds have an extension other than .Rmd."
    ),
    envir = env
  )
}

messages$errorutilitiesmarkdownL4XX <- function(env = parent.frame()) {
  eval(quote("Error: NewName has an extension other than .Rmd."), envir = env)
}

messages$errorutilitiespkParameterL1 <- function(env = parent.frame()) {
  eval(
    quote("Please define ouputPaths in plot configuration xlsx"),
    envir = env
  )
}

messages$errorutilitiespkParameterL1X <- function(env = parent.frame()) {
  eval(
    quote(
      "empty string is not possible as displayUnit in the sheet 'Userdef PK Parameter',
    workaround: use % and set displayUnit in sheet derived from template-sheet to empty string"
    ),
    envir = env
  )
}

messages$errorutilitiespkParameterL1XX <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "pkParameter",
      userPar,
      'is not defined in "Userdef PK Parameter" sheet.'
    )),
    envir = env
  )
}

messages$errorutilitiespkParameterL1XXX <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "pkParameter",
      userPar,
      'is not unique in "Userdef PK Parameter" sheet.'
    )),
    envir = env
  )
}

messages$errorutilitiespkParameterL1XXXX <- function(env = parent.frame()) {
  eval(
    quote("Please define ouputPaths in plot configuration xlsx"),
    envir = env
  )
}

messages$errorutilitiespkParameterL1XXXXX <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "PK Parameter for",
      scenarioName,
      "is not calculated!",
      "Use function calculatePKParameterForCalculation to generate the result"
    )),
    envir = env
  )
}

messages$errorutilitiespkParameterL1XXXXXX <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "no PK-Parameter available for",
      onePlotConfig$plotName[1]
    )),
    envir = env
  )
}

messages$errorutilitiespkParameterL1XXXXXXX <- function(env = parent.frame()) {
  eval(
    quote(
      "Please check pkParameterDT. It seems that displayUnitPKParameter is not consistent for outputPathId and pkParameter"
    ),
    envir = env
  )
}

messages$errorutilitiesplotL1 <- function(env = parent.frame()) {
  eval(quote(err), envir = env)
}

messages$errorutilitiesplotL2 <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "no valid values for scalevector for",
      paste0(namesOfScaleVector, collapse = ", ")
    )),
    envir = env
  )
}

messages$errorutilitiesplotL2X <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "To many colors for colorVector, maximal",
      length(ospsuite.plots::colorMaps[["ospDefault"]]),
      "allowed"
    )),
    envir = env
  )
}

messages$errorutilitiesplotL2XX <- function(env = parent.frame()) {
  eval(
    quote(
      "no default shape sets for ospsuite.plots. Please use ospsuite.plots::setDefaults()"
    ),
    envir = env
  )
}

messages$errorutilitiesplotL2XXX <- function(env = parent.frame()) {
  eval(quote("not enough shapes available"), envir = env)
}

messages$errorutilitiesplotL2XXXX <- function(env = parent.frame()) {
  eval(
    quote(
      "Invalid plot configuration table. For Rows with headers all other columns must be empty."
    ),
    envir = env
  )
}

messages$errorutilitiesplotL2XXXXX <- function(env = parent.frame()) {
  eval(
    quote("Invalid plot configuration table. Missing header for level"),
    envir = env
  )
}

messages$errorutilitiesplotL2XXXXXX <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "Plot configuration column",
      col,
      "has entries but no allowed values.
                     Did you forget some inputs e.g. observedData or pkParameterDT?"
    )),
    envir = env
  )
}

messages$errorutilitiesplotL2XXXXXXX <- function(env = parent.frame()) {
  eval(
    quote(paste("Invalid inputs in plot configuration column", col)),
    envir = env
  )
}

messages$errorutilitiesplotL2XXXXXXXX <- function(env = parent.frame()) {
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

messages$errorutilitiesplotL2XXXXXXXXX <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "Invalid configTable, each plot row needs at least one entry in one of the columns",
      paste(columnVector, collapse = ", ")
    )),
    envir = env
  )
}

messages$errorutilitiesplotL2XXXXXXXXXX <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "values for",
      col,
      "should be the same within outputPathId"
    )),
    envir = env
  )
}

messages$errorutilitiesplotL2XXXXXXXXXXX <- function(env = parent.frame()) {
  eval(
    quote(paste0(
      'Please check sheet Outputs in plotconfiguration file. Unit "',
      unit,
      '" is not valid'
    )),
    envir = env
  )
}

messages$errorutilitiesplotL2XXXXXXXXXXXX <- function(env = parent.frame()) {
  eval(
    quote(
      "colorLegend must be a character string concatenated from two characters separated by |."
    ),
    envir = env
  )
}

messages$errorutilitiesplotL2XXXXXXXXXXXXX <- function(env = parent.frame()) {
  eval(
    quote("All values in colorVector must be valid color names."),
    envir = env
  )
}

messages$warningutilitiesplotL1 <- function(env = parent.frame()) {
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

messages$errorutilitiesreportingL1 <- function(env = parent.frame()) {
  eval(
    quote("Pandoc is not installed, word report was not created."),
    envir = env
  )
}

messages$errorutilitiessensitivityL1 <- function(env = parent.frame()) {
  eval(
    quote(
      "SensitivityParameter xlsx is not added to the projectConfiguration Please call 'addSensitivityTable(projectConfiguration)'"
    ),
    envir = env
  )
}

messages$errorutilitiesworkflowL1 <- function(env = parent.frame()) {
  eval(
    quote(
      "Option stopHelperFunction to control valid runs is not initialized. Please call `setWorkflowOptions(isValidRun)`"
    ),
    envir = env
  )
}

messages$errorutilitiesworkflowL1X <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "You are using a helper function, which is not allowed during a valid run. Called from:",
      callingFunction
    )),
    envir = env
  )
}

messages$warningutilitiesworkflowL1 <- function(env = parent.frame()) {
  eval(
    quote(
      "Environment Variable 'QCpassed' not found, empty or a non logical, set 'QCpassed' to FALSE"
    ),
    envir = env
  )
}

messages$errorutilitiesxlsxL1 <- function(env = parent.frame()) {
  eval(
    quote(paste("Sheet", clonedSheet, "does not exist in the workbook.")),
    envir = env
  )
}

messages$errorutilitiesxlsxL1X <- function(env = parent.frame()) {
  eval(
    quote(paste("Template file", templatePath, "does not exist.")),
    envir = env
  )
}

messages$errorutilitiesxlsxL1XX <- function(env = parent.frame()) {
  eval(quote(paste("Sheet", sheetName, "does not exist.")), envir = env)
}

messages$errorutilitiesxlsxL1XXX <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "ambiguous header names in sheet",
      existingData,
      paste(names(existingData)[ix], collapse = ",")
    )),
    envir = env
  )
}

messages$warningutilitiesxlsxL1 <- function(env = parent.frame()) {
  eval(
    quote(paste(
      sheetName,
      "already exists. Existing content will be cleared."
    )),
    envir = env
  )
}

messages$warningutilitiesxlsxL1X <- function(env = parent.frame()) {
  eval(
    quote(
      "Output definition in Scenario.xlsx and Plot.xlsx is inconsistent. Please synchronize manually"
    ),
    envir = env
  )
}

messages$errorWorkflowScriptExporterL1 <- function(env = parent.frame()) {
  eval(
    quote(
      "Error: Please provide either scenarioNames or workflowRmd. Only one of the two is required."
    ),
    envir = env
  )
}

messages$errorWorkflowScriptExporterL1X <- function(env = parent.frame()) {
  eval(
    quote(
      "Error: Please provide either scenarioNames or workflowRmd. At least one of the two is required for initialization."
    ),
    envir = env
  )
}

messages$errorWorkflowScriptExporterL1XX <- function(env = parent.frame()) {
  eval(
    quote("Error: The file did not become available within 1 minute."),
    envir = env
  )
}

messages$errorWorkflowScriptExporterL1XXX <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "Chunks are missing in workflowRmd check:",
      paste(tmp, collapse = ", ")
    )),
    envir = env
  )
}

messages$errorWorkflowScriptExporterL2 <- function(env = parent.frame()) {
  eval(quote("no scenarios for export available"), envir = env)
}

messages$errorWorkflowScriptExporterL2X <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "Error: Please use only population scenarios that have exported populations",
      "in workflows intended for an electronic package."
    )),
    envir = env
  )
}

messages$errorWorkflowScriptExporterL2XX <- function(env = parent.frame()) {
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

messages$errorWorkflowScriptExporterL2XXX <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "Error: File copy to the ePackage folder failed for the following files:",
      paste(inputFiles[!success]$fileName, collapse = ", "),
      ". Please check the source paths and ensure the files exist."
    )),
    envir = env
  )
}

messages$errorWorkflowScriptExporterL5 <- function(env = parent.frame()) {
  eval(
    quote(paste(
      "Error: Inconsisten placeholders in workflow script template and chunk Names.",
      "Placeholder for chunk",
      chunkName,
      "is missing",
      "\nThat should not happen. Please ask package administrator for help."
    )),
    envir = env
  )
}

messages$errorWorkflowScriptExporterL6 <- function(env = parent.frame()) {
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

messages$errorWorkflowScriptExporterL6X <- function(env = parent.frame()) {
  eval(
    quote(paste("Error: Invalid file extension for", fileName)),
    envir = env
  )
}

messages$errorWorkflowScriptExporterL6XX <- function(env = parent.frame()) {
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

messages$errorWorkflowScriptExporterL8 <- function(env = parent.frame()) {
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

messages$warningWorkflowScriptExporterL2 <- function(env = parent.frame()) {
  eval(
    quote(paste0(
      "Warning: Adjusted filenames due to naming requirements:\n",
      paste(
        basename(changedInputFiles$source),
        "->",
        changedInputFiles$fileName,
        collapse = "\n"
      ),
      "\nYou may use the input variable `fileNameReplacements` of the workflow export function",
      " to configure file names more appropriately."
    )),
    envir = env
  )
}

messages$errorWorkflowScriptExporterL8X <- function(env = parent.frame()) {
  eval(quote(paste0("JSON file does not exist: ", jsonPath)), envir = env)
}
