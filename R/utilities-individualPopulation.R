#' Setup Individual Population Configuration
#'
#' This function sets up a configuration sheet for individual populations based on the provided project configuration and observed data.
#'
#' @param projectConfiguration A list containing project configuration details, including file paths for populations and scenarios.
#' @param dataObserved A data object containing observed data, which may be of class "DataCombined".
#' @param groups A character vector of group names to filter the individual population. Defaults to NULL.
#'
#' @return Returns NULL invisibly if no groups are available for individual population creation. Modifies the workbook specified in projectConfiguration.
#' @export
#'
#' @examples
#' # Example usage:
#' setupIndPopConfig(projectConfiguration, dataObserved, groups = c("Group1", "Group2"))
setupIndPopConfig <- function(projectConfiguration, dataObserved, groups = NULL) {

  checkmate::assertCharacter(groups,any.missing = FALSE,null.ok = TRUE)

  # Check if dataObserved is of class "DataCombined" and convert it if necessary
  if ("DataCombined" %in% class(dataObserved)) {
    dataObserved <- convertDataCombinedToDataTable(dataObserved)
  }

  # Load configuration table for IndividualPopulation
  wb <- openxlsx::loadWorkbook(projectConfiguration$populationsFile)

  # Check if the 'IndividualPopulation' sheet exists
  if (!('IndividualPopulation' %in% wb$sheet_names)) {
    dtIndPops <- xlsxReadData(projectConfiguration$scenariosFile, sheetName = 'Scenarios') %>%
      data.table::setnames("PopulationId", 'PopulationName') %>%
      dplyr::mutate('DataGroup' = '') %>%
      dplyr::select(c('PopulationName', 'DataGroup', "IndividualId", "ModelParameterSheets", "ApplicationProtocol")) %>%
      dplyr::filter(FALSE)
  } else {
    dtIndPops <- xlsxReadData(wb, 'IndividualPopulation')
  }

  # Remove any groups that are already in dtIndPops
  groups <- setdiff(groups, unique(dtIndPops$DataGroup))
  groups <- getIndividualDataGroups(dataObserved, groups)

  # Check if any groups are available for individual population creation
  if (length(groups) == 0) {
    warning("No groups available for individual population creation")
    return(NULL)
  }

  # Create new individual population data
  dtIndPopsNew <- dataObserved[group %in% groups, c('group', 'individualId')] %>%
    unique() %>%
    dplyr::mutate(PopulationName = group) %>%
    data.table::setnames(old = c('group', 'individualId'),
                         new = c('DataGroup', 'IndividualId')) %>%
    data.table::setorderv(c('DataGroup', 'IndividualId'))

  # Combine the existing and new individual population data
  dtIndPops <- rbind(dtIndPops, dtIndPopsNew, fill = TRUE)

  # Write data to the workbook
  if (!('IndividualPopulation' %in% wb$sheet_names)) {
    xlsxAddSheet(wb = wb, sheetName = 'IndividualPopulation', dt = dtIndPops)
  } else {
    xlsxWriteData(wb = wb, sheetName = 'IndividualPopulation', dt = dtIndPops)
  }

  # Save the workbook
  openxlsx::saveWorkbook(wb = wb, file = projectConfiguration$populationsFile, overwrite = TRUE)

  return(invisible())
}


generateIndividualPopulation <- function(projectConfiguration,modelFile,overwrite = TRUE){

  # initialize model for default values and Unit conversion
  sim <- ospsuite::loadSimulation(file.path(projectConfiguration$modelFolder,modelFile))

  # read list of poulations to create
  dtIndPops <- xlsxReadData(wb = projectConfiguration$populationsFile,
                            sheetName = "IndividualPopulation",
                            emptyAsNA = FALSE)


  #Read all parameter sheets
  params <- mapply(
    function(sheet, file) {
      message(sheet)
      .getAllParameterForSheets(
        projectConfiguration = projectConfiguration,
        sheets = .cleanUpSheetList(dtIndPops[[sheet]]),
        paramsXLSpath = file,
        sim = sim
      )
    },
    c(
      'ModelParameterSheets',
      'IndividualId',
      'ApplicationProtocol'
    ),
    c(
      projectConfiguration$modelParamsFile,
      projectConfiguration$individualsFile,
      projectConfiguration$applicationsFile
    ),
    SIMPLIFY = FALSE
  )

  # get IndividualBiometrics
  dtIndividualBiometrics <- xlsxReadData(wb = projectConfiguration$individualsFile,sheetName = "IndividualBiometrics")
  dtIndividualBiometrics <- dtIndividualBiometrics[IndividualId %in% dtIndPops$IndividualId]

  biometrics = list()

  # Create a progress bar
  pb <- utils::txtProgressBar(min = 1, max = nrow(biometrics), style = 3)

  for (indId in dtIndividualBiometrics$IndividualId) {
    utils::setTxtProgressBar(pb, indId) # Update the progress bar

    biomForInd <- dtIndividualBiometrics[IndividualId == indId, ]


    # Create ontogenies for the proteins
    moleculeOntogenies <- esqlabsR:::.readOntongeniesFromXLS(biomForInd)

    # Create the IndividualCharacteristics object
    individualCharacteristics <- ospsuite::createIndividualCharacteristics(
      species = biomForInd$Species,
      population = biomForInd$Population,
      gender = biomForInd$Gender,
      weight = biomForInd$`Weight [kg]`,
      height = biomForInd$`Height [cm]`,
      age = biomForInd$`Age [year(s)]`,
      moleculeOntogenies = moleculeOntogenies
    )

    individual <- createIndividual(individualCharacteristics)

    # Convert to data.table
    distributedParameters <- rbind(
      .convertBiomForIndStatics(biomForInd),
        data.table::as.data.table(individual$derivedParameters)[paths %in% c("Organism|Weight", "Organism|BMI", "Organism|BSA")],
        data.table::as.data.table(individual$distributedParameters)
      )

    # Create the output data.table
    output <- data.table(
      path = distributedParameters$paths,
      value = as.numeric(distributedParameters$values),
      unit = distributedParameters$units
    )

    # make sure evrythin is in baseunit
    output[, `:=`(
      dimension = getParameter(path, container = sim)$dimension),
      by ='path']
    output[, `:=`(
      base_value = toBaseUnit(quantityOrDimension = dimension, values = values, unit = units)),
      by ='paths']


    results = setNames(as.list(output$base_value),output$path)
    if (is.null(params$IndividualId[[indId]])) {
      params$IndividualId[[indId]] <- results
    } else {
      params$IndividualId[[indId]] <- utils::modifyList(params$IndividualId[[indId]],results )
    }

  }

  close(pb)

  popTableMatch = data.table()

  for (dPop in split(dtIndPops, by = "PopulationName")) {

    poptable <- .buildIndividualPopulation(projectConfiguration = projectConfiguration,
                                           params = params,
                                           dPop = dPop)

    # a population needs a numeric columns individualId
    poptable[,IndividualId := .I-1]
    setcolorder(poptable,'IndividualId')


    write.csv(x = poptable,
              file = file.path(
                projectConfiguration$populationsFolder,
                paste0(dPop$PopulationName[1], '.csv')
              ),
              fileEncoding = 'UTF8',
              sep = ',',
              row.names = FALSE
    )


  }

}


#'
#' @param projectConfiguration
#' @param dtScenarioG
#' @param dataG
#'
#' @return
#' @export
#'
#' @examples
.buildIndividualPopulation <- function(projectConfiguration,
                                       params,
                                       dPop) {




  poptable = data.table()
  poptableUnits <- data.table()

  for (d in split(dPop,by = c('IndividualId','DataGroup'))){

    popRow <- list(PopulationName = d$PopulationName,
                     DataGroup = d$DataGroup)

    for (parType in names(params)){

      sheets = cleanUpSheetList(d[[parType]])

      for (sheet in sheets){

        newValues = params[[parType]][[sheet]]

        popRow <- utils::modifyList(popRow,newValues)

      }

    }

    if (nrow(poptable) > 1){
      checkmate::assertNames(names(popRow),permutation.of = names(poptable))
    }
    poptable <- rbind(poptable,
                      data.table::as.data.table(popRow))

  }

  return(poptable)

}




#' Title
#'
#' @param sheets
#' @param paramsXLSpath
#'
#' @return
.getAllParameterForSheets <- function(projectConfiguration,sheets,paramsXLSpath,sim) {

  wb <- openxlsx::loadWorkbook(paramsXLSpath)
  sheets <- intersect(openxlsx::sheets(wb),sheets)

  if (length(sheets) == 0) {
    return(list())
  }

  params = lapply(sheets,function(sheet){
    tmp <-
      readParametersFromXLS(
        paramsXLSpath = paramsXLSpath,
        sheets = sheet) %>%
      data.table::as.data.table()

    tmp[, `:=`(
      dimension = getParameter(paths, container = sim)$dimension),
      by ='paths']
    tmp[, `:=`(
      base_value = toBaseUnit(quantityOrDimension = dimension, values = values, unit = units)),
      by ='paths']

    results = setNames(as.list(tmp$base_value),tmp$paths)
    return(results)
  })


  names(params) <- sheets

  return(params)
}



#' Title
#'
#' @param sheets
#'
#' @return
.cleanUpSheetList<- function(sheets){
  sheet <- unique(sheets)
  sheets <- unlist(strsplit(sheets,','))
  sheets <- trimws(sheets)
  sheets <- sheets[!is.na(sheets) & sheets != ""]

  return(sheets)
}



.convertBiomForIndStatics <- function(biomForInd){


  dtSelection = data.table(pName = c("IndividualId",
                                     "Population",
                                     "Gender",
                                     "Height [cm]",
                                     "Age [year(s)]",
                                     "Gestational Age [week(s)]"),
                           units = c('','','','dm','year(s)','week(s)'),
                           paths = c("ObservedIndividualId",
                                     "Population",
                                     "Gender",
                                     "Organism|Height",
                                     "Organism|Age",
                                     "Organism|Gestational age"))

  tmp <- biomForInd %>%
    dplyr::select(any_of(dtSelection$pName)) %>%
    data.table::setnames(
      old = dtSelection$pName,
      new = dtSelection$paths,
      skip_absent = TRUE
    )
  if  (!is.null(tmp$`Organism|Height`))
    tmp$`Organism|Height` <- ospsuite::toBaseUnit(
      quantityOrDimension = getParameter(path = 'Organism|Height',container = sim)$dimension,
      unit = 'cm',
      values = tmp$`Organism|Height`)

  result <- data.table(path = names(tmp),
                       values = as.vector(unlist(tmp)))

  result[, unit := dtSelection$units[match(path, dtSelection$paths)]]

  return(result)
}
