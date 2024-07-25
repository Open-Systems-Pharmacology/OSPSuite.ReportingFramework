#' Title
#'
#' @template projectConfig
#' @param functionKey
#' @param plotFunction
#' @param subfolder
#' @param inputs
#'
#' @return
#' @export
#'
#' @examples
runPlot <- function(projectConfiguration,
                    functionKey = NULL,
                    plotFunction = NULL,
                    subfolder = functionKey,
                    inputs = list()){
  # get function
  if (!is.null(functionKey)){
    plotFunction <- getFunctionByKey(functionKey)
  }
  checkmate::assertFunction(plotFunction)

  resultDirectory = file.path(projectConfiguration$figures,subfolder)
  if (!dir.exists(resultDirectory))
    dir.create(resultDirectory, recursive = TRUE)

  # execute plotfunction
  rmdContainer = do.call(what = plotFunction,
          args = c(projectConfiguration,resultDirectory,inputs))

  # create rmd
  rmdContainer$createRmd(projectConfiguration$figures)

  return(invisible())
}


#' select function by key
#'
#' @param key character for function selection
#'
#' @return
#' @export
getFunctionByKey <- function(key){
  plotFunction <-
    switch(key,
           TimeProfile = plotTimeProfilePanels,
           stop('unkown function key'))
}


#' adds the default configuration to the template configuration
#'
#' if the template does not exist in the plotconfigurationfile in the project directory
#' it is taken from the plotconfigurationfile of the package installation.
#' In this case formats are not preserved
#'
#' @param wb Plotconfiguration file
#' @param templateSheet name of the template sheet
#' @param sheetName name of new sheet
#' @param dtNewConfig `data.table` with default configuration
#'
#' @return Plotconfiguration file
#' @export
addConfigToTemplate <- function(wb,templateSheet,sheetName,dtNewConfig){

  # get template
  if (templateSheet %in% wb$sheet_names){
    templateConfiguration <- xlsxReadData(wb = wb,sheetName  = templateSheet)
  } else{
    templateConfiguration <-
      xlsxReadData(
        wb = system.file(
          "templates",
          "templateProject",
          "Parameters",
          "Plots.xlsx",
          package = "ospsuite.reportingframework",
          mustWork = TRUE
        ),
        sheetName  = templateSheet
      )
  }

  dtNewConfig <- rbind(templateConfiguration[1,],
                     dtNewConfig,
                     fill = TRUE)

  if (templateSheet != sheetName){
    xlsxCloneAndSet(wb = wb,clonedSheet = templateSheet,sheetName = sheetName,dt = dtNewConfig)
  } else{
    xlsxWriteData(wb = wb,sheetName = sheetName,dt = dtNewConfig)
  }

  return(wb)

}


#' Validation of data.table with outputPath Ids
#'
#' @param dtOutputPaths data.table with outputPath Ids
validateOutputIdsForPlot <- function(dtOutputPaths){

  checkmate::assertCharacter(dtOutputPaths$OutputPathId,any.missing = FALSE)

  # Check for unique values for outputpathids
  uniqueColumns <- c('DisplayName','DisplayUnit')
  uniqueIDValues <-
    dtOutputPaths[, lapply(.SD, function(x)
      length(unique(x))), by = OutputPathId, .SDcols = uniqueColumns]
  tmp <-lapply(uniqueColumns,function(col){
    if (any(uniqueIDValues[[col]]>1)) stop(paste('values for',col, 'should be the same within OutputPathId'))})

  return(invisible())
}
