#' Title
#'
#' @param projectConfiguration
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
  # Validate Inputs

  # get function
  if (!is.null(functionKey)){
    plotFunction <- getFunctionByKey(functionKey)
  }

  resultDirectory = file.path(projectConfiguration$figures,subfolder)

  # execute plotfunction
  rmdContainer = do.call(what = plotFunction,
          args = c(projectConfiguration,resultDirectory,inputs))

  # create rmd
  rmdContainer$createRmd(projectConfiguration$figures)

}


getFunctionByKey <- function(key){
  plotFunction <-
    switch(key,
           TimeProfile = plotTimeProfilePanels,
           stop('unkown function key'))
}
