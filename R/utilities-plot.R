#' #' Title
#' #'
#' #' @param projectConfig
#' #' @param functionKey
#' #' @param plotFunction
#' #' @param subfolder
#' #' @param inputs
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' runPlot <- function(projectConfig,
#'                     functionKey = NULL,
#'                     plotFunction = NULL,
#'                     subfolder = functionKey,
#'                     inputs = list()){
#'   # Validate Inputs
#'
#'   # get function
#'   if (!is.null(functionKey)){
#'     plotFunction <- getFunctionByKey(functionKey)
#'   }
#'
#'   resultDirectory = file.path(projectConfig$figures,'subfolder')
#'
#'   # execute plotfunction
#'   rmdContainer = do.call(what = plotFunction,
#'           args = c(projectConfig,resultDirectory,inputs))
#'
#'   # create rmd
#'   rmdContainer$createRmd(projectConfig$figures)
#'
#' }
