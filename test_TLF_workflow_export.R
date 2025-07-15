knitr::opts_chunk$set(echo = FALSE)

library(ospsuite.reportingframework)


pathsCustomfunctions <- list()

if (length(pathsCustomfunctions) > 0)
  lapply(pathsCustomfunctions,source)

qCpassed <- suppressWarnings(as.logical(as.double(Sys.getenv(x='QCpassed',unset = 0))))
# check if QCpassed was set as "TRUE" or "FALSE"
if (is.na(qCpassed))   qCpassed <- as.logical(Sys.getenv(x='QCpassed',unset = 0))
if (is.na(qCpassed)) stop("Environment Variable 'QCpassed' is not a logical")

setWorkflowOptions(isValidRun = qCpassed)

projectConfiguration <-
  ospsuite.reportingframework::createProjectConfiguration(
    path =  file.path("ProjectConfiguration.xlsx"))

initLogfunction(projectConfiguration)

# use this line if you have no observed time profile data
dataObserved <- NULL

# use code below if you have observed time profile data
dataObserved <- readObservedDataByDictionary(projectConfiguration = projectConfiguration,
                                             dataClassType = 'timeprofile',
                                             fileIds = NULL)

# here you can add your own code to manipulate and clean the data if necessary


# check the format
validateObservedData(dataObserved,dataClassType = 'timeprofile')

#  add quality checks to ensure data contains only data which is needed.
print(names(dataObserved))


# use this line if you have no observed pk-Parameter data
dataObservedPK <- NULL

# use code below you have observed pk-Parameter data
dataObservedPK <- readObservedDataByDictionary(projectConfiguration = projectConfiguration,
                                             dataClassType = 'pkParameter',
                                             fileIds = NULL)
# here you can add your own code to manipulate and clean the data if necessary

# check the format
validateObservedData(dataObserved,dataClassType = 'pkParameter')

#  add quality checks to ensure data contains only data which is needed.
print(names(dataObservedPK))


scenarioNames = c()

scenarioList <-
  createScenarios.wrapped(projectConfiguration = projectConfiguration,
                          scenarioNames = scenarioNames)

# It is assumed that all required scenarios were simulated in previous workflow started in same project directory.
scenarioResults <- loadScenarioResults(projectConfiguration = projectConfiguration,
                                       scenarioList = scenarioList)

pkParameterDT <- loadPKParameter(projectConfiguration = projectConfiguration,
                                 scenarioList = scenarioList)

# set graphic
ospsuite.plots::setDefaults()
theme_update(legend.position = 'top')
options(knitr.kable.NA = '')

# timeprofiles
runPlot(
  nameOfplotFunction = "plotTimeProfiles",
  projectConfiguration = projectConfiguration,
  inputs = list(
    configTableSheet = "TimeProfiles",
    dataObserved = dataObserved,
    scenarioResults = scenarioResults
  )
)




addMessageToLog("finalize workflow")

# save Session Infos including the loaded packages and R version, into a log file
saveSessionInfo()
