aggregationFun <-
  getAggregationFunction("GeometricStdDev")


configTableSheet = "TimeProfiles"
subfolder = configTableSheet
nFacetColumns = 2
nMaxFacetRows = 3
facetAspectRatio = 0.5
referenceScaleVector = list(
  Control = c(NA, NA),
  Reference = c(NA, NA)
)

configTable <- readTimeprofileConfigTable(sheetName = configTableSheet,projectConfiguration = projectConfiguration,dataObserved = dataObserved)

rmdContainer <-
  RmdContainer$new(
    rmdfolder = file.path(projectConfiguration$outputFolder),
    subfolder = subfolder
  )

iRow = 3
iEnd = 3
onePlotConfig <- split(configTable[seq(iRow,iEnd)], by = "plotName")[[1]]

plotType <- 'TP'
timeRangeFilter <- 'total'
yScale <- 'log'
plotCounter <- 1


plotType <- 'PvO'
plotType <- "ResvT"


# PK Ratio
pkParameterObserved = NULL

functionKey = "PK_RatioForestByBootstrap"
configTableSheet = "PKParameter_Forest"
subfolder <- getSubfolderByKey(functionKey,subfolder = NULL,configTableSheet)
coefficientOfVariation = 0
nObservationDefault = 16
digitsToRound = 3
digitsToShow = 3
statFun = function(x) exp(mean(log(x)))
confLevel = 0.9
nBootstrap = 1000
xlabel = 'DDI Ratio'
seed = 123
vlineIntercept = 1
scaleVactors = list(
  simulated = list(color ='darkgrey',
                   fill = 'darkgrey',
                   shape = 'circle'),
  observed = list(color = 'darkgrey',
                  fill = 'lightgrey',
                  shape = 'triangle filled'))
configTable <-
  readPKForestConfigTable(
    sheetName = configTableSheet,
    projectConfiguration = projectConfiguration,
    pkParameterDT = pkParameterDT
  )


rmdContainer <- RmdContainer$new(
  rmdfolder = file.path(projectConfiguration$outputFolder),
  subfolder = subfolder
)

iRow = 6
iEnd = 9
onePlotConfig <- split(configTable[seq(iRow,iEnd)], by = "plotName")[[1]]

outputPathIdLoop <- splitInputs(onePlotConfig$outputPathId[1])[1]
