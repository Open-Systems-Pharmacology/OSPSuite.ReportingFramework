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

functionKey = "PK_DDIRatio"
configTableSheet = "PK_DDIRatio"
subfolder <- configTableSheet
pkParameterDT = pkParameterDT
coefficentOfVariation = 0
nObservationDefault = 16
digitsToRound = 3
digitsToShow = 3
nBootstrap = 1000
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
  readPKRatioConfigTable(
    sheetName = configTableSheet,
    projectConfiguration = projectConfiguration,
    pkParameterDT = pkParameterDT
  )


rmdContainer <- RmdContainer$new(
  rmdfolder = file.path(projectConfiguration$outputFolder),
  subfolder = subfolder
)

iRow = 10
iEnd = 17
onePlotConfig <- split(configTable[seq(iRow,iEnd)], by = "plotName")[[1]]

outputPathIdLoop <- unique(pkParameterDT$outputPathId)[1]
