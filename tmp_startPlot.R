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

iRow = 25
iEnd = 25
onePlotConfig <- split(configTable[seq(iRow,iEnd)], by = "plotName")[[1]]

plotType <- 'TP'
timeRangeFilter <- 'total'
yScale <- 'log'
plotCounter <- 1


plotType <- 'PvO'
