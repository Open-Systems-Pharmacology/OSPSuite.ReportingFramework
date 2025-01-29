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

rmdPlotManager <-
  RmdPlotManager$new(
    rmdfolder = file.path(projectConfiguration$outputFolder),
    subfolder = subfolder
  )

iRow = 14
iEnd = 15
onePlotConfig <- split(configTable[seq(iRow,iEnd)], by = "plotName")[[1]]

plotType <- 'TP'
timeRangeFilter <- 'total'
yScale <- 'log'
plotCounter <- 1


plotType <- 'PvO'
plotType <- "ResvT"


# PK Ratio Forest ----------------
pkParameterObserved = dataObservedPK

functionKey = "PK_RatioForestByBootstrap"
configTableSheet = "PKParameter_Forest"
subfolder <- paste0(configTableSheet,getFuncionKeys()[[functionKey]]$subfolderOffset)
ratioCalculationMethod = 'byBootsTrapping'
ratioCalculationInputs = list(
  coefficientOfVariation = 0,
  nObservationDefault = 24,
  nBootstrap = nBootstrap,
  confLevel = 0.9,
  statFun = function(x) exp(mean(log(x))),
  seed = seed
)
digitsToRound = 3
digitsToShow = 3
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
tableLabels =  c('Ratio',
                paste0(confLevel * 100, '%\nCI lower'),
                paste0(confLevel * 100, '%\nCI upper'))

configTable <-
  readPKForestConfigTable(
    sheetName = configTableSheet,
    projectConfiguration = projectConfiguration,
    pkParameterDT = pkParameterDT
  )


rmdPlotManager <- RmdPlotManager$new(
  rmdfolder = file.path(projectConfiguration$outputFolder),
  subfolder = subfolder
)

iRow = 6
iEnd = 9
onePlotConfig <- split(configTable[seq(iRow,iEnd)], by = "plotName")[[1]]

outputPathIdLoop <- splitInputs(onePlotConfig$outputPathId[1])[1]


# PK boxplot --------------------------
configTableSheet = "PKParameter_Boxplot"
functionKey = "PK_Boxwhisker_Absolute"
subfolder <- paste0(configTableSheet,getFuncionKeys()[[functionKey]]$subfolderOffset)
asRatio = FALSE
xAxisTextAngle = 45
facetAspectRatio = 0.5
percentiles = getOspsuite.plots.option(optionKey = OptionKeys$Percentiles)
outliers = TRUE
yScale = 'log'
plotCaptionAddon <- onePlotConfig$plotCaptionAddon[1]

configTable <- readPKBoxwhiskerConfigTable(
  sheetName = configTableSheet,
  projectConfiguration = projectConfiguration,
  pkParameterDT = pkParameterDT
)

iRow = 2
iEnd = 5
onePlotConfig <- split(configTable[seq(iRow,iEnd)], by = "plotName")[[1]]
