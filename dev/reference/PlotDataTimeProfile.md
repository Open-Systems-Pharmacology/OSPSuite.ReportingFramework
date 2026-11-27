# plotData for Time profile plot Data

An object to collect plot Data and other information to create a time
profile plot

## Super class

[`ospsuite.utils::Printable`](https://www.open-systems-pharmacology.org/OSPSuite.RUtils/reference/Printable.html)
-\> `PlotDataTimeProfile`

## Active bindings

- `data`:

  Data for Plot

- `dataReference`:

  Data with references for data

- `configTable`:

  Configuration table for one plot

- `plotName`:

  plotName of configTable

- `dtCaption`:

  Data.table with caption information

- `scaleVectors`:

  List with scaling vectors to manually scale aesthetics

- `tpLabelSimulatedMean`:

  Label for simulated mean

- `tpLabelSimulatedRange`:

  Label for simulated range

- `tpLabelObserved`:

  Label for observed data

- `tpLabels`:

  List of time profile labels to add on ggplot

- `nFacetColumns`:

  Number of facet columns

- `timeRangeTagFilter`:

  List with filters for time range tags

- `reverseLegend`:

  if true color legend will be reversed

## Methods

### Public methods

- [`PlotDataTimeProfile$new()`](#method-PlotDataTimeProfile-initialize)

- [`PlotDataTimeProfile$loadSimulatedResults()`](#method-PlotDataTimeProfile-loadSimulatedResults)

- [`PlotDataTimeProfile$filterObservedDataForPlot()`](#method-PlotDataTimeProfile-filterObservedDataForPlot)

- [`PlotDataTimeProfile$addTimeRangeTags()`](#method-PlotDataTimeProfile-addTimeRangeTags)

- [`PlotDataTimeProfile$splitDataToPanels()`](#method-PlotDataTimeProfile-splitDataToPanels)

- [`PlotDataTimeProfile$setOrderAndFactors()`](#method-PlotDataTimeProfile-setOrderAndFactors)

- [`PlotDataTimeProfile$setIndexColumns()`](#method-PlotDataTimeProfile-setIndexColumns)

- [`PlotDataTimeProfile$getTimeLabelForTimeRange()`](#method-PlotDataTimeProfile-getTimeLabelForTimeRange)

- [`PlotDataTimeProfile$addPredictedForObserved()`](#method-PlotDataTimeProfile-addPredictedForObserved)

- [`PlotDataTimeProfile$getDataForTimeRange()`](#method-PlotDataTimeProfile-getDataForTimeRange)

- [`PlotDataTimeProfile$useColorIndex()`](#method-PlotDataTimeProfile-useColorIndex)

- [`PlotDataTimeProfile$useShapeIndex()`](#method-PlotDataTimeProfile-useShapeIndex)

- [`PlotDataTimeProfile$hasSimulatedPop()`](#method-PlotDataTimeProfile-hasSimulatedPop)

- [`PlotDataTimeProfile$hasObservedData()`](#method-PlotDataTimeProfile-hasObservedData)

- [`PlotDataTimeProfile$hasObservedDataRange()`](#method-PlotDataTimeProfile-hasObservedDataRange)

- [`PlotDataTimeProfile$hasReferenceComparison()`](#method-PlotDataTimeProfile-hasReferenceComparison)

- [`PlotDataTimeProfile$clone()`](#method-PlotDataTimeProfile-clone)

------------------------------------------------------------------------

### `PlotDataTimeProfile$new()`

Initialize a new instance of the class

#### Usage

    PlotDataTimeProfile$new(
      projectConfiguration = projectConfiguration,
      onePlotConfig = onePlotConfig
    )

#### Arguments

- `projectConfiguration`:

  Object of class \`ProjectConfiguration\` containing information on
  paths and file names

- `onePlotConfig`:

  plot configuration for one plot

- `dataObserved`:

  \`data.table\` with observed data

- `aggregationFun`:

  function to aggregate simulated data

#### Returns

QmdPlotManager object Load simulated results for the specified project
configuration

------------------------------------------------------------------------

### `PlotDataTimeProfile$loadSimulatedResults()`

#### Usage

    PlotDataTimeProfile$loadSimulatedResults(
      projectConfiguration,
      aggregationFun,
      scenarioResults
    )

#### Arguments

- `projectConfiguration`:

  Configuration for the project

- `aggregationFun`:

  Function to aggregate simulated data

- `scenarioResults`:

  list with simulated scenario results Filter observed data for plotting

------------------------------------------------------------------------

### `PlotDataTimeProfile$filterObservedDataForPlot()`

#### Usage

    PlotDataTimeProfile$filterObservedDataForPlot(dataObserved)

#### Arguments

- `dataObserved`:

  \`data.table\` with observed data

#### Returns

\`data.table\` with filtered observed data for plotting Replicates data
for each time tag

------------------------------------------------------------------------

### `PlotDataTimeProfile$addTimeRangeTags()`

#### Usage

    PlotDataTimeProfile$addTimeRangeTags()

#### Returns

invisible(NULL) Creates plotId for each scenario outputs group

------------------------------------------------------------------------

### `PlotDataTimeProfile$splitDataToPanels()`

#### Usage

    PlotDataTimeProfile$splitDataToPanels(nMaxFacetRows)

#### Arguments

- `nMaxFacetRows`:

  maximal number of facet rows @title Set factors for plotting

------------------------------------------------------------------------

### `PlotDataTimeProfile$setOrderAndFactors()`

#### Usage

    PlotDataTimeProfile$setOrderAndFactors()

#### Returns

invisible(NULL) set columns for aesthetics

------------------------------------------------------------------------

### `PlotDataTimeProfile$setIndexColumns()`

#### Usage

    PlotDataTimeProfile$setIndexColumns(referenceScaleVector)

#### Arguments

- `referenceScaleVector`:

  scale vector to scale aesthetic color and fill for scenarios with
  reference scenario Get the time label for the filtered time range

------------------------------------------------------------------------

### `PlotDataTimeProfile$getTimeLabelForTimeRange()`

#### Usage

    PlotDataTimeProfile$getTimeLabelForTimeRange(filterName)

#### Arguments

- `filterName`:

  character with name of time range filter @title Add predicted values
  to observed data

------------------------------------------------------------------------

### `PlotDataTimeProfile$addPredictedForObserved()`

#### Usage

    PlotDataTimeProfile$addPredictedForObserved()

#### Returns

invisible(NULL) Get the data for the filtered time range

------------------------------------------------------------------------

### `PlotDataTimeProfile$getDataForTimeRange()`

#### Usage

    PlotDataTimeProfile$getDataForTimeRange(
      filterName,
      plotCounter,
      yScale,
      typeFilter = NULL
    )

#### Arguments

- `filterName`:

  name of time range filter

- `plotCounter`:

  counter for different plots

- `yScale`:

  scale of y-axis

- `typeFilter`:

  filter for data type

#### Returns

\`data.table\` with filtered plot data Function to determine if color
legend is needed

------------------------------------------------------------------------

### `PlotDataTimeProfile$useColorIndex()`

#### Usage

    PlotDataTimeProfile$useColorIndex()

#### Returns

Logical Function to determine if shape legend is needed

------------------------------------------------------------------------

### `PlotDataTimeProfile$useShapeIndex()`

#### Usage

    PlotDataTimeProfile$useShapeIndex()

#### Returns

Logical Function to determine if simulation is aggregated

------------------------------------------------------------------------

### `PlotDataTimeProfile$hasSimulatedPop()`

#### Usage

    PlotDataTimeProfile$hasSimulatedPop()

#### Returns

Logical Function to determine if data contains observed data

------------------------------------------------------------------------

### `PlotDataTimeProfile$hasObservedData()`

#### Usage

    PlotDataTimeProfile$hasObservedData()

#### Returns

Logical Function to determine if data contains observed data range

------------------------------------------------------------------------

### `PlotDataTimeProfile$hasObservedDataRange()`

#### Usage

    PlotDataTimeProfile$hasObservedDataRange()

#### Returns

Logical Function to determine if a reference population should be
plotted

------------------------------------------------------------------------

### `PlotDataTimeProfile$hasReferenceComparison()`

#### Usage

    PlotDataTimeProfile$hasReferenceComparison()

#### Returns

Logical

------------------------------------------------------------------------

### `PlotDataTimeProfile$clone()`

The objects of this class are cloneable with this method.

#### Usage

    PlotDataTimeProfile$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
