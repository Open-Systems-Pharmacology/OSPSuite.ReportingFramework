# plotData for Time profile plot Data

An object to collect plot Data and other information to create a time
profile plot

## Value

RmdPlotManager object Load simulated results for the specified project
configuration

## Super class

[`ospsuite.utils::Printable`](https://www.open-systems-pharmacology.org/OSPSuite.RUtils/reference/Printable.html)
-\> `RmdPlotManager`

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

- [`PlotDataTimeProfile$new()`](#method-RmdPlotManager-new)

- [`PlotDataTimeProfile$loadSimulatedResults()`](#method-RmdPlotManager-loadSimulatedResults)

- [`PlotDataTimeProfile$filterObservedDataForPlot()`](#method-RmdPlotManager-filterObservedDataForPlot)

- [`PlotDataTimeProfile$addTimeRangeTags()`](#method-RmdPlotManager-addTimeRangeTags)

- [`PlotDataTimeProfile$splitDataToPanels()`](#method-RmdPlotManager-splitDataToPanels)

- [`PlotDataTimeProfile$setOrderAndFactors()`](#method-RmdPlotManager-setOrderAndFactors)

- [`PlotDataTimeProfile$setIndexColumns()`](#method-RmdPlotManager-setIndexColumns)

- [`PlotDataTimeProfile$getTimeLabelForTimeRange()`](#method-RmdPlotManager-getTimeLabelForTimeRange)

- [`PlotDataTimeProfile$addPredictedForObserved()`](#method-RmdPlotManager-addPredictedForObserved)

- [`PlotDataTimeProfile$getDataForTimeRange()`](#method-RmdPlotManager-getDataForTimeRange)

- [`PlotDataTimeProfile$useColorIndex()`](#method-RmdPlotManager-useColorIndex)

- [`PlotDataTimeProfile$useShapeIndex()`](#method-RmdPlotManager-useShapeIndex)

- [`PlotDataTimeProfile$hasSimulatedPop()`](#method-RmdPlotManager-hasSimulatedPop)

- [`PlotDataTimeProfile$hasObservedData()`](#method-RmdPlotManager-hasObservedData)

- [`PlotDataTimeProfile$hasObservedDataRange()`](#method-RmdPlotManager-hasObservedDataRange)

- [`PlotDataTimeProfile$hasReferenceComparison()`](#method-RmdPlotManager-hasReferenceComparison)

- [`PlotDataTimeProfile$clone()`](#method-RmdPlotManager-clone)

------------------------------------------------------------------------

### Method `new()`

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

------------------------------------------------------------------------

### Method `loadSimulatedResults()`

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

### Method `filterObservedDataForPlot()`

#### Usage

    PlotDataTimeProfile$filterObservedDataForPlot(dataObserved)

#### Arguments

- `dataObserved`:

  \`data.table\` with observed data

#### Returns

\`data.table\` with filtered observed data for plotting Replicates data
for each time tag

------------------------------------------------------------------------

### Method `addTimeRangeTags()`

#### Usage

    PlotDataTimeProfile$addTimeRangeTags()

#### Returns

invisible(NULL) Creates plotId for each scenario outputs group

------------------------------------------------------------------------

### Method `splitDataToPanels()`

#### Usage

    PlotDataTimeProfile$splitDataToPanels(nMaxFacetRows)

#### Arguments

- `nMaxFacetRows`:

  maximal number of facet rows @title Set factors for plotting

------------------------------------------------------------------------

### Method `setOrderAndFactors()`

#### Usage

    PlotDataTimeProfile$setOrderAndFactors()

#### Returns

invisible(NULL) set columns for aesthetics

------------------------------------------------------------------------

### Method `setIndexColumns()`

#### Usage

    PlotDataTimeProfile$setIndexColumns(referenceScaleVector)

#### Arguments

- `referenceScaleVector`:

  scale vector to scale aesthetic color and fill for scenarios with
  reference scenario Get the time label for the filtered time range

------------------------------------------------------------------------

### Method `getTimeLabelForTimeRange()`

#### Usage

    PlotDataTimeProfile$getTimeLabelForTimeRange(filterName)

#### Arguments

- `filterName`:

  character with name of time range filter @title Add predicted values
  to observed data

------------------------------------------------------------------------

### Method `addPredictedForObserved()`

#### Usage

    PlotDataTimeProfile$addPredictedForObserved()

#### Returns

invisible(NULL) Get the data for the filtered time range

------------------------------------------------------------------------

### Method `getDataForTimeRange()`

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

### Method `useColorIndex()`

#### Usage

    PlotDataTimeProfile$useColorIndex()

#### Returns

Logical Function to determine if shape legend is needed

------------------------------------------------------------------------

### Method `useShapeIndex()`

#### Usage

    PlotDataTimeProfile$useShapeIndex()

#### Returns

Logical Function to determine if simulation is aggregated

------------------------------------------------------------------------

### Method `hasSimulatedPop()`

#### Usage

    PlotDataTimeProfile$hasSimulatedPop()

#### Returns

Logical Function to determine if data contains observed data

------------------------------------------------------------------------

### Method `hasObservedData()`

#### Usage

    PlotDataTimeProfile$hasObservedData()

#### Returns

Logical Function to determine if data contains observed data range

------------------------------------------------------------------------

### Method `hasObservedDataRange()`

#### Usage

    PlotDataTimeProfile$hasObservedDataRange()

#### Returns

Logical Function to determine if a reference population should be
plotted

------------------------------------------------------------------------

### Method `hasReferenceComparison()`

#### Usage

    PlotDataTimeProfile$hasReferenceComparison()

#### Returns

Logical

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PlotDataTimeProfile$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
