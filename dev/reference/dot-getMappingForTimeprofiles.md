# Creates mapping for plotData.

This function generates a mapping for the plotting based on the provided
plot data and metadata.

## Usage

``` r
.getMappingForTimeprofiles(plotData, metaData, userMapping)
```

## Arguments

- plotData:

  Data to map.

- metaData:

  A list with metadata for plotData.

- userMapping:

  Mapping provided by the user; this will update the internal mapping.

## Value

A mapping object for ggplot2.
