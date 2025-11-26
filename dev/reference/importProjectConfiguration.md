# Import Project Configuration

This function imports the project configuration from an electronic
package (ePackage) into a specified directory. It reads the
configuration data from a JSON file and creates the necessary Excel
sheets.

## Usage

``` r
importProjectConfiguration(
  configurationDirectory,
  ePackageFolder,
  wfIdentifier
)
```

## Arguments

- configurationDirectory:

  A character string representing the directory where the configuration
  files will be stored.

- ePackageFolder:

  A character string representing the path to the electronic package
  folder containing the configuration.

- wfIdentifier:

  An integer identifier for the workflow being imported.

## Value

A character string indicating the direction of synchronization (e.g.,
"plotToScenario", "bothways").
