# Synchronize Scenarios Between Scenario and Plot Files

This function synchronizes scenarios between two Excel files: one
containing scenarios for a project and the other containing scenarios
related to plots. It adds any missing scenarios from the scenarios file
to the plots file.

## Usage

``` r
synchronizeScenariosWithPlots(projectConfiguration)
```

## Arguments

- projectConfiguration:

  An object of class ProjectConfiguration containing the file paths for
  scenariosFile and plotsFile.

## Value

Returns invisibly.
