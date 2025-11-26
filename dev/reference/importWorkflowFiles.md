# Import Workflow Files

This function imports workflow files from an electronic package
(ePackage) into a new project configuration. It handles different file
types and copies them to the appropriate directories.

## Usage

``` r
importWorkflowFiles(projectConfigurationNew, ePackageFolder, wfIdentifier)
```

## Arguments

- projectConfigurationNew:

  An object representing the new project configuration where files will
  be imported.

- ePackageFolder:

  A character string representing the path to the electronic package
  folder containing the workflow files.

- wfIdentifier:

  An integer identifier for the workflow being imported.

## Value

Invisible NULL.
