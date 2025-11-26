# Initialize Project Directory

This function initializes a project directory by creating necessary
subdirectories and copying configuration files from a source Excel file.
It reads the directory structure and files specified in the provided
Excel template and sets up the project environment accordingly.

## Usage

``` r
initProject(
  configurationDirectory = ".",
  sourceConfigurationXlsx = system.file("templates", "ProjectConfiguration.xlsx", package
    = "ospsuite.reportingframework"),
  templatePath = system.file("templates", package = "ospsuite.reportingframework"),
  overwrite = FALSE
)
```

## Arguments

- configurationDirectory:

  A character string specifying the path to the project directory to be
  initialized. Defaults to the current working directory ('.').

- sourceConfigurationXlsx:

  A character string representing the path to the source Excel file
  containing the project configuration. By default, it uses the template
  provided by the 'ospsuite.reportingframework' package.

- templatePath:

  path of all template files

- overwrite:

  A logical value indicating whether to overwrite existing files in the
  project directory. Defaults to FALSE, meaning existing files will not
  be overwritten.

## Value

This function returns an invisible NULL. It is used for its side effects
of creating directories and copying files rather than producing a value.

## See also

Other project initialization:
[`ProjectConfigurationRF`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/ProjectConfigurationRF.md),
[`createProjectConfiguration()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createProjectConfiguration.md),
[`getQCpassedEnvironmentVariable()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/getQCpassedEnvironmentVariable.md),
[`setWorkflowOptions()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setWorkflowOptions.md)
