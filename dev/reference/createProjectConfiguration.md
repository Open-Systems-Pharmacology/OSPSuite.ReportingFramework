# \#' Create a \`ProjectConfiguration\`

Create a \`ProjectConfigurationRF\` based on the
\`"ProjectConfiguration.xlsx"\`

based on esqlabsR::ProjectConfiguration but with additional file
information for PK Parameter definitions

## Usage

``` r
createProjectConfiguration(path = file.path("ProjectConfiguration.xlsx"))
```

## Arguments

- path:

  path to the \`ProjectConfiguration.xlsx\` file. default to the
  \`ProjectConfiguration.xlsx\` file located in the working directory.

## Value

Object of type \`ProjectConfigurationRF\`

## See also

Other project initialization:
[`ProjectConfigurationRF`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/ProjectConfigurationRF.md),
[`getQCpassedEnvironmentVariable()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/getQCpassedEnvironmentVariable.md),
[`initProject()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/initProject.md),
[`setWorkflowOptions()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setWorkflowOptions.md)
