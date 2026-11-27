# Initialize a ReportingFramework Project Directory

Convenience wrapper that initializes a complete ReportingFramework
project by first calling
[`esqlabsR::initProject()`](https://esqlabs.github.io/esqlabsR/reference/initProject.html)
for the base esqlabsR structure and then
[`upgradeToReportingFramework()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/upgradeToReportingFramework.md)
for the RF-specific configuration.

For more control, call the two steps separately:

1.  `esqlabsR::initProject(destination)`

2.  `upgradeToReportingFramework(configurationDirectory)`

## Usage

``` r
initProject(
  projectDirectory = ".",
  configurationDirectory = NULL,
  analysisSubfolderName = "ReportingFramework",
  templatePath = system.file("templates", package = "ospsuite.reportingframework"),
  overwrite = FALSE,
  keepEsqlabsRStructure = FALSE
)
```

## Arguments

- projectDirectory:

  A character string specifying the root project directory passed to
  [`esqlabsR::initProject()`](https://esqlabs.github.io/esqlabsR/reference/initProject.html).
  Defaults to the current working directory.

- configurationDirectory:

  A character string specifying the path to the RF configuration
  subdirectory. If omitted, defaults to
  `file.path(projectDirectory, "Scripts", analysisSubfolderName)`.

- analysisSubfolderName:

  Name of the analysis subfolder used to build default script and output
  paths. Defaults to `"ReportingFramework"`.

- templatePath:

  Path to the RF template files. Defaults to the package templates.

- overwrite:

  A logical value indicating whether to overwrite existing files.
  Defaults to `FALSE`.

- keepEsqlabsRStructure:

  If \`TRUE\`, preserve the esqlabsR directory and workbook layout when
  calling \`upgradeToReportingFramework()\`. Defaults to \`FALSE\` so a
  freshly initialized project uses the ReportingFramework default
  structure.

## Value

Invisible NULL.

## See also

Other project initialization:
[`ProjectConfigurationRF`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/ProjectConfigurationRF.md),
[`createAIPromptFromTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createAIPromptFromTask.md),
[`createAIWorkflowPrompt()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createAIWorkflowPrompt.md),
[`createProjectConfiguration()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createProjectConfiguration.md),
[`exportWorkflowContext()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/exportWorkflowContext.md),
[`getQCpassedEnvironmentVariable()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/getQCpassedEnvironmentVariable.md),
[`restoreProjectConfigurationRF()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/restoreProjectConfigurationRF.md),
[`setWorkflowOptions()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setWorkflowOptions.md),
[`snapshotProjectConfigurationRF()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/snapshotProjectConfigurationRF.md),
[`upgradeToReportingFramework()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/upgradeToReportingFramework.md),
[`validateWorkflowForAI()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateWorkflowForAI.md)
