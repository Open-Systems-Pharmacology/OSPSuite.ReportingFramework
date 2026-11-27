# Snapshot a ReportingFramework project configuration to JSON

Extends
[`esqlabsR::snapshotProjectConfiguration()`](https://esqlabs.github.io/esqlabsR/reference/snapshotProjectConfiguration.html)
with reporting-framework-specific metadata: the
\`ospsuiteReportingFrameworkVersion\` and a list of RF-specific add-on
file paths.

Note: overlapping file properties are excluded to avoid conflicts when
the JSON is later restored via esqlabsR or RF restore functions.

## Usage

``` r
snapshotProjectConfigurationRF(projectConfig, outputDir = NULL, ...)
```

## Arguments

- projectConfig:

  A `ProjectConfigurationRF` object.

- outputDir:

  Directory where the JSON file is written. Defaults to the same
  directory as `ProjectConfiguration.xlsx`.

- ...:

  Additional arguments forwarded to
  [`esqlabsR::snapshotProjectConfiguration()`](https://esqlabs.github.io/esqlabsR/reference/snapshotProjectConfiguration.html).

## Value

Invisible named list with the full snapshot data (including base
esqlabsR content and RF-specific metadata).

## Details

Referenced add-on workbooks are attached through
\`.rfAddReferencedAddonWorkbooks()\` after the base esqlabsR snapshot is
created.

## See also

Other project initialization:
[`ProjectConfigurationRF`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/ProjectConfigurationRF.md),
[`createAIPromptFromTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createAIPromptFromTask.md),
[`createAIWorkflowPrompt()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createAIWorkflowPrompt.md),
[`createProjectConfiguration()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createProjectConfiguration.md),
[`exportWorkflowContext()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/exportWorkflowContext.md),
[`getQCpassedEnvironmentVariable()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/getQCpassedEnvironmentVariable.md),
[`initProject()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/initProject.md),
[`restoreProjectConfigurationRF()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/restoreProjectConfigurationRF.md),
[`setWorkflowOptions()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setWorkflowOptions.md),
[`upgradeToReportingFramework()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/upgradeToReportingFramework.md),
[`validateWorkflowForAI()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateWorkflowForAI.md)
