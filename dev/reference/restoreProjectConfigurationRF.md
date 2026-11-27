# Restore a ReportingFramework project configuration from a JSON snapshot

Extends
[`esqlabsR::restoreProjectConfiguration()`](https://esqlabs.github.io/esqlabsR/reference/restoreProjectConfiguration.html)
by also restoring RF-specific metadata stored in the snapshot (e.g.
\`ospsuiteReportingFrameworkVersion\`, add-on file paths).

## Usage

``` r
restoreProjectConfigurationRF(jsonPath, outputDir = NULL, ...)
```

## Arguments

- jsonPath:

  Path to the `ProjectConfiguration.json` snapshot file.

- outputDir:

  Directory where xlsx files are restored. Defaults to the same
  directory as the JSON file.

- ...:

  Additional arguments forwarded to
  [`esqlabsR::restoreProjectConfiguration()`](https://esqlabs.github.io/esqlabsR/reference/restoreProjectConfiguration.html).

## Value

A `ProjectConfigurationRF` object for the restored project.

## See also

Other project initialization:
[`ProjectConfigurationRF`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/ProjectConfigurationRF.md),
[`createAIPromptFromTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createAIPromptFromTask.md),
[`createAIWorkflowPrompt()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createAIWorkflowPrompt.md),
[`createProjectConfiguration()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createProjectConfiguration.md),
[`exportWorkflowContext()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/exportWorkflowContext.md),
[`getQCpassedEnvironmentVariable()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/getQCpassedEnvironmentVariable.md),
[`initProject()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/initProject.md),
[`setWorkflowOptions()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setWorkflowOptions.md),
[`snapshotProjectConfigurationRF()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/snapshotProjectConfigurationRF.md),
[`upgradeToReportingFramework()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/upgradeToReportingFramework.md),
[`validateWorkflowForAI()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateWorkflowForAI.md)
