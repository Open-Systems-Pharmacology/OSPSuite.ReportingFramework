# Validate workflow configuration for AI-assisted usage

Runs lightweight checks to detect missing sheets, unknown data classes,
and numeric-values configuration inconsistencies before users execute
AI-generated workflow code.

## Usage

``` r
validateWorkflowForAI(projectConfiguration, strict = FALSE)
```

## Arguments

- projectConfiguration:

  Project configuration object.

- strict:

  If \`TRUE\`, warnings are treated as failing checks.

## Value

A list with \`ok\`, \`errors\`, \`warnings\`, and \`suggestions\`.

## Details

Experimental: this helper is intended for early evaluation and may
change in future releases.

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
[`snapshotProjectConfigurationRF()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/snapshotProjectConfigurationRF.md),
[`upgradeToReportingFramework()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/upgradeToReportingFramework.md)
