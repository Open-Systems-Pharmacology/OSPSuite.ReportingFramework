# Create an AI-ready workflow prompt

Builds a project-specific prompt text that users can paste into an AI
assistant. The prompt includes available scenarios, outputs, groups, and
data-import metadata.

## Usage

``` r
createAIWorkflowPrompt(
  projectConfiguration,
  task = "create-workflow",
  includeExamples = TRUE,
  maxItemsPerSection = 30
)
```

## Arguments

- projectConfiguration:

  Project configuration object.

- task:

  Intended AI task label (e.g. \`"create-workflow"\`).

- includeExamples:

  If \`TRUE\`, include example commands and snippets.

- maxItemsPerSection:

  Maximum number of list items included per section.

## Value

A character string containing the prompt.

## Details

Experimental: this helper is intended for early evaluation and may
change in future releases.

## See also

Other project initialization:
[`ProjectConfigurationRF`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/ProjectConfigurationRF.md),
[`createAIPromptFromTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createAIPromptFromTask.md),
[`createProjectConfiguration()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createProjectConfiguration.md),
[`exportWorkflowContext()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/exportWorkflowContext.md),
[`getQCpassedEnvironmentVariable()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/getQCpassedEnvironmentVariable.md),
[`initProject()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/initProject.md),
[`restoreProjectConfigurationRF()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/restoreProjectConfigurationRF.md),
[`setWorkflowOptions()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setWorkflowOptions.md),
[`snapshotProjectConfigurationRF()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/snapshotProjectConfigurationRF.md),
[`upgradeToReportingFramework()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/upgradeToReportingFramework.md),
[`validateWorkflowForAI()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateWorkflowForAI.md)
