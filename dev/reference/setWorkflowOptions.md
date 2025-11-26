# Sets options for a reporting frame work workflow.

Sets options for a reporting frame work workflow.

## Usage

``` r
setWorkflowOptions(isValidRun = NULL)
```

## Arguments

- isValidRun:

  A logical value indicating if the run is valid. If TRUE, options are
  set for a valid run; if FALSE, options are set for an invalid run.

## Details

This function configures options for a reporting framework workflow,
which is typically applied in two distinct phases:

1\. Exploratory Analysis: This phase involves preparation of outputs
that are not yet qualified. In this case: - Watermarks are not
applied. - Helper functions for building configuration tables are
allowed.

2\. Production of Final Output: This phase involves generating the
"final" output (a valid run). In this case: - Figures should not have
watermarks. - Functions that manipulate inputs are not allowed. - The
workflow will stop if an error occurs during execution.

Relevant Options:

\- \`ospsuite.plots.watermark_enabled\`: Set to TRUE when \`isValidRun\`
is FALSE to display watermarks on figures, and FALSE when \`isValidRun\`
is TRUE.

\- \`OSPSuite.RF.skipFailingPlots\`: Set to TRUE when \`isValidRun\` is
FALSE to skip plots that fail to generate, and FALSE when \`isValidRun\`
is TRUE.

\- \`OSPSuite.RF.stopHelperFunction\`: Set to TRUE when \`isValidRun\`
is TRUE to stop the execution of helper functions during valid runs.

## See also

Other project initialization:
[`ProjectConfigurationRF`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/ProjectConfigurationRF.md),
[`createProjectConfiguration()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createProjectConfiguration.md),
[`getQCpassedEnvironmentVariable()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/getQCpassedEnvironmentVariable.md),
[`initProject()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/initProject.md)
