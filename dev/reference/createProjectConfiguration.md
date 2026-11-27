# Create a \`ProjectConfiguration\`

Create a \`ProjectConfigurationRF\` based on the
\`"ProjectConfiguration.xlsx"\`

based on esqlabsR::ProjectConfiguration but with additional file
information for PK Parameter definitions

## Usage

``` r
createProjectConfiguration(
  path = file.path("ProjectConfiguration.xlsx"),
  ignoreVersionCheck = FALSE
)
```

## Arguments

- path:

  path to the \`ProjectConfiguration.xlsx\` file. default to the
  \`ProjectConfiguration.xlsx\` file located in the working directory.

- ignoreVersionCheck:

  If \`TRUE\`, skip version mismatch checks for esqlabsR and the
  reporting framework when loading the configuration file. Defaults to
  \`FALSE\`.

## Value

Object of type \`ProjectConfigurationRF\`

## See also

Other project initialization:
[`ProjectConfigurationRF`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/ProjectConfigurationRF.md),
[`createAIPromptFromTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createAIPromptFromTask.md),
[`createAIWorkflowPrompt()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createAIWorkflowPrompt.md),
[`exportWorkflowContext()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/exportWorkflowContext.md),
[`getQCpassedEnvironmentVariable()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/getQCpassedEnvironmentVariable.md),
[`initProject()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/initProject.md),
[`restoreProjectConfigurationRF()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/restoreProjectConfigurationRF.md),
[`setWorkflowOptions()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setWorkflowOptions.md),
[`snapshotProjectConfigurationRF()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/snapshotProjectConfigurationRF.md),
[`upgradeToReportingFramework()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/upgradeToReportingFramework.md),
[`validateWorkflowForAI()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateWorkflowForAI.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Initialize a new reporting project configuration folder first
initProject(configurationDirectory = "path/to/Configuration")

# Then load the generated ProjectConfiguration.xlsx
pc <- createProjectConfiguration(
  path = file.path("path/to/Configuration", "ProjectConfiguration.xlsx")
)
} # }
```
