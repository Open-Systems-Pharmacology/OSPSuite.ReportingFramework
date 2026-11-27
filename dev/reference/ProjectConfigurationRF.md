# ProjectConfigurationRF

An object storing configuration used project-wide, composed of an
esqlabsR project configuration and reporting-framework add-ons.

## See also

Other project initialization:
[`createAIPromptFromTask()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createAIPromptFromTask.md),
[`createAIWorkflowPrompt()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createAIWorkflowPrompt.md),
[`createProjectConfiguration()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createProjectConfiguration.md),
[`exportWorkflowContext()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/exportWorkflowContext.md),
[`getQCpassedEnvironmentVariable()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/getQCpassedEnvironmentVariable.md),
[`initProject()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/initProject.md),
[`restoreProjectConfigurationRF()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/restoreProjectConfigurationRF.md),
[`setWorkflowOptions()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setWorkflowOptions.md),
[`snapshotProjectConfigurationRF()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/snapshotProjectConfigurationRF.md),
[`upgradeToReportingFramework()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/upgradeToReportingFramework.md),
[`validateWorkflowForAI()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/validateWorkflowForAI.md)

## Public fields

- `baseProjectconfiguration`:

  esqlabsR project configuration object Initialize

## Active bindings

- `modelFolder`:

  Path to the folder containing model files. Delegated to the base
  \`esqlabsR::ProjectConfiguration\`.

- `configurationsFolder`:

  Path to the folder containing configuration files. Delegated to the
  base \`esqlabsR::ProjectConfiguration\`.

- `modelParamsFile`:

  Path to the model parameters file. Delegated to the base
  \`esqlabsR::ProjectConfiguration\`.

- `individualsFile`:

  Path to the individuals file. Delegated to the base
  \`esqlabsR::ProjectConfiguration\`.

- `populationsFile`:

  Path to the populations file. Delegated to the base
  \`esqlabsR::ProjectConfiguration\`.

- `populationsFolder`:

  Path to the folder containing population files. Delegated to the base
  \`esqlabsR::ProjectConfiguration\`.

- `scenariosFile`:

  Path to the scenarios file. Delegated to the base
  \`esqlabsR::ProjectConfiguration\`.

- `applicationsFile`:

  Path to the applications file. Delegated to the base
  \`esqlabsR::ProjectConfiguration\`.

- `plotsFile`:

  Path to the plots configuration file. Delegated to the base
  \`esqlabsR::ProjectConfiguration\`.

- `parameterIdentificationFile`:

  Path to the parameter identification file. Delegated to the base
  \`esqlabsR::ProjectConfiguration\`.

- `dataFolder`:

  Path to the folder containing observed data files. Delegated to the
  base \`esqlabsR::ProjectConfiguration\`.

- `dataFile`:

  Path to the observed data file. Delegated to the base
  \`esqlabsR::ProjectConfiguration\`.

- `dataImporterConfigurationFile`:

  Path to the data importer configuration file. Delegated to the base
  \`esqlabsR::ProjectConfiguration\`.

- `outputFolder`:

  Path to the folder where outputs are written. Delegated to the base
  \`esqlabsR::ProjectConfiguration\`.

- `esqlabsRVersion`:

  Version of the \`esqlabsR\` package stored in the project
  configuration. Read-only; delegated to the base
  \`esqlabsR::ProjectConfiguration\`.

- `projectConfigurationDirPath`:

  Absolute path to the directory containing the project configuration
  file. Read-only; delegated to the base
  \`esqlabsR::ProjectConfiguration\`.

- `modified`:

  Logical flag indicating whether the project configuration has been
  modified since it was last saved. Read-only; delegated to the base
  \`esqlabsR::ProjectConfiguration\`.

- `projectConfigurationFilePath`:

  Path to the project configuration file.

- `addOns`:

  list with non default configurations

- `ospsuiteReportingFrameworkVersion`:

  Version of the reporting framework package stored in the project
  configuration. Read-only.

## Methods

### Public methods

- [`ProjectConfigurationRF$new()`](#method-ProjectConfigurationRF-initialize)

- [`ProjectConfigurationRF$print()`](#method-ProjectConfigurationRF-print)

- [`ProjectConfigurationRF$save()`](#method-ProjectConfigurationRF-save)

- [`ProjectConfigurationRF$addAddOnFileToConfiguration()`](#method-ProjectConfigurationRF-addAddOnFileToConfiguration)

- [`ProjectConfigurationRF$addAddOnFolderToConfiguration()`](#method-ProjectConfigurationRF-addAddOnFolderToConfiguration)

- [`ProjectConfigurationRF$getConfigSheetAsJson()`](#method-ProjectConfigurationRF-getConfigSheetAsJson)

- [`ProjectConfigurationRF$getConfigSheetsAsJson()`](#method-ProjectConfigurationRF-getConfigSheetsAsJson)

- [`ProjectConfigurationRF$copy()`](#method-ProjectConfigurationRF-copy)

------------------------------------------------------------------------

### `ProjectConfigurationRF$new()`

#### Usage

    ProjectConfigurationRF$new(
      projectConfigurationFilePath = character(),
      ignoreVersionCheck = FALSE
    )

#### Arguments

- `projectConfigurationFilePath`:

  A string representing the path to the project configuration file.

- `ignoreVersionCheck`:

  If \`TRUE\`, skip the version mismatch check when loading the
  configuration file. Defaults to \`FALSE\`. Print

------------------------------------------------------------------------

### `ProjectConfigurationRF$print()`

print prints a summary of the Project Configuration.

#### Usage

    ProjectConfigurationRF$print(className = TRUE)

#### Arguments

- `className`:

  Whether to print the name of the class at the beginning. default to
  TRUE.

------------------------------------------------------------------------

### `ProjectConfigurationRF$save()`

Export ProjectConfiguration object to ProjectConfiguration.xlsx

#### Usage

    ProjectConfigurationRF$save(path = NULL)

#### Arguments

- `path`:

  a string representing the path or file name where to save the file.

------------------------------------------------------------------------

### `ProjectConfigurationRF$addAddOnFileToConfiguration()`

Adds an add-on file to the project configuration.

#### Usage

    ProjectConfigurationRF$addAddOnFileToConfiguration(
      property,
      value,
      description,
      templatePath
    )

#### Arguments

- `property`:

  A string representing the name of the property to add.

- `value`:

  A string representing the basename of the file to add.

- `description`:

  A string providing a description of the property.

- `templatePath`:

  A string representing the path of the file to add.

------------------------------------------------------------------------

### `ProjectConfigurationRF$addAddOnFolderToConfiguration()`

Adds an add-on folder to the project configuration.

#### Usage

    ProjectConfigurationRF$addAddOnFolderToConfiguration(
      property,
      value,
      description
    )

#### Arguments

- `property`:

  A string representing the name of the property to add.

- `value`:

  A string representing the path of the value to add.

- `description`:

  A string providing a description of the property.

------------------------------------------------------------------------

### `ProjectConfigurationRF$getConfigSheetAsJson()`

Get a configuration sheet as a JSON-friendly list structure. Reads an
xlsx sheet and converts it to a list with column names and rows.

#### Usage

    ProjectConfigurationRF$getConfigSheetAsJson(
      filePath,
      sheetName,
      convertHeaders = TRUE,
      skipDescriptionRow = FALSE
    )

#### Arguments

- `filePath`:

  A string representing the path to the xlsx file.

- `sheetName`:

  A string representing the sheet name to read.

- `convertHeaders`:

  Logical flag to convert header names (default: TRUE).

- `skipDescriptionRow`:

  Logical flag to skip the description row (default: FALSE).

#### Returns

A named list with: - \`column_names\`: Character vector of column
names - \`rows\`: List of rows, where each row is a list of character
values

------------------------------------------------------------------------

### `ProjectConfigurationRF$getConfigSheetsAsJson()`

Get multiple configuration sheets as JSON-friendly list structures.
Reads multiple xlsx sheets and converts them to named list format.

#### Usage

    ProjectConfigurationRF$getConfigSheetsAsJson(
      filePath,
      sheetNames = NULL,
      convertHeaders = TRUE,
      skipDescriptionRow = FALSE
    )

#### Arguments

- `filePath`:

  A string representing the path to the xlsx file.

- `sheetNames`:

  A character vector of sheet names to read. If NULL, reads all sheets.

- `convertHeaders`:

  Logical flag to convert header names (default: TRUE).

- `skipDescriptionRow`:

  Logical flag to skip the description row (default: FALSE).

#### Returns

A named list where each element corresponds to a sheet and contains the
structure returned by \`getConfigSheetAsJson()\`.

------------------------------------------------------------------------

### `ProjectConfigurationRF$copy()`

Creates a copy of this object.

#### Usage

    ProjectConfigurationRF$copy(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
