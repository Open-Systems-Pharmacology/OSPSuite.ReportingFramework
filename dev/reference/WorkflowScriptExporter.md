# WorkflowScriptExporter

Manages the export of an ePackage workflow.

## Super class

[`ospsuite.utils::Printable`](https://www.open-systems-pharmacology.org/OSPSuite.RUtils/reference/Printable.html)
-\> `WorkflowScriptExporter`

## Active bindings

- `wfIdentifier`:

  Identifier of the workflow.

- `scenarioNames`:

  Relevant scenarios to export.

- `workflowRmd`:

  Input markdown file for TLF workflow.

- `fileNameReplacements`:

  List of file name replacements.

- `inputFiles`:

  Table with all input files.

- `changedInputFiles`:

  Table with all changed input files.

- `configurationSheets`:

  A list containing configuration sheets .

- `codeChunks`:

  \# A list with code read in from workflowRmd .

## Methods

### Public methods

- [`WorkflowScriptExporter$new()`](#method-WorkflowScriptExporter-new)

- [`WorkflowScriptExporter$extractCodeChunks()`](#method-WorkflowScriptExporter-extractCodeChunks)

- [`WorkflowScriptExporter$evalCodeChunks()`](#method-WorkflowScriptExporter-evalCodeChunks)

- [`WorkflowScriptExporter$addEPackageConfigurationForPlotting()`](#method-WorkflowScriptExporter-addEPackageConfigurationForPlotting)

- [`WorkflowScriptExporter$exportWorkflowText()`](#method-WorkflowScriptExporter-exportWorkflowText)

- [`WorkflowScriptExporter$addEPackageInputfilesForScenarioNames()`](#method-WorkflowScriptExporter-addEPackageInputfilesForScenarioNames)

- [`WorkflowScriptExporter$addEPackageConfigurationForScenarioNames()`](#method-WorkflowScriptExporter-addEPackageConfigurationForScenarioNames)

- [`WorkflowScriptExporter$exportInputFiles()`](#method-WorkflowScriptExporter-exportInputFiles)

- [`WorkflowScriptExporter$exportConfigSheets()`](#method-WorkflowScriptExporter-exportConfigSheets)

- [`WorkflowScriptExporter$clone()`](#method-WorkflowScriptExporter-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new instance of the class.

#### Usage

    WorkflowScriptExporter$new(
      projectConfiguration,
      wfIdentifier,
      scenarioNames = NULL,
      workflowRmd = NULL,
      fileNameReplacements = c()
    )

#### Arguments

- `projectConfiguration`:

  An object of class projectConfiguration containing information like
  paths.

- `wfIdentifier`:

  A unique identifier for the workflow.

- `scenarioNames`:

  A vector of scenario names to be included in the workflow (optional).

- `workflowRmd`:

  Path to the input markdown file for the workflow (optional).

- `fileNameReplacements`:

  A vector of file name replacements (optional).

------------------------------------------------------------------------

### Method `extractCodeChunks()`

Extract code chunks from the specified R Markdown file. This method
reads the R Markdown file provided in the \`workflowRmd\` field and
extracts code chunks for further processing. It also ensures that
necessary code chunks are present and replaces any placeholders with the
corresponding values from the template.

#### Usage

    WorkflowScriptExporter$extractCodeChunks()

#### Details

The method will raise an error if required code chunks are missing from
the R Markdown file. It also handles the temporary creation of a script
file to read the code chunks. A timeout mechanism is implemented to
ensure that the script file is available for reading.

#### Returns

Invisible NULL. The function does not return any value but populates the
\`codeChunks\` field with the extracted code chunks.

------------------------------------------------------------------------

### Method `evalCodeChunks()`

This function evaluates the code chunks extracted from the R Markdown
file associated with the workflow. It iterates through the code chunks,
validating and executing them to set up the necessary variables for the
workflow export process. The evaluation includes handling specific
chunks related to custom functions, scenario names, and data import.

#### Usage

    WorkflowScriptExporter$evalCodeChunks()

#### Returns

Invisible NULL. The function does not return any value but performs
evaluations that update the internal state of the
\`WorkflowScriptExporter\` object. If successful, the function will
complete without errors; otherwise, it will raise an informative error
if any evaluation fails.

------------------------------------------------------------------------

### Method `addEPackageConfigurationForPlotting()`

Add referenced plotting sheets to Configuration

#### Usage

    WorkflowScriptExporter$addEPackageConfigurationForPlotting(
      projectConfiguration
    )

#### Arguments

- `projectConfiguration`:

  An object of class projectConfiguration containing information like
  paths.

#### Returns

Invisible NULL.

------------------------------------------------------------------------

### Method `exportWorkflowText()`

Export workflow text to a file.

#### Usage

    WorkflowScriptExporter$exportWorkflowText(projectConfiguration)

#### Arguments

- `projectConfiguration`:

  An object of class projectConfiguration containing information like
  paths.

#### Returns

Invisible NULL.

------------------------------------------------------------------------

### Method `addEPackageInputfilesForScenarioNames()`

Retrieve input files for the ePackage.

#### Usage

    WorkflowScriptExporter$addEPackageInputfilesForScenarioNames(
      projectConfiguration
    )

#### Arguments

- `projectConfiguration`:

  An object of class projectConfiguration containing information like
  paths and filenames.

#### Returns

Invisible NULL.

------------------------------------------------------------------------

### Method `addEPackageConfigurationForScenarioNames()`

Get E-Package Configuration for Scenario Names. This function retrieves
the configuration data for specified scenario names from various Excel
sheets related to a project configuration.

#### Usage

    WorkflowScriptExporter$addEPackageConfigurationForScenarioNames(
      projectConfiguration
    )

#### Arguments

- `projectConfiguration`:

  A list containing project configuration details, including file paths.

#### Returns

Invisible NULL.

------------------------------------------------------------------------

### Method `exportInputFiles()`

Export input files to the electronic package folder.

#### Usage

    WorkflowScriptExporter$exportInputFiles()

#### Returns

Invisible NULL.

------------------------------------------------------------------------

### Method `exportConfigSheets()`

Export configuration sheets to the electronic package folder.

#### Usage

    WorkflowScriptExporter$exportConfigSheets()

#### Returns

Invisible NULL.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    WorkflowScriptExporter$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
