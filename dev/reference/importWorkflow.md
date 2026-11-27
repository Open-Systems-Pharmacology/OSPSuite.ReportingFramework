# Import Workflow

This function imports a workflow from an electronic package (ePackage)
into a project directory. It initializes the project configuration,
retrieves relevant project files, and synchronizes scenarios with plots.
The function ensures that the necessary directories exist and that the
configuration is set up correctly for further processing. Files listed
in the workflow input manifest are restored, including workflow data
files (for example \`wwfIdentifier_data_numericvalues_csv.txt\`) into
the workflow-specific data folder used by the exported script.

## Usage

``` r
importWorkflow(projectDirectory, wfIdentifier, ePackageFolder)
```

## Arguments

- projectDirectory:

  A character string representing the path to the project directory
  where the workflow will be imported.

- wfIdentifier:

  An integer identifier for the workflow being imported. This identifier
  is used to distinguish between different workflows.

- ePackageFolder:

  A character string representing the path to the electronic package
  folder that contains the workflow files and configurations to be
  imported.

## Value

An object containing the updated project configuration after the
workflow has been imported.

## See also

Other electronic package:
[`exportSimulationWorkflowToEPackage()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/exportSimulationWorkflowToEPackage.md),
[`exportTLFWorkflowToEPackage()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/exportTLFWorkflowToEPackage.md)
