# Workflow Setup and Execution

## Typical Package Workflow

This vignette describes how to set up a workflow file and details the
different sections of the workflow for the
[ospsuite.reportingframework](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/)
package. It serves as a guide for users to effectively utilize the
package for processing pharmacokinetic data.

### Accessing the Workflow Template

To access the default workflow template provided by the package, you can
use the following function:

``` r
openWorkflowTemplate()
```

This function opens the workflow template as a new document in your
RStudio environment, allowing you to customize it for your specific
needs. For detailed information on what each function in the workflow
does, please refer to the help functionality in R.

#### Access via RStudio Addins

You can also access the workflow template through an RStudio Addin. To
use the Addins, simply navigate to the Addins menu in RStudio and select
“Open Template Workflow.” This will execute the
[`openWorkflowTemplate()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/openWorkflowTemplate.md)
function and open the template in a new document.

#### Customizing the Workflow Template

By default, the workflow template is loaded from the package’s internal
templates. If you wish to customize this template, follow these steps:

1.  Create your own workflow template script and save it with the name
    `template_workflow.R`.
2.  Place this file in your designated template directory.
3.  Set the path to your custom template by using the following option:

``` r
options(OSPSuite.RF.PathForWorkflowTemplate = "myTemplateDirectory")
```

After setting this option, calling
[`openWorkflowTemplate()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/openWorkflowTemplate.md)
or using the Addins will open your customized template instead of the
default one. You can save this option in your “.Rprofile” file
(`file.path(Sys.getenv("HOME"), ".Rprofile"`)) to set it automatically.

## Project Initialization

The first step in the workflow is to initialize the project. This
includes:

- Loading necessary libraries.
- Setting the workflow options.
- Setting up the project directory and configuration files using the
  project structure of [esqlabsR](https://github.com/esqLABS/esqlabsR).
- Initializing the log file.

## Data Import

After initialization, the first step should always be data import. The
input format for data is CSV files, which can be imported using a
dictionary. The imported data types include individual and aggregated
time profile data as well as individual and aggregated PK parameter
data.

For more information, see **[Data Import by
Dictionary](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/articles/Data_import_by_dictionary.md)**.

## Setting Up and Running Simulations Including PK Parameter Calculation

Simulations are set up and run using the
[esqlabsR](https://github.com/esqLABS/esqlabsR) package. For more
information and detailed documentation, users can visit the [esqlabsR
Documentation](https://esqlabs.github.io/esqlabsR/).

The
[ospsuite.reportingframework](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/)
package provides additional functionality to:

- Export random and virtual twin populations (see the
  **[Population](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/articles/Population.md)**).
- Calculate PK Parameters (see the **[PK
  Parameters](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/articles/PK-Parameter)**).

## Plot and Report Generation

The core functionality of this package lies in plot and report creation.
It includes various plotting functions that generate visualizations
based on the analyses performed.

Each plot function is designed to create an R Markdown (`.Rmd`) file,
which contains the code and output for the respective plot. This allows
for easy documentation and sharing of results.

The package provides a collection of plot functions and interfaces to
adjust plots and utilize customized plot functions.

For more information, see the **[Plot and Report
Generation](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/articles/Plot_and_Report_Generation.md)**.

## Conclusion

This vignette has guided you through the setup and execution of
workflows using the OSPSuite Reporting Framework. For further
exploration, consult the linked vignettes and documentation.
