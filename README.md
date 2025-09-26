# OSPSuite.ReportingFramework

[![](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://open-systems-pharmacology.github.io/OSPSuite.ReportingFramework/)
[![codecov](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite.ReportingFramework/branch/main/graph/badge.svg)](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite.ReportingFramework)

A powerful R package designed to streamline the process of data analysis, visualization, and reporting in pharmacokinetic and clinical research contexts. This framework provides comprehensive tools for importing data, running simulations, calculating pharmacokinetic parameters, and generating insightful visualizations and reports.

## Key Features

- **Data Import**: Import observed data and pharmacokinetic parameters with ease
- **Simulation Management**: Set up and run simulations, including calculation of PK parameters  
- **Plotting Capabilities**: Generate a variety of plots to visualize data and results effectively
- **Report Generation**: Compile results into well-structured reports for regulatory submissions
- **Project Structure**: Based on `{esqlabsR}` with well-defined project structure using Excel files

## Installation

### System Requirements

- R (>= 2.10)
- Pandoc (required for generating Word reports)

### Prerequisites

This package depends on several other packages from the OSPSuite ecosystem:

- `{esqlabsR}` - Base package for modeling and simulation workflows
- `{ospsuite}` - Core OSPSuite functionality 
- `{ospsuite.plots}` - Plotting utilities
- `{ospsuite.utils}` - Utility functions

### Installing from GitHub

You can install the development version from GitHub using:

```r
# Install remotes if you haven't already
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install OSPSuite.ReportingFramework
remotes::install_github("Open-Systems-Pharmacology/OSPSuite.ReportingFramework")
```

### Installing Dependencies

The package has several required dependencies that will be installed automatically. However, you may want to install suggested packages for full functionality:

```r
# Install suggested packages for enhanced functionality
install.packages(c("rstudioapi", "officer", "cowplot", "spelling", "vdiffr", "withr"))
```

### Verifying Installation

After installation, you can verify that everything is working correctly:

```r
library(ospsuite.reportingframework)

# Check if the package loads correctly
packageVersion("ospsuite.reportingframework")
```

## Quick Start

### Using the Workflow Template

The package provides a template workflow to get you started quickly:

```r
library(ospsuite.reportingframework)

# Open the workflow template
openWorkflowTemplate()
```

This will open a template script that demonstrates the typical workflow including:

1. Project initialization and setup
2. Data import
3. Population export 
4. Simulation runs
5. Plot generation
6. Report creation

### Basic Usage Example

```r
library(ospsuite.reportingframework)

# Set graphic defaults
ospsuite.plots::setDefaults()
theme_update(legend.position = 'top')

# Initialize project structure
initProject()

# Create project configuration
projectConfiguration <- createProjectConfiguration(
  path = file.path("ProjectConfiguration.xlsx")
)

# Read observed data
dataObserved <- readObservedDataByDictionary(
  projectConfiguration = projectConfiguration,
  dataClassType = 'timeprofile'
)
```

## Documentation

Comprehensive documentation and tutorials are available through vignettes:

- **[Introduction](https://open-systems-pharmacology.github.io/OSPSuite.ReportingFramework/articles/OSPSuite_ReportingFramework.html)**: Overview of the package and its capabilities
- **[Workflow Setup](https://open-systems-pharmacology.github.io/OSPSuite.ReportingFramework/articles/Workflow.html)**: Setting up and executing workflows
- **[Data Import](https://open-systems-pharmacology.github.io/OSPSuite.ReportingFramework/articles/data_import_by_dictionary.html)**: Importing observed data and PK parameters
- **[Population](https://open-systems-pharmacology.github.io/OSPSuite.ReportingFramework/articles/Population.html)**: Working with populations
- **[PK Parameters](https://open-systems-pharmacology.github.io/OSPSuite.ReportingFramework/articles/PK-Parameter.html)**: Calculating and visualizing PK parameters
- **[Plot and Report Generation](https://open-systems-pharmacology.github.io/OSPSuite.ReportingFramework/articles/Plot_and_Report_Generation.html)**: Creating plots and reports
- **[Tutorial: Time Profile Plotting](https://open-systems-pharmacology.github.io/OSPSuite.ReportingFramework/articles/tutorial-timeprofiles.html)**: Step-by-step plotting guide
- **[Electronic Package](https://open-systems-pharmacology.github.io/OSPSuite.ReportingFramework/articles/electronic-Package.html)**: Creating regulatory submission packages

You can also access vignettes from within R:

```r
# List all available vignettes
vignette(package = "ospsuite.reportingframework")

# View a specific vignette
vignette("OSPSuite_ReportingFramework", package = "ospsuite.reportingframework")
```

## Getting Help

- Browse the [documentation website](https://open-systems-pharmacology.github.io/OSPSuite.ReportingFramework/)
- Check the [GitHub Issues](https://github.com/Open-Systems-Pharmacology/OSPSuite.ReportingFramework/issues) for known problems
- Use `?function_name` in R to get help on specific functions

## Contributing

This package is part of the Open Systems Pharmacology Suite. For information about contributing, please visit the main [OSPSuite website](http://www.open-systems-pharmacology.org/).

## License

The package is released under the GPL-2 license. See [LICENSE](LICENSE) file for details.
