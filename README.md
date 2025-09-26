# ospsuite.reportingframework: Frame work to prepare and generate Reports

<!-- badges: start -->

  [![Latest release download count badge](https://img.shields.io/github/downloads/Open-Systems-Pharmacology/OSPSuite.ReportingFramework/latest/total?label=%E2%AD%B3%20Downloads%20latest%20release)](https://github.com/Open-Systems-Pharmacology/OSPSuite.ReportingFramework/releases/latest)
  [![Total downloads count badge](https://img.shields.io/github/downloads/Open-Systems-Pharmacology/OSPSuite.ReportingFrameworks/total?label=%E2%AD%B3%20Downloads%20total)](https://github.com/Open-Systems-Pharmacology/OSPSuite.ReportingFrameworks/releases)

  [![build](https://img.shields.io/github/actions/workflow/status/Open-Systems-Pharmacology/OSPSuite.ReportingFramework/main-workflow.yaml?logo=github&logoColor=white&label=Build)](https://github.com/Open-Systems-Pharmacology/OSPSuite.ReportingFramework/actions/workflows/main-workflow.yaml)
  [![codecov](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite.ReportingFramework/branch/main/graph/badge.svg)](https://codecov.io/gh/Open-Systems-Pharmacology/OSPSuite.ReportingFrameworks)
  [![Lint Test](https://img.shields.io/github/actions/workflow/status/Open-Systems-Pharmacology/OSPSuite.ReportingFramework/lint.yaml?logo=githubactions&logoColor=white&label=lint)](https://github.com/Open-Systems-Pharmacology/OSPSuite.ReportingFramework/actions/workflows/lint.yaml)

<!-- badges: end -->


The `{ospsuite.reportingframework}` , which is based on the `{esqlabsR package}`, 
is a robust tool designed to systematically define simulation scenarios, their inputs and outputs,
and standardize data import and matching processes. 
These functionalities allow for easy and automatable model execution and plot generation, 
ultimately facilitating the generation of customizable reports for 
OSP physiologically-based pharmacokinetic (PBPK) models. 
As a result, this framework significantly enhances reporting efficiency, 
enabling researchers to create clear, tailored reports that effectively communicate complex modeling results.

Pharmacology ecosystem:

-   [`{ospsuite}`](https://www.open-systems-pharmacology.org/OSPSuite-R/)
-   [`{ospsuite.plots}`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/)

This is the beta release of the  `{ospsuite.reportingframework}`. We welcome your feedback as we refine its features and performance.

## Installation

# OSPSuite.ReportingFramework

Framework to create R workflows which configures simulations, runs them plots results and creates a report

## Installation

The `{ospsuite.reportingframework}` package is compatible with version 2.10 and higher of R. Please follow the installation instructions below:

### From Github <img src=https://avatars.githubusercontent.com/github width=15px></img>

You can install the development version of `{ospsuite.reportingframework}` from [GitHub](https://github.com/) with:

```r
# install.packages("remotes")
remotes::install_github("Open-Systems-Pharmacology/OSPSuite.ReportingFramework")
```

### Required packages

`{ospsuite.reportingframework}` requires following packages to be installed:

- From the [Open Systems Pharmacology](https://github.com/Open-Systems-Pharmacology) ecosystem:

|Package|Version|Installation Instructions|
|-------|-------|------------|
|[`{rSharp}`](https://github.com/Open-Systems-Pharmacology/rSharp)|latest|Download and install package bundle [here &#128230;](https://github.com/Open-Systems-Pharmacology/rSharp/releases)|
|[`{ospsuite.plots}`](https://github.com/Open-Systems-Pharmacology/OSPSuite.Plots)|latest|Download and install package bundle [here &#128230;](https://github.com/Open-Systems-Pharmacology/OSPSuite.Plots/releases)|
|[`{ospsuite}`](https://github.com/Open-Systems-Pharmacology/OSPSuite-R)|$\geq$ 12.1|Download and install package bundle [here &#128230;](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/releases)<br>Instructions are available [here &#128214;](https://github.com/Open-Systems-Pharmacology/OSPSuite-R#installation)<br>&#9888; Visit [`{ospsuite}` Documentation](https://github.com/Open-Systems-Pharmacology/OSPSuite-R) to install its dependencies &#9888;|
|[`{ospsuite.utils}`](https://github.com/Open-Systems-Pharmacology/OSPSuite.RUtils)|$\geq$ 1.5|Download and install package bundle [here &#128230;](https://github.com/Open-Systems-Pharmacology/OSPSuite.RUtils/releases)|


- From the [esqLABS](https://github.com/esqLABS) ecosystem:

|Package|Version|Installation Instructions|
|-------|-------|------------|
|[`{esqlabsR}`](https://github.com/Open-Systems-Pharmacology/esqlabsR)|latest|Download and install package bundle [here &#128230;](https://github.com/esqLABS/esqlabsR/releases)|

Once the bundles downloaded, you can install the packages by using the code below:

```r
# bundlePath <- path/to/osp/bundles
install.packages(file.path(bundlePath,"rSharp.zip"), repos = NULL)
install.packages(file.path(bundlePath,"ospsuite.plots.zip"), repos = NULL)
install.packages(file.path(bundlePath,"ospsuite.zip"), repos = NULL)
install.packages(file.path(bundlePath,"ospsuite.utils.zip"), repos = NULL)
install.packages(file.path(bundlePath,"esqlabsR.zip"), repos = NULL)
```

- Required packages from [CRAN](https://cran.r-project.org/):
  > Some of these packages may already be installed as dependencies of the Open Systems Pharmacology ecosystem
  - [`{data.table}`](https://cran.r-project.org/web/packages/data.table)
  - [`{checkmate}`](https://cran.r-project.org/web/packages/checkmate)
  - [`{knitr}`](https://cran.r-project.org/web/packages/knitr)
  - [`{rmarkdown}`](https://cran.r-project.org/web/packages/rmarkdown)
  - [`{ggplot2}`](https://cran.r-project.org/web/packages/ggplot2) 
  - [`{tidyr}`](https://cran.r-project.org/web/packages/tidyr)
  - [`{dplyr}`](https://cran.r-project.org/web/packages/dplyr)
  - [`{openxlsx}`](https://cran.r-project.org/web/packages/openxlsx)
  - [`{jsonlite}`](https://cran.r-project.org/web/packages/jsonlite)

- Optional packages from [CRAN](https://cran.r-project.org/)
  - [`{rstudioapi}`](https://cran.r-project.org/web/packages/rstudioapi)
  - [`{cowplot}`](https://cran.r-project.org/web/packages/cowplot)
  
To install these packages, use the code below:

```r
# Required packages
install.packages("data.table")
install.packages("checkmate")
install.packages("knitr")
install.packages("rmarkdown")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")
install.packages("openxlsx")
install.packages("jsonlite")

# Optional packages
install.packages("rstudioapi")
install.packages("cowplot")
```

## MS-Word reports

It is possible to convert markdown reports to MS-Word (`.docx` format) from the `{ospsuite.reportingframework}` package.
This conversion requires the installation of an additional software: [Pandoc](https://pandoc.org/).

### [OPTIONAL] Pandoc Installation

Install **Pandoc** (required for generation of reports in MS-Word format) by downloading one of the following files:

- [Pandoc Installer (Windows)](https://github.com/jgm/pandoc/releases/download/3.1.2/pandoc-3.1.2-windows-x86_64.msi)

- [Pandoc Installer (Linux)](https://github.com/jgm/pandoc/releases/download/3.1.2/pandoc-3.1.2-linux-amd64.tar.gz)

## Code of conduct

Everyone interacting in the Open Systems Pharmacology community (codebases,
issue trackers, chat rooms, mailing lists etc.) is expected to follow the Open
Systems Pharmacology [code of
conduct](https://dev.open-systems-pharmacology.org/r-development-resources/coding_standards_r).

## Contribution

We encourage contribution to the Open Systems Pharmacology community.
Before getting started please read the [contribution
guidelines](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CONTRIBUTING.md).
If you are contributing code, please be familiar with the [coding
standards](https://github.com/Open-Systems-Pharmacology/Suite/blob/master/CODING_STANDARDS_R.md).

## License

OSPSuite.ReportingFrameworks Library is released under the [GPLv2 License](LICENSE).

All trademarks within this document belong to their legitimate owners.
