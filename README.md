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
|[`{esqlabsR}`](https://github.com/Open-Systems-Pharmacology/esqlabsR)|latest|Download and install package bundle [here &#128230;](https://github.com/Open-Systems-Pharmacology/esqlabsR/releases)|
|[`{ospsuite.plots}`](https://github.com/Open-Systems-Pharmacology/OSPSuite.Plots)|latest|Download and install package bundle [here &#128230;](https://github.com/Open-Systems-Pharmacology/OSPSuite.Plots/releases)|
|[`{ospsuite}`](https://github.com/Open-Systems-Pharmacology/OSPSuite-R)|$\geq$ 12.1|Download and install package bundle [here &#128230;](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/releases)<br>Instructions are available [here &#128214;](https://github.com/Open-Systems-Pharmacology/OSPSuite-R#installation)<br>&#9888; Visit [`{ospsuite}` Documentation](https://github.com/Open-Systems-Pharmacology/OSPSuite-R) to install its dependencies &#9888;|
|[`{ospsuite.utils}`](https://github.com/Open-Systems-Pharmacology/OSPSuite.RUtils)|$\geq$ 1.5|Download and install package bundle [here &#128230;](https://github.com/Open-Systems-Pharmacology/OSPSuite.RUtils/releases)|

Once the bundles downloaded, you can install the packages by using the code below:

```r
# bundlePath <- path/to/osp/bundles
install.packages(file.path(bundlePath,"esqlabsR.zip"), repos = NULL)
install.packages(file.path(bundlePath,"ospsuite.plots.zip"), repos = NULL)
install.packages(file.path(bundlePath,"ospsuite.zip"), repos = NULL)
install.packages(file.path(bundlePath,"ospsuite.utils.zip"), repos = NULL)
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
  - [`{officer}`](https://cran.r-project.org/web/packages/officer)
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
install.packages("officer")
install.packages("cowplot")
```

## MS-Word reports

It is possible to convert markdown reports to MS-Word (`.docx` format) from the `{ospsuite.reportingframework}` package.
This conversion requires the installation of an additional software: [Pandoc](https://pandoc.org/).

### [OPTIONAL] Pandoc Installation

Install **Pandoc** (required for generation of reports in MS-Word format) by downloading one of the following files:

- [Pandoc Installer (Windows)](https://github.com/jgm/pandoc/releases/download/3.1.2/pandoc-3.1.2-windows-x86_64.msi)

- [Pandoc Installer (Linux)](https://github.com/jgm/pandoc/releases/download/3.1.2/pandoc-3.1.2-linux-amd64.tar.gz)
