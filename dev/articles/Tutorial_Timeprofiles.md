# Tutorial: Time Profile Plotting

    #> INFO [2026-11-27 13:20:57] Start simulation of one_individual
    #> INFO [2026-11-27 13:21:00] Start simulation of virtual_twin_population
    #> INFO [2026-11-27 13:21:04] Start simulation of random_population_po
    #> INFO [2026-11-27 13:21:14] Start simulation of random_population_iv
    #> INFO [2026-11-27 13:21:25] Add virtual twin population configuration to the Population configuration file:
    #> INFO [2026-11-27 13:21:25] populationName     N
    #>             <char> <int>
    #> 1: 1234_iv_1234_po     6
    #> INFO [2026-11-27 13:21:25] Observed Data:
    #> INFO [2026-11-27 13:21:25] group No of data points No of individuals No of outputs
    #>     <char>             <int>             <int>         <int>
    #> 1: 1234_iv               198                 6             3
    #> 2: 1234_po               198                 6             3

## Introduction

This tutorial shows the end-to-end time-profile workflow for
`ospsuite.reportingframework`, from project setup to plot/report
generation.

Vignettes used in this tutorial:

- [`vignette("Workflow", package = "ospsuite.reportingframework")`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/articles/Workflow.md)
- [`vignette("Plot_and_Report_Generation", package = "ospsuite.reportingframework")`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/articles/Plot_and_Report_Generation.md)
- `vignette("Data_import_by_dictionary", package = "ospsuite.reportingframework")`
- [`vignette("Population", package = "ospsuite.reportingframework")`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/articles/Population.md)

Function help pages used throughout:

- [`?initProject`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/initProject.md),
  [`?setWorkflowOptions`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setWorkflowOptions.md),
  [`?createScenariosWrapped`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createScenariosWrapped.md)
- [`?readObservedDataByDictionary`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/readObservedDataByDictionary.md),
  [`?runPlot`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/runPlot.md),
  [`?mergeQmds`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mergeQmds.md),
  [`?renderWord`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/renderWord.md)

## Package Materials

Tutorial inputs and final example outputs are available in package
extdata:

- Data files used for the tutorial: `timeprofiles_study1234_iv.csv`,
  `timeprofiles_study1234_po.csv`
- Final configuration tables: `DataImportMap.xlsx`, `Individuals.xlsx`,
  `Populations.xlsx`, `Scenarios.xlsx`, `Reports.xlsx`
- The final workflow: `workflow.R`
- The final Word document containing all plots generated during this
  tutorial: `TutorialPlots.docx`

You can access the folder containing this material using the command
below:

``` r

print(system.file(
  "extdata", "Tutorial",
  package = "ospsuite.reportingframework",
  mustWork = TRUE
))
```

The models used in the tutorial (“iv_1_mg_5_min.pkml” and
“po_3_mg_solution.pkml”) are included in the Tutorial materials.

``` r

print(system.file(
  "extdata", "Tutorial",
  package = "ospsuite.reportingframework",
  mustWork = TRUE
))
#> [1] "/home/runner/work/_temp/Library/ospsuite.reportingframework/extdata/Tutorial"
```

## Table of Contents

1.  [Setting Up the Project and Basic
    Simulations](#id_1-setting-up-the-project-and-basic-simulations)  
    Set up project structure and run a first scenario.

2.  [Adding virtual twin
    populations](#id_2-adding-virtual-twin-populations)  
    Add and simulate virtual twin populations.

3.  [Adding Random Populations and Use of Aggregated
    Data](#id_3-adding-random-populations-and-use-of-aggregated-data)  
    Aggregate observed data and compare to random populations.

4.  [Adding Reference Scenarios](#id_4-adding-reference-scenarios)  
    Add reference scenarios for side-by-side comparison.

5.  [Adding plots with different time
    ranges](#id_5-adding-plots-with-different-time-ranges)  
    Configure plots for different time windows.

6.  [Report Creation](#id_6-report-creation)  
    Merge and render the final report.

## 1) Setting Up the Project and Basic Simulations

### Step 1: Save Template Workflow

- Create/select `<rootDirectory>`.
- Create/select `<rootDirectory>/Scripts/<analysisSubfolderName>`.
- In RStudio Addins, run “Open Template Workflow” and save as
  `workflow.R` in that folder.

For full setup context, see
[`vignette("Workflow", package = "ospsuite.reportingframework")`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/articles/Workflow.md).

### Step 2: Initialize Project Structure

Execute the initialization block from your workflow. It sets plotting
defaults, initializes the project structure, loads the configuration,
and configures logging.

``` r

# Workflow template for ospsuite.reportingframework.
#
# Quick documentation entry points:
# - Workflow vignette: vignette("Workflow", package = "ospsuite.reportingframework")
# - Plot/report vignette: vignette("Plot_and_Report_Generation", package = "ospsuite.reportingframework")
# - Sensitivity vignette: vignette("Sensitivity_Analysis", package = "ospsuite.reportingframework")
# - Function help: ?initProject, ?setWorkflowOptions, ?runPlot, ?mergeQmds

# Initialization -------------------------------------------------------------
# Load package and set plotting/report defaults.
library(ospsuite.reportingframework)

# Set graphic defaults
# (see vignette(package = 'ospsuite.plots', topic = 'ospsuite_plots'))
ggplot2::theme_set(theme_osp())
options(knitr.kable.NA = "")

runPlotTheme <- ggplot2::theme(
  legend.position = "top",
  legend.title = ggplot2::element_blank()
)

# Run mode: see ?setWorkflowOptions.
# If isValidRun is NULL (default), QCpassed environment variable is used.
setWorkflowOptions()

# Setup project structure ----------------------------------------------------
# Initialize project structure and configuration snapshot.
# See ?initProject for layout options and arguments.
analysisSubfolderName <- "ReportingFramework"
projectDirectory <- file.path("..", "..")
configurationDirectory <- "."
initProject(
  projectDirectory = projectDirectory,
  configurationDirectory = configurationDirectory,
  analysisSubfolderName = analysisSubfolderName,
  overwrite = TRUE,
  keepEsqlabsRStructure = FALSE
)

# Load project configuration from ProjectConfiguration.xlsx.
projectConfiguration <- ospsuite.reportingframework::createProjectConfiguration(
  path = file.path(configurationDirectory, "ProjectConfiguration.xlsx")
)
# For older ProjectConfiguration.xlsx files without a stored framework version,
# use ignoreVersionCheck = TRUE (see ?createProjectConfiguration).

setLogPath(
  logFolder = file.path(projectConfiguration$outputFolder, "logs"),
  logFileName = "workflow.log"
)
```

For logging helpers and diagnostics, see
**[Workflow](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/articles/Workflow.md)**.

When loading an existing project configuration that was created before
version tracking was introduced, pass `ignoreVersionCheck = TRUE` to
[`createProjectConfiguration()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createProjectConfiguration.md)
or update the file once with `projectConfiguration$save()`.

### Configuration in Git: Save JSON Snapshot

Keep `*.xlsx` files local and commit `ProjectConfiguration.json`. After
changing configuration tables, refresh the JSON snapshot:

``` r

projectConfiguration <- ospsuite.reportingframework::createProjectConfiguration(
  path = file.path(configurationDirectory, "ProjectConfiguration.xlsx")
)

ospsuite.reportingframework::snapshotProjectConfigurationRF(
  projectConfiguration,
  outputDir = configurationDirectory
)
```

Commit `ProjectConfiguration.json` to Git.

To recreate the configuration from JSON:

``` r

projectConfiguration <- ospsuite.reportingframework::restoreProjectConfigurationRF(
  snapshotFilePath = file.path(configurationDirectory, "ProjectConfiguration.json"),
  outputDir = configurationDirectory
)
```

### Step 3: Copy the Sample Data to Your Project Structure

You have now the complete project structure with all configuration
files. Copy the model files to the folder `<rootDirectory>/Models` and
the data files to `<rootDirectory>/Data`.

### Step 4: Read Observed Data

(See **[Data Import by
Dictionary](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/articles/DataImport_by_Dictionary.md)**)

Open the `<configurationDirectory>/DataImportMap.xlsx`.

- List in the sheet `DataFiles` your available data. The paths should be
  relative to the configuration table which is in
  `<rootDirectory>/Scripts/<analysisSubfolderName>`, the data are saved
  in `<rootDirectory>/Data`, so the correct path is `../../Data`.
- Each file gets a file identifier, which can be used as a filter in the
  `readObservedDataByDictionary` function.
- As dictionary, select `tpDictionary`.
- We do not need a filter, so ensure the column `DataFilter` is empty.
- The data is of class `tp Individual` (individual time profile data).

| FileIdentifier | DataFile | Dictionary | DataFilter | DataClass |
|:---|:---|:---|---:|:---|
| tp_iv | ../../Data/timeprofiles_study1234_iv.csv | tpDictionary |  | tp Individual |
| tp_po | ../../Data/timeprofiles_study1234_po.csv | tpDictionary |  | tp Individual |

- Match existing columns of the datasets when available.
- For `individualId` and `group`, concatenate existing columns by
  setting `ValueExpr` to the corresponding expression and
  `ConditionExpr` to `TRUE` (or leave empty).
- For the ‘time profile’ columns, use the source columns `Time`, `DV`,
  `DVUNIT`, and `LLOQ`.
- For `biometrics` columns, use the source columns `Age`, `WGHT0`,
  `HGHT0`, and `SEX`.
- Set `population` to `European_ICRP_2002` by using `ConditionExpr` and
  `ValueExpr` rules.
- Metadata `route` is available; `dose` and `outputPathId` are set by
  dedicated `ConditionExpr`/`ValueExpr` rules, each occupying a separate
  row in the dictionary.

| TargetColumn | Type | ValueExpr | SourceUnit | ConditionExpr | Description |
|:---|:---|:---|:---|---:|:---|
| studyId | identifier | STUD |  |  | character, study ID |
| subjectId | identifier | SID |  |  | character, subject ID |
| individualId | identifier | paste0(“I”,STUD,SID) |  |  | character, unique over all studies, ignored by aggregated Data |
| group | identifier | paste(STUD,route,sep = ’\_’) |  |  | Must be unique over studies and dataclasses |
| outputPathId | identifier | outputPathId |  |  | character, output ID |
| xValues | timeprofile | TIME | h |  | Time (0 = start of simulation in PK-Sim/Mobi) |
| yValues | timeprofile | DV |  |  | Units is coded in column “dvUnit” |
| yUnit | timeprofile | DVUNIT |  |  | character, dv Unit must be valid PK-Sim unit |
| lloq | timeprofile | LLOQ |  |  | for values below lloq set dv to lloq/2, if not available set to NA |
| age | biometrics | AGE | year(s) |  | character, age BI |
| weight | biometrics | WGHT0 | kg |  | character, weight BI |
| height | biometrics | HGHT0 | cm |  | character, height BI |
| gender | biometrics | SEX |  |  | Use characters Male Female (case insensitive) or numeric coding 1=male 2= female |
| population | biometrics | “European_ICRP_2002” |  |  | character, PK Sim population name (get available list by calling ospsuite::HumanPopulation) |
| species | biometrics | “Human” |  |  | character, PK-Sim Species name (ospsuite::Species) |
| route | metadata | route |  |  | meta data used for PK-Sim import, if not available, delete row or set all values to NA |

Now you can execute the next workflow line:

``` r

# Read data as data.table
dataObserved <- readObservedDataByDictionary(projectConfiguration = projectConfiguration)
```

This will return a data.table with the observed data and add data to the
configuration tables.

- The biometrics of the individuals in the observed data are added to
  the `Individuals.xlsx`. Information not in the observed data, like
  ontogeny, must be added manually.

| IndividualId | Species | Population | Gender | Weight \[kg\] | Height \[cm\] | Age \[year(s)\] | Protein Ontogenies |
|:---|:---|:---|:---|---:|---:|---:|:---|
| I123413 | Human | European_ICRP_2002 | MALE | 58.8 | 161 | 21 | CYP3A4:CYP3A4, UGT1A4:UGT1A4 |
| I123430 | Human | European_ICRP_2002 | MALE | 64.0 | 164 | 33 | CYP3A4:CYP3A4, UGT1A4:UGT1A4 |
| I123441 | Human | European_ICRP_2002 | MALE | 61.8 | 158 | 36 | CYP3A4:CYP3A4, UGT1A4:UGT1A4 |
| I123450 | Human | European_ICRP_2002 | FEMALE | 58.8 | 158 | 20 | CYP3A4:CYP3A4, UGT1A4:UGT1A4 |
| I123466 | Human | European_ICRP_2002 | FEMALE | 64.3 | 176 | 22 | CYP3A4:CYP3A4, UGT1A4:UGT1A4 |
| I123478 | Human | European_ICRP_2002 | FEMALE | 64.3 | 160 | 33 | CYP3A4:CYP3A4, UGT1A4:UGT1A4 |

- The identifiers for the data groups and outputs are added to the
  `Reports.xlsx`, sheets `DataGroups` and `Outputs`. These tables
  configure properties for output plots such as `DisplayName` and
  `DisplayUnit`. Please add the `OutputPaths` for the `OutputPathsIds`.

| OutputPathId | OutputPath | DisplayName | DisplayUnit | Color | Fill |
|:---|:---|:---|:---|---:|---:|
| Plasma | Organism\|PeripheralVenousBlood\|DrugX\|Plasma (Peripheral Venous Blood) | drugX plasma concentration | µg/L |  |  |
| CYP3A4total | Organism\|DrugX-CYP3A4-Optimized Metabolite\|Total fraction of dose-DrugX | drugX metabolized by CYP3A4 |  |  |  |
| CYP3A4Liver | Organism\|Liver\|Periportal\|Intracellular\|DrugX-CYP3A4-Optimized Metabolite\|Fraction of dose-DrugX | drugX metabolized by CYP3A4 in liver |  |  |  |

### Step 5: Set Up and Run Scenario

We will skip the section on exporting populations and start with a
simple scenario to simulate one individual, defined by biometrics, with
an iv application.

Open the `Scenarios.xlsx` and edit the sheet `Scenarios`. Fill in the
columns:

- `Scenario_name`: identifier of the scenario.
- `IndividualId`: corresponds to the individual identifier in the
  configuration table `Individuals.xlsx`.
- `ModelFile`: this file must exist in the folder `Models`

| Scenario_name | IndividualId | PopulationId | ReadPopulationFromCSV | ModelFile | ModelParameterSheets | ApplicationProtocol | SimulationTime | SimulationTimeUnit | SteadyState | SteadyStateTime | SteadyStateTimeUnit | OutputPathsIds |
|:---|:---|:---|---:|:---|---:|---:|---:|---:|---:|---:|---:|:---|
| one_individual | I123413 |  |  | iv_1_mg_5_min.pkml |  |  |  |  |  |  |  |  |

Now we can set up the `scenarioList` and run the simulation:

``` r

scenarioList <-
  createScenariosWrapped(
    projectConfiguration = projectConfiguration,
    scenarioNames = NULL
  )

scenarioResults <- runAndSaveScenarios(
  projectConfiguration = projectConfiguration,
  scenarioList = scenarioList,
  simulationRunOptions = ospsuite::SimulationRunOptions$new(
    showProgress = TRUE
  )
)
```

The results are saved in
`<rootDirectory>/Outputs/<analysisSubfolderName>/SimulationResults`.

### Step 6: Configure and Generate Plots

For the plot configuration, call a helper function that adds all defined
scenarios with default settings to the plot configuration table. This
helper function changes the input configuration of a workflow and is not
intended to be part of a final workflow.

``` r

addDefaultConfigForTimeProfilePlots(
  projectConfiguration = projectConfiguration,
  sheetName = "TimeProfiles",
  overwrite = FALSE
)
```

Then we open the `Reports.xlsx` sheet `TimeProfiles`. It starts with a
header line and one line for the previously defined scenario.

- In the `Section` column, add a level-2 header row (e.g.,
  `## 1) Basic Individual Simulation`). This header will appear as a
  section title in the exported Word report.

- Edit the columns:

  - `PlotName`: The plot name should be meaningful as it is also used
    for the exported figure names.
  - `PlotCaptionAddon`: Add-on for the caption used for all scenarios in
    this plot.
  - `DataGroupIds`: Data group identifier corresponding to this
    scenario, must be available in `dataObserved$group`.
  - `IndividualIds`: Identifier of the individual, should correspond to
    the `IndividualId` of the scenario and the `individualId` in the
    observed data.
  - `TimeRange_firstApplication` and `TimeRange_lastApplication`: Set to
    empty, as we have a single dose.

This line will create a time-profile plot that compares the simulated
data to the observed data.

We also want to add goodness of fit plots. Because the two outputs have
different physical dimensions (e.g., plasma concentration and AUC),
goodness of fit plots must be restricted to a single dimension at a time
to keep the axes consistent. We do this in a separate row:

- Copy the plot line and edit the columns:
  - `PlotName`: each plot must have a unique Plot identifier.
  - `OutputPathIds`: ensure we have only outputs of one dimension
    selected.
  - Deselect `Plot_TimeProfiles`.
  - Select `Plot_PredictedVsObserved`, `Plot_ResidualsAsHistogram`,
    `Plot_ResidualsVsTime`, `Plot_ResidualsVsObserved`, and `Plot_QQ`.

Check also the sheet `Scenarios`. Here you can define for each scenario
a `LongName` that will be used in captions as scenario descriptions.

| Level | Header | PlotName | Scenario | DataGroupIds | OutputPathIds | IndividualIds | ScenarioName | plotCaptionAddon | TimeUnit | TimeOffset | ReferenceScenario | TimeOffset_Reference | ColorLegend | TimeRange_total | TimeRange_firstApplication | TimeRange_lastApplication | yScale | SplitPlotsPerTimeRange | nFacetColumns | FacetScale | Plot_TimeProfiles | ylimit_linear | ylimit_log | Plot_PredictedVsObserved | FoldDistance_PvO | Plot_ResidualsAsHistogram | Plot_ResidualsVsTime | Plot_ResidualsVsObserved | Plot_QQ |
|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|:---|---:|:---|:---|---:|---:|:---|---:|---:|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | Concentration time profiles |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
| 2 | Setting Up the Project and Basic Simulations |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
|  |  | one_individual | one_individual | 1234_iv | Plasma, CYP3A4total, CYP3A4Liver | I123413 |  | DrugX was administered as a 1mg Iv application with an infusion time of 5 minutes. | h | 0 |  | 0 |  | total |  |  | linear, log | 1 | 3 | fixed | 1 |  |  | 0 |  | 0 | 0 | 0 | 0 |
|  |  | one_individual-gof | one_individual | 1234_iv | Plasma | I123413 |  | DrugX was administered as a 1mg Iv application with an infusion time of 5 minutes. | h | 0 |  | 0 |  | total |  |  | linear, log | 1 | 3 | fixed | 0 |  |  | 1 |  | 1 | 1 | 1 | 1 |

Now we can execute the `runPlot` call to generate the plots. For now, we
suppress the export of the figures by setting the input parameter
`plotNames = c('one_individual', 'one_individual-gof')`. `runPlot`
returns a list of ggplot objects. The caption is added as an attribute
to each plot.

``` r

plotList <- runPlot(
  nameOfplotFunction = "plotTimeProfiles",
  projectConfiguration = projectConfiguration,
  configTableSheet = "TimeProfiles",
  plotNames = c("one_individual", "one_individual-gof"),
  theme = runPlotTheme,
  inputs = list(
    dataObserved = dataObserved,
    scenarioResults = scenarioResults
  )
)
#> INFO [2026-11-27 13:21:26] Create Plot one_individual
#> INFO [2026-11-27 13:21:27] Create Plot one_individual-gof
```

Below the logarithmic time-profile is displayed. See the Word document
for the other plots.

``` r

print(plotList[["one_individual-TP-log-total"]])
```

![Logarithmic time-profile plot comparing simulated data to observed
data.](Tutorial_Timeprofiles_files/figure-html/plot1figure-1.png)

``` r

print(attr(plotList[["one_individual-TP-log-total"]], "caption"))
```

\[1\] “Simulated Concentration-time profiles for drugX metabolized by
CYP3A4 (B), drugX metabolized by CYP3A4 in liver (C) and drugX plasma
concentration (A) for simulation of an 1mg iv application for individual
id 13 study 1234 on a logarithmic y-scale. DrugX was administered as a
1mg Iv application with an infusion time of 5 minutes.”

## 2) Adding Virtual Twin Populations

(See
**[Population](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/articles/Population.md)**).

### Step 1: Edit Population Configuration

Manually adjust your Excel configuration to define virtual twin
populations. The `VirtualTwinPopulation` sheet within the
`Individuals.xlsx` file was created during data import. It contains all
data groups with individual data. Since this is a crossover study and
the individuals in group ‘IV’ are also in group ‘PO’, only one
population is suggested for both data groups.

For now, we will leave the default configuration and only adjust the
`populationName`:

| populationName | dataGroups | individualId | modelParameterSheets | applicationProtocol |
|:---|:---|:---|---:|---:|
| study_1234_population | 1234_adults_iv, 1234_adults_po | I123413 |  |  |
| study_1234_population | 1234_adults_iv, 1234_adults_po | I123430 |  |  |
| study_1234_population | 1234_adults_iv, 1234_adults_po | I123441 |  |  |
| study_1234_population | 1234_adults_iv, 1234_adults_po | I123450 |  |  |
| study_1234_population | 1234_adults_iv, 1234_adults_po | I123466 |  |  |
| study_1234_population | 1234_adults_iv, 1234_adults_po | I123478 |  |  |
| 1234_iv_1234_po | 1234_iv, 1234_po | I123413 |  |  |
| 1234_iv_1234_po | 1234_iv, 1234_po | I123430 |  |  |
| 1234_iv_1234_po | 1234_iv, 1234_po | I123441 |  |  |
| 1234_iv_1234_po | 1234_iv, 1234_po | I123450 |  |  |
| 1234_iv_1234_po | 1234_iv, 1234_po | I123466 |  |  |
| 1234_iv_1234_po | 1234_iv, 1234_po | I123478 |  |  |

### Step 2: Export Population

- Go to the Export populations section of the workflow and uncomment the
  lines for the virtualTwin export.
- Adjust the input parameter `modelFile`. The model file is used for
  unit conversions. Because the virtual twin population contains only
  individual physiological parameters (height, weight, age, etc.) and no
  dosing-protocol parameters, either model file can be used.

Now you can execute the following lines:

``` r

exportVirtualTwinPopulations(
  projectConfiguration = projectConfiguration,
  populationNames = NULL,
  modelFile = "po_3_mg_solution.pkml",
  overwrite = FALSE
)
```

This will create a population based on the configuration table from
Step 1. It is saved as a CSV file in `Models/Populations`.

### Step 3: Set Up and Run Scenario

Open `Scenarios.xlsx` again and add a new row to the sheet `Scenarios`.
Instead of `IndividualId`, fill `PopulationId` with the name of the
newly created population and set the value to `1` (TRUE) in the column
`ReadPopulationFromCSV`.

| Scenario_name | IndividualId | PopulationId | ReadPopulationFromCSV | ModelFile | ModelParameterSheets | ApplicationProtocol | SimulationTime | SimulationTimeUnit | SteadyState | SteadyStateTime | SteadyStateTimeUnit | OutputPathsIds |
|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|
| one_individual | I123413 |  |  | iv_1_mg_5_min.pkml |  |  |  |  |  |  |  |  |
| virtual_twin_population |  | study_1234_population | 1 | iv_1_mg_5_min.pkml |  |  |  |  |  |  |  |  |

To omit a rerun from previously defined scenarios, we change the
`scenarioNames` parameter in the `scenarioList` initialization from
`NULL` to the names of the newly created scenarios.

``` r

scenarioList <-
  createScenariosWrapped(
    projectConfiguration = projectConfiguration,
    scenarioNames = dtScenarios$Scenario_name[2]
  )

scenarioResults <- runAndSaveScenarios(
  projectConfiguration = projectConfiguration,
  scenarioList = scenarioList,
  simulationRunOptions = ospsuite::SimulationRunOptions$new(
    numberOfCores = NULL,
    checkForNegativeValues = NULL,
    showProgress = TRUE
  )
)
```

### Step 4: Configure and Generate Plots

We will add the new scenario to the plot configuration table using the
helper function `addDefaultConfigForTimeProfilePlots`.

Then we open the configuration table and adjust the plot configurations.

First, we add a plot as an example to filter individuals explicitly:

- Copy the `virtual_twin_population` line.
- Adjust the column `plotName` to generate two independent plots.
- Add the `DataGroupId` and as `IndividualId` for the first plot the
  first three IDs of the data set, and for the second plot the last
  three.
- Set a bracket around the outputs in the column `OutputPathIds`. This
  way, both outputs will be plotted in one panel.

Then we add a plot as an example to filter individuals with shortcuts:

- Copy one of the previous plots for the `virtual_twin_population`
  scenario.
- As `IndividualId`, use `'*'`. Now all available individuals will be
  plotted. If you have many individuals, this could lead to very large
  plots with many rows, so the plot is split into different plots. The
  maximum row number is an input parameter of the plot function.

As we did in Section 1, we will add a line for the goodness of fit
plots. Copy the plot line for the last plot and edit the columns:

- `PlotName`
- `OutputPathIds`: ensure we have only outputs of one dimension
  selected.
- Deselect `Plot_TimeProfiles`.
- Select `Plot_PredictedVsObserved`, `Plot_ResidualsAsHistogram`,
  `Plot_ResidualsVsTime`, `Plot_ResidualsVsObserved`, and `Plot_QQ`.
- Set `IndividualId` to `'*'`. In this way, all individuals are plotted
  in one panel. This is only available for goodness of fit plots, not
  for the time profile plots.

Do not forget to edit the columns `PlotCaptionAddon`,
`TimeRange_firstApplication`, and `TimeRange_lastApplication`.

| Level | Header | PlotName | Scenario | DataGroupIds | OutputPathIds | IndividualIds | ScenarioName | plotCaptionAddon | TimeUnit | TimeOffset | ReferenceScenario | TimeOffset_Reference | ColorLegend | TimeRange_total | TimeRange_firstApplication | TimeRange_lastApplication | yScale | SplitPlotsPerTimeRange | nFacetColumns | FacetScale | Plot_TimeProfiles | ylimit_linear | ylimit_log | Plot_PredictedVsObserved | FoldDistance_PvO | Plot_ResidualsAsHistogram | Plot_ResidualsVsTime | Plot_ResidualsVsObserved | Plot_QQ |
|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|:---|---:|:---|:---|---:|---:|:---|---:|---:|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 2 | Adding Virtual Twin Populations |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
| 3 | filtered individual |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
|  |  | selected_individuals-1 | virtual_twin_population | 1234_iv | (Plasma, CYP3A4total) | I123413, I123430, I123441 |  | DrugX was administered as a 1mg Iv application with an infusion time of 5 minutes. | h | 0 |  | 0 |  | total |  |  | linear, log | 1 | 3 | fixed | 1 |  |  | 0 |  | 0 | 0 | 0 | 0 |
|  |  | selected_individuals-2 | virtual_twin_population | 1234_iv | (Plasma, CYP3A4total) | I123450, I123466, I123478 |  | DrugX was administered as a 1mg Iv application with an infusion time of 5 minutes. | h | 0 |  | 0 |  | total |  |  | linear, log | 1 | 3 | fixed | 1 |  |  | 0 |  | 0 | 0 | 0 | 0 |
| 3 | shortcut for individual filter |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
|  |  | all_individuals | virtual_twin_population | 1234_iv | (Plasma, CYP3A4total) | \* |  | DrugX was administered as a 1mg Iv application with an infusion time of 5 minutes. | h | 0 |  | 0 |  | total |  |  | linear, log | 1 | 3 | fixed | 1 |  |  | 0 |  | 0 | 0 | 0 | 0 |
|  |  | all_individuals-gof | virtual_twin_population | 1234_iv | Plasma | (\*) |  | DrugX was administered as a 1mg Iv application with an infusion time of 5 minutes. | h | 0 |  | 0 |  | total |  |  | linear, log | 1 | 3 | fixed | 0 |  |  | 1 |  | 1 | 1 | 1 | 1 |

Execute the `runPlot` call again to generate the plots. We take the
newly generated plot names. The variable `plotList` contains all
generated plots.

``` r

plotList <- runPlot(
  nameOfplotFunction = "plotTimeProfiles",
  projectConfiguration = projectConfiguration,
  configTableSheet = "TimeProfiles",
  plotNames = c("selected_individuals-1", "selected_individuals-2", "all_individuals", "all_individuals-gof"),
  theme = runPlotTheme,
  inputs = list(
    dataObserved = dataObserved,
    scenarioResults = scenarioResults
  )
)
#> INFO [2026-11-27 13:21:31] Create Plot selected_individuals-1
#> INFO [2026-11-27 13:21:31] Create Plot selected_individuals-2
#> INFO [2026-11-27 13:21:32] Create Plot all_individuals
#> INFO [2026-11-27 13:21:33] Create Plot all_individuals-gof
```

Below the logarithmic time-profile for all individuals is displayed. See
the Word document for the other plots.

``` r

print(plotList[["all_individuals-TP-log-total"]])
```

![Logarithmic time-profile plot for all
individuals.](Tutorial_Timeprofiles_files/figure-html/plot2figure-1.png)

``` r

print(attr(plotList[["all_individuals-TP-log-total"]], "caption"))
```

\[1\] “Simulated Concentration-time profiles for drugX metabolized by
CYP3A4 and drugX plasma concentration for simulation of an 1mg iv
application for individuals of study 1234 for subject I123413 (A),
I123430 (B), I123441 (C), I123450 (D), I123466 (E) and I123478 (F) on a
logarithmic y-scale. DrugX was administered as a 1mg Iv application with
an infusion time of 5 minutes.”

## 3) Adding Random Populations and Use of Aggregated Data

In this section, we plot aggregated observed data against aggregated
simulated random populations.

### Step 1: Data Aggregation

Since the observed data contains only individual-level records, we first
compute group-level aggregates. Update your data import script as
follows. The call to
[`updateDataGroupId()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/updateDataGroupId.md)
is required to register the new aggregated groups in the `Reports.xlsx`
configuration so that the plot function can find them.

To learn more about the aggregation options, see
[`?aggregateObservedDataGroups`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/aggregateObservedDataGroups.md).

``` r

dataObserved <- readObservedDataByDictionary(projectConfiguration = projectConfiguration)

# add aggregated  groups of data
dataObserved <- rbind(dataObserved,
  aggregateObservedDataGroups(
    dataObserved = dataObserved,
    groups = c("1234_iv", "1234_po")
  ),
  fill = TRUE
)
# For the manually added groups, the configuration sheet in Reports.xlsx has to be updated
updateDataGroupId(projectConfiguration, dataObserved)
```

### Step 2: Edit Population Configuration

Open the sheet `Demographics` in the `Populations.xlsx`.

In the example below, the population ‘adults’ is defined. As ontogeny,
“CYP3A4” and “UGT1A4” are added.

| PopulationName | species | population | numberOfIndividuals | proportionOfFemales | weightMin | weightMax | weightUnit | heightMin | heightMax | heightUnit | ageMin | ageMax | BMIMin | BMIMax | BMIUnit | Protein Ontogenies |
|:---|:---|:---|---:|---:|---:|---:|:---|---:|---:|:---|---:|---:|---:|---:|:---|:---|
| random_population | Human | European_ICRP_2002 | 100 | 50 |  |  | kg |  |  | cm | 20 | 40 |  |  | kg/m² | CYP3A4:CYP3A4, UGT1A4:UGT1A4 |

### Step 3: Export Population

Go to the Export populations section of the workflow and uncomment lines
to export random populations:

``` r

exportRandomPopulations(
  projectConfiguration = projectConfiguration,
  populationNames = NULL,
  overwrite = FALSE
)
```

This will create a population based on the configuration table from the
previous section. It is saved as a CSV file in `Models/Populations`.

### Step 4: Set Up and Run Scenario

Add two more scenarios to the `Scenarios` sheet:

| Scenario_name | IndividualId | PopulationId | ReadPopulationFromCSV | ModelFile | ModelParameterSheets | ApplicationProtocol | SimulationTime | SimulationTimeUnit | SteadyState | SteadyStateTime | SteadyStateTimeUnit | OutputPathsIds |
|:---|:---|:---|---:|:---|---:|---:|---:|---:|---:|---:|---:|:---|
| one_individual | I123413 |  |  | iv_1_mg_5_min.pkml |  |  |  |  |  |  |  |  |
| virtual_twin_population |  | study_1234_population | 1 | iv_1_mg_5_min.pkml |  |  |  |  |  |  |  |  |
| random_population_po |  | random_population | 1 | po_3_mg_solution.pkml |  |  |  |  |  |  |  |  |
| random_population_iv |  | random_population | 1 | iv_1_mg_5_min.pkml |  |  |  |  |  |  |  |  |

Since the simulation results from earlier sections are already cached on
disk, set `scenarioNames = NULL` to run all scenarios including the new
random-population ones. Previously cached results will be loaded from
disk rather than re-simulated.

``` r

scenarioList <-
  createScenariosWrapped(
    projectConfiguration = projectConfiguration,
    scenarioNames = NULL
  )

scenarioResults <- runAndSaveScenarios(
  projectConfiguration = projectConfiguration,
  scenarioList = scenarioList,
  simulationRunOptions = ospsuite::SimulationRunOptions$new(
    numberOfCores = NULL,
    checkForNegativeValues = NULL,
    showProgress = TRUE
  )
)
```

### Step 5: Configure and Generate Plots

We will add the new scenarios to the plot configuration table using the
helper function `addDefaultConfigForTimeProfilePlots`.

Then we open the configuration table and adjust the plot configurations.

In this example, we want both scenarios in one plot:

- Update the `PlotName` for both newly added scenarios to
  ‘random_population’.
- As `DataGroupIds`, we can now use the groups added in Step 1: Data
  Aggregation.

Do not forget to edit the columns `TimeRange_firstApplication`,
`TimeRange_lastApplication`, and add `LongName` entries in the sheet
`Scenarios`.

| Level | Header | PlotName | Scenario | DataGroupIds | OutputPathIds | IndividualIds | ScenarioName | plotCaptionAddon | TimeUnit | TimeOffset | ReferenceScenario | TimeOffset_Reference | ColorLegend | TimeRange_total | TimeRange_firstApplication | TimeRange_lastApplication | yScale | SplitPlotsPerTimeRange | nFacetColumns | FacetScale | Plot_TimeProfiles | ylimit_linear | ylimit_log | Plot_PredictedVsObserved | FoldDistance_PvO | Plot_ResidualsAsHistogram | Plot_ResidualsVsTime | Plot_ResidualsVsObserved | Plot_QQ |
|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|:---|---:|:---|:---|---:|---:|:---|---:|---:|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 2 | Adding Random Populations and use of aggregated data |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
|  |  | random_population | random_population_iv | 1234_iv_aggregated | Plasma, CYP3A4total, CYP3A4Liver |  |  |  | h | 0 |  | 0 |  | total |  |  | linear, log | 1 | 3 | fixed | 1 |  |  | 0 |  | 0 | 0 | 0 | 0 |
|  |  | random_population | random_population_po | 1234_po_aggregated | Plasma, CYP3A4total, CYP3A4Liver |  |  |  | h | 0 |  | 0 |  | total |  |  | linear, log | 1 | 3 | fixed | 1 |  |  | 0 |  | 0 | 0 | 0 | 0 |

Execute the `runPlot` call again to generate the plots.

``` r

plotList <- runPlot(
  nameOfplotFunction = "plotTimeProfiles",
  projectConfiguration = projectConfiguration,
  configTableSheet = "TimeProfiles",
  plotNames = c("random_population"),
  theme = runPlotTheme,
  inputs = list(
    dataObserved = dataObserved,
    scenarioResults = scenarioResults
  )
)
#> INFO [2026-11-27 13:21:38] Create Plot random_population
```

Below the logarithmic time-profile is displayed. See the Word document
for the other plots.

``` r

print(plotList[["random_population-TP-log-total"]])
```

![Logarithmic time-profile plot for random
populations.](Tutorial_Timeprofiles_files/figure-html/plot3figure-1.png)

``` r

print(attr(plotList[["random_population-TP-log-total"]], "caption"))
```

\[1\] “Simulated Concentration-time profiles for drugX metabolized by
CYP3A4 (B, E), drugX metabolized by CYP3A4 in liver (C, F) and drugX
plasma concentration (A, D) for simulation of an 1mg iv application for
an adult population (A, B, C) and simulation of an 5mg oral application
for an adult population (D, E, F) on a logarithmic y-scale.”

## 4) Adding Reference Scenarios

In this section, we overlay two scenarios on a single plot: `adults_po`
as the primary scenario and `adults_iv` as the reference. The reference
scenario is displayed with a distinct colour so the two curves can be
compared directly.

### Configure and Generate Plots

- Copy the `adults_po` plot row and adjust `PlotName`.
- Set `ReferenceScenario` to `adults_iv`.
- Add legend labels for the two scenarios in the `ColorLegend` column.
- Use the `ScenarioName` column for a shared display name for both
  scenarios (this overrides the default `LongName` from the `Scenarios`
  sheet).

Do not forget to edit the columns `DataGroupIds`, `PlotCaptionAddon`,
`TimeRange_firstApplication`, and `TimeRange_lastApplication`.

| Level | Header | PlotName | Scenario | DataGroupIds | OutputPathIds | IndividualIds | ScenarioName | plotCaptionAddon | TimeUnit | TimeOffset | ReferenceScenario | TimeOffset_Reference | ColorLegend | TimeRange_total | TimeRange_firstApplication | TimeRange_lastApplication | TimeRange_h0_6 | TimeRange_h6_24 | yScale | SplitPlotsPerTimeRange | nFacetColumns | FacetScale | Plot_TimeProfiles | ylimit_linear | ylimit_log | Plot_PredictedVsObserved | FoldDistance_PvO | Plot_ResidualsAsHistogram | Plot_ResidualsVsTime | Plot_ResidualsVsObserved | Plot_QQ |
|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|:---|---:|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 2 | Adding Reference Populations |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
|  |  | reference | random_population_po | 1234_po_aggregated | Plasma, CYP3A4total, CYP3A4Liver |  | simulation of a 5mg oral administration vs 1mg iv simulation |  | h | 0 | random_population_iv | 0 | 5mg oral administration\|1mg iv simulation | total |  |  |  |  | linear, log | 1 | 3 | fixed | 1 |  |  | 0 |  | 0 | 0 | 0 | 0 |

To adjust the reference colour, pass `referenceScaleVector` using the
labels from the `colorLegend` column.

``` r

plotList <- runPlot(
  nameOfplotFunction = "plotTimeProfiles",
  projectConfiguration = projectConfiguration,
  configTableSheet = "TimeProfiles",
  plotNames = c("reference"),
  theme = runPlotTheme,
  inputs = list(
    dataObserved = dataObserved,
    scenarioResults = scenarioResults,
    referenceScaleVector = list("1mg iv simulation" = "grey")
  )
)
#> INFO [2026-11-27 13:21:40] Create Plot reference
```

Below the logarithmic time-profile is displayed. See the Word document
for the other plot.

``` r

print(plotList[["reference-TP-log-total"]])
```

![Logarithmic time-profile plot comparing reference
populations.](Tutorial_Timeprofiles_files/figure-html/plot4figure-1.png)

``` r

print(attr(plotList[["reference-TP-log-total"]], "caption"))
```

\[1\] “Simulated Concentration-time profiles for drugX metabolized by
CYP3A4 (B), drugX metabolized by CYP3A4 in liver (C) and drugX plasma
concentration (A) for simulation of a 5mg oral administration vs 1mg iv
simulation on a logarithmic y-scale.”

## 5) Adding Plots with Different Time Ranges

Visualizing pharmacokinetic data across various time ranges can reveal
different aspects of drug behavior. A typical example would be to show
the first and last application in a multi-dosing simulation. As we have
only a single dose in our example, we show the principle by splitting
the time range into two slots: 0 - 6 hours and 6 to 24 hours.

### Step 1: Configure Time Ranges

In your plot configuration Excel file, in the sheet `TimeRange`, specify
different time ranges for which you’d like to generate plots. This might
involve adding new rows or adjusting existing settings in the
`Reports.xlsx` file.

The `TimeRange` has the columns:

- `Tag`: identifier corresponds to the columns in the plot configuration
  sheet beginning with `TimeRange_`.
- `CaptionText`: Text added to the plot caption.
- `TimeLabel`: Label of the x-axis.
- `TimeShift`: An offset used to shift the time axis. If you want to
  display time after the last dose, your timeshift would be the time of
  the last dosing. (This is set automatically for the `firstApplication`
  and `lastApplication` tags if `TimeShift` is empty).

In the example below, a new row for the range between 0 and 6 hours and
a row for the range between 6 and 24 hours have been added.

| Tag              | CaptionText                      | TimeLabel       | TimeShift |
|:-----------------|:---------------------------------|:----------------|----------:|
| total            |                                  | Time            |         0 |
| firstApplication | Zoom on the first dose           | Time            |           |
| lastApplication  | Zoom on steady state dose        | Time after dose |           |
| h0_6             | Zoom on first 6 hours            | Time            |         0 |
| h6_24            | Zoom on time range 6 to 24 hours | Time after dose |         0 |

### Step 2: Configure and Generate Plots

Open the plot configuration table and adjust the plot configurations.

- Copy the plot line for the `one_individual` plot name to the end of
  the table and adjust `PlotName` for this line.
- Add new columns `TimeRange_h0_6` and `TimeRange_h6_24` for the newly
  created time ranges.
- Set the `TimeRange_h0_6` column to `c(0, 6)` and the `TimeRange_h6_24`
  column to `c(6, 24)`.
- Set the `TimeRange_total` column to `''`.

This configuration will produce one plot for each of the newly created
time ranges.

We add another example where we display the time ranges in one plot:

- Copy the previous plot line and adjust the `PlotName`.
- Set the column `SplitPlotsPerTimeRange` to `FALSE` (0).
- Set the column `nFacetColumns` to 2. This way, in the final plot, we
  will have two columns for time ranges and three rows for outputs.
- Set the column `FacetScale` to `free_x`. All plots will have the same
  y-axis but different x-axes.

| Level | Header | PlotName | Scenario | DataGroupIds | OutputPathIds | IndividualIds | ScenarioName | plotCaptionAddon | TimeUnit | TimeOffset | ReferenceScenario | TimeOffset_Reference | ColorLegend | TimeRange_total | TimeRange_firstApplication | TimeRange_lastApplication | TimeRange_h0_6 | TimeRange_h6_24 | yScale | SplitPlotsPerTimeRange | nFacetColumns | FacetScale | Plot_TimeProfiles | ylimit_linear | ylimit_log | Plot_PredictedVsObserved | FoldDistance_PvO | Plot_ResidualsAsHistogram | Plot_ResidualsVsTime | Plot_ResidualsVsObserved | Plot_QQ |
|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|:---|---:|:---|:---|---:|---:|:---|:---|:---|---:|---:|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 2 | Adding Plots with different Time Ranges |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
|  |  | timeranges | one_individual | 1234_iv | Plasma, CYP3A4total, CYP3A4Liver | I123413 |  | DrugX was administered as a 1mg Iv application with an infusion time of 5 minutes. | h | 0 |  | 0 |  |  |  |  | c(0,6) | c(6,24) | linear, log | 1 | 3 | fixed | 1 |  |  | 0 |  | 0 | 0 | 0 | 0 |
|  |  | timeranges-split | one_individual | 1234_iv | Plasma, CYP3A4total, CYP3A4Liver | I123413 |  | DrugX was administered as a 1mg Iv application with an infusion time of 5 minutes. | h | 0 |  | 0 |  |  |  |  | c(0,6) | c(6,24) | linear, log | 0 | 2 | free_x | 1 |  |  | 0 |  | 0 | 0 | 0 | 0 |

Execute the `runPlot` call again to generate the plots. The variable
`plotList` contains all generated plots. In the example below, the input
argument `xScaleArgs = list(limits = c(NA, NA))` is added. This argument
is passed to the function `ospsuite.plots::plotTimeprofile` and
overwrites the default time-profile x-limits, which is `c(0, NA)`.

``` r

plotList <- runPlot(
  nameOfplotFunction = "plotTimeProfiles",
  projectConfiguration = projectConfiguration,
  configTableSheet = "TimeProfiles",
  plotNames = c("timeranges", "timeranges-split"),
  theme = runPlotTheme,
  inputs = list(
    dataObserved = dataObserved,
    scenarioResults = scenarioResults,
    xScaleArgs = list(limits = c(NA, NA))
  )
)
#> INFO [2026-11-27 13:21:42] Create Plot timeranges
#> INFO [2026-11-27 13:21:44] Create Plot timeranges-split
```

Below the logarithmic time-profile for the plot that combines the time
profiles is displayed. See the Word document for the other plots.

``` r

print(plotList[["timeranges-split-TP-log-allTimeRanges"]])
```

![Logarithmic time-profile plot showing combined time
ranges.](Tutorial_Timeprofiles_files/figure-html/plot5figure-1.png)

``` r

print(attr(plotList[["timeranges-split-TP-log-allTimeRanges"]], "caption"))
```

\[1\] “Simulated Concentration-time profiles for drugX metabolized by
CYP3A4 (C, D), drugX metabolized by CYP3A4 in liver (E, F) and drugX
plasma concentration (A, B) for simulation of an 1mg iv application for
individual id 13 study 1234 on a logarithmic y-scale Zoom on first 6
hours (A, C, E) and Zoom on time range 6 to 24 hours (B, D, F). DrugX
was administered as a 1mg Iv application with an infusion time of 5
minutes.”

## 6) Report Creation

In this section, all plots from the previous sections are exported to
disk and assembled into a single Word document. The workflow has two
steps:

1.  Call
    [`runPlot()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/runPlot.md)
    without a `plotNames` argument so that every configured plot is
    generated and saved as an image and an `.Rmd` fragment.
2.  Call
    [`mergeQmds()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mergeQmds.md)
    to concatenate the individual `.qmd` fragments into one document,
    then
    [`renderWord()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/renderWord.md)
    to produce the final `.docx`.

``` r

runPlot(
  nameOfplotFunction = "plotTimeProfiles",
  projectConfiguration = projectConfiguration,
  configTableSheet = "TimeProfiles",
  theme = runPlotTheme,
  inputs = list(
    dataObserved = dataObserved,
    scenarioResults = scenarioResults,
    referenceScaleVector = list("1mg iv simulation" = "grey"),
    xScaleArgs = list(limits = c(NA, NA))
  )
)
```

The first step is to run all plots without a `plotNames` filter so every
configured plot is generated and saved. The second step merges the
individual `.Qmd` fragments into a single document and renders it to
Word.

``` r

mergeQmds(
  projectConfiguration = projectConfiguration,
  newName = "TutorialPlots",
  title = "Plots generated by tutorial",
  sourceRmds = c("TimeProfiles")
)

renderWord(fileName = file.path(projectConfiguration$outputFolder, "TutorialPlots.Rmd"))
```

## Conclusion

This tutorial covered the core time profile plotting workflow:

- Simulating a single individual and generating basic time profile and
  goodness of fit plots
- Building virtual twin and random populations and plotting aggregated
  results
- Comparing conditions using a reference scenario overlay
- Splitting plots across custom time-range windows
- Assembling all plots into a Word report

Other package vignettes:

- **[OSPSuite Reporting
  Framework](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/articles/OSPSuite_ReportingFramework.md)**
  — package overview and entry points
- **[PK
  Parameter](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/articles/PK_Parameter.md)**
  — PK parameter calculation and configuration
- **[Sensitivity
  Analysis](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/articles/Sensitivity_Analysis.md)**
  — sensitivity workflows, including ratio and representative-population
  modes
- **[Electronic
  Package](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/articles/Electronic_Package.md)**
  — assembling and validating reporting deliverables
