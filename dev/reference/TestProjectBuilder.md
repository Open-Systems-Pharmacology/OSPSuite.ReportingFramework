# TestProjectBuilder

Manages the creation and writing test project, which can be used for
this package or packages which use the reporting framework as base to
build a test project

## Super class

[`ospsuite.utils::Printable`](https://www.open-systems-pharmacology.org/OSPSuite.RUtils/reference/Printable.html)
-\> `TestProjectBuilder`

## Active bindings

- `rootDirectory`:

  root directory of test project. This function synchronizes files from
  one directory to another, copying only the files that are not already
  present. This function updates the display names for scenarios and
  output paths in the project configuration.

## Methods

### Public methods

- [`TestProjectBuilder$new()`](#method-TestProjectBuilder-new)

- [`TestProjectBuilder$iniTestProject()`](#method-TestProjectBuilder-iniTestProject)

- [`TestProjectBuilder$setupRandomPopulations()`](#method-TestProjectBuilder-setupRandomPopulations)

- [`TestProjectBuilder$setupSimulations()`](#method-TestProjectBuilder-setupSimulations)

- [`TestProjectBuilder$setUpSensitivity()`](#method-TestProjectBuilder-setUpSensitivity)

- [`TestProjectBuilder$addRandomTPData()`](#method-TestProjectBuilder-addRandomTPData)

- [`TestProjectBuilder$addRandomPKData()`](#method-TestProjectBuilder-addRandomPKData)

- [`TestProjectBuilder$mockManualEditingsCleanup()`](#method-TestProjectBuilder-mockManualEditingsCleanup)

- [`TestProjectBuilder$mockManualEditingsPopulation()`](#method-TestProjectBuilder-mockManualEditingsPopulation)

- [`TestProjectBuilder$mockManualEditingsScenario()`](#method-TestProjectBuilder-mockManualEditingsScenario)

- [`TestProjectBuilder$mockManualEditingsPKParameter()`](#method-TestProjectBuilder-mockManualEditingsPKParameter)

- [`TestProjectBuilder$mockManualEditingsIndividuals()`](#method-TestProjectBuilder-mockManualEditingsIndividuals)

- [`TestProjectBuilder$mockManualEditingsSensitivity()`](#method-TestProjectBuilder-mockManualEditingsSensitivity)

- [`TestProjectBuilder$clone()`](#method-TestProjectBuilder-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new instance of the class.

#### Usage

    TestProjectBuilder$new()

------------------------------------------------------------------------

### Method `iniTestProject()`

A short description...

This function initializes the project structure and configuration for
the test project.

#### Usage

    TestProjectBuilder$iniTestProject(rootDirectory, verbose = FALSE)

#### Arguments

- `rootDirectory`:

  A character string specifying the root directory for the test project.

- `verbose`:

  A logical value indicating whether to print messages about the
  process.

#### Returns

A ProjectConfiguration object containing the details of the initialized
project.

------------------------------------------------------------------------

### Method `setupRandomPopulations()`

This function sets up populations for the test project by copying
necessary files and exporting defined populations.

#### Usage

    TestProjectBuilder$setupRandomPopulations(
      projectConfiguration,
      populationNames,
      writeTestData = FALSE,
      templateDir = NULL
    )

#### Arguments

- `projectConfiguration`:

  A ProjectConfiguration object containing the project configuration
  details.

- `populationNames`:

  A character vector of population names to be copied.

- `writeTestData`:

  A logical value indicating whether newly created test data should be
  filed to the inst directory of the package.

- `templateDir`:

  character string specifying the installation directory for the
  package.

#### Returns

NULL

------------------------------------------------------------------------

### Method `setupSimulations()`

This function sets up simulations for the test project by synchronizing
necessary directories and running initialized scenarios to calculate
pharmacokinetic (PK) parameters.

#### Usage

    TestProjectBuilder$setupSimulations(
      projectConfiguration,
      scenarioList,
      writeTestData,
      templateDir = NULL
    )

#### Arguments

- `projectConfiguration`:

  A ProjectConfiguration object containing the project configuration
  details.

- `scenarioList`:

  A list of scenarios initialized for the project.

- `writeTestData`:

  A logical value indicating whether newly created test data should be
  filed to the inst directory of the package.

- `templateDir`:

  A character string specifying the installation directory for the
  package.

#### Returns

A list containing the results from running the scenarios. Set Up
Sensitivity Analysis

This function sets up sensitivity analysis for the test project based on
the defined scenarios.

------------------------------------------------------------------------

### Method `setUpSensitivity()`

#### Usage

    TestProjectBuilder$setUpSensitivity(
      projectConfiguration,
      scenarioList,
      scenarioName,
      writeTestData,
      parameterList,
      sensitivitySheet = "smallSelection",
      templateDir = NULL
    )

#### Arguments

- `projectConfiguration`:

  A ProjectConfiguration object containing project configuration
  details.

- `scenarioList`:

  A list of scenarios initialized for the project.

- `scenarioName`:

  The name of the scenario used for sensitivity

- `writeTestData`:

  A logical value indicating whether newly created test data should be
  filed to the inst directory of the package.

- `parameterList`:

  A named list where each name corresponds to a parameter in the Excel
  sheet, and each value is the new value to set for that parameter.

- `sensitivitySheet`:

  Name of sheet with parameter selection

- `templateDir`:

  A character string specifying the installation directory for the
  package.

#### Returns

A ProjectConfiguration object containing the updated project
configuration.

------------------------------------------------------------------------

### Method `addRandomTPData()`

This function adds random time profile data to the test project for
selected individuals.

#### Usage

    TestProjectBuilder$addRandomTPData(
      projectConfiguration,
      scenarioResults,
      ids,
      timePoints = c(15, 30, 45, 60, 90, c(3, 6, 9, 12, 18, 24) * 60),
      outputPathIds = c("Plasma", "CYP3A4total", "CYP3A4Liver"),
      sdShift = 0.05,
      lloqValue = 0.1
    )

#### Arguments

- `projectConfiguration`:

  A ProjectConfiguration object containing project configuration
  details.

- `scenarioResults`:

  List of scenario results.

- `ids`:

  A vector of individual IDs to include in the random data.

- `timePoints`:

  A numeric vector specifying the time points at which to generate data
  (default is c(15, 30, 45, 60, 90, c(3, 6, 9, 12, 18, 24) \* 60)).

- `outputPathIds`:

  A character vector specifying the output path IDs to include in the
  random data (default is c('Plasma', 'CYP3A4total', 'CYP3A4Liver')).

- `sdShift`:

  A numeric value indicating the standard deviation shift applied to the
  random values (default is 0.05).

- `lloqValue`:

  A numeric value representing the lower limit of quantification
  (default is 0.1).

#### Returns

Invisible NULL. This function adds random pharmacokinetic (PK) data to
the test project, including shifting values and calculating statistics
for selected individuals.

------------------------------------------------------------------------

### Method `addRandomPKData()`

#### Usage

    TestProjectBuilder$addRandomPKData(
      projectConfiguration,
      pkParameterDT,
      ids,
      outputPathIds = c("Plasma", "CYP3A4total", "CYP3A4Liver"),
      sdShift = 0.05
    )

#### Arguments

- `projectConfiguration`:

  A ProjectConfiguration object containing project configuration
  details.

- `pkParameterDT`:

  A data.table containing PK parameters and values.

- `ids`:

  A vector of individual IDs to include in the random data.

- `outputPathIds`:

  A character vector specifying the output path IDs to include in the
  random data (default is c('Plasma', 'CYP3A4total', 'CYP3A4Liver')).

- `sdShift`:

  A numeric value indicating the standard deviation shift applied to the
  random values (default is 0.05).

#### Returns

Invisible NULL.

------------------------------------------------------------------------

### Method `mockManualEditingsCleanup()`

This function cleans up the project configuration files by clearing
existing data from relevant Excel sheets.

#### Usage

    TestProjectBuilder$mockManualEditingsCleanup(projectConfiguration)

#### Arguments

- `projectConfiguration`:

  A ProjectConfiguration object containing project configuration
  details.

#### Returns

NULL

------------------------------------------------------------------------

### Method `mockManualEditingsPopulation()`

This function adds biometric ranges for virtual pediatric and one adult
populations to the configuration files

#### Usage

    TestProjectBuilder$mockManualEditingsPopulation(
      projectConfiguration,
      randomPops = data.table(populationName = c("adults", "toddler", "children",
        "school-children", "adolescents"), species = "Human", population =
        "European_ICRP_2002", numberOfIndividuals = 100, proportionOfFemales = 50, ageMin =
        c(20, 0.5, 2, 6, 12), ageMax = c(40, 2, 6, 12, 18), weightUnit = "kg", heightUnit =
        "cm", bMIUnit = "kg/m²", `protein Ontogenies` = "CYP3A4:CYP3A4, UGT1A4:UGT1A4")
    )

#### Arguments

- `projectConfiguration`:

  A ProjectConfiguration object containing project configuration
  details.

- `randomPops`:

  data.table with content for sheet Demographics in Population.xlsx

#### Returns

NULL

------------------------------------------------------------------------

### Method `mockManualEditingsScenario()`

This function sets up scenarios based on the defined populations and
adds PK Parameters and output paths to the project configuration.

#### Usage

    TestProjectBuilder$mockManualEditingsScenario(
      projectConfiguration,
      dtTestScenarios,
      pKParameter = NULL
    )

#### Arguments

- `projectConfiguration`:

  A ProjectConfiguration object containing project configuration
  details.

- `dtTestScenarios`:

  \`data.table\` with content for scenarios

- `pKParameter`:

  vector with pkParameter sheets

#### Returns

NULL

------------------------------------------------------------------------

### Method `mockManualEditingsPKParameter()`

This function adds PK parameters to the project configuration based on
templates.

#### Usage

    TestProjectBuilder$mockManualEditingsPKParameter(projectConfiguration)

#### Arguments

- `projectConfiguration`:

  A ProjectConfiguration object containing project configuration
  details.

#### Returns

NULL

------------------------------------------------------------------------

### Method `mockManualEditingsIndividuals()`

This function adjust the bio-metrics with in individuals and exports
virtual twin populations.

#### Usage

    TestProjectBuilder$mockManualEditingsIndividuals(projectConfiguration)

#### Arguments

- `projectConfiguration`:

  A \`ProjectConfiguration\` object containing project configuration
  details.

#### Returns

\`data.table\` with content of sheet \`VirtualTwinPopulation\`

------------------------------------------------------------------------

### Method `mockManualEditingsSensitivity()`

This function modifies specific parameters in a given Excel sheet based
on the provided parameter list and saves the changes back to the
workbook.

#### Usage

    TestProjectBuilder$mockManualEditingsSensitivity(
      projectConfiguration,
      sheetName,
      parameterList
    )

#### Arguments

- `projectConfiguration`:

  A list containing project configuration details, including the path to
  the sensitivity file.

- `sheetName`:

  A string representing the name of the sheet in the Excel workbook that
  will be edited.

- `parameterList`:

  A named list where each name corresponds to a parameter in the Excel
  sheet, and each value is the new value to set for that parameter.

#### Returns

NULL This function is called for its side effects (i.e., modifying the
Excel file).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TestProjectBuilder$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
