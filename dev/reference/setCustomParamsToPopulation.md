# Set Custom Parameters to Population

This function updates the parameter values of a population based on
custom parameters defined in a scenario. It first checks if the scenario
is of type "Population" and whether custom parameters are available.

## Usage

``` r
setCustomParamsToPopulation(scenario)
```

## Arguments

- scenario:

  An object of class \`Scenario\` containing the following components: -
  \`scenarioType\`: A character string indicating the type of
  scenario. - \`finalCustomParams\`: A list with custom parameters. -
  \`population\`: An object representing the population, which includes
  a method to set parameter values. - \`simulation\`: An object
  containing simulation details, used to retrieve parameter dimensions.

## Value

The updated \`scenario\` object, with the population's parameters set
accordingly. If the scenario type is not "Population" or if there are no
custom parameters, the original scenario is returned unchanged.

## Details

The function filters the custom parameters to include only those that
exist in the population's parameter paths. It calculates the base values
for these parameters and sets them for the entire population if
applicable.
