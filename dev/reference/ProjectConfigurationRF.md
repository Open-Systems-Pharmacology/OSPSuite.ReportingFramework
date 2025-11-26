# ProjectConfiguration

An object storing configuration used project-wide

## See also

Other project initialization:
[`createProjectConfiguration()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createProjectConfiguration.md),
[`getQCpassedEnvironmentVariable()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/getQCpassedEnvironmentVariable.md),
[`initProject()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/initProject.md),
[`setWorkflowOptions()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setWorkflowOptions.md)

## Super class

[`esqlabsR::ProjectConfiguration`](https://esqlabs.github.io/esqlabsR/reference/ProjectConfiguration.html)
-\> `ProjectConfigurationRF`

## Active bindings

- `addOns`:

  list with non default configurations

## Methods

### Public methods

- [`ProjectConfigurationRF$new()`](#method-ProjectConfigurationRF-new)

- [`ProjectConfigurationRF$print()`](#method-ProjectConfigurationRF-print)

- [`ProjectConfigurationRF$addAddOnFileToConfiguration()`](#method-ProjectConfigurationRF-addAddOnFileToConfiguration)

- [`ProjectConfigurationRF$addAddOnFolderToConfiguration()`](#method-ProjectConfigurationRF-addAddOnFolderToConfiguration)

- [`ProjectConfigurationRF$clone()`](#method-ProjectConfigurationRF-clone)

Inherited methods

- [`esqlabsR::ProjectConfiguration$save()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/esqlabsR/html/ProjectConfiguration.html#method-ProjectConfiguration-save)

------------------------------------------------------------------------

### Method `new()`

Initializes the ProjectConfiguration object with a specified
configuration file path.

Adds new line to configuration xlsx

Read configuration from file Initialize

#### Usage

    ProjectConfigurationRF$new(projectConfigurationFilePath = character())

#### Arguments

- `projectConfigurationFilePath`:

  A string representing the path to the project configuration file.
  Print

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

print prints a summary of the Project Configuration.

#### Usage

    ProjectConfigurationRF$print(className = TRUE)

#### Arguments

- `className`:

  Whether to print the name of the class at the beginning. default to
  TRUE.

------------------------------------------------------------------------

### Method `addAddOnFileToConfiguration()`

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

### Method `addAddOnFolderToConfiguration()`

Adds an add-on file to the project configuration.

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

- `templatePath`:

  A string representing the path of the template file.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ProjectConfigurationRF$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
