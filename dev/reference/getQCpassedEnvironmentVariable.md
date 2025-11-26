# Get QC Passed Environment Variable

This function retrieves the value of the environment variable
\`QCpassed\`. It attempts to convert the value to a logical type. If the
environment variable is not set, is empty, or is non-logical, a warning
is issued and the function defaults the value to \`FALSE\`.

## Usage

``` r
getQCpassedEnvironmentVariable()
```

## Value

A logical value indicating whether the QC passed (\`TRUE\` or
\`FALSE\`).

## See also

Other project initialization:
[`ProjectConfigurationRF`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/ProjectConfigurationRF.md),
[`createProjectConfiguration()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/createProjectConfiguration.md),
[`initProject()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/initProject.md),
[`setWorkflowOptions()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setWorkflowOptions.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Set the environment variable for testing
Sys.setenv(QCpassed = "TRUE")
getQCpassedEnvironmentVariable() # Should return TRUE

# Unset the environment variable for testing
Sys.unsetenv("QCpassed")
getQCpassedEnvironmentVariable() # Should return FALSE and issue a warning
} # }
```
