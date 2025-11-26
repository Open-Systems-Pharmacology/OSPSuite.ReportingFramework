# Initializing a Log Function

This function initialize the logging during a workflow. It is called at
the start of the workflow script. It is used to configure options for
the log file folder, warnings which should not logged and messages which
should not logged.

## Usage

``` r
initLogfunction(
  projectConfiguration,
  warningsNotDisplayed = c("introduced infinite values",
    "Each group consists of only one observation", "rows containing non-finite values",
    "rows containing missing values", "Ignoring unknown parameters",
    "was deprecated in ggplot2", "font family not found in Windows font database",
    "mbcsToSbcs"),
  messagesNotDisplayed = c("Each group consists of only one observation"),
  verbose = TRUE
)
```

## Arguments

- projectConfiguration:

  Object of class \`ProjectConfiguration\` containing information on
  paths and file names

- warningsNotDisplayed:

  A list of warnings that should not be logged.

- messagesNotDisplayed:

  A list of messages that should not be logged.

- verbose:

  boolean, if true log message will be shown on the console

## See also

Other log file management:
[`addMessageToLog()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addMessageToLog.md),
[`captureLog()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/captureLog.md),
[`saveSessionInfo()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/saveSessionInfo.md),
[`setShowLogMessages()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setShowLogMessages.md),
[`writeTableToLog()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/writeTableToLog.md),
[`writeToLog()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/writeToLog.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Initialize the log function
logFunction <- initLogfunction(rootDirectory = "path/to/project")
} # }
```
