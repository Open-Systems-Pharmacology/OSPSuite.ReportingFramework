# Writing to Log

The \`writeToLog\` function is used to append log messages to a log
file. The path for the logfile has to be initialized by the function
initLogfile

## Usage

``` r
writeToLog(type, msg, filename = NULL)
```

## Arguments

- type:

  The type of message (e.g., Error, Info).

- msg:

  The message to be logged.

- filename:

  The name of the log file.

## See also

Other log file management:
[`addMessageToLog()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addMessageToLog.md),
[`captureLog()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/captureLog.md),
[`initLogfunction()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/initLogfunction.md),
[`saveSessionInfo()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/saveSessionInfo.md),
[`setShowLogMessages()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setShowLogMessages.md),
[`writeTableToLog()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/writeTableToLog.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Write a log message
writeToLog(type = "Info", msg = "This is an information message", filename = "run.log")
} # }
```
