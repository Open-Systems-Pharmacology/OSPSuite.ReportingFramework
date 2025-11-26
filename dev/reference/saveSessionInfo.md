# Save Session Info

This function can be called at the end of your script to save the
session information, including the loaded packages and R version, into a
log file. The path for the log file has to be initialized by the
\`initLogfunction\`

## Usage

``` r
saveSessionInfo()
```

## See also

Other log file management:
[`addMessageToLog()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addMessageToLog.md),
[`captureLog()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/captureLog.md),
[`initLogfunction()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/initLogfunction.md),
[`setShowLogMessages()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setShowLogMessages.md),
[`writeTableToLog()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/writeTableToLog.md),
[`writeToLog()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/writeToLog.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Save session info to log file
saveSessionInfo()
} # }
```
