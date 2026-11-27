# Set log folder and file name

Configures file logging through the \`ospsuite.utils\` logging backend
and overrides the default \`log.txt\` target with a caller-defined file
name.

## Usage

``` r
setLogPath(logFolder = NULL, logFileName = "log.txt")
```

## Arguments

- logFolder:

  character string with the folder for log files. If \`NULL\`, file
  logging is disabled.

- logFileName:

  character string with the log file name.

## Value

NULL (invisible).

## See also

Other log file management:
[`addMessageToLog()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addMessageToLog.md),
[`captureLog()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/captureLog.md),
[`saveSessionInfo()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/saveSessionInfo.md),
[`setShowLogMessages()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setShowLogMessages.md),
[`writeTableToLog()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/writeTableToLog.md)
