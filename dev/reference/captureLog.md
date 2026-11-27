# Catch messages, warnings, and errors and route them to the log

Wraps \`ospsuite.utils::logCatch\` and preserves an optional \`finally\`
expression for backwards compatibility.

## Usage

``` r
captureLog(expr, finallyExpression = invisible())
```

## Arguments

- expr:

  The expression to evaluate.

- finallyExpression:

  The expression to evaluate finally.

## Value

NULL (invisible).

## See also

Other log file management:
[`addMessageToLog()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addMessageToLog.md),
[`saveSessionInfo()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/saveSessionInfo.md),
[`setLogPath()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setLogPath.md),
[`setShowLogMessages()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setShowLogMessages.md),
[`writeTableToLog()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/writeTableToLog.md)
