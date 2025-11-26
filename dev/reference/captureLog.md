# function that catches messages, warnings, and errors. This function has to be initialized by \`initLogfunction\` function

function that catches messages, warnings, and errors. This function has
to be initialized by \`initLogfunction\` function

## Usage

``` r
captureLog(expr, finallyExpression = invisible())
```

## Arguments

- expr:

  The expression to evaluate.

- finallyExpression:

  The expression to evaluate finally

## See also

Other log file management:
[`addMessageToLog()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addMessageToLog.md),
[`initLogfunction()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/initLogfunction.md),
[`saveSessionInfo()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/saveSessionInfo.md),
[`setShowLogMessages()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setShowLogMessages.md),
[`writeTableToLog()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/writeTableToLog.md),
[`writeToLog()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/writeToLog.md)
