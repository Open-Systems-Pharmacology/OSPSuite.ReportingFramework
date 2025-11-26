# Stop Helper Function

This function checks if the current execution context is valid for
running helper functions. If the helper function is being called during
a valid run, it raises an error.

## Usage

``` r
stopHelperFunction()
```

## Details

The function retrieves the option \`OSPSuite.RF.stopHelperFunction\` to
determine if helper functions are allowed during a valid run. If the
option is not initialized, it prompts the user to call
\`executeAsValidRun(isValidRun)\`.

Stops execution with an error message if called during a valid run or if
the option is not set.
