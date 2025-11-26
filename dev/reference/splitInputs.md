# Split the elements of a vector by comma

This function takes an original vector as input and splits its elements
by comma.

## Usage

``` r
splitInputs(originalVector)
```

## Arguments

- originalVector:

  The original vector to be split.

## Value

A vector containing the split elements.

## See also

Other function to read from and write to xlsx:
[`setHeadersToLowerCase()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/setHeadersToLowerCase.md),
[`xlsxAddDataUsingTemplate()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxAddDataUsingTemplate.md),
[`xlsxAddSheet()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxAddSheet.md),
[`xlsxCloneAndSet()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxCloneAndSet.md),
[`xlsxReadData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxReadData.md),
[`xlsxWriteData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxWriteData.md)

## Examples

``` r
if (FALSE) { # \dontrun{
originalVector <- c("group1, group2", "group3")
splitInputs(originalVector)
# Result: c('group1', 'group2', 'group3')
} # }
```
