# Convert Data Table Column Names to Lowercase

This function takes a data.table and converts the first letter of all
column names to lowercase.

## Usage

``` r
setHeadersToLowerCase(dt)
```

## Arguments

- dt:

  A data.table object whose column names need to be converted to
  lowercase.

## Value

The modified data.table with updated column names.

## See also

Other function to read from and write to xlsx:
[`splitInputs()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/splitInputs.md),
[`xlsxAddDataUsingTemplate()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxAddDataUsingTemplate.md),
[`xlsxAddSheet()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxAddSheet.md),
[`xlsxCloneAndSet()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxCloneAndSet.md),
[`xlsxReadData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxReadData.md),
[`xlsxWriteData()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/xlsxWriteData.md)

## Examples

``` r
dt <- data.table::data.table(FirstName = c("John", "Jane"), LastName = c("Doe", "Smith"))
dtLower <- setHeadersToLowerCase(dt)
print(names(dtLower)) # Result: c("firstname", "lastname")
#> [1] "firstName" "lastName" 
```
