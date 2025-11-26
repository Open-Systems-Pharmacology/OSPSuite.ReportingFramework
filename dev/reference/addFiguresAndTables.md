# Loop on figure keys, add figure or table

Loop on figure keys, add figure or table

## Usage

``` r
addFiguresAndTables(
  keyList,
  subfolder,
  numbersOf,
  customStyles = list(),
  digitsOfSignificance = 3
)
```

## Arguments

- keyList:

  list of keys

- subfolder:

  sub-folder of relative to .Rmd file where figures are saved

- numbersOf:

  list with numbers of tables and figures in rmd

- customStyles:

  custom-styles to render word document

- digitsOfSignificance:

  significance digits for tables

## Value

list of numbers and figures after loop

## See also

Other markdown helper function:
[`mdBullet()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdBullet.md),
[`mdBullet0()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdBullet0.md),
[`mdCaption()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdCaption.md),
[`mdFigure()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdFigure.md),
[`mdFootNote()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdFootNote.md),
[`mdHeading()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdHeading.md),
[`mdLink()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdLink.md),
[`mdNewline()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdNewline.md),
[`mdNewpage()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdNewpage.md),
[`mdPaste()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdPaste.md),
[`mdPaste0()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdPaste0.md),
[`mdTable()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdTable.md)
