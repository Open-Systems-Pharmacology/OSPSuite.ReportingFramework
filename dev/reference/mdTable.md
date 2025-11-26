# mdTable

Insert a table into a markdown document. Essentially a wrapper for
\`knitr::kable\` with \`format = "markdown"\` and newlines.

## Usage

``` r
mdTable(
  tableNumber,
  tableCsv,
  captionFile,
  footNoteFile,
  subfolder,
  customStyles,
  digitsOfSignificance = 3,
  addNewPage = TRUE,
  ...
)
```

## Arguments

- tableNumber:

  number of table for caption prefix

- tableCsv:

  path of .csv file

- captionFile:

  file containing caption

- footNoteFile:

  file containing footnotes

- subfolder:

  The folder where the file is located relative to Rmd

- customStyles:

  list of custom styles usable for figure and table captions and
  footnotes

- digitsOfSignificance:

  significance digits to display (default 3)

- addNewPage:

  boolean if TRUE (default) new page is added after

- ...:

  passed to \`knitr::kable\`

## Value

\`x\`, invisibly

## See also

Other markdown helper function:
[`addFiguresAndTables()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addFiguresAndTables.md),
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
[`mdPaste0()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdPaste0.md)
