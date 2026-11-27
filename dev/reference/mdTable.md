# Insert table from CSV with caption.

Insert table from CSV with caption.

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

  Table number for prefix.

- tableCsv:

  CSV file path.

- captionFile:

  Caption file.

- footNoteFile:

  Footnote file.

- subfolder:

  Folder location.

- customStyles:

  Custom styles list.

- digitsOfSignificance:

  Significant digits (default 3).

- addNewPage:

  Add page break after (default TRUE).

- ...:

  Passed to \`knitr::kable\`.

## Value

Invisible \`NULL\`.

## See also

Other markdown helper function:
[`mdBullet()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdBullet.md),
[`mdCaption()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdCaption.md),
[`mdFigure()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdFigure.md),
[`mdFootNote()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdFootNote.md),
[`mdHeading()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdHeading.md),
[`mdLink()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdLink.md),
[`mdNewline()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdNewline.md),
[`mdNewpage()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdNewpage.md),
[`mdPaste()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdPaste.md),
[`mdPaste0()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdPaste0.md)
