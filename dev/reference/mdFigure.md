# Include figure with caption and footnote.

Include figure with caption and footnote.

## Usage

``` r
mdFigure(
  figureNumber,
  figureFile,
  captionFile,
  footNoteFile = NULL,
  subfolder,
  addNewPage = TRUE,
  customStyles = list()
)
```

## Arguments

- figureNumber:

  Figure number for caption prefix.

- figureFile:

  Figure file name.

- captionFile:

  Caption file.

- footNoteFile:

  Footnote file (optional).

- subfolder:

  Folder relative to Rmd.

- addNewPage:

  Add page break after (default TRUE).

- customStyles:

  Custom styles list.

## Value

Invisible \`NULL\`.

## See also

Other markdown helper function:
[`mdBullet()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdBullet.md),
[`mdCaption()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdCaption.md),
[`mdFootNote()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdFootNote.md),
[`mdHeading()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdHeading.md),
[`mdLink()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdLink.md),
[`mdNewline()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdNewline.md),
[`mdNewpage()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdNewpage.md),
[`mdPaste()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdPaste.md),
[`mdPaste0()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdPaste0.md),
[`mdTable()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdTable.md)
