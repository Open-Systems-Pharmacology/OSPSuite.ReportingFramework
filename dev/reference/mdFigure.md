# mdFigure

Include a figure file with caption

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

  number of figure used in caption prefix

- figureFile:

  file of Figure

- captionFile:

  file containing caption

- footNoteFile:

  file containing footnotes

- subfolder:

  The folder where the file is located relative to Rmd

- addNewPage:

  boolean if TRUE (default) new page is added after

- customStyles:

  list of custom styles usable for figure and table captions and
  footnotes

## See also

Other markdown helper function:
[`addFiguresAndTables()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/addFiguresAndTables.md),
[`mdBullet()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdBullet.md),
[`mdBullet0()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdBullet0.md),
[`mdCaption()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdCaption.md),
[`mdFootNote()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdFootNote.md),
[`mdHeading()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdHeading.md),
[`mdLink()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdLink.md),
[`mdNewline()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdNewline.md),
[`mdNewpage()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdNewpage.md),
[`mdPaste()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdPaste.md),
[`mdPaste0()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdPaste0.md),
[`mdTable()`](https://www.open-systems-pharmacology.org/OSPSuite.ReportingFramework/dev/reference/mdTable.md)
