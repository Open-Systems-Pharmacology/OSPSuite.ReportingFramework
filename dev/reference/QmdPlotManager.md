# QmdPlotManager

Manages the creation and writing of .qmd files for plots.

## Super class

[`ospsuite.utils::Printable`](https://www.open-systems-pharmacology.org/OSPSuite.RUtils/reference/Printable.html)
-\> `QmdPlotManager`

## Active bindings

- `digitsOfSignificance`:

  Digits for significance in table display.

- `configTable`:

  Configuration table.

- `plotFunction`:

  Function to create plot list.

- `validateConfigTableFunction`:

  Function to read config table.

- `suppressExport`:

  A logical value indicating whether to suppress export.

## Methods

### Public methods

- [`QmdPlotManager$new()`](#method-QmdPlotManager-initialize)

- [`QmdPlotManager$writeQmd()`](#method-QmdPlotManager-writeQmd)

- [`QmdPlotManager$addHeader()`](#method-QmdPlotManager-addHeader)

- [`QmdPlotManager$addNewline()`](#method-QmdPlotManager-addNewline)

- [`QmdPlotManager$addNewpage()`](#method-QmdPlotManager-addNewpage)

- [`QmdPlotManager$exportPlotList()`](#method-QmdPlotManager-exportPlotList)

- [`QmdPlotManager$addAndExportFigure()`](#method-QmdPlotManager-addAndExportFigure)

- [`QmdPlotManager$addAndExportTable()`](#method-QmdPlotManager-addAndExportTable)

- [`QmdPlotManager$clone()`](#method-QmdPlotManager-clone)

------------------------------------------------------------------------

### `QmdPlotManager$new()`

Initialize a new instance of the class.

#### Usage

    QmdPlotManager$new(
      qmdfolder,
      qmdName,
      nameOfplotFunction,
      suppressExport = FALSE,
      digitsOfSignificance = 3
    )

#### Arguments

- `qmdfolder`:

  Folder where the .qmd file should be saved.

- `qmdName`:

  A character string for the name of the .qmd file (without extension).

- `nameOfplotFunction`:

  The name of the plot-function as character.

- `suppressExport`:

  A logical value indicating whether to suppress export. Default is
  FALSE.

- `digitsOfSignificance`:

  Number of significant digits to display in tables. \#'

#### Returns

An instance of the QmdPlotManager object.

------------------------------------------------------------------------

### `QmdPlotManager$writeQmd()`

Write the .qmd file.

#### Usage

    QmdPlotManager$writeQmd(fileName = NULL)

#### Arguments

- `fileName`:

  Name of the .qmd file. If NULL, the file name will default to the
  subfolder name.

#### Returns

NULL. This function writes the .qmd file to the specified location and
returns nothing.

------------------------------------------------------------------------

### `QmdPlotManager$addHeader()`

Insert a heading with a specified level.

#### Usage

    QmdPlotManager$addHeader(..., level = 1, newlines = 2)

#### Arguments

- `...`:

  Arguments passed to \`mdPaste\`.

- `level`:

  The header level, i.e., the number of \`#\`s. Defaults to 1.

- `newlines`:

  The number of newlines inserted after the heading. Defaults to 2.

#### Returns

NULL. The function modifies the internal qmd lines.

------------------------------------------------------------------------

### `QmdPlotManager$addNewline()`

Insert line endings and start a new line.

#### Usage

    QmdPlotManager$addNewline(n = 1)

#### Arguments

- `n`:

  Number of new lines. Defaults to 1.

#### Returns

NULL. The function modifies the internal qmd lines.

------------------------------------------------------------------------

### `QmdPlotManager$addNewpage()`

Insert a page break and a newline.

#### Usage

    QmdPlotManager$addNewpage()

#### Returns

NULL. The function modifies the internal qmd lines.

------------------------------------------------------------------------

### `QmdPlotManager$exportPlotList()`

Export a list of plots.

#### Usage

    QmdPlotManager$exportPlotList(plotList)

#### Arguments

- `plotList`:

  A list of plots to export.

#### Returns

NULL. The function exports the plots to the specified location.

------------------------------------------------------------------------

### `QmdPlotManager$addAndExportFigure()`

Add and export a figure with caption and footnote.

#### Usage

    QmdPlotManager$addAndExportFigure(
      plotObject,
      caption,
      figureKey,
      footNoteLines = NULL,
      exportArguments = NULL
    )

#### Arguments

- `plotObject`:

  A ggplot object to export.

- `caption`:

  A character string for the caption text.

- `figureKey`:

  A key used to generate file names; it should be unique for the folder.

- `footNoteLines`:

  A character string for figure footnotes.

- `exportArguments`:

  additional arguments passed on to ospsuite.plots::export

- `...`:

  Additional parameters passed to \`ospsuite.plots::exportPlot()\`.

#### Returns

NULL. The function exports the figure and its metadata.

------------------------------------------------------------------------

### `QmdPlotManager$addAndExportTable()`

Add and export tables with caption and footnote.

#### Usage

    QmdPlotManager$addAndExportTable(
      table,
      caption,
      tableKey,
      footNoteLines = NULL
    )

#### Arguments

- `table`:

  A data.table object to export.

- `caption`:

  A character string for the caption text.

- `tableKey`:

  A key used to generate filenames; it should be unique for the folder.

- `footNoteLines`:

  A character string for table footnotes.

#### Returns

NULL. The function exports the table and its metadata.

------------------------------------------------------------------------

### `QmdPlotManager$clone()`

The objects of this class are cloneable with this method.

#### Usage

    QmdPlotManager$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
