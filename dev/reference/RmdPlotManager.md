# RmdPlotManager

Manages the creation and writing of .Rmd files for plots.

## Super class

[`ospsuite.utils::Printable`](https://www.open-systems-pharmacology.org/OSPSuite.RUtils/reference/Printable.html)
-\> `RmdPlotManager`

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

- [`RmdPlotManager$new()`](#method-RmdPlotManager-new)

- [`RmdPlotManager$writeRmd()`](#method-RmdPlotManager-writeRmd)

- [`RmdPlotManager$addHeader()`](#method-RmdPlotManager-addHeader)

- [`RmdPlotManager$addNewline()`](#method-RmdPlotManager-addNewline)

- [`RmdPlotManager$addNewpage()`](#method-RmdPlotManager-addNewpage)

- [`RmdPlotManager$exportPlotList()`](#method-RmdPlotManager-exportPlotList)

- [`RmdPlotManager$addAndExportFigure()`](#method-RmdPlotManager-addAndExportFigure)

- [`RmdPlotManager$addAndExportTable()`](#method-RmdPlotManager-addAndExportTable)

- [`RmdPlotManager$clone()`](#method-RmdPlotManager-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new instance of the class.

#### Usage

    RmdPlotManager$new(
      rmdfolder,
      rmdName,
      nameOfplotFunction,
      suppressExport = FALSE,
      digitsOfSignificance = 3
    )

#### Arguments

- `rmdfolder`:

  Folder where the .Rmd file should be saved.

- `rmdName`:

  A character string for the name of the .Rmd file (without extension).

- `nameOfplotFunction`:

  The name of the plot-function as character.

- `suppressExport`:

  A logical value indicating whether to suppress export. Default is
  FALSE.

- `digitsOfSignificance`:

  Number of significant digits to display in tables. \#'

#### Returns

An instance of the RmdPlotManager object.

------------------------------------------------------------------------

### Method `writeRmd()`

Write the .Rmd file.

#### Usage

    RmdPlotManager$writeRmd(fileName = NULL)

#### Arguments

- `fileName`:

  Name of the .Rmd file. If NULL, the file name will default to the
  subfolder name.

#### Returns

NULL. This function writes the .Rmd file to the specified location and
returns nothing.

------------------------------------------------------------------------

### Method `addHeader()`

Insert a heading with a specified level.

#### Usage

    RmdPlotManager$addHeader(..., level = 1, newlines = 2)

#### Arguments

- `...`:

  Arguments passed to \`mdPaste\`.

- `level`:

  The header level, i.e., the number of \`#\`s. Defaults to 1.

- `newlines`:

  The number of newlines inserted after the heading. Defaults to 2.

#### Returns

NULL. The function modifies the internal Rmd lines.

------------------------------------------------------------------------

### Method `addNewline()`

Insert line endings and start a new line.

#### Usage

    RmdPlotManager$addNewline(n = 1)

#### Arguments

- `n`:

  Number of new lines. Defaults to 1.

#### Returns

NULL. The function modifies the internal Rmd lines.

------------------------------------------------------------------------

### Method `addNewpage()`

Insert a page break and a newline.

#### Usage

    RmdPlotManager$addNewpage()

#### Returns

NULL. The function modifies the internal Rmd lines.

------------------------------------------------------------------------

### Method `exportPlotList()`

Export a list of plots.

#### Usage

    RmdPlotManager$exportPlotList(plotList)

#### Arguments

- `plotList`:

  A list of plots to export.

#### Returns

NULL. The function exports the plots to the specified location.

------------------------------------------------------------------------

### Method `addAndExportFigure()`

Add and export a figure with caption and footnote.

#### Usage

    RmdPlotManager$addAndExportFigure(
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

### Method `addAndExportTable()`

Add and export tables with caption and footnote.

#### Usage

    RmdPlotManager$addAndExportTable(
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

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RmdPlotManager$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
