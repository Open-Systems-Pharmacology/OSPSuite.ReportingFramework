# Convert a .qmd file to Word

Convert a .qmd file to Word

## Usage

``` r
renderWord(
  fileName,
  wordConversionTemplate = NULL,
  customStyles = list(FigureCaption = NULL, FigureFootnote = NULL, TableCaption = NULL,
    TableFootnote = NULL),
  ...
)
```

## Arguments

- fileName:

  Name of a \`.qmd\` file to convert to Word format (\`.docx\`).

- wordConversionTemplate:

  template used for conversion

- customStyles:

  list of custom styles usable for figure and table captions and
  footnotes available list elements for styles are: \`FigureCaption\`,
  \`FigureFootnote\`, \`TableCaption\` and \`TableFootnote\` The
  selected styles should be defined in the \`wordConversionTemplate\`

- ...:

  passed to render

## Value

NULL (invisible).

## Examples

``` r
if (FALSE) { # \dontrun{
# Example with customstyles for Footnotes
renderWord(
  fileName = "myReport.qmd",
  wordConversionTemplate = "path/to/template/myTemplate.docx",
  customStyles = list(FigureFootnote = "myFootnoteFormat", TableFootnote = "myFootnoteFormat")
)
} # }
```
