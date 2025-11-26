# converts .Rmd file to word

converts .Rmd file to word

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

  name of .Rmd file to convert to word format (".docx")

- wordConversionTemplate:

  template used for conversion

- customStyles:

  list of custom styles usable for figure and table captions and
  footnotes available list elements for styles are: \`FigureCaption\`,
  \`FigureFootnote\`, \`TableCaption\` and \`TableFootnote\` The
  selected styles should be defined in the \`wordConversionTemplate\`

- ...:

  passed to render

## Examples

``` r
if (FALSE) { # \dontrun{
# Example with customstyles for Footnotes
renderWord(
  fileName = "myReport.Rmd",
  wordConversionTemplate = "path/to/template/myTemplate.docx",
  customStyles = list(FigureFootnote = "myFootnoteFormat", TableFootnote = "myFootnoteFormat")
)
} # }
```
