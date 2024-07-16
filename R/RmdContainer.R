#' @title RmdContainer
#' @docType class
#' @description An object storing configuration used project-wide
#' @format NULL
#' @import fs
#' @export
RmdContainer <- R6::R6Class(
  "RmdContainer",
  inherit = ospsuite.utils::Printable,
  cloneable = TRUE,
  public = list(
    #' Initialize
    #'
    initialize = function(rmdfolder,subfolder) {

      private$.rmdfolder <- rmdfolder
      private$.subfolder <- subfolder

      if (!dir.exists( file.path(private$.rmdfolder,private$.subfolder)))
        dir.create( file.path(private$.rmdfolder,private$.subfolder),recursive = TRUE)

      private$.rmdLines <- private$.startRMD(rmdfolder)

    },
    # writeRmd File
    writeRmd = function(fileName){
      checkmate::assertPathForOutput(fileName,extension = 'Rmd',overwrite = TRUE)
      if(basename(fileName) != fileName)
        stop('Please insert fileName as basename, File will be saved in folder defined by class object')

      private$closeFigureKeys()
      writeLines(text =  private$.rmdLines,
                 con = file.path(private$.rmdfolder,fileName),
                 sep = '\n')
    },
    # adds a header line
    addHeader = function(..., level = 1, newlines = 2){
      private$closeFigureKeys()
      tmp <- utils::capture.output(mdHeading(...,level = level,newlines = newlines))
      private$.rmdLines <- append( private$.rmdLines,
                                   tmp)
    },
    # adds a new line
    addNewline = function(..., n){
      private$closeFigureKeys()

      tmp <- utils::capture.output(mdNewline(...,n = 3))
      private$.rmdLines <- append( private$.rmdLines,
                                   tmp)
    },
    # adds a new page
    addNewpage = function(){
      private$closeFigureKeys()

      tmp <- utils::capture.output(mdNewpage())
      private$.rmdLines <- append( private$.rmdLines,
                                   tmp)
    },
    # adds and exports figure with caption and footnote
    addAndExportFigure = function(
      plotObject,
      caption,
      figureKey,
      footNoteLines = NULL
    ){

      figurePath <- file.path(private$.rmdfolder,private$.subfolder)

      ospsuite.plots::exportPlot(plotObject = plotObject,
                                 filepath = figurePath,
                                 filename = paste0(figureKey,'.png'))

      if (!is.null(footNoteLines)){
        writeLines(text = "Footnote1.\n Footnote2",
                   con =  file.path(figurePath,paste0(figureKey,'.footnote')))
      }

      writeLines(text = "This is my caption.",
                 con =  file.path(figurePath,paste0(figureKey,'.caption')))


      private$.keyCollectionIsOpen <- TRUE
      private$.listOfKeys <- append(private$.listOfKeys,figureKey)


    }
  ),

  active = list(),
  private = list(
    # the final rmds in lines
    .rmdLines = c(),
    # folder where rmd is saved
    .rmdfolder = NULL,
    # subfolder of rmdfolder where figures are saved
    .subfolder = NULL,
    # boolean to tell, if last entry was a Key
    .keyCollectionIsOpen = FALSE,
    # boolean to tell, if last entry was a Key
    .listOfKeys = c(),
    # function to initialize rmdLines
    .startRMD = function(rmdfolder){
      return(c(
        '---',
        paste0('title: "Report"'),
        'output:',
        '  word_document',
        '---',
        ' ' ,
        '```{r setup, include=FALSE}',
        'knitr::opts_chunk$set(echo = FALSE,warning = FALSE,results = "asis",error = FALSE,message = FALSE)',
        '```',
        ' ',
        '```{r}',
        'dev <- ospsuite.plots::getOspsuite.plots.option(optionKey = ospsuite.plots::OptionKeys$export.device)',
        ' ',
        'numbersOf <- list(figures = 0,',
        '         tables = 0)',
        '```'))
    },
    # switches keyCollectionIsOpen to FALSE, and call functions for figure settings
    closeFigureKeys = function(){
      if (!private$.keyCollectionIsOpen) return()

      tmp <- c(
        '```{r}',
        paste0('keyList <- c("',paste(private$.listOfKeys,collapse = '",\n"'),'")'),
        ' ',
        'numbersOf <- addFiguresAndTables(keyList = keyList,',
        paste0('            subfolder = "',private$.subfolder,'",'),
        '            numbersOf = numbersOf)',
        '```'
      )
      private$.rmdLines <- append( private$.rmdLines,
                                   tmp)

      private$.keyCollectionIsOpen <- FALSE
      private$.listOfKeys <- c()

      return(invisible())

    }
  )
)

