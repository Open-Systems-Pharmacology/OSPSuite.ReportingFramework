#' Opens the workflow template as new document
#'
#' Per default the workflow template of the package is loaded.
#' To customize it save the file with name "template_workflow.R" in your template directory and
#' set the path to the template via option `options(OSPSuite.RF.PathForWorkflowTemplate = 'myTemplateDirectory')`
#'
#' @return Invisibly returns `NULL` after triggering the template command in RStudio.
#' @export
#' @family Addins Support
openWorkflowTemplate <- function() {
  rstudioapi::callFun(
    "sendToConsole",
    "ospsuite.reportingframework::createWorkflowTemplate()"
  )

  return(invisible(NULL))
}

#' Opens the  template for figure creation as new document
#'
#' @return Invisibly returns `NULL` after triggering the template command in RStudio.
#' @export
#' @family Addins Support
openFigureTemplate <- function() {
  rstudioapi::callFun(
    "sendToConsole",
    "ospsuite.reportingframework::createDocumentFromTemplate(template = 'template_plot.R')"
  )

  return(invisible(NULL))
}

#' Opens the  template for figure creation as new document
#'
#' @return Invisibly returns `NULL` after triggering the template command in RStudio.
#' @export
#' @family Addins Support
openEPackageTemplate <- function() {
  rstudioapi::callFun(
    "sendToConsole",
    "ospsuite.reportingframework::createDocumentFromTemplate(template = 'template_ePackageWorkflow.Rmd')"
  )

  return(invisible(NULL))
}


#' Opens the workflow template as new document
#'
#' @return Invisibly returns a new RStudio document object.
#' @export
#' @family Addins Support
createWorkflowTemplate <- function() {
  return(invisible(ospsuite.reportingframework::createDocumentFromTemplate(
    template = "template_workflow.R",
    templatePath = getOption(
      "OSPSuite.RF.PathForWorkflowTemplate",
      default = system.file(
        "templates",
        package = "ospsuite.reportingframework"
      )
    )
  )))
}


#' Opens a template file a new document
#'
#' @param template name of template script
#' @param templatePath  path of template script
#' @return Invisibly returns a new RStudio document object.
#'
#' @export
#' @family Addins Support
createDocumentFromTemplate <- function(
  template = "template_workflow",
  templatePath = system.file(
    "templates",
    package = "ospsuite.reportingframework"
  )
) {
  templateFile <- file.path(
    templatePath,
    template
  )
  type <- switch(fs::path_ext(template), "R" = "r", "Rmd" = "rmarkdown")
  templateContent <-
    readLines(templateFile) # Read the content of the template file
  templateText <-
    paste(templateContent, collapse = "\n")
  return(invisible(rstudioapi::documentNew(
    text = templateText,
    type = type
  )))
}
