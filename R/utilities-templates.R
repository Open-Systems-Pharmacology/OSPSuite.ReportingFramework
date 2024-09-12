#' Opens the workflow template as new document
#'
#' Per default the workflow template of the package is loaded.
#' To customize it save the file with name "template_workflow.R" in your template directory and
#' set the path to the template via option `options(OSPSuite.RF.PathForWorkflowTemplate = 'myTemplateDirectory'`)
#'
#' @export
openWorkflowTemplate <- function() {
  rstudioapi::callFun("sendToConsole", "ospsuite.reportingframework::createWorkflowTemplate()")
}

#' Opens the  template for figure creation as new document
#'
#' @export
openFigureTemplate <- function() {
  rstudioapi::callFun("sendToConsole", "ospsuite.reportingframework::createDocumentFromTemplate(template = 'template_plot')")
}


#' Opens the workflow template as new document
#'
#' @export
createWorkflowTemplate <- function() {
  ospsuite.reportingframework::createDocumentFromTemplate(
    template = "template_workflow",
    templatePath = getOption(
      "OSPSuite.RF.PathForWorkflowTemplate",
      default = system.file("templates", package = "ospsuite.reportingframework")
    )
  )
}


#' Opens a template file a new document
#'
#' @param template name of template script
#' @param templatePath  path of template script
#'
#' @export
createDocumentFromTemplate <- function(template = "template_workflow",
                                       templatePath = system.file("templates", package = "ospsuite.reportingframework")) {
  templateFile <- file.path(
    templatePath,
    paste0(template, ".R")
  )
  templateContent <-
    readLines(templateFile) # Read the content of the template file
  templateText <-
    paste(templateContent, collapse = "\n") # Concatenate the lines into a single string
  invisible(rstudioapi::documentNew(text = templateText)) # Create a new document in RStudio using the template content
}


#' Returns Path of template directory
#'
#' @return pathname
#' @export
templateDirectory <- function() {
  system.file(
    "templates",
    "TemplateProject",
    package = "ospsuite.reportingframework",
    mustWork = TRUE
  )
}
