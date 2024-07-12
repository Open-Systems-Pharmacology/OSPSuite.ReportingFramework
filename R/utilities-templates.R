#' Opens the workflow template as new document
#'
#' Per default the workflow template of the package is loaded.
#' To customize it save the file with name "template_workflow.R" in your template directory and
#' set the path to the template via option options(OSPSuite.REF.PathForWorkflowTemplate = 'myTemplateDirectory')
#'
#' @export
open_workflow_template <- function() {
  rstudioapi::callFun("sendToConsole", "createWorkflowTemplate()")
}

#' Opens the  template for figure creation as new document
#'
#' @export
open_figure_template <- function() {
  rstudioapi::callFun("sendToConsole", "createDocumentFromTemplate(template = 'template_plot')")
}


#' Opens the workflow template as new document
#'
createWorkflowTemplate <- function() {
  createDocumentFromTemplate(
    template = "template_workflow",
    templatePath = getOption(
      "OSPSuite.REF.PathForWorkflowTemplate",
      default = system.file("templates", package = "OSPSuite.ReportingFramework")
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
                                       templatePath = system.file("templates", package = "OSPSuite.ReportingFramework")) {
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
    package = "OSPSuite.ReportingFramework",
    mustWork = TRUE
  )
}
