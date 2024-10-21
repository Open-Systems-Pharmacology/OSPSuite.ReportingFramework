#' Title
#'
#' @param projectConfiguration
#'
#' @return
#' @export
#'
#' @examples
mockManualEditings.PKParameter <- function(projectConfiguration){


  # add all data files which are used in the project
  wb <- openxlsx::loadWorkbook(projectConfiguration$addOns$pKParameterFile)

  dtTemplate <- xlsxReadData(wb = wb,sheetName = 'Template')

  dtTemplate <- dtTemplate[c(1,which(dtTemplate$name %in% c("C_max","AUC_inf")))]
  dtTemplate[name %in% c("C_max","AUC_inf"),outputPathIds := "Concentration"]

  xlsxCloneAndSet(wb = wb,clonedSheet = 'Template', sheetName  = 'PK_Plasma', dt = dtTemplate)


  dtTemplate <- xlsxReadData(wb = wb,sheetName = 'Template')

  dtTemplate <- dtTemplate[c(1,which(dtTemplate$name %in% c("F_tEnd")))]
  dtTemplate[name %in% c("F_tEnd"),outputPathIds := "Fraction"]

  xlsxCloneAndSet(wb = wb,clonedSheet = 'Template', sheetName  = 'PK_Fraction', dt = dtTemplate)


  openxlsx::saveWorkbook(wb, projectConfiguration$addOns$pKParameterFile, overwrite = TRUE)
}





