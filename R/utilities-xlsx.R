#' adds a new sheet to a workbook
#'
#' wrappes openxlsx::addWorksheet, and adds new data to the sheet
#'
#' @inheritParams openxlsx::addWorksheet
#' @param dt `data.table`  to attach
#'
#' @export
xlsxAddSheet <- function(wb, sheetName, dt) {
  if (sheetName %in% wb$sheet_names) {
    warning(paste(sheetName, "already exist"))
    #clear sheet by reading
    invisible(xlsxReadData(wb, sheetName))
  } else {
    openxlsx::addWorksheet(wb = wb, sheetName = sheetName)
  }
  openxlsx::writeData(wb = wb, sheet = sheetName, x = dt)

  return(invisible())
}

#' adds data to worksheet
#'
#' wrappes openxlsx::writeData, but delete before all current content
#'
#' @inheritParams openxlsx::writeData
#' @param dt `data.table` to write
#'
#' @export
xlsxWriteData <- function(wb, sheetName, dt) {
  # clear existing
  tmp <- xlsxReadData(wb, sheetName = sheetName)
  tmp[, names(tmp) := NA]
  openxlsx::writeData(wb = wb, sheet = sheetName, x = tmp)

  openxlsx::writeData(wb = wb, sheet = sheetName, x = dt)

  return(invisible())
}

#' create a clone of a sheet and set new contet
#'
#' wrappes openxlsx::cloneWorksheet but set new content
#'
#' @inheritParams openxlsx::cloneWorksheet
#' @param dt `data.table` with new content
#'
#' @return workbook with cloned sheet
xlsxCloneAndSet <- function(wb, clonedSheet, sheetName, dt) {
  if (!(sheetName %in% wb$sheet_names)) {
    openxlsx::cloneWorksheet(wb = wb, clonedSheet = clonedSheet, sheetName = sheetName)
  }

  # clear data by reading
  invisible(xlsxReadData(wb = wb, sheetName = sheetName))

  xlsxWriteData(wb = wb, sheetName = sheetName, dt = dt)

  return(wb)
}



#' reads data
#'
#' wrappes openxlsx::read.xlsx but returns data as data.table()
#'
#'
#' @param wb workbook or xlsxfile
#' @param sheetName sheet name
#'
#' @return `data.table` with sheet data
#' @export
xlsxReadData <- function(wb, sheetName) {
  dt <- data.table::setDT(openxlsx::read.xlsx(
    xlsxFile = wb,
    sheet = sheetName,
    sep.names = " ",
    skipEmptyRows = TRUE
  ))

  return(dt)
}
