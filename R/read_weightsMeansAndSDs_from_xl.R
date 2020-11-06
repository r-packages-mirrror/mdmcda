#' Read weightsMeansAndSDs data frame from an Excel file
#'
#' These function reads a weightsMeansAndSDs data frame from an Excel file.
#'
#' @param file The filename.
#' @param idCol,labelCol For `read_labels_from_xl()`, the name of the
#' columns with the identifiers and the labels.
#'
#' @return A data frame.
#'
#' @export
read_weightsMeansAndSDs_from_xl <- function(file) {

  if (!file.exists(file)) {
    stop("You specified file '", file,
         "' to read from, but it does not exist!");
  }

  dat <- openxlsx::read.xlsx(file);

  return(as.data.frame(dat));

}
