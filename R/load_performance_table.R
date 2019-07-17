#' @export
load_performance_table <- function(file,
                                   sheetIndex=1,
                                   sep=",",
                                   ...) {

  if (!is.character(file) || (length(file) != 1)) {
    stop("Specify only one filename in the 'file' argument!");
  } else if (!file.exists(file)) {
    stop("The file specified as 'file' does not exist!");
  }

  if (grepl('\\.xls', file)) {
    if (!requireNamespace("xlsx", quietly = TRUE)) {
      stop("To import from excel format, the \"xlsx\" package is required. ",
           "It needs to be installed and it needs to be able to load ",
           "its dependency the \"rJava\" package. That package can only ",
           "load if it can find where you installed Java. So, you either need ",
           "to install the xlsx package using `install.packages('xlsx');`,",
           "or you need to install Java (make sure to install the version ",
           "matching your R version; so either 32-bit or 64-bit!).");
    } else {
      res <-
        xlsx::read.xlsx(file=file,
                        sheetIndex=sheetIndex,
                        as.data.frame = TRUE,
                        stringsAsFactors=FALSE,
                        header=FALSE,
                        ...)
    }
  } else {
    res <-
      utils::read.table(file=file,
                        sep=sep,
                        stringsAsFactors=FALSE,
                        header=FALSE,
                        ...);
  }

  class(res) <- c('performance_table', class(res));

  attr(res, "estimatorCode") <- res[1,1];

  return(invisible(res));
}
