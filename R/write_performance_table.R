#' @export
write_performance_table <- function(performance_table,
                                    file,
                                    overwrite = FALSE,
                                    sep = ",",
                                    ...) {

  if (!('performance_table' %in% class(performance_table))) {
    stop("As argument 'performance_table', you have to provide a ",
         "performance table (sorry if that was not clear :-)). You ",
         "provided an object with class ", vecTxtQ(performance_table),
         ".");
  }

  if (!is.character(file) || (length(file) != 1)) {
    stop("Specify only one filename in the 'file' argument!");
  } else if (!dir.exists(dirname(file))) {
    stop("The directory specified where to write the file ('",
         dirname(file), "') does not exist!");
  } else if (file.exists(file) && !overwrite) {
    stop("A file with the specified filename ('",
         file, "' already exists, and argument 'overwrite' is ",
         "set to FALSE!");
  }

  if (grepl('\\.xls', file)) {
    if (!requireNamespace("xlsx", quietly = TRUE)) {
      stop("To export to excel format, the \"xlsx\" package is required. ",
           "It needs to be installed and it needs to be able to load ",
           "its dependency the \"rJava\" package. That package can only ",
           "load if it can find where you installed Java. So, you either need ",
           "to install the xlsx package using `install.packages('xlsx');`,",
           "or you need to install Java (make sure to install the version ",
           "matching your R version; so either 32-bit or 64-bit!).");
    } else {
      xlsx::write.xlsx(performance_table,
                       file = file,
                       col.names = FALSE,
                       row.names = FALSE,
                       append = FALSE,
                       ...);
    }
  } else {
    utils::write.table(performance_table,
                       file = file,
                       col.names = FALSE,
                       row.names = FALSE,
                       append = FALSE,
                       sep=sep,
                       ...);
  }

  return(invisible(performance_table));
}
