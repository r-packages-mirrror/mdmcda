#' @export
write_performance_table <- function(performance_table,
                                    filename,
                                    overwrite = FALSE,
                                    ...) {

  if (!('performance_table' %in% class(performance_table))) {
    stop("As argument 'performance_table', you have to provide a ",
         "performance table (sorry if that was not clear :-)). You ",
         "provided an object with class ", vecTxtQ(performance_table),
         ".");
  }

  if (!is.character(filename) || (length(filename) != 1)) {
    stop("Specify only one filename in the 'filename' argument!");
  } else if (!dir.exists(dirname(filename))) {
    stop("The directory specified where to write the file ('",
         dirname(filename), "') does not exist!");
  } else if (file.exists(filename) && !overwrite) {
    stop("A file with the specified filename ('",
         filename, "' already exists, and argument 'overwrite' is ",
         "set to FALSE!");
  }

  if (grepl('\\.xls', filename)) {
    if (!requireNamespace("xlsx", quietly = TRUE)) {
      stop("To export to excel format, the \"xlsx\" package is required. ",
           "Please install it using `install.packages('xlsx');`.",
           call. = FALSE);
    } else {
      writeFunction <- xlsx::write.xlsx;
    }
  } else {
    writeFunction <- utils::write.table;
  }

  writeFunction(performance_table,
                col.names = FALSE,
                row.names = FALSE,
                append = FALSE,
                ...);

  return(invisible(performance_table));
}
