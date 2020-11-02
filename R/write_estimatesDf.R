#' @export
write_estimatesDf <- function(estimatesDf,
                              file,
                              cols=c('decision_label',
                                     'alternative_label',
                                     'criterion_label',
                                     'value',
                                     'label',
                                     'description',
                                     'criterion_id',
                                     'decision_id',
                                     'alternative_value'),
                              overwrite = FALSE,
                              sep = ",",
                              encoding="UTF-8",
                              silent=TRUE,
                              ...) {

  if (!is.data.frame(estimatesDf)) {
    stop("As 'estimatesDf', you have to pass a dataframe with estimates!");
  }

  if (is.null(cols)) {
    cols <- names(estimatesDf);
  }

  if (!all(cols %in% names(estimatesDf))) {
    stop("The dataframe suppied as 'estimatesDf' does not contain all ",
         "specified columns (",
         vecTxtQ(cols),
         ").");
  }

  if (!dir.exists(dirname(file))) {
    stop("The directory specified where to write the file ('",
         dirname(file), "') does not exist!");
  }

  ext <- sub("^.*\\.(.*)$",
             "\\1",
             file);

  wantsXls <- grepl('xls', ext);

  if (!('performance_table' %in% class(performance_table))) {
    stop("As argument 'performance_table', you have to provide a ",
         "performance table (sorry if that was not clear :-)). You ",
         "provided an object with class ", vecTxtQ(performance_table),
         ".");
  }

  if (wantsXls) {
    if (!requireNamespace("xlsx", quietly = TRUE)) {
      stop("To export to excel format, the \"xlsx\" package is required. ",
           "It needs to be installed and it needs to be able to load ",
           "its dependency the \"rJava\" package. That package can only ",
           "load if it can find where you installed Java. So, you either need ",
           "to install the xlsx package using `install.packages('xlsx');`,",
           "or you need to install Java (make sure to install the version ",
           "matching your R version; so either 32-bit or 64-bit!).");
    } else {
      writeFun <- function(performance_table,
                           file) {
        xlsx::write.xlsx(performance_table,
                         file = file,
                         col.names = TRUE,
                         row.names = FALSE,
                         append = FALSE,
                         ...);
      }
    }
  } else {
    writeFun <- function(performance_table,
                         file) {
      utils::write.table(performance_table,
                         file = file,
                         col.names = TRUE,
                         row.names = FALSE,
                         append = FALSE,
                         sep=sep,
                         ...);
    }
  }

  if (!file.exists(file) || overwrite) {
    writeFun(estimatesDf[, cols],
             file=file);
  }

  return(invisible(estimatesDf));

}
