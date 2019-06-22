#' @export
load_performance_tables <- function(input,
                                    extension="xlsx",
                                    regex=NULL,
                                    ignore="empty-performance-table",
                                    sheetIndex=1,
                                    sep=",",
                                    recursive=TRUE,
                                    silent=TRUE,
                                    ...) {

  if (!is.character(input) || !length(input)==1) {
    stop("Only specify a single string as 'input'!");
  }

  if (!dir.exists(input)) {
    stop("Directory provided to read from ('",
         input,
         "') does not exist!");
  }

  if (is.null(regex)) {
    regex <- paste0("^(.*)\\.", extension, "$");
  }

  if (grepl('\\.xls', regex)) {
    if (!requireNamespace("xlsx", quietly = TRUE)) {
      stop("To import from excel format, the \"xlsx\" package is required. ",
           "It needs to be installed and it needs to be able to load ",
           "its dependency the \"rJava\" package. That package can only ",
           "load if it can find where you installed Java. So, you either need ",
           "to install the xlsx package using `install.packages('xlsx');`,",
           "or you need to install Java (make sure to install the version ",
           "matching your R version; so either 32-bit or 64-bit!).");
    }
  }

  ### Get list of files
  performanceTableFiles <-
    list.files(input,
               pattern=regex,
               full.names=TRUE,
               recursive = recursive);

  ### Remove files to ignore
  performanceTableFiles <-
    performanceTableFiles[!grepl(ignore,
                                 performanceTableFiles)];

  if (!silent) {
    cat("\nStarting to process the following list of files:\n\n",
        paste0("- ", performanceTableFiles, "\n"));
  }

  ### Load all performance tables
  res <- list(performance_tables = list());
  for (filename in performanceTableFiles) {
    res$performance_tables[[basename(filename)]] <-
      load_performance_table(filename,
                             sheetIndex=sheetIndex,
                             sep=sep,
                             ...);
  }

  ### Get dimensions of all performance tables
  res$dimensions <-
    lapply(res$performance_tables,
           dim);

  return(invisible(res));
}
