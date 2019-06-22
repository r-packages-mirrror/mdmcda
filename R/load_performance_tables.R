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

  ### Store estimate dataframes
  res$estimates <-
    lapply(res$performance_tables,
           estimates_from_performance_table);

  ### Combine estimates is one dataframe
  res$multiEstimateLegend <-
    names(res$estimates);
  names(res$multiEstimateLegend) <-
    paste0("value_", seq_along(res$multiEstimateLegend));
  res$multiEstimateInverseLegend <-
    paste0("value_", seq_along(res$multiEstimateLegend));
  names(res$multiEstimateInverseLegend) <-
    names(res$estimates);

  res$multiEstimateDf <- data.frame();
  for (i in res$multiEstimateLegend) {
    for (j in nrow(res$estimates[[i]])) {
      newRowNr <-
        nrow(res$multiEstimateDf) + 1;
      res$multiEstimateDf[newRowNr, 'decision_id'] <-
        res$estimates[[i]][j, 'decision_id'];
      res$multiEstimateDf[newRowNr, 'decision_label'] <-
        res$estimates[[i]][j, 'decision_label'];
      res$multiEstimateDf[newRowNr, 'decision_alternative_value'] <-
        res$estimates[[i]][j, 'decision_alternative_value'];
      res$multiEstimateDf[newRowNr, 'decision_alternative_label'] <-
        res$estimates[[i]][j, 'decision_alternative_label'];
      res$multiEstimateDf[newRowNr, 'criterion_id'] <-
        res$estimates[[i]][j, 'criterion_id'];
      res$multiEstimateDf[newRowNr, 'criterion_label'] <-
        res$estimates[[i]][j, 'criterion_label'];
      res$multiEstimateDf[newRowNr, res$multiEstimateInverseLegend[i]] <-
        res$estimates[[i]][j, 'value'];
    }
  }

  ### Get consensus matrix


  return(invisible(res));
}
