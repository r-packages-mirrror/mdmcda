#' @export
load_performance_table <- function(file,
                                   estimatesSheet="Estimates",
                                   confidencesSheet = "Confidences",
                                   sep=",",
                                   ...) {

  if (!is.character(file) || (length(file) != 1)) {
    stop("Specify only one filename in the 'file' argument!");
  } else if (!file.exists(file)) {
    stop("The file specified as 'file' does not exist!");
  }

  res <- list();

  if (grepl('\\.xls', file)) {
    if (!requireNamespace("XLConnect", quietly = TRUE)) {
      #if (!requireNamespace("xlsx", quietly = TRUE)) {
      stop("To import from excel format, the \"XLConnect\" package is required. ",
           "It needs to be installed and it needs to be able to load ",
           "its dependency the \"rJava\" package. That package can only ",
           "load if it can find where you installed Java. So, you either need ",
           "to install the XLConnect package using `install.packages('XLConnect');`,",
           "or you need to install Java (make sure to install the version ",
           "matching your R version; so either 32-bit or 64-bit!).");
    } else {
      res$estimates <-
        XLConnect::readWorksheetFromFile(file = file,
                                         sheet = estimatesSheet,
                                         header=FALSE,
                                         ...);
      res$confidences <-
        XLConnect::readWorksheetFromFile(file = file,
                                         sheet = confidencesSheet,
                                         header=TRUE,
                                         ...);

      # res <-
      #   xlsx::read.xlsx(file=file,
      #                   sheetIndex=sheetIndex,
      #                   as.data.frame = TRUE,
      #                   stringsAsFactors=FALSE,
      #                   header=FALSE,
      #                   ...)
    }
  } else {
    res$estimates <-
      utils::read.table(file=file,
                        sep=sep,
                        stringsAsFactors=FALSE,
                        header=FALSE,
                        ...);
  }

  ### Convert confidences to numeric
  tryCatch({
    res$confidences$Confidence <-
      ufs::convertToNumeric(res$confidences$Confidence);
  }, error = function(e) {
    ufs::cat0("Could not convert the Confidences in '",
              basename(file), "' to numeric values! All ",
              "non-NA Confidence values in that file are: ",
              ufs::vecTxtQ(res$confidences$Confidence[!is.na(res$confidences$Confidence)]),
              ".");
  });

  class(res) <- c("performance_table_and_confidences", class(res));

  class(res$estimates) <- c('performance_table', class(res$estimates));

  attr(res, "estimatorCode") <- res$estimates[1,1];
  attr(res, "confidencesMean") <- mean(res$confidences$Confidence, na.rm=TRUE);
  attr(res, "confidencesVar") <- var(res$confidences$Confidence, na.rm=TRUE);

  return(invisible(res));
}
