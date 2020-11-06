#' @export
read_performance_table <- function(file,
                                   estimatesSheet="Estimates",
                                   confidencesSheet = "Confidences",
                                   sep=",",
                                   ...) {

  criterionId_col          <- mdmcda::opts$get("criterionId_col");
  criterionLabel_col       <- mdmcda::opts$get("criterionLabel_col");
  criterionDescription_col <- mdmcda::opts$get("criterionDescription_col");
  parentCriterionId_col    <- mdmcda::opts$get("parentCriterionId_col");
  decisionId_col           <- mdmcda::opts$get("decisionId_col");
  decisionLabel_col        <- mdmcda::opts$get("decisionLabel_col");
  alternativeValue_col     <- mdmcda::opts$get("alternativeValue_col");
  alternativeLabel_col     <- mdmcda::opts$get("alternativeLabel_col");
  scenarioId_col           <- mdmcda::opts$get("scenarioId_col");
  weightProfileId_col      <- mdmcda::opts$get("weightProfileId_col");
  score_col                <- mdmcda::opts$get("score_col");
  leafCriterion_col        <- mdmcda::opts$get("leafCriterion_col");
  rootCriterionId          <- mdmcda::opts$get("rootCriterionId");

  if (!is.character(file) || (length(file) != 1)) {
    stop("Specify only one filename in the 'file' argument!");
  } else if (!file.exists(file)) {
    stop("The file specified as 'file' does not exist!");
  }

  res <- list();

  if (grepl('\\.xls', file)) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("To import from excel format, the \"openxlsx\" package is required.");
    } else {
      suppressMessages(
        res$estimates <-
          as.data.frame(openxlsx::read.xlsx(file,
                                           sheet = estimatesSheet,
                                           colNames = FALSE),
                        stringsAsFactors = FALSE)
      )
      res$confidences <-
        as.data.frame(openxlsx::read.xlsx(file,
                                         sheet = confidencesSheet,
                                         colNames = TRUE),
                      stringsAsFactors = FALSE);
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
      convertToNumeric(res$confidences$Confidence);
  }, error = function(e) {
    cat0("Could not convert the Confidences in '",
              basename(file), "' to numeric values! All ",
              "non-NA Confidence values in that file are: ",
              vecTxtQ(res$confidences$Confidence[!is.na(res$confidences$Confidence)]),
              ".");
  });

  class(res) <- c("performance_table_and_confidences", class(res));

  class(res$estimates) <- c('performance_table', class(res$estimates));

  attr(res, "estimatorCode") <- res$estimates[1,1];
  attr(res, "confidencesMean") <- mean(res$confidences$Confidence, na.rm=TRUE);
  attr(res, "confidencesVar") <- var(res$confidences$Confidence, na.rm=TRUE);

  return(invisible(res));
}
