#' Load estimates from an Excel file
#'
#' This function loads estimates that have been exported
#' from an Excel file. For loading estimates from
#' completed performance tables, use [mdmcda::load_performance_tables()].
#'
#' @param file The filename.
#' @param multiEstimateDf Whether the file contains an exported
#' `multiEstimateDf`.
#'
#' @return The object with the estimates, containing the spreadsheet in
#' `$multiEstimateDf`.
#'
#' @export
load_estimates_from_xl <- function(file,
                                   multiEstimateDf = TRUE) {

  dat <- openxlsx::read.xlsx(file);

  res <- list(
    performance_subtables = NULL,
    confidences = NULL,
    estimateCoders = NULL,
    estimatorCodeVector = NULL,
    estimatorCodes = NULL,
    estimates = NULL,
    multiEstimateLegend = NULL,
    multiEstimateInverseLegend = NULL,
    multiEstimateDf = NULL,
    mergedConfidences = NULL,
    processedConfidences = NULL,
    alternativeValues = NULL,
    collapsedConfidences = NULL,
    confidencesByDecisionPlot = NULL,
    confidencesByCriterionPlot = NULL,
    confidencesInDetail = NULL,
    originalAlternativeValues = NULL
  );

  if (multiEstimateDf) {

    res$multiEstimateDf <- dat;

  } else {
    stop("No other importing functionality added yet, sorry!");
  }

  return(res);

}
