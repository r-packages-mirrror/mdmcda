#' Get the aggregated scores for each scenario
#'
#' @param weightedEstimates The `weightedEstimates` object as created by
#' [mdmcda::build_weighted_estimate_df()] and filled with the desired
#' weighted estimates by [mdmcda::weight_estimates_by_profile()].
#' @param estimateCols The column name(s) of the estimates to aggregate using
#' function `fun`
#' @param fun The function to use for the aggregation.
#' @param ... Additional arguments are passed to `fun`.
#'
#' @return A data frame with the scenario identifiers and the aggregated
#' scores.
#' @export
scores_by_scenario <- function(weightedEstimates,
                               estimateCols,
                               fun = sum,
                               ...) {

  scenarioId_col           <- mdmcda::opts$get("scenarioId_col");

  res <- list();
  for (currentEstimateCol in estimateCols) {
    res[[currentEstimateCol]] <-
      as.data.frame(cbind(by(weightedEstimates[[currentEstimateCol]],
                             weightedEstimates[, scenarioId_col],
                             fun,
                             ...)),
                    stringsAsFactors = FALSE);
    res[[currentEstimateCol]] <-
      cbind(row.names(res[[currentEstimateCol]]),
            res[[currentEstimateCol]],
            stringsAsFactors = FALSE);
    names(res[[currentEstimateCol]]) <-
      c("scenario_id", currentEstimateCol);
    row.names(res[[currentEstimateCol]]) <- NULL;
  }
  if (length(res) == 1) {
    return(res[[1]]);
  } else {
    return(res);
  }
}
