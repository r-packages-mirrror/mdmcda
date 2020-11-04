#' @export
scores_by_scenario <- function(weighedEstimates,
                               estimateCols) {

  scenarioId_col           <- mdmcda::opts$get("scenarioId_col");

  res <- list();
  for (currentEstimateCol in estimateCols) {
    res[[currentEstimateCol]] <-
      as.data.frame(cbind(by(weighedEstimates[[currentEstimateCol]],
                             weighedEstimates[, scenarioId_col],
                             sum)),
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
