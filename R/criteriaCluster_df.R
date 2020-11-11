#' Create a data frame with scores per criteria cluster per scenario
#'
#' @param weightedEstimates A `weightedEstimates` object.
#' @param estimateCol The column name with the estimates to use.
#' @param parentCriterionOrder The parent criteria to include.
#' @param parentCriterionLabels The labels for the parent criteria.
#' @param scenarioOrder The scenarios to include.
#' @param scenarioLabels The labels for the scenarios.
#' @param sortByScore,decreasing Whether to sort the scenarios by their total
#' scores, and if so, whether to sort them in decreasing order (from the left
#' side) or in increasing order.
#'
#' @return A data frame.
#' @export
criteriaCluster_df <- function(weightedEstimates,
                               estimateCol,
                               parentCriterionOrder = unique(weightedEstimates$parentCriterion_id),
                               parentCriterionLabels = NULL,
                               scenarioOrder = unique(weightedEstimates$scenario_id),
                               scenarioLabels = NULL,
                               sortByScore = FALSE,
                               decreasing = TRUE) {

  scenarioId_col        <- mdmcda::opts$get("scenarioId_col");
  parentCriterionId_col <- mdmcda::opts$get("parentCriterionId_col");
  parentCriterionLabel_col <- mdmcda::opts$get("parentCriterionLabel_col");
  scenarioLabel_col     <- mdmcda::opts$get("scenarioLabel_col");

  if (is.null(parentCriterionLabels)) {
    parentCriterionLabels <-
      stats::setNames(parentCriterionOrder,
                      nm = parentCriterionOrder);
  }

  if (is.null(scenarioLabels)) {
    scenarioLabels <-
      stats::setNames(scenarioOrder,
                      nm = scenarioOrder);
  }

  res <-
    do.call(
      rbind,
      lapply(
        scenarioOrder,
        function(scenario_id) {
          res <-
            aggregate_estimates_by_criterionCluster(
              weightedEstimates[
                weightedEstimates[, scenarioId_col]==scenario_id,
                ,
                drop=FALSE
              ],
              estimateCol);
          res[, scenarioId_col] <- scenario_id;
          return(res);
        }
      )
    );

  res <- res[
    res[, parentCriterionId_col] %in% parentCriterionOrder,
    ,
    drop = FALSE
  ];

  res[, parentCriterionLabel_col] <-
    factor(res[, parentCriterionId_col],
           levels=parentCriterionOrder,
           labels=parentCriterionLabels[parentCriterionOrder],
           ordered=TRUE);

  if (sortByScore) {
    scenarioScores <- scores_by_scenario(
      weightedEstimates = weightedEstimates[
        (weightedEstimates[, scenarioId_col] %in%
           scenarioOrder) &
          (weightedEstimates[, parentCriterionId_col] %in%
             parentCriterionOrder),
        ,
        drop=FALSE
        ],
      estimateCols = estimateCol
    );
    scenarioOrder <-
      scenarioScores[
        order(scenarioScores[, estimateCol],
              decreasing = decreasing),
        scenarioId_col]
  }

  res[, scenarioLabel_col] <-
    factor(res[, scenarioId_col],
           levels=scenarioOrder,
           labels=scenarioLabels[scenarioOrder],
           ordered=TRUE);

  row.names(res) <- NULL;

  return(res);

}
