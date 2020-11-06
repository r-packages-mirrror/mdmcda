#' @export
criteriaCluster_df <- function(weighedEstimates,
                               estimateCol,
                               parentCriterion_ids = unique(weighedEstimates$parentCriterion_id),
                               parentCriterion_labels = NULL,
                               scenario_ids = unique(weighedEstimates$scenario_id),
                               scenario_labels = NULL) {

  scenarioId_col           <- mdmcda::opts$get("scenarioId_col");

  if (is.null(parentCriterion_labels)) {
    parentCriterion_labels <-
      stats::setNames(parentCriterion_ids,
                      nm = parentCriterion_ids);
  }

  if (is.null(scenario_labels)) {
    scenario_labels <-
      stats::setNames(scenario_ids,
                      nm = scenario_ids);
  }

  res <-
    do.call(rbind,
            lapply(scenario_ids,
                   function(scenario_id) {
                     res <-
                       aggregate_estimates_by_criterionCluster(
                         weighedEstimates[weighedEstimates[, scenarioId_col]==scenario_id, ],
                         estimateCol);
                     res[, scenarioId_col] <- scenario_id;
                     return(res);
                   }));

  res$parentCriterion_id <- as.character(res$parentCriterion_id);

  res <- res[
    res$parentCriterion_id %in% parentCriterion_ids,
  ];

  res$parentCriterion_label <-
    factor(res$parentCriterion_id,
           levels=parentCriterion_ids,
           labels=parentCriterion_labels[parentCriterion_ids],
           ordered=TRUE);

  res$scenario_id <- as.character(res$scenario_id);

  res$scenario_label <-
    factor(res$scenario_id,
           levels=scenario_ids,
           labels=scenario_labels[scenario_ids],
           ordered=TRUE);

  row.names(res) <- NULL;

  return(res);

}
