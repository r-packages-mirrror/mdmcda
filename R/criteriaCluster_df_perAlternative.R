#' Create a data frame with scores per criteria cluster per alternative
#'
#' @param multiEstimateDf A `multiEstimateDf` data frame.
#' @param estimateCol The column name with the estimates to use.
#' @param parentCriterionOrder The parent criteria to include.
#' @param parentCriterionLabels The labels for the parent criteria.
#' @param decisionOrder The decisions to include.
#' @param decisionLabels The labels for the decisions.
#'
#' @return A data frame.
#' @export
criteriaCluster_df_perAlternative <- function(multiEstimateDf,
                                              estimateCol,
                                              criteria,
                                              alternativeLabels,
                                              parentCriterionOrder = unique(weightedEstimates$parentCriterion_id),
                                              parentCriterionLabels = NULL,
                                              decisionOrder = unique(weightedEstimates$decision_id),
                                              decisionLabels = NULL) {

  scenarioId_col <-           mdmcda::opts$get("scenarioId_col");
  parentCriterionId_col <-    mdmcda::opts$get("parentCriterionId_col");
  parentCriterionLabel_col <- mdmcda::opts$get("parentCriterionLabel_col");
  decisionId_col <-           mdmcda::opts$get("decisionId_col");
  criterionId_col <-          mdmcda::opts$get("criterionId_col");
  alternativeValue_col <-     mdmcda::opts$get("alternativeValue_col");
  alternativeLabel_col <-     mdmcda::opts$get("alternativeLabel_col");
  decisionLabel_col <-        mdmcda::opts$get("decisionLabel_col");
  criterionLabel_col <-       mdmcda::opts$get("criterionLabel_col");

  if (is.null(parentCriterionLabels)) {
    parentCriterionLabels <-
      stats::setNames(parentCriterionOrder,
                      nm = parentCriterionOrder);
  }

  if (is.null(decisionLabels)) {
    decisionLabels <-
      stats::setNames(decisionOrder,
                      nm = decisionOrder);
  }

  res <-
    rbind_df_list(
      unlist(
        lapply(
          decisionOrder,
          function(currentDecision_id) {
            return(
              lapply(
                names(alternativeLabels[[currentDecision_id]]),
                function(currentAlternative_value) {
                  tmpDf <-
                    unique(
                      multiEstimateDf[
                        multiEstimateDf[, decisionId_col] == currentDecision_id &
                          multiEstimateDf[, alternativeValue_col] == currentAlternative_value,
                        c(decisionId_col,
                          alternativeValue_col,
                          criterionId_col,
                          estimateCol
                        )
                      ]
                    );
                  tmpDf[, parentCriterionId_col] <-
                    criteria$convenience$parentCriterionIds_by_childId[
                      tmpDf[, criterionId_col]
                    ]
                  tmpDf <-
                    mdmcda::aggregate_estimates_by_criterionCluster(
                      tmpDf,
                      estimateCol = estimateCol,
                      parentCriterionOrder = parentCriterionOrder
                    );

                  tmpDf[, decisionId_col] <-
                    currentDecision_id;
                  tmpDf[, alternativeValue_col] <-
                    currentAlternative_value;
                  tmpDf[, decisionLabel_col] <-
                    decisionLabels[currentDecision_id];

                  tmpDf[, alternativeLabel_col] <-
                    alternativeLabels[[currentDecision_id]][[currentAlternative_value]];

                  tmpDf[, parentCriterionLabel_col] <-
                    parentCriterionLabels[tmpDf[, parentCriterionId_col]];

                  tmpDf <-
                    tmpDf[
                      ,
                      c(decisionId_col,
                        alternativeValue_col,
                        parentCriterionId_col,
                        decisionLabel_col,
                        alternativeLabel_col,
                        parentCriterionLabel_col,
                        estimateCol)
                    ]
                  return(
                    tmpDf
                  );
                }
              )
            );
          }
        ),
        recursive = FALSE
      )
    );

  row.names(res) <- NULL;

  return(res);

}
