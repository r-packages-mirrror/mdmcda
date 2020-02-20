#' @export
replace_estimates <- function(multiEstimateDf,
                              criteria,
                              scorer,
                              transformationFunction,
                              decision = NULL,
                              decision_alternative_value = NULL,
                              criterion = NULL,
                              silent = TRUE,
                              ...) {

  if (!(scorer %in% names(multiEstimateDf))) {
    stop("Specified scorer ('", scorer,
         "') does not exist as a column in the object passed ",
         "as estimates object!");
  }

  decisionSelection <-
    ufs::ifelseObj(is.null(decision),
                   rep(TRUE, nrow(multiEstimateDf)),
                   multiEstimateDf$decision_id==decision);

  decision_alternative_valueSelection <-
    ufs::ifelseObj(is.null(decision_alternative_value),
                   rep(TRUE, nrow(multiEstimateDf)),
                   multiEstimateDf$decision_alternative_value==decision_alternative_value);

  if (all(criterion %in% criteria$convenience$childCriteriaByCluster)) {
    criterionSelectionList <- criterion;
    criterionSelection <-
      multiEstimateDf$criterion_id %in% criterionSelectionList;
  } else if (all(criterion %in% criteria$convenience$parentCriteriaIds)) {
    criterionSelectionList <- unlist(criteria$convenience$childCriteriaIds[[criterion]]);
    criterionSelection <-
      multiEstimateDf$criterion_id %in% criterionSelectionList;
  } else {
    criterionSelection <-
      rep(TRUE, nrow(multiEstimateDf));
    criterionSelectionList <-
      criteria$convenience$childCriteriaByCluster;
  }

  rowsToReplace <-
    decisionSelection & decision_alternative_valueSelection & criterionSelection;

  if (!silent) {
    ufs::cat0("\n- For decision ",
              ufs::vecTxtQ(decision),
              ", alternatives ",
              ifelse(is.null(decision_alternative_value),
                     "*",
                     ufs::vecTxtQ(decision_alternative_value)),
              ", and criteria ",
              ufs::vecTxtQ(criterionSelectionList),
              ", replacing ", sum(rowsToReplace), " estimates.\n");
  }

  for (currentCriterion in criterionSelectionList) {
    multiEstimateDf[rowsToReplace &
                      (multiEstimateDf$criterion_id == currentCriterion),
                    scorer] <-
      transformationFunction(multiEstimateDf[rowsToReplace &
                                               (multiEstimateDf$criterion_id == currentCriterion),
                                             scorer],
                             anchoringDf = criteria$anchoringDf,
                             criterion = currentCriterion,
                             decision = decision,
                             decision_alternative_value = decision_alternative_value,
                             ...);
  }

  return(multiEstimateDf);

}


#' @export
setToZero <- function(x, ...) return(rep(0, length(x)));

#' @export
setToMin <- function(x,
                     anchoringDf,
                     criterion,
                     decision = NULL,
                     decision_alternative_value = NULL,
                     ...) {
  criterionMin <-
    anchoringDf[anchoringDf$id %in% criterion,
                'lo_score'];
  return(rep(criterionMin, length(x)));
}

#' @export
setToMax <- function(x,
                     anchoringDf,
                     criterion,
                     decision = NULL,
                     decision_alternative_value = NULL,
                     ...) {
  criterionMax <-
    anchoringDf[anchoringDf$id %in% criterion,
                'hi_score'];
  return(rep(criterionMax, length(x)));
}
