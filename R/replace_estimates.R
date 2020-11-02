#' @export
replace_estimates <- function(multiEstimateDf,
                              criteria,
                              scorer,
                              transformationFunction,
                              decision = NULL,
                              alternative_value = NULL,
                              criterion = NULL,
                              silent = TRUE,
                              ...) {

  criterionId_col <- mdmcda::opts$get("criterionId_col");
  criterionLabel_col <- mdmcda::opts$get("criterionLabel_col");
  decisionId_col <- mdmcda::opts$get("decisionId_col");
  decisionLabel_col <- mdmcda::opts$get("decisionLabel_col");
  alternativeValue_col <- mdmcda::opts$get("alternativeValue_col");
  alternativeLabel_col <- mdmcda::opts$get("alternativeLabel_col");

  if (!(scorer %in% names(multiEstimateDf))) {
    stop("Specified scorer ('", scorer,
         "') does not exist as a column in the object passed ",
         "as estimates object!");
  }

  decisionSelection <-
    ifelseObj(is.null(decision),
              rep(TRUE, nrow(multiEstimateDf)),
              multiEstimateDf$decision_id==decision);

  alternative_valueSelection <-
    ifelseObj(is.null(alternative_value),
              rep(TRUE, nrow(multiEstimateDf)),
              multiEstimateDf[, alternativeValue_col]==alternative_value);

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
    decisionSelection & alternative_valueSelection & criterionSelection;

  if (!silent) {
    cat0("\n- For decision ",
              vecTxtQ(decision),
              ", alternatives ",
              ifelse(is.null(alternative_value),
                     "*",
                     vecTxtQ(alternative_value)),
              ", and criteria ",
              vecTxtQ(criterionSelectionList),
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
                             alternative_value = alternative_value,
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
                     alternative_value = NULL,
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
                     alternative_value = NULL,
                     ...) {
  criterionMax <-
    anchoringDf[anchoringDf$id %in% criterion,
                'hi_score'];
  return(rep(criterionMax, length(x)));
}
