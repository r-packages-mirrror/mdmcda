#' @export
replace_estimates <- function(multiEstimateDf,
                              criteria,
                              scorer,
                              transformationFunction,
                              decision_id = NULL,
                              alternative_value = NULL,
                              criterion_id = NULL,
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
    ifelseObj(is.null(decision_id),
              rep(TRUE, nrow(multiEstimateDf)),
              multiEstimateDf[, decisionId_col]==decision_id);

  alternative_valueSelection <-
    ifelseObj(is.null(alternative_value),
              rep(TRUE, nrow(multiEstimateDf)),
              multiEstimateDf[, alternativeValue_col]==alternative_value);

  if (all(criterion_id %in% criteria$convenience$leafCriterionIds)) {
    criterionSelectionList <- criterion_id;
    criterionSelection <-
      multiEstimateDf[, criterionId_col] %in% criterionSelectionList;
  } else if (all(criterion_id %in% criteria$convenience$parentCriteriaIds)) {
    criterionSelectionList <-
      unlist(criteria$convenience$childCriterionIds_by_parentId[[criterion_id]]);
    criterionSelection <-
      multiEstimateDf[, criterionId_col] %in% criterionSelectionList;
  } else {
    criterionSelection <-
      rep(TRUE, nrow(multiEstimateDf));
    criterionSelectionList <-
      criteria$convenience$leafCriterionIds;
  }

  rowsToReplace <-
    decisionSelection & alternative_valueSelection & criterionSelection;

  if (!silent) {
    cat0("\n- For decision ",
              vecTxtQ(decision_id),
              ", alternative ",
              ifelse(is.null(alternative_value),
                     "*",
                     vecTxtQ(alternative_value)),
              ", and criterion ",
              vecTxtQ(criterionSelectionList),
              ", replacing ", sum(rowsToReplace), " estimates.\n");
  }

  for (currentCriterion in criterionSelectionList) {
    multiEstimateDf[rowsToReplace &
                      (multiEstimateDf[, criterionId_col] == currentCriterion),
                    scorer] <-
      transformationFunction(
        multiEstimateDf[rowsToReplace &
                          (multiEstimateDf[, criterionId_col] == currentCriterion),
                        scorer],
        anchoringDf = criteria$anchoringDf,
        criterion_id = currentCriterion,
        decision_id = decision_id,
        alternative_value = alternative_value,
        ...
      );
  }

  return(multiEstimateDf);

}


#' @export
setToZero <- function(x, ...) return(rep(0, length(x)));

#' @export
setToMin <- function(x,
                     anchoringDf,
                     criterion_id,
                     decision_id = NULL,
                     alternative_value = NULL,
                     ...) {
  criterionMin <-
    anchoringDf[anchoringDf$criterion_id %in% criterion_id,
                'lo_score'];
  return(rep(criterionMin, length(x)));
}

#' @export
setToMax <- function(x,
                     anchoringDf,
                     criterion_id,
                     decision_id = NULL,
                     alternative_value = NULL,
                     ...) {
  criterionMax <-
    anchoringDf[anchoringDf$criterion_id %in% criterion_id,
                'hi_score'];
  return(rep(criterionMax, length(x)));
}
