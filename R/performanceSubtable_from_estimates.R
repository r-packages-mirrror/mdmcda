#' Produce one or more 'clean' performance subtables
#'
#' These functions take the `estimates`, `criteria`, and `alternativeLabels`
#' objects and produce one or more clean performance subtables.
#'
#' @param estimates The `estimates` object.
#' @param criteria The `criteri` object.
#' @param alternativeLabels The `alternativeLabels` object.
#' @param parentCriterion_id The identifier of the criterion cluster (parent
#' criterion) for which to produce the performance subtable.
#' @param parentCriterionOrder The vector with the parent criterion identifiers
#' to process (in the right order).
#' @param criterionLabels The named vector with criterion (and parent
#' criterion) labels.
#' @param decision_id The identifier of the decisions for which to produce the
#' performance subtable.
#' @param decisionOrder The vector with the decision identifiers to process (in
#' the right order).
#' @param decision_label The decision labels.
#' @param decisionLabels The named vector with decision labels.
#' @param estimateColumn The column with the estimates; this can be changed to,
#' for example, the column with the estimates of a specific scorer, or the
#' weighed estimates, etc.
#' @param humanReadableOnly Whether to strip the columns with the decision
#' identifiers and the alternative values (if `humanReadableOnly=TRUE`).
#'
#' @return For `performanceSubtable_from_estimates()`, a data frame; for
#' `all_performanceSubtables_from_estimates()`, a list of lists of data frames.
#'
#' @rdname performanceSubtable_from_estimates
#' @export
performanceSubtable_from_estimates <- function(estimates,
                                               criteria,
                                               alternativeLabels,
                                               parentCriterion_id,
                                               criterionLabels,
                                               decision_id,
                                               decision_label,
                                               #decision_description,
                                               estimateColumn = "all",
                                               humanReadableOnly = FALSE) {

  criterionId_col <- mdmcda::opts$get("criterionId_col");
  criterionLabel_col <- mdmcda::opts$get("criterionLabel_col");
  decisionId_col <- mdmcda::opts$get("decisionId_col");
  decisionLabel_col <- mdmcda::opts$get("decisionLabel_col");
  decisionDescription_col <- mdmcda::opts$get("decisionDescription_col");
  alternativeValue_col <- mdmcda::opts$get("alternativeValue_col");
  alternativeLabel_col <- mdmcda::opts$get("alternativeLabel_col");

  if (!decision_id %in% names(alternativeLabels)) {
    stop("No `alternativeLabels` for `decision_id` '",
         decision_id, "'!");
  }

  alternatives <-
    alternativeLabels[[decision_id]];
  alternativeLabels <-
    unlist(alternatives);
  alternativeValues <-
    names(alternatives);

  criterionIds <-
    criteria$convenience$childCriterionIds_by_parentId[[parentCriterion_id]];

  res <-
    do.call(
      rbind,
      lapply(
        alternativeValues,
        function(alternative_value) {
          return(
            cbind(
              ### First columns of the data frame
              data.frame(
                decision_id = decision_id,
                alternative_value = alternative_value,
                decision_label = decision_label,
                #decision_description = decision_description,
                alternative_label = alternativeLabels[alternative_value],
                stringsAsFactors = FALSE
              ),
              ### The estimates from the correct rows in the multiEstimateDf
              t(
                estimates$multiEstimateDf[
                  estimates$multiEstimateDf[, decisionId_col] ==
                    decision_id &
                    estimates$multiEstimateDf[, alternativeValue_col] ==
                    alternative_value &
                    estimates$multiEstimateDf[, criterionId_col] %in%
                    criterionIds,
                  estimateColumn
                ]
              )
            )
          );
        }
      )
    );

  names(res) <-
    c(decisionId_col,
      alternativeValue_col,
      decisionLabel_col,
      #decisionDescription_col,
      alternativeLabel_col,
      criterionLabels[criterionIds]
    );

  if (humanReadableOnly) {
    res <- res[
      ,
      c(decisionLabel_col,
        #decisionDescription_col,
        alternativeLabel_col,
        criterionLabels[criterionIds])
    ];
  }

  return(res);
}

#' @export
#' @rdname performanceSubtable_from_estimates
#'
all_performanceSubtables_from_estimates <- function(estimates,
                                                    criteria,
                                                    alternativeLabels,
                                                    parentCriterionOrder,
                                                    criterionLabels,
                                                    decisionOrder,
                                                    decisionLabels,
                                                    #decisionDescriptions,
                                                    estimateColumn = "all",
                                                    humanReadableOnly = FALSE) {

  res <-
    lapply(
      decisionOrder,
      function(decision_id) {
        res <-
          lapply(
            parentCriterionOrder,
            function(parentCriterion_id) {
              return(
                performanceSubtable_from_estimates(
                  estimates = estimates,
                  criteria = criteria,
                  alternativeLabels = alternativeLabels,
                  parentCriterion_id = parentCriterion_id,
                  criterionLabels = criterionLabels,
                  decision_id = decision_id,
                  decision_label = decisionLabels[decision_id],
                  #decision_description = decisionDescriptions[decision_id],
                  estimateColumn = estimateColumn,
                  humanReadableOnly = humanReadableOnly
                )
              );
            }
          );
        names(res) <-
          parentCriterionOrder;
        return(res);
      }
    );
  names(res) <- decisionOrder;

  return(res);

}
