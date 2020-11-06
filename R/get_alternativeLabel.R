#' Get the alternative labels based on a data frame
#'
#' @param x The data frame with a column with the decision identifier and the
#' alternative value.
#' @param alternativeLabels The object with the alternative labels (a named
#' list of named lists).
#' @param decisionId_colalternativeValue_col The names of the columns of the
#' decision identifier and the alternative value in data frame `x`.
#'
#' @return A vector with alternative labels for the alternative values for
#' each decision.
#' @export
get_alternativeLabel <- function(x,
                                 alternativeLabels,
                                 decisionId_col = mdmcda::opts$get("decisionId_col"),
                                 alternativeValue_col = mdmcda::opts$get("alternativeValue_col")) {
  return(
    unlist(
      mapply(
        function(decision_id, alternative_value) {
          return(alternativeLabels[[decision_id]][[alternative_value]])
        },
        x[, decisionId_col],
        as.character(x[, alternativeValue_col])
      )
    )
  );
}
