#' Set postsets
#'
#' Postsets are specifications of estimates in a non-performance-table
#' format that can be applied to a `multiEstimateDf`.
#'
#' @param multiEstimateDf The `multiEstimateDf` to set the postsets in.
#' @param postsets The data frame with the postsets. This data frame must
#' have at least the four columns specified by the other four arguments.
#' @param coder The name of the column containing the estimates to replace
#' (typically estimated by a particular coder).
#' @param decisionId_col,criterionId_col,alternativeValue_col The names of
#' the columns with the decision and criterion identifiers and the alternative
#' value to which each estimate by `coder` pertains.
#'
#' @return The `multiEstimateDf` with replaced postset estimates.
#' @export
set_postsets <- function(multiEstimateDf,
                         postsets,
                         coder,
                         decisionId_col = mdmcda::opts$get("decisionId_col"),
                         criterionId_col = mdmcda::opts$get("criterionId_col"),
                         alternativeValue_col = mdmcda::opts$get("alternativeValue_col")) {

  for (i in 1:nrow(postsets)) {
    if (!is.na(postsets[i, coder])) {
      estimatesRowNr <-
        which(multiEstimateDf[, decisionId_col]==postsets[i, decisionId_col] &
                multiEstimateDf[, criterionId_col]==postsets[i, criterionId_col] &
                multiEstimateDf[, alternativeValue_col]==postsets[i, alternativeValue_col]);
      cat("\n- replacing estimate for the effect of alternative ",
          postsets[i, alternativeValue_col], " for decision ",
          postsets[i, decisionId_col], " on criterion ",
          postsets[i, criterionId_col], ", which was ",
          multiEstimateDf[estimatesRowNr, coder],
          ", with ", postsets[i, coder], ".\n", sep="");
      multiEstimateDf[estimatesRowNr, coder] <-
        postsets[i, coder];
    }
  }
  return(multiEstimateDf);
}
