#' Aggregate estimates by criterion clusters (parent criteria)
#'
#' @param multiEstimateDf A `multiEstimateDf` data frame.
#' @param estimateCol The column with the estimates to use.
#' @param parentCriterionId_col The columns containing the parent criterion
#' identifiers (i.e. the cluster identifiers).
#' @param fun The function to use to aggregate the scores.
#' @param ... Any additional arguments are passed to `fun`.
#'
#' @return A data frame with aggregated estimates.
#' @export
aggregate_estimates_by_criterionCluster <- function(multiEstimateDf,
                                                    estimateCol,
                                                    parentCriterionId_col = mdmcda::opts$get("parentCriterionId_col"),
                                                    fun = sum,
                                                    ...) {
  res <-
    do.call(rbind,
            lapply(by(multiEstimateDf[, estimateCol],
                      multiEstimateDf[, parentCriterionId_col],
                      fun,
                      ...),
                   as.data.frame));
  names(res) <- estimateCol;
  res[, parentCriterionId_col] <- row.names(res);
  return(res);
};
