#' Aggregate estimates by criterion
#'
#' @param multiEstimateDf A `multiEstimateDf` data frame.
#' @param estimateCol The column with the estimated to use.
#' @param criterionId_col The columns containing the criterion identifiers.
#' @param fun The function to use to aggregate the scores.
#' @param ... Any additional arguments are passed to `fun`.
#'
#' @return A data frame with aggregated estimates.
#' @export
aggregate_estimates_by_criterion <- function(multiEstimateDf,
                                             estimateCol,
                                             criterionId_col = mdmcda::opts$get("criterionId_col"),
                                             fun = sum,
                                             ...) {
  res <-
    do.call(rbind,
            lapply(by(multiEstimateDf[[estimateCol]],
                      multiEstimateDf[[criterionId_col]],
                      fun,
                      ...),
                   as.data.frame));
  names(res) <- estimateCol;
  res$criterion_id <- row.names(res);
  return(res);
};
