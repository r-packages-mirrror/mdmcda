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
