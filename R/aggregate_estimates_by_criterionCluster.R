#' @export
aggregate_estimates_by_criterionCluster <- function(multiEstimateDf,
                                                    estimateCol,
                                                    fun = sum,
                                                    ...) {
  res <-
    do.call(rbind,
            lapply(by(multiEstimateDf[[estimateCol]],
                      multiEstimateDf$parentCriterion_id,
                      fun,
                      ...),
                   as.data.frame));
  names(res) <- estimateCol;
  res$parentCriterion_id <- row.names(res);
  return(res);
};
