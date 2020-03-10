#' @export
aggregate_estimates_by_decision <- function(multiEstimateDf,
                                            estimateCol,
                                            fun = sum,
                                            ...) {
  res <-
    do.call(rbind,
            lapply(by(multiEstimateDf[[estimateCol]],
                      multiEstimateDf$decision_id,
                      fun,
                      ...),
                   as.data.frame));
  names(res) <- estimateCol;
  res$decision_id <- row.names(res);
  return(res);
};
