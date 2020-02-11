#' @export
aggregate_estimates_by_criterion <- function(multiEstimateDf,
                                             estimateCol,
                                             fun = sum,
                                             ...) {
  res <-
    do.call(rbind,
            lapply(by(multiEstimateDf[[estimateCol]],
                      multiEstimateDf$criterion_id,
                      fun,
                      ...),
                   as.data.frame));
  names(res) <- estimateCol;
  res$criterion_id <- row.names(res);
  return(res);
};
