#' Aggregate the (potentially weighted) estimates by decision
#'
#' @param multiEstimateDf The `multiEstimateDf`.
#' @param estimateCol The name of the column with the estimates to aggregate.
#' @param decisionId_col,alternativeValue_col The columns containing the
#' decision identifiers and the values in each decision
#' @param fun The function to use to aggregate the estimates.
#' @param ... Any additional arguments are passed to `fun`.
#'
#' @return A data frame with aggregated estimates.
#' @export
aggregate_estimates_by_decision <- function(multiEstimateDf,
                                            estimateCol,
                                            decisionId_col = mdmcda::opts$get("decisionId_col"),
                                            alternativeValue_col = mdmcda::opts$get("alternativeValue_col"),
                                            fun = sum,
                                            ...) {

  if (("decision_alternative_value" %in% names(multiEstimateDf)) &&
      (!(alternativeValue_col %in% names(multiEstimateDf)))) {
    stop("Found column 'decision_alternative_value': this is obsolete!");
  }

  res <-
    do.call(rbind,
            lapply(by(multiEstimateDf[[estimateCol]],
                      multiEstimateDf[, decisionId_col],
                      fun,
                      ...),
                   as.data.frame));
  names(res) <- estimateCol;
  res[, decisionId_col] <- row.names(res);

  ### Add alternative values for each decision
  alternativeLabels <-
    unlist(
      lapply(
        unique(multiEstimateDf[, decisionId_col]),
        function(x) {

          res <-
            as.character(
              multiEstimateDf[
                multiEstimateDf[, decisionId_col] == x,
                alternativeValue_col
              ]
            );

          if (length(unique(res)) == 1) {
            return(unique(res));
          } else if (length(unique(res)) < 1) {
            warning(
              paste0(
                "When aggregating estimates by decision, I found a decision ",
                "without specified alternative values:",
                "'", x, "' had as specified alternative values: ",
                vecTxtQ(res), "!"
              )
            );
            return(mode(res)[1]);
          } else {
            stop("When aggregating estimates by decision, I found that ",
                 "not all alternative values across criteria were the ",
                 "same for decision with identifier '", x, "': ",
                 vecTxtQ(res), "!");
          }
          return(res);
        }
      )
    );

  names(alternativeLabels) <-
    unique(multiEstimateDf[, decisionId_col]);

  res[, alternativeValue_col] <-
    alternativeLabels[res[, decisionId_col]];

  return(res);
}
