#' @export
aggregate_estimates_by_decision <- function(multiEstimateDf,
                                            estimateCol,
                                            fun = sum,
                                            ...) {

  decisionId_col <- mdmcda::opts$get("decisionId_col");
  alternativeValue_col <- mdmcda::opts$get("alternativeValue_col");

  if (("decision_alternative_value" %in% names(multiEstimateDf)) &&
      (!(alternativeValue_col %in% names(multiEstimateDf)))) {
    alternativeValue_col <- "decision_alternative_value";
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

  res[, mdmcda::opts$get("alternativeValue_col")] <-
    alternativeLabels[res[, decisionId_col]];

  return(res);
}
