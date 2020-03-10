#' @export
scenario_overview <- function(multiEstimateDf,
                              estimateCol,
                              scenario,
                              alternativeValues) {
  res <- list();
  res$estimates <-
    select_scenario_estimates(multiEstimateDf = multiEstimateDf,
                              scenario = scenario);
  res$byCriterion <-
    aggregate_estimates_by_criterion(res$estimates,
                                     estimateCol = estimateCol,
                                     na.rm=TRUE);

  res$byDecision <-
    aggregate_estimates_by_decision(res$estimates,
                                    estimateCol = estimateCol,
                                    na.rm=TRUE);

  res$byDecision$alternative_value <-
    scenario[res$byDecision$decision_id];

  res$byDecision$alternative_label <-
    unlist(lapply(1:nrow(res$byDecision),
                  function(i) {
                    alternative <- as.character(res$byDecision$alternative_value[i]);
                    decision <- as.character(res$byDecision$decision_id[i]);
                    if (grepl(" or ",
                              alternative)) {
                      alternatives <- unlist(strsplit(alternative,
                                                      " or "));
                      return(ufs::vecTxtQ(alternativeValues[[decision]][alternatives]));
                    } else {
                      return(paste0("'", alternativeValues[[decision]][[alternative]],
                                    "'"));
                    }
                  }));

  return(res);
}
