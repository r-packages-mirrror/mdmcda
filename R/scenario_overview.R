#' @export
scenario_overview <- function(multiEstimateDf,
                              estimateCol,
                              scenario,
                              alternativeValues,
                              createPlots = TRUE,
                              decisionPlotOrder = "decreasing",
                              criterionPlotOrder = "decreasing",
                              criteriaOrder = NULL,
                              criteriaLabels = NULL,
                              parentCriterionIds = NULL,
                              decisionOrder = NULL,
                              decisionLabels = NULL,
                              alternativeLabels = NULL,
                              decision_alternative_sep = ": ") {

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

  res$byDecision$decision_and_alternative <-
    paste0(res$byDecision$decision_id,
           decision_alternative_sep,
           res$byDecision$alternative_label);

  if (createPlots) {

    res$scoreBarchart_decisions <-
      scoreBarchart_decisions(
        res$byDecision,
        estimateCol = estimateCol,
        fill = "white",
        decisionOrder = decisionPlotOrder,
        decisionLabels = res$byDecision$decision_and_alternative
      );

    res$scoreBarchart_criteria <-
      scoreBarchart_criteria(
        estimatesByCriterion = res$byCriterion,
        estimateCol = estimateCol,
        fill = "white",
        criteriaOrder = criterionPlotOrder,
        criteriaLabels = criteriaLabels,
        parentCriterionIds = parentCriterionIds
      );
  }

  return(res);
}
