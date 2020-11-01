#' @export
scenario_overview <- function(multiEstimateDf,
                              estimateCol,
                              scenario,
                              alternativeValues,
                              createPlots = TRUE,
                              decisionPlotOrder = "decreasing",
                              criterionPlotOrder = "decreasing",
                              criterionOrder = NULL,
                              criterionLabels = NULL,
                              parentCriterionOrder = NULL,
                              parentCriterionIds_by_childId = NULL,
                              decisionOrder = NULL,
                              decisionLabels = NULL,
                              alternativeLabels = NULL,
                              scenarioLabel = NULL,
                              verticalPlots = FALSE,
                              decision_alternative_sep = ": ",
                              scoreBarchart_criteria_args = NULL,
                              scoreBarchart_decisions_args = NULL) {

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

  if (is.null(scenarioLabel)) {
    scenarioLabel <- scenario;
  }

  if (createPlots) {

    if (is.null(decisionOrder)) {
      decisionOrder <-
        decisionPlotOrder;
    }

    res$scoreBarchart_decisions <-
      scoreBarchart_decisions(
        res$byDecision,
        estimateCol = estimateCol,
        fill = "white",
        decisionOrder = decisionOrder,
        decisionLabels = res$byDecision$decision_and_alternative
      ) +
      ggplot2::theme(
        plot.margin = ggplot2::margin(l=5, r=1, t=1, b=2,
                                      unit = "line")
      );

    if (is.null(criterionOrder)) {
      criterionOrder <-
        criterionPlotOrder;
    }

    if (is.null(scoreBarchart_criteria_args)) {
      scoreBarchart_criteria_args <- NULL;
    }

    scoreBarchart_criteria_args_default <-
      list(
        estimatesByCriterion = res$byCriterion,
        estimateCol = estimateCol,
        fill = "white",
        title = paste0("MDMCDA scores per criterion for ",
                       scenarioLabel),
        criterionOrder = criterionOrder,
        criterionLabels = criterionLabels,
        parentCriterionOrder = parentCriterionOrder,
        parentCriterionIds_by_childId = parentCriterionIds_by_childId,
        verticalPlot = verticalPlots
      );

    scoreBarchart_criteria_args <-
      c(scoreBarchart_criteria_args,
        scoreBarchart_criteria_args_default[
          setdiff(names(scoreBarchart_criteria_args_default),
                  names(scoreBarchart_criteria_args))
        ]);

    res$scoreBarchart_criteria <-
      do.call(
        scoreBarchart_criteria,
        scoreBarchart_criteria_args
      ) +
      ggplot2::theme(
        plot.margin = ggplot2::margin(l=5, r=1, t=1, b=2,
                                      unit = "line")
      );

  }

  return(res);
}
