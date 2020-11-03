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
                              useDecisionAlternativeLabels = TRUE,
                              decision_alternative_pre = "**",
                              decision_alternative_sep = "**:<br />",
                              decision_alternative_suf = "",
                              scoreBarchart_criteria_args = NULL,
                              scoreBarchart_decisions_args = NULL) {

  criterionId_col          <- mdmcda::opts$get("criterionId_col");
  criterionLabel_col       <- mdmcda::opts$get("criterionLabel_col");
  criterionDescription_col <- mdmcda::opts$get("criterionDescription_col");
  decisionId_col           <- mdmcda::opts$get("decisionId_col");
  decisionLabel_col        <- mdmcda::opts$get("decisionLabel_col");
  alternativeValue_col     <- mdmcda::opts$get("alternativeValue_col");
  alternativeLabel_col     <- mdmcda::opts$get("alternativeLabel_col");
  scenarioId_col           <- mdmcda::opts$get("scenarioId_col");
  weightProfileId_col      <- mdmcda::opts$get("weightProfileId_col");
  score_col                <- mdmcda::opts$get("score_col");

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

  if (is.null(decisionLabels)) {
    res$byDecision$decision_label <-
      res$byDecision[, decisionId_col];
  } else {
    res$byDecision$decision_label <-
      decisionLabels[res$byDecision[, decisionId_col]];
  }

  if (!is.null(alternativeLabels)) {
    altLabels <-
      get_alternativeLabel(res$byDecision,
                           alternativeLabels = alternativeLabels);
    res$byDecision$alternative_label <-
      altLabels[res$byDecision[, decisionId_col]];

  } else {

    warning(
      paste0(
        "There is still a bug in this code; please provide alternative ",
        "labels using argument `alternativeLabels`!"
      )
    );

    if (("decision_alternative_value" %in% names(multiEstimateDf)) &&
        (!(alternativeValue_col %in% names(multiEstimateDf)))) {
      warning("Found column 'decision_alternative_value': this is obsolete!");
      alternativeValue_col <- "decision_alternative_value";
    }

    res$byDecision[, mdmcda::opts$get("alternativeValue_col")] <-
        unlist(
          lapply(
            unique(res$estimates[, decisionId_col]),
            function(x) {

              res <-
                res$estimates[
                  res$estimates[, decisionId_col] == x,
                  alternativeValue_col
                ];

              if (length(unique(res)) == 1) {
                res <- unique(res);
              } else {
                warning(
                  paste0(
                    "When collecting the alternative labels, I found a decision ",
                    "that did not have exactly one alternative label in the ",
                    " multiEstimateDf you specified. Specifically, ",
                    "'", x, "' had as specified alternative labels: ",
                    vecTxtQ(res), "!"
                  )
                );
              }

              return(res[1]);
            }
          )
        );

  }

  res$byDecision$decision_and_alternative <-
    paste0(decision_alternative_pre,
           res$byDecision$decision_label,
           decision_alternative_sep,
           res$byDecision$alternative_label,
           decision_alternative_suf);

  if (useDecisionAlternativeLabels) {
    decisionLabels <-
      stats::setNames(
        res$byDecision$decision_and_alternative,
        nm = res$byDecision[, decisionId_col]
      );
  }

  if (is.null(scenarioLabel)) {
    scenarioLabel <- scenario;
  }

  if (createPlots) {

    ###-------------------------------------------------------------------------
    ### Decisions plot
    ###-------------------------------------------------------------------------

    if (is.null(decisionOrder)) {
      decisionOrder <-
        decisionPlotOrder;
    }

    if (is.null(scoreBarchart_decisions_args)) {
      scoreBarchart_decisions_args <- NULL;
    }

    scoreBarchart_decisions_args_default <-
      list(
        estimatesByDecision = res$byDecision,
        estimateCol = estimateCol,
        decisionOrder = decisionOrder,
        decisionLabels = decisionLabels,
        title = paste0("MDMCDA scores per criterion for ",
                       scenarioLabel),
        verticalPlot = verticalPlots
      );

    scoreBarchart_decisions_args <-
      c(scoreBarchart_decisions_args,
        scoreBarchart_decisions_args_default[
          setdiff(names(scoreBarchart_decisions_args_default),
                  names(scoreBarchart_decisions_args))
        ]);

    res$scoreBarchart_decisions <-
      do.call(
        scoreBarchart_decisions,
        scoreBarchart_decisions_args
      );

    ###-------------------------------------------------------------------------
    ### Criteria plot
    ###-------------------------------------------------------------------------

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
      );

  }

  return(res);
}
