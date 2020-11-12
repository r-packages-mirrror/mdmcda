#' Create an object with various useful data frames and plots for a scenario
#'
#' @param multiEstimateDf A `multiEstimateDf`.
#' @param estimateCol The column name with the estimates to use.
#' @param scenario The `scenarioDefinition` object with the scenario
#' definition: a named vector, with each element's name corresponding to
#' a decision, and each element's value to the alternative value for that
#' decision within the scenario.
#' @param createPlots Whether to create the plots.
#' @param decisionPlotOrder,criterionPlotOrder If no decisionOrder is
#' specified, whether to sort the decisions in the decision plot and criterion
#' plot in decreasing or increasing order.
#' @param criterionOrder,parentCriterionOrder,decisionOrder The order used to
#' organise the plots. These are vectors of identifiers.
#' @param criterionLabels,decisionLabels,alternativeLabels,scenarioLabel The
#' labels to use for the criteria, decisions, alternatives, and the scenario.
#' These are named vectors, with the elements being the labels, and their names
#' the corresponding identifiers (e.g. decision identifiers).
#' @param parentCriterionIds_by_childId Optionally, a named vector with the
#' names being the criteria, and the elements being their parent criterion. If
#' provided, the criteria plot will be organised and coloured by parent
#' criterion (i.e. criteria cluster).
#' @param verticalPlots Whether to print the plots vertically or horizontally.
#' @param useDecisionAlternativeLabels Whether to label the decision plot with
#' the decisions or combined labels that also include the alternative for
#' the scenario.
#' @param decision_alternative_pre,decision_alternative_sep,decision_alternative_suf If
#' `useDecisionAlternativeLabels` is `TRUE`, these prefix, separator, and suffix
#' are used to compose the decision plot labels.
#' @param alternativeToOmitInTable This alternative is not shown in the
#' table with highlighted options, unless it's selected for a scenario.
#' @param scoreBarchart_criteria_args,scoreBarchart_decisions_args These
#' arguments can be used to specify named lists with additional arguments for
#' `scoreBarChart_decisions` and `scoreBarChart_criteria`.
#'
#' @return And object with two plots and three data frames.
#' @export
scenario_overview <- function(multiEstimateDf,
                              estimateCol,
                              scenario,
                              scenarioLabel = NULL,
                              createPlots = TRUE,
                              decisionPlotOrder = "decreasing",
                              criterionPlotOrder = "decreasing",
                              criterionOrder = NULL,
                              parentCriterionOrder = NULL,
                              decisionOrder = NULL,
                              criterionLabels = NULL,
                              decisionLabels = NULL,
                              alternativeLabels = NULL,
                              parentCriterionIds_by_childId = NULL,
                              verticalPlots = FALSE,
                              useDecisionAlternativeLabels = TRUE,
                              decision_alternative_pre = "**",
                              decision_alternative_sep = "**:<br />",
                              decision_alternative_suf = "",
                              alternativeToOmitInTable = "Not applicable",
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
           res$byDecision[, decisionLabel_col],
           decision_alternative_sep,
           res$byDecision[, alternativeLabel_col],
           decision_alternative_suf);

  if (useDecisionAlternativeLabels) {
    decisionLabels_plot <-
      stats::setNames(
        res$byDecision$decision_and_alternative,
        nm = res$byDecision[, decisionId_col]
      );
  } else {
    decisionLabels_plot <- decisionLabels;
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
        decisionLabels = decisionLabels_plot,
        title = paste0("MDMCDA scores per decision for ",
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

  ###-------------------------------------------------------------------------
  ### Table with only the selected alternatives
  ###-------------------------------------------------------------------------

  res$scenario_alternative_table <-
    mdmcda::scenario_alternative_table(
      scenarioDefinition = scenario,
      alternativeLabels = alternativeLabels,
      decisionOrder = decisionOrder,
      decisionLabels = decisionLabels
    );

  ###-------------------------------------------------------------------------
  ### Table with selected alternatives highlighted
  ###-------------------------------------------------------------------------

  scores_per_alternative <-
    mdmcda::compute_scores_per_alternative(
      multiEstimateDf = estimates$multiEstimateDf,
      weightProfiles = weightProfiles
    );

  res$highlighted_alternative_table <-
    mdmcda::highlighted_alternative_table(
      scores_per_alternative = scores_per_alternative,
      scenario=scenario,
      alternativeLabels = alternativeLabels,
      decisionOrder = decisionOrder,
      decisionLabels = decisionLabels,
      colNames = c("Policy instruments and selected options", "Scores"),
      omit = alternativeToOmitInTable,
      caption = paste0("Overview of alternatives ",
                       "with the alternatives selected in scenario '",
                       scenarioLabel, "' in bold."),
      estimateParseFunction = round,
      digits = 0
    );

  return(res);
}
