#' Conduct a sensitivity analysis based on confidence scores
#'
#' When estimating the effects of alternatives on the criteria,
#' the raters can attach a confidence score to the estimates in the
#' performance table (or, more accurately, to the entire performance
#' table). This function uses these to test the robustness of the
#' results.
#'
#' In the confidence-based sensitivity analyses, by default 10% of the
#' estimates about which the raters expressed the least confidence
#' are sequentially set to the result of the `transformationFunction`.
#' The total score for each scenario is then computed again.
#'
#' @param multiEstimateDf A multi estimate data frame that should contain
#' columns `decision_id`, `decision_label`, `alternative_value`,
#' `alternative_label`, `criterion_id`, `criterion_label`, and
#' one or more estimates in columns named with the scorer identifiers. Columns
#' with the `_id` suffix contain identifiers, and columns with the `_label`
#' suffix contain human-readable labels. This dataframe is stored in the
#' object called `multiEstimateDf` returned by a call to
#' [read_performance_tables()] to read a set of scored performance tables. Note
#' that different column names can be set using [mdmcda::opts].
#' @param collapsedConfidences The `collapsedConfidences` object.
#' @param scenarioDefinitions The `scenarioDefinitions` object.
#' @param weightProfiles The `weightProfiles` object.
#' @param criteria The `criteria` object.
#' @param collapsedConfidences_criterionIdCol The column name of the
#' criterion identifiers in the `collapsedConfidences` object to use.
#' @param transformationFunction The function to apply to the estimates in
#' performance tables below the confidence threshold
#' @param scenarioOrder The scenarios to process and the order in which to
#' organise them.
#' @param scenarioLabels The labels of the scenarios.
#' @param scorer The scorer of whom to process the confidences.
#' @param confidenceThresholds The confidence thresholds to process (by
#' default, 0 to 1 in steps of .1).
#' @param setMissingEstimates What to set missing values to, if any are
#' encountered.
#' @param silent Whether to suppress messages about progress.
#' @param lineSize The line size to use in the plots.
#' @param theme The `ggplot2` theme to use.
#'
#' @return An object with a data frame summarizing the sensitivity analysis,
#' and two produced plots: one with the scores, and one with the ranks.
#' @export
confidence_based_sensitivity_analysis <-
  function(multiEstimateDf,
           collapsedConfidences,
           scenarioDefinitions,
           weightProfiles,
           criteria,
           collapsedConfidences_criterionIdCol = "parentCriterion_id",
           transformationFunction = setToZero,
           scenarioOrder = names(scenarioDefinitions),
           scenarioLabels = stats::setNames(names(scenarioDefinitions),
                                            names(scenarioDefinitions)),
           scorer = "all",
           confidenceThresholds = seq(0, 1, by=.1),
           setMissingEstimates = 0,
           silent = mdmcda::opts$get("silent"),
           lineSize = 1,
           theme = ggplot2::theme_minimal(base_size = mdmcda::opts$get("ggBaseSize"))) {

    criterionId_col          <- mdmcda::opts$get("criterionId_col");
    criterionLabel_col       <- mdmcda::opts$get("criterionLabel_col");
    criterionDescription_col <- mdmcda::opts$get("criterionDescription_col");
    parentCriterionId_col    <- mdmcda::opts$get("parentCriterionId_col");
    parentCriterionLabel_col <- mdmcda::opts$get("parentCriterionLabel_col");
    decisionId_col           <- mdmcda::opts$get("decisionId_col");
    decisionLabel_col        <- mdmcda::opts$get("decisionLabel_col");
    alternativeValue_col     <- mdmcda::opts$get("alternativeValue_col");
    alternativeLabel_col     <- mdmcda::opts$get("alternativeLabel_col");
    scenarioId_col           <- mdmcda::opts$get("scenarioId_col");
    weightProfileId_col      <- mdmcda::opts$get("weightProfileId_col");
    score_col                <- mdmcda::opts$get("score_col");
    estimate_col             <- mdmcda::opts$get("estimate_col");
    leafCriterion_col        <- mdmcda::opts$get("leafCriterion_col");
    rootCriterionId          <- mdmcda::opts$get("rootCriterionId");

    res <- list(input = as.list(environment()));
    res$sensitivityAnalyses <-
      lapply(
        confidenceThresholds,
        function(lowConfidence) {

          if (!silent) {
            cat0("\n\n**Starting to process confidence <= quantile ",
                      lowConfidence, ".\n\n");
          }


          if (!silent) {
            cat0("Replacing estimates:\n\n");
          }

          res <- list();
          res$multiEstimateDf <-
            replace_estimates_based_on_confidence(
              multiEstimateDf = multiEstimateDf,
              collapsedConfidences = collapsedConfidences,
              collapsedConfidences_criterionIdCol = collapsedConfidences_criterionIdCol,
              confidenceQuantile = lowConfidence,
              transformationFunction = transformationFunction,
              scorer = scorer,
              criteria = criteria,
              silent = silent);

          if (!silent) {
            cat0("\nPreparing dataframe to weigh estimates.\n");
          }

          ### Create dataframe for the weighted estimates
          res$weightedEstimates <-
            build_weighted_estimate_df(multiEstimateDf = res$multiEstimateDf,
                                      criterionOrder = unique(res$multiEstimateDf[, criterionId_col]),
                                      decisionOrder = unique(res$multiEstimateDf[, decisionId_col]),
                                      scenarioOrder = scenarioOrder,
                                      scenarioDefinitions = scenarioDefinitions,
                                      scorer = scorer,
                                      setMissingEstimates = setMissingEstimates,
                                      warnForMissingEstimates = !silent,
                                      warnForDuplicateEstimates = !silent);

          if (!silent) {
            cat0("Actually weighting estimates.\n");
          }

          ### Actually weigh the estimates
          res$weightedEstimates <-
            weight_estimates_by_profile(weighted_estimate_df = res$weightedEstimates,
                                       weight_profiles = weightProfiles,
                                       weightProfileNames = names(weightProfiles));
          if (!silent) {
            cat0("Computing scenario scores.\n");
          }

          ### Process the estimates to get to scenario-level scores
          res$scores_per_alternative <-
            compute_scores_per_alternative(multiEstimateDf = res$multiEstimateDf,
                                           weightProfiles = weightProfiles);
          res$bestAlternatives <-
            compute_best_alternatives(scores_per_alternative=res$scores_per_alternative);

          res$scoresPerScenario <-
            by(res$weightedEstimates[, estimate_col],
               res$weightedEstimates[, scenarioId_col],
               sum);
          return(res);
        });
    names(res$sensitivityAnalyses) <-
      as.character(confidenceThresholds);

    if (!silent) {
      cat0("\nDone with replacements. Combining all scenario scores into one dataframe.\n");
    }

    res$dat <-
      do.call(
        rbind,
        lapply(
          names(res$sensitivityAnalyses),
          function(x) {
            res <-
              do.call(
                rbind,
                as.list(res$sensitivityAnalyses[[x]]$scoresPerScenario)
              );
            res <- data.frame(
              scenario_id = factor(
                row.names(res),
                levels = scenarioOrder,
                labels = scenarioLabels[scenarioOrder],
                ordered = TRUE
              ),
              score = res[, 1],
              lowConfidenceMeanThreshold = x
            );
            res$rank <-
              rank(res$score);
            return(res);
          }));

    if (!silent) {
      cat0("\nBuilding plot.\n");
    }

    res$scorePlot <-
      ggplot2::ggplot(data = res$dat,
                      mapping = ggplot2::aes_string(x = "lowConfidenceMeanThreshold",
                                                    y = "score",
                                                    group = "scenario_id",
                                                    color = "scenario_id")) +
        ggplot2::geom_line(size=lineSize) +
        ggplot2::scale_color_viridis_d(end = .9) +
        ggplot2::labs(x = "'Low confidence' threshold determining estimate replacement",
                      color = "Scenario",
                      y = "Scores") +
        theme;

    res$rankPlot <-
      ggplot2::ggplot(data = res$dat,
                      mapping = ggplot2::aes_string(x = "lowConfidenceMeanThreshold",
                                                    y = "rank",
                                                    group = "scenario_id",
                                                    color = "scenario_id")) +
      ggplot2::geom_line(size=lineSize) +
      ggplot2::scale_color_viridis_d(end = .9) +
      ggplot2::labs(x = "'Low confidence' threshold determining estimate replacement",
                    color = "Scenario",
                    y = "Ranks") +
      theme;

    return(res);

}
