#' @export
confidence_based_sensitivity_analysis <-
  function(multiEstimateDf,
           collapsedConfidences,
           scenarioDefinitions,
           weightProfiles,
           criteria,
           transformationFunction = setToZero,
           scenarioOrder = names(scenarioDefinitions),
           scenarioLabels = scenarioOrder,
           scorer = "all",
           criteria = unique(collapsedConfidences[i, 'criterion']),
           confidenceThresholds = seq(0, 1, by=.1),
           setMissingEstimates = 0,
           silent = FALSE) {

    res <- list();
    res$sensitivityAnalyses <-
      lapply(
        confidenceThresholds,
        function(lowConfidence) {
          res <- list();
          res$multiEstimateDf <-
            replace_estimates_based_on_confidence(
              multiEstimateDf = multiEstimateDf,
              collapsedConfidences = collapsedConfidences,
              confidenceQuantile = lowConfidence,
              transformationFunction = transformationFunction,
              scorer = scorer,
              criteria = criteria,
              silent = silent);

          ### Create dataframe for the weighed estimates
          res$weighedEstimates <-
            build_weighed_estimate_df(multiEstimateDf = res$multiEstimateDf,
                                      criterionNames = unique(multiEstimateDf$criterion_id),
                                      decisionNames = unique(multiEstimateDf$decision_id),
                                      scenarioNames = names(scenarioDefinitions),
                                      scenarioDefinitions = scenarioDefinitions,
                                      scorer = scorer,
                                      setMissingEstimates = setMissingEstimates,
                                      warnForMissingEstimates = !silent,
                                      warnForDuplicateEstimates = !silent);

          ### Actually weigh the estimates
          res$weighedEstimates <-
            weigh_estimates_by_profile(weighed_estimate_df = res$weighedEstimates,
                                       weight_profiles = weightProfiles,
                                       weightProfileNames = names(weightProfiles));

          ### Process the estimates to get to scenario-level scores
          res$scores_per_alternative <-
            compute_scores_per_alternative(multiEstimateDf = res$multiEstimateDf,
                                           weightProfiles = weightProfiles);
          res$bestAlternatives <-
            compute_best_alternatives(scores_per_alternative=res$scores_per_alternative);

          res$scoresPerScenario <-
            by(res$weighedEstimates$meanWeights_weighed_estimate,
               res$weighedEstimates$scenario_id,
               sum);
        });
    names(res$sensitivityAnalyses) <-
      as.character(confidenceThresholds);

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
            res <- data.frame(scenario_id = factor(row.names(res),
                                                   levels=scenarioOrder,
                                                   labels=scenarioLabels[scenarioOrder]),
                              score = res[, 1],
                              lowConfidenceMeanThreshold = x);
            return(res);
          }));

    res$plot <-
      ggplot2::ggplot(data = sensitivityAnalyses$setToCriterionMin_ScoresPerScenario,
                      mapping = ggplot2::aes_string(x = "lowConfidenceMeanThreshold",
                                                    y = "score",
                                                    group = "scenario_id",
                                                    color = "scenario_id")) +
        ggplot2::geom_line(size=1) +
        ggplot2::scale_color_viridis_d() +
        ggplot2::theme_minimal();

    return(res);

}
