#' @export
sensitivity_analyses <- function(multiEstimateDf,
                                 collapsedConfidences,
                                 collapsedConfidencesIndices,
                                 transformationFunction,
                                 scorer,
                                 criteria,
                                 scenarioDefinitions,
                                 weightProfiles,
                                 setMissingEstimates = 0,
                                 silent = FALSE) {

  criterionNames <- unique(multiEstimateDf$criterion_id);
  decisionNames <- unique(multiEstimateDf$decision_id);
  scenarioNames <- names(scenarioDefinitions);

  res <- list(multiEstimateDf=multiEstimateDf);

  ### Process each performance table
  for (i in which(estimates$collapsedConfidences$confidenceMean <= lowConfidenceMeanThreshold)) {
    res$multiEstimateDf <-
      replace_estimates(multiEstimateDf = res$multiEstimateDf,
                        criteria = criteria,
                        scorer= scorer,
                        transformationFunction = transformationFunction,
                        decision = collapsedConfidences[i, 'decision'],
                        criterion = collapsedConfidences[i, 'criterion'],
                        silent=silent);
  }

  ### Create dataframe for the weighed estimates
  res$weighedEstimates <-
    build_weighed_estimate_df(multiEstimateDf = res$multiEstimateDf,
                              criterionNames = criterionNames,
                              decisionNames = decisionNames,
                              scenarioNames = scenarioNames,
                              scenarioDefinitions = scenarioDefinitions,
                              scorer = scorer,
                              setMissingEstimates = setMissingEstimates,
                              silent=silent,
                              warnForMissingEstimates = !silent,
                              warnForDuplicateEstimates = !silent);

  ### Actually weigh the estimates
  res$weighedEstimates <-
    weigh_estimates(weighed_estimate_df = res$weighedEstimates,
                    weight_profiles = weightProfiles,
                    weightProfileNames = names(weightProfiles));

  res$scores_per_alternative <-
    compute_scores_per_alternative(multiEstimateDf = res$multiEstimateDf,
                                   weightProfiles = weightProfiles);
  res$bestAlternatives <-
    compute_best_alternatives(scores_per_alternative=res$scores_per_alternative);

  res$scoresPerScenario <-
    by(res$weighedEstimates$meanWeights_weighed_estimate,
       res$weighedEstimates$scenario_id,
       sum);

  return(res);

}
