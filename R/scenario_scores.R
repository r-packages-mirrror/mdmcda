scenario_scores <- function(criteria,
                            weights,
                            scenarios_and_options,
                            decisions_and_options,
                            estimates) {

  weightsDf <- weights$weightsDf;
  multipliedWeights <- weights$multipliedWeights;
  autofilledEstimatesDf <- estimates$autofilledEstimatesDf;
  scenarioOptionsDf <- scenarios_and_options$scenarioOptionsDf;
  decisionsDf <- decisions_and_options$decisionsDf;

  ### Select all multiplied estimates for each scenario
  scenarioScores <- list();

  ### Separately per each weight profile
  for (currentScenario in scenariosMetadataDf$scenario_id) {
    scenarioScores[[currentScenario]] <- list();
    ### And per scenario
    for (currentWtProf in unique(weightsDf$weight_profile_id)) {
      scenarioScores[[currentScenario]][[currentWtProf]] <- data.frame();
      ### And per instrument
      for (currentInstr in decisionsDf$id) {
        ### And only for the chosen option
        currentOption <-
          scenarioOptionsDf[scenarioOptionsDf$scenario_id==currentScenario &
                              scenarioOptionsDf$instrument_id==currentInstr, 'instrument_option_value'];
        if (length(currentOption) == 0) {
          warning("For scenario '",
                  currentScenario,
                  "' and instrument '",
                  currentInstr,
                  "', no option is selected!");
        } else {
          ### And per outcome
          for (currentOutcome in outcomesDf$id[outcomesDf$isLeaf]) {
            scenarioScores[[currentScenario]][[currentWtProf]] <-
              rbind(scenarioScores[[currentScenario]][[currentWtProf]],
                    data.frame(scenario_id = currentScenario,
                               weight_profile_id = currentWtProf,
                               instrument_id = currentInstr,
                               instrument_option_value = currentOption,
                               outcome_id = currentOutcome,
                               score = autofilledEstimatesDf[
                                 autofilledEstimatesDf$instrument_id==currentInstr &
                                   autofilledEstimatesDf$instrument_option_value==currentOption &
                                   autofilledEstimatesDf$outcome_id==currentOutcome,
                                 paste0(currentWtProf, "___score")]));
          }
        }
      }
    }
  }

  return(invisible(scenarioScores));

}
