#' @export
build_weighed_estimate_df <-
  function(multiEstimateDf,
           criterionNames,
           decisionNames,
           scenarioNames,
           scenarioDefinitions,
           scorer,
           setMissingEstimates = NULL,
           warnForMissingEstimates = TRUE,
           warnForDuplicateEstimates = TRUE) {

    weighedEstimates <- data.frame(scenario_id=character(),
                                   decision_id=character(),
                                   alternative_value=numeric(),
                                   criterion_id=character(),
                                   estimate=numeric());
    for (currentScenario in scenarioNames) {
      for (currentDecision in decisionNames) {
        for (currentCriterion in criterionNames) {
          estimate <-
            multiEstimateDf[multiEstimateDf$decision_id == currentDecision &
                              multiEstimateDf$decision_alternative_value ==
                              scenarioDefinitions[[currentScenario]][currentDecision] &
                              multiEstimateDf$criterion_id==currentCriterion,
                            scorer];
          if (is.null(estimate) || is.na(estimate) || (length(estimate) == 0)) {
            if (!is.null(setMissingEstimates) & is.numeric(setMissingEstimates) &
                (length(setMissingEstimates) == 1)) {
              if (warnForMissingEstimates) {
                cat("\n- Warning: no estimate found for the effect of alternative ",
                    scenarioDefinitions[[currentScenario]][currentDecision],
                    " of decision ", currentDecision, " on criterion ",
                    currentCriterion, " - setting it to the value of `setMissingEstimates` (",
                    setMissingEstimates, ")\n", sep="");
              }
              estimate <- setMissingEstimates;
            } else {
              if (warnForMissingEstimates) {
                cat("\n- Error: no estimate found for the effect of alternative ",
                    scenarioDefinitions[[currentScenario]][currentDecision],
                    " of decision ", currentDecision, " on criterion ",
                    currentCriterion, ".\n", sep="");
              }
              estimate <- NA;
            }
          }
          if (length(estimate) > 1) {
            if (warnForDuplicateEstimates) {
              cat("\n- Warning: multiple estimates found for the effect of alternative ",
                  scenarioDefinitions[[currentScenario]][currentDecision],
                  " of decision ", currentDecision, " on criterion ",
                  currentCriterion, ". Averaging them.\n", sep="");
            }
            estimate <- mean(estimate, na.rm = TRUE);
          }
          weighedEstimates <-
            rbind(weighedEstimates,
                  data.frame(scenario_id=currentScenario,
                             decision_id=currentDecision,
                             alternative_value=scenarioDefinitions[[currentScenario]][currentDecision],
                             criterion_id=currentCriterion,
                             estimate=estimate,
                             stringsAsFactors = FALSE));
          # cat("\nWithin scenario ", currentScenario, ", for the effect of decision ",
          #     currentDecision, " for criterion ", currentCriterion,
          #     ", found estimate ", estimate, ".", sep="");
        }
      }
    }
    return(weighedEstimates);
  }

