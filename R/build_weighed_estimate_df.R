#' @export
build_weighed_estimate_df <-
  function(multiEstimateDf,
           criterionNames,
           scorer,
           decisionNames = NULL,
           scenarioNames = NULL,
           scenarioDefinitions = NULL,
           setMissingEstimates = NULL,
           warnForMissingEstimates = TRUE,
           warnForDuplicateEstimates = TRUE) {

    if (is.null(scenarioNames) && is.null(scenarioDefinitions) && is.null(decisionNames)) {

      currentDecision <- unique(estimates$multiEstimateDf$decision_id);

      if (length(currentDecision) > 1) {
        stop("When not working with scenarios, I can analyze only ",
             "one decision, but the estimates your provided concern ",
             length(unique(estimates$multiEstimateDf$decision_id)),
             " decisions (specifically, ",
             vecTxtQ(unique(estimates$multiEstimateDf$decision_id)), ").");
      }

      decision_alternative_values <-
        unique(estimates$multiEstimateDf$decision_alternative_value);

      weighedEstimates <- data.frame(scenario_id=character(),
                                     decision_id=character(),
                                     alternative_value=numeric(),
                                     criterion_id=character(),
                                     estimate=numeric());

      for (currentAlternativeValue in decision_alternative_values) {
        for (currentCriterion in criterionNames) {
          estimate <-
            multiEstimateDf[multiEstimateDf$decision_id == currentDecision &
                              multiEstimateDf$decision_alternative_value ==
                              currentAlternativeValue &
                              multiEstimateDf$criterion_id==currentCriterion,
                            scorer];
          if (is.null(estimate) || is.na(estimate) || (length(estimate) == 0)) {
            if (!is.null(setMissingEstimates) & is.numeric(setMissingEstimates) &
                (length(setMissingEstimates) == 1)) {
              if (warnForMissingEstimates) {
                cat("\n- Warning: no estimate found for the effect of alternative ",
                    currentAlternativeValue,
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
                  currentAlternativeValue,
                  " of decision ", currentDecision, " on criterion ",
                  currentCriterion, ". Averaging them.\n", sep="");
            }
            estimate <- mean(estimate, na.rm = TRUE);
          }
          weighedEstimates <-
            rbind(weighedEstimates,
                  data.frame(scenario_id="none",
                             decision_id=currentDecision,
                             alternative_value=currentAlternativeValue,
                             criterion_id=currentCriterion,
                             estimate=estimate,
                             stringsAsFactors = FALSE));
          # cat("\nWithin scenario ", currentScenario, ", for the effect of decision ",
          #     currentDecision, " for criterion ", currentCriterion,
          #     ", found estimate ", estimate, ".", sep="");
        }
      }

    } else if (!is.null(scenarioNames) &&
               !is.null(scenarioDefinitions) &&
               !is.null(decisionNames)) {

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
    } else {
      stop("When working with scenarios, all three arguments ",
          "`decisionNames`, `scenarioNames`, and `scenarioDefinitions` ",
          "must be provided; when not working with scenarios (and ",
          "sticking to one decision), none of these three must be provided!");
    }

    return(weighedEstimates);

  }

