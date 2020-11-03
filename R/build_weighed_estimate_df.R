#' Build a dataframe for weighed estimates
#'
#' This function is used to produce a data frame that can then be filled with
#' weighed estimates using [weigh_estimates_by_profile()].
#'
#' @param multiEstimateDf A multi estimate data frame that should contain
#' columns `decision_id`, `decision_label`, `alternative_value`,
#' `alternative_label`, `criterion_id`, `criterion_label`, and
#' one or more estimates in columns named with the scorer identifiers. Columns
#' with the `_id` suffix contain identifiers, and columns with the `_label`
#' suffix contain human-readable labels. This dataframe is stored in the
#' object called `multiEstimateDf` returned by a call to
#' [read_performance_tables()] to read a set of scored performance tables.
#' @param criterionNames A vector with the identifiers of the criteria
#' to process.
#' @param scorer The name of the scorer whose estimates to process.
#' @param decisionNames A vector with the identifiers of the decisions
#' to process.
#' @param scenarioNames A vector with the identifiers of the scenarios to
#' process.
#' @param scenarioDefinitions A named list of named vectors. Every named vector
#' contains the selected alternative from each decision (with the decision's
#' identifier being each elements name), and every vectors name is the
#' identifier of the scenario it defines.
#' @param setMissingEstimates The value to set for missing estimates.
#' @param warnForMissingEstimates Whether to warn when missing estimates are
#' envountered (and replaced by `setMissingEstimates`).
#' @param warnForDuplicateEstimates Whether to warn when duplicate estimates are
#' encountered. If multiple estimates _are_ encountered, the mean is taken.
#'
#' @return A dataframe with columns `scenario_id`, `decision_id`,
#' `alternative_value`, `criterion_id`, and `estimate`. This data frame can
#' be supplied to [weigh_estimates_by_profile()].
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

    criterionId_col          <- mdmcda::opts$get("criterionId_col");
    criterionLabel_col       <- mdmcda::opts$get("criterionLabel_col");
    criterionDescription_col <- mdmcda::opts$get("criterionDescription_col");
    parentCriterionId_col    <- mdmcda::opts$get("parentCriterionId_col");
    decisionId_col           <- mdmcda::opts$get("decisionId_col");
    decisionLabel_col        <- mdmcda::opts$get("decisionLabel_col");
    alternativeValue_col     <- mdmcda::opts$get("alternativeValue_col");
    alternativeLabel_col     <- mdmcda::opts$get("alternativeLabel_col");
    scenarioId_col           <- mdmcda::opts$get("scenarioId_col");
    weightProfileId_col      <- mdmcda::opts$get("weightProfileId_col");
    score_col                <- mdmcda::opts$get("score_col");
    leafCriterion_col        <- mdmcda::opts$get("leafCriterion_col");
    rootCriterionId          <- mdmcda::opts$get("rootCriterionId");

    if (is.null(scenarioNames) && is.null(scenarioDefinitions) && is.null(decisionNames)) {

      currentDecision <- unique(estimates$multiEstimateDf$decision_id);

      if (length(currentDecision) > 1) {
        stop("When not working with scenarios, I can analyze only ",
             "one decision, but the estimates you provided concern ",
             length(unique(estimates$multiEstimateDf$decision_id)),
             " decisions (specifically, ",
             vecTxtQ(unique(estimates$multiEstimateDf$decision_id)), ").");
      }

      alternative_values <-
        unique(estimates$multiEstimateDf[, alternativeValue_col]);

      weighedEstimates <- data.frame(scenario_id=character(),
                                     decision_id=character(),
                                     alternative_value=numeric(),
                                     criterion_id=character(),
                                     estimate=numeric());

      for (currentAlternativeValue in alternative_values) {
        for (currentCriterion in criterionNames) {
          estimate <-
            multiEstimateDf[multiEstimateDf$decision_id == currentDecision &
                              multiEstimateDf[, alternativeValue_col] ==
                              currentAlternativeValue &
                              multiEstimateDf$criterion_id==currentCriterion,
                            scorer];
          if (is.null(estimate) || is.na(estimate) || (length(estimate) == 0)) {
            if (!is.null(setMissingEstimates) & is.numeric(setMissingEstimates) &
                (length(setMissingEstimates) == 1)) {
              if (warnForMissingEstimates) {
                cat("\n- Warning: no estimate found for the effect of alternative '",
                    currentAlternativeValue,
                    "' of decision ", currentDecision, "' on criterion '",
                    currentCriterion, "' - setting it to the value of `setMissingEstimates` ('",
                    setMissingEstimates, "')\n", sep="");
              }
              estimate <- setMissingEstimates;
            } else {
              if (warnForMissingEstimates) {
                cat("\n- Error: no estimate found for the effect of alternative '",
                    scenarioDefinitions[[currentScenario]][currentDecision],
                    "' of decision '", currentDecision, "' on criterion '",
                    currentCriterion, "'.\n", sep="");
              }
              estimate <- NA;
            }
          }
          if (length(estimate) > 1) {
            if (warnForDuplicateEstimates) {
              cat("\n- Warning: multiple estimates found for the effect of alternative '",
                  currentAlternativeValue,
                  "' of decision '", currentDecision, "' on criterion '",
                  currentCriterion, "'. Averaging them.\n", sep="");
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
                                multiEstimateDf[, alternativeValue_col] ==
                                scenarioDefinitions[[currentScenario]][currentDecision] &
                                multiEstimateDf$criterion_id==currentCriterion,
                              scorer];
            if (is.null(estimate) || is.na(estimate) || (length(estimate) == 0)) {
              if (!is.null(setMissingEstimates) & is.numeric(setMissingEstimates) &
                  (length(setMissingEstimates) == 1)) {
                if (warnForMissingEstimates) {
                  cat("\n- Warning: no estimate found for the effect of alternative '",
                      scenarioDefinitions[[currentScenario]][currentDecision],
                      "' of decision '", currentDecision, "' on criterion '",
                      currentCriterion, "' - setting it to the value of `setMissingEstimates` ('",
                      setMissingEstimates, "')\n", sep="");
                }
                estimate <- setMissingEstimates;
              } else {
                if (warnForMissingEstimates) {
                  cat("\n- Error: no estimate found for the effect of alternative '",
                      scenarioDefinitions[[currentScenario]][currentDecision],
                      "' of decision '", currentDecision, "' on criterion '",
                      currentCriterion, "'.\n", sep="");
                }
                estimate <- NA;
              }
            }
            if (length(estimate) > 1) {
              if (warnForDuplicateEstimates) {
                cat("\n- Warning: multiple estimates found for the effect of alternative '",
                    scenarioDefinitions[[currentScenario]][currentDecision],
                    "' of decision '", currentDecision, "' on criterion '",
                    currentCriterion, "'. Averaging them.\n", sep="");
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

    class(weighedEstimates) <-
      c("mdmcda_weighedEstimates",
        class(weighedEstimates)
      );

    return(weighedEstimates);

  }

