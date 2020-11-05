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
#' [read_performance_tables()] to read a set of scored performance tables. Note
#' that different column names can be set using [mdmcda::opts].
#' @param criterionOrder A vector with the identifiers of the criteria
#' to process.
#' @param scorer The name of the scorer whose estimates to process.
#' @param decisionOrder A vector with the identifiers of the decisions
#' to process.
#' @param scenarioOrder A vector with the identifiers of the scenarios to
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
           criterionOrder,
           scorer,
           decisionOrder = NULL,
           scenarioOrder = NULL,
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
    estimate_col             <- mdmcda::opts$get("estimate_col");
    score_col                <- mdmcda::opts$get("score_col");
    leafCriterion_col        <- mdmcda::opts$get("leafCriterion_col");
    rootCriterionId          <- mdmcda::opts$get("rootCriterionId");

    if (!is.data.frame(multiEstimateDf)) {
      stop("`multiEstimateDf` must be a data frame!");
    }

    if (!(all(c(criterionId_col, decisionId_col,
                alternativeValue_col, scorer) %in% names(multiEstimateDf)))) {
      stop("`multiEstimateDf` must contain columns with ",
           "criterion identifiers (", criterionId_col,
           "), decision identifiers (", decisionId_col, "),
           alternative values (", alternativeValue_col, "), and estimates ",
           "(you specified column name '", scorer, "'. However, the columns ",
           "in `multiEstimateDf` are: ", vectTxtQ(names(multiEstimateDf)), ".");
    }

    if (!is.vector(decisionOrder)) {
      stop("`decisionOrder` has to be a vector (and isn't)!");
    }

    if (!is.vector(scenarioOrder)) {
      stop("`scenarioOrder` has to be a vector (and isn't)!");
    }

    if (is.null(scenarioOrder) && is.null(scenarioDefinitions) && is.null(decisionOrder)) {

      currentDecision <- unique(multiEstimateDf[, decisionId_col]);

      if (length(currentDecision) > 1) {
        stop("When not working with scenarios, I can analyze only ",
             "one decision, but the estimates you provided concern ",
             length(unique(multiEstimateDf[, decisionId_col])),
             " decisions (specifically, ",
             vecTxtQ(unique(multiEstimateDf[, decisionId_col])), ").");
      }

      alternative_values <-
        unique(multiEstimateDf[, alternativeValue_col]);

      weighedEstimates <- data.frame(scenario_id=character(),
                                     decision_id=character(),
                                     alternative_value=numeric(),
                                     criterion_id=character(),
                                     estimate=numeric());

      for (currentAlternativeValue in alternative_values) {
        for (currentCriterion in criterionOrder) {
          estimate <-
            multiEstimateDf[multiEstimateDf[, decisionId_col] == currentDecision &
                              multiEstimateDf[, alternativeValue_col] ==
                              currentAlternativeValue &
                              multiEstimateDf[, criterionId_col]==currentCriterion,
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

    } else if (!is.null(scenarioOrder) &&
               !is.null(scenarioDefinitions) &&
               !is.null(decisionOrder)) {

      weighedEstimates <- data.frame(scenario_id=character(),
                                     decision_id=character(),
                                     alternative_value=numeric(),
                                     criterion_id=character(),
                                     estimate=numeric());

      for (currentScenario in scenarioOrder) {

        if (!(currentScenario %in% names(scenarioDefinitions))) {
          stop("Trying to process scenario '", currentScenario,
               "', but it does not exist in the scenarioDefinitions object!");
        }

        for (currentDecision in decisionOrder) {
          for (currentCriterion in criterionOrder) {
            estimate <-
              multiEstimateDf[multiEstimateDf[, decisionId_col] == currentDecision &
                                multiEstimateDf[, alternativeValue_col] ==
                                scenarioDefinitions[[currentScenario]][currentDecision] &
                                multiEstimateDf[, criterionId_col]==currentCriterion,
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
              rbind(
                weighedEstimates,
                stats::setNames(
                  data.frame(scenario_id=currentScenario,
                             decision_id=currentDecision,
                             alternative_value=scenarioDefinitions[[currentScenario]][currentDecision],
                             criterion_id=currentCriterion,
                             estimate=estimate,
                             stringsAsFactors = FALSE),
                  nm = c(scenarioId_col, decisionId_col, alternativeValue_col,
                         criterionId_col, estimate_col)
                )
              );
            # cat("\nWithin scenario ", currentScenario, ", for the effect of decision ",
            #     currentDecision, " for criterion ", currentCriterion,
            #     ", found estimate ", estimate, ".", sep="");
          }
        }
      }
    } else {
      stop("When working with scenarios, all three arguments ",
          "`decisionOrder`, `scenarioOrder`, and `scenarioDefinitions` ",
          "must be provided; when not working with scenarios (and ",
          "sticking to one decision), none of these three must be provided!");
    }

    class(weighedEstimates) <-
      c("mdmcda_weighedEstimates",
        class(weighedEstimates)
      );

    return(weighedEstimates);

  }

