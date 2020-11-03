#' @export
compute_scores_per_alternative <- function(multiEstimateDf,
                                           weightProfiles,
                                           silent=FALSE,
                                           setMissingEstimates = 0,
                                           warnForMissingEstimates = !silent) {

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

  ### Create dataframe with scores per alternative (i.e. summed
  ### over criteria)
  alternativeScores <- data.frame(weightProfile = character(),
                                  decision_id = character(),
                                  alternative_id = character(),
                                  score = numeric(),
                                  stringsAsFactors = FALSE);

  for (currentWeightProfile in names(weightProfiles)) {
    for (currentDecision in unique(multiEstimateDf$decision_id)) {

      tmpDf <-
        multiEstimateDf[multiEstimateDf$decision_id==currentDecision,
                        c(criterionId_col,
                          alternativeValue_col,
                          paste0(currentWeightProfile,
                                 '_weighed_estimate'))];

      missingWeighedEstimates <-
        is.na(tmpDf[, paste0(currentWeightProfile,
                             '_weighed_estimate')]);

      if (any(missingWeighedEstimates)) {
        if (warnForMissingEstimates) {
          cat0("\n- Warning: no weighed estimates found for weight profile '",
                    currentWeightProfile, "' and for the effect of decision '",
                    currentDecision,
                    "', specifically not for the effects of ",
                    vecTxt(paste0("alternative '",
                                       tmpDf[missingWeighedEstimates, alternativeValue_col],
                                       "' on criterion '",
                                       tmpDf[missingWeighedEstimates, 'criterion_id'],
                                       "'")), ". Setting the estimate to 0.\n", sep="");
        }
        if (!is.null(setMissingEstimates) & is.numeric(setMissingEstimates) &
            (length(setMissingEstimates) == 1)) {
          tmpDf[missingWeighedEstimates,
                paste0(currentWeightProfile,
                       '_weighed_estimate')] <- setMissingEstimates;
        } else {
          stop("You did not set `setMissingEstimates` to a number, which means ",
               "I cannot replace missing estimates. That means I cannot continue.");
        }
      }

      summedForAllCriteria <-
        by(tmpDf$meanWeights_weighed_estimate,
           tmpDf[, alternativeValue_col],
           sum,
           na.rm=TRUE);

      tmpDf <-
        data.frame(weightProfile = rep(currentWeightProfile, length(summedForAllCriteria)),
                   decision_id = rep(currentDecision, length(summedForAllCriteria)),
                   alternative_id = names(summedForAllCriteria),
                   score = unclass(summedForAllCriteria),
                   stringsAsFactors = FALSE);

      alternativeScores <-
        rbind(alternativeScores,
              tmpDf,
              stringsAsFactors = FALSE);

    }
  }

  return(alternativeScores);

}

