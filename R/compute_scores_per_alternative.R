#' @export
compute_scores_per_alternative <- function(multiEstimateDf,
                                           weightProfiles,
                                           silent=FALSE,
                                           setMissingEstimates = 0,
                                           warnForMissingEstimates = !silent) {

  ### Create dataframe with scores per alternative (i.e. summed
  ### over criteria)
  alternativeScores <- data.frame(weightProfile = character(),
                                  decision_id = character(),
                                  alternative_id = character(),
                                  score = numeric());

  for (currentWeightProfile in names(weightProfiles)) {
    for (currentDecision in unique(multiEstimateDf$decision_id)) {

      tmpDf <-
        multiEstimateDf[multiEstimateDf$decision_id==currentDecision,
                        c('criterion_id',
                          'decision_alternative_value',
                          paste0(currentWeightProfile,
                                 '_weighed_estimate'))];

      missingWeighedEstimates <-
        is.na(tmpDf[, paste0(currentWeightProfile,
                             '_weighed_estimate')]);

      if (any(missingWeighedEstimates)) {
        if (warnForMissingEstimates) {
          ufs::cat0("\n- Warning: no weighed estimates found for weight profile '",
                    currentWeightProfile, "' and for the effect of decision '",
                    currentDecision,
                    "', specifically not for the effects of ",
                    ufs::vecTxt(paste0("alternative '",
                                       tmpDf[missingWeighedEstimates, 'decision_alternative_value'],
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
           tmpDf$decision_alternative_value,
           sum,
           na.rm=TRUE);

      tmpDf <-
        data.frame(weightProfile = rep(currentWeightProfile, length(summedForAllCriteria)),
                   decision_id = rep(currentDecision, length(summedForAllCriteria)),
                   alternative_id = names(summedForAllCriteria),
                   score = unclass(summedForAllCriteria));

      alternativeScores <-
        rbind(alternativeScores,
              tmpDf);

    }
  }

  return(alternativeScores);

}

