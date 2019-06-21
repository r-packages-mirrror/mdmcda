#' @export
weigh_estimates <- function(estimates,
                            weights) {

  weightsDf <- weights$weightsDf;
  multipliedWeights <- weights$multipliedWeights;

  ### Multiply each estimate with the multiplied weights for each
  ### profile
  for (wtProf in unique(weightsDf$weight_profile_id)) {
    convenienceVector <-
      multipliedWeights[multipliedWeights$profile_id==wtProf,
                        "multipliedWeight"];
    names(convenienceVector) <-
      multipliedWeights[multipliedWeights$profile_id==wtProf,
                        "outcome_id"];
    estimates$estimatesDf[, paste0(wtProf,
                         '___score')] <-
      estimatesDf$value * convenienceVector[estimatesDf$outcome_id];
    estimates$autofilledEstimatesDf[, paste0(wtProf,
                                   '___score')] <-
      autofilledEstimatesDf$value * convenienceVector[autofilledEstimatesDf$outcome_id];
  }

  return(invisible(estimatesDf));

}
