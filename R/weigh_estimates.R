#' @export
weight_estimates <- function(estimates,
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
                        "criterion_id"];

    estimates$estimatesDf[, paste0(wtProf,
                         '___score')] <-
      estimates$estimatesDf$value *
      convenienceVector[estimates$estimatesDf$criterion_id];

    estimates$autofilledEstimatesDf[, paste0(wtProf,
                                   '___score')] <-
      estimates$autofilledEstimatesDf$value *
      convenienceVector[estimates$autofilledEstimatesDf$criterion_id];
  }

  return(invisible(estimates));

}
