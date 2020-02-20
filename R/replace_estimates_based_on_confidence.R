#' @export
replace_estimates_based_on_confidence <-
  function(multiEstimateDf,
           collapsedConfidences,
           criteria,
           confidenceQuantile = .10,
           transformationFunction = setToZero,
           scorer = "all",
           onlyForCriteria = unique(collapsedConfidences[i, 'criterion']),
           silent = FALSE) {

    lowConfidenceMeanThreshold <-
      quantile(collapsedConfidences$confidenceMean,
               confidenceQuantile);
    lowConfidenceIndices <-
      which(collapsedConfidences$confidenceMean <= lowConfidenceMeanThreshold);

    ### Process each performance table
    for (i in lowConfidenceIndices) {
      multiEstimateDf <-
        replace_estimates(multiEstimateDf = multiEstimateDf,
                          criteria = criteria,
                          scorer= scorer,
                          transformationFunction = transformationFunction,
                          decision = collapsedConfidences[i, 'decision'],
                          criterion = collapsedConfidences[i, 'criterion'],
                          silent=silent);
    }

    return(multiEstimateDf);

}
