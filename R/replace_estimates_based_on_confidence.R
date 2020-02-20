#' @export
replace_estimates_based_on_confidence <-
  function(multiEstimateDf,
           collapsedConfidences,
           confidenceQuantile = .10,
           transformationFunction = setToZero,
           scorer = "all",
           criteria = unique(collapsedConfidences[i, 'criterion']),
           silent = FALSE) {

    lowConfidenceMeanThreshold <-
      quantile(collapsedConfidences$confidenceMean,
               confidenceQuantile);
    lowConfidenceIndices <-
      which(collapsedConfidences$confidenceMean <= lowConfidenceMeanThreshold);

    ### Process each performance table
    for (i in collapsedConfidencesIndices) {
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
