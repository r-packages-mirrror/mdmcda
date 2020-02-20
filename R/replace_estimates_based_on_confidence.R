#' @export
replace_estimates_based_on_confidence <-
  function(multiEstimateDf,
           collapsedConfidences,
           criteria,
           confidenceQuantile = .10,
           transformationFunction = setToZero,
           scorer = "all",
           silent = TRUE) {

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
                          decision = collapsedConfidences[i, 'decision', drop=FALSE],
                          criterion = collapsedConfidences[i, 'criterion', drop=FALSE],
                          silent=silent);
    }

    return(multiEstimateDf);

}
