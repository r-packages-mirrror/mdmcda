#' @export
replace_estimates_based_on_confidence <-
  function(multiEstimateDf,
           collapsedConfidences,
           criteria,
           collapsedConfidences_criterionIdCol = collapsedConfidences_criterionIdCol,
           confidenceQuantile = .10,
           transformationFunction = setToZero,
           scorer = "all",
           silent = mdmcda::opts$get("silent")) {

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
                          decision_id = unique(collapsedConfidences[i, decisionId_col]),
                          criterion_id = unique(collapsedConfidences[i, collapsedConfidences_criterionIdCol]),
                          silent = silent);
    }

    return(multiEstimateDf);

}
