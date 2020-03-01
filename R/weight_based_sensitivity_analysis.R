#' @export
weight_based_sensitivity_analysis <- function(multiEstimateDf,
                                              weightsMeansAndSDs,
                                              criteria,
                                              scenarioDefinitions,
                                              scenarioOrder = names(scenarioDefinitions),
                                              scenarioLabels = seq_along(scenarioOrder),
                                              scorer = "all",
                                              weightCols = c(raw = 'weight_mean_proportion',
                                                             rescaled = 'weight_mean_rescaled_proportion'),
                                              steps = 10) {

  criteriaClusters <-
    names(criteria$criteriaTree$children);

  ### We process all weight profiles, and for each weight
  ### profile, we process all criteria clusters.

  res <-
    lapply(
      names(weightCols),
      function(weightCol) {

        ### Get the weights of each cluster in this column
        criteriaClusterWeights <-
          stats::setNames(weightsMeansAndSDs[criteriaClusters,
                                             weightCol],
                          criteriaClusters);

        ### Set the steps
        oneStep <- criteriaClusterWeights / steps;

        ### Compute fractions
        weightFractionsPerStep <-
          sapply(0:steps,
                 function(i) return(1 - (i*(1/steps))));

        ### Multiply each fraction with each cluster weight
        weightsPerStep <-
          sapply(weightFractionsPerStep, `*`, criteriaClusterWeights)
        colnames(weightsPerStep) <- weightFractionsPerStep;

        weightsPerCluster <-
          lapply(criteriaClusters,
                 function(cC) {
                   res <- weightsPerStep;
                   res[cC, ] <- criteriaClusterWeights[cC];
                   return(res);
                 });
        names(weightsPerCluster) <- criteriaClusters;

        ### Now process all criteria clusters
        return(
          lapply(
            criteriaClusters,
            function(criteriaClusterName) {

              ### Loop through the steps (the weight fractions)
              return(
                lapply(
                  weightFractionsPerStep,
                  function(weightFraction) {

                    ### Now we create a new temporary weightsMeansAndSDs, where
                    ### we replaced the criteria cluster's weights in `weightCol`
                    ### with those in `weightsPerCluster[, weightFraction]`. We
                    ### then send it to `combine_weights_and_criteria()`, generate
                    ### a weight profile, and recompute the scores for each scenario
                    ### in scenarioDefinitions. We then compute each scenario's
                    ### rank, and finally, create two plots.

                    tmpDf <- weightsMeansAndSDs;
                    weightsMeansAndSDs[criteriaClusters, weightCol] <-
                      weightsPerCluster[[criteriaClusterName]][criteriaClusters, weightFraction];

                    tmpDf <-
                      combine_weights_and_criteria(weightsMeansAndSDs = tmpDf,
                                                   criteria = criteria,
                                                   weightCols = stats::setNames("weightCol",
                                                                                "weightCol"))$weightsMeansAndSDs;




                  }));

            }));
      });

  return(res);

}
