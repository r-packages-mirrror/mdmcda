weight_based_sensitivity_analysis <- function(multiEstimateDf,
                                              weightsMeansAndSDs,
                                              criteria,
                                              weightCols = c(raw = 'weight_mean_percentage',
                                                             rescaled = 'weight_mean_rescaled_percentage'),
                                              steps = 10) {

  criteriaClusters <-
    names(criteria$criteriaTree$children);

  ### We process all weight profiles, and for each weight
  ### profile, we process all criteria clusters.

  res <-
    lapply(
      names(weightProfiles),
      function(weightProfileName) {

        ### Get the weights of each cluster
        criteriaClusterWeights <-


        ### Now process all criteria clusters
        return(
          lapply(
            criteriaClusters,
            function(criteriaClusterName) {
              ### Now
            }));
      });

  return(res);

}
