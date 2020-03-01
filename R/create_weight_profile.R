#' @export
create_weight_profile <- function(weightsMeansAndSDs,
                                  criteria,
                                  profileName,
                                  weightCol = "rescaled_total_percentage",
                                  clusterWeightCol = "rescaled_product") {

  criteriaWeights <-
    stats::setNames(weightsMeansAndSDs$rescaled_total_percentage,
                    weightsMeansAndSDs$criterion_id);
  criteriaWeights <-
    criteriaWeights[!is.na(criteriaWeights)];

  res <-
    list(criteriaWeights);
  names(res) <- profileName;

  ### Add criteria cluster weights as attribute
  criteriaClusters <-
    names(criteria$criteriaTree$children);

  attr(res[[profileName]], "criteriaClusterWeights") <-
    stats::setNames(weightsMeansAndSDs[criteriaClusters, clusterWeightCol],
                    criteriaClusters);

  return(res);

}
