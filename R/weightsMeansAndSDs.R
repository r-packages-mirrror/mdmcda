#' @export
weightsMeansAndSDs <- function(weights,
                               rootParentCriterion_id = "-") {

  ### Compute means and standard deviations for the weights
  weightMeans <-
    unclass(by(weights$allWeights$weight, weights$allWeights$criterion_id, mean, na.rm=TRUE));
  weightSDs <-
    unclass(by(weights$allWeights$weight, weights$allWeights$criterion_id, sd, na.rm=TRUE));

  criterionIds <-
    sort(unique(weights$allWeights$criterion_id));

  parentCriterionIds <-
    unique(weights$allWeights[, c('criterion_id', 'parentCriterion_id')]);
  parentCriterionIds <-
    parentCriterionIds[order(parentCriterionIds$criterion_id), ];
  parentCriterionIds <-
    stats::setNames(parentCriterionIds$parentCriterion_id,
                    parentCriterionIds$criterion_id);

  res <-
    data.frame(criterion_id = criterionIds,
               parentCriterion_id = parentCriterionIds[criterionIds],
               weight_mean = weightMeans,
               weight_sd = weightSDs);

  for (i in unique(res$parentCriterion_id)) {
    if (i != rootParentCriterion_id) {

      res[res$parentCriterion_id==i, 'clusterSize'] <-
        sum(res$parentCriterion_id == i);

      ### Rescale mean weights to proportions
      res[res$parentCriterion_id==i, 'mean_clusterSizeWeighted'] <-
        res[res$parentCriterion_id==i, 'weight_mean'] / 100;

      res[res$parentCriterion_id==i, 'clusterTotalWeight'] <-
        sum(res[res$parentCriterion_id == i, 'mean_clusterSizeWeighted']);

      res[res$parentCriterion_id==i, 'weight_mean_percentage'] <-
        res[res$parentCriterion_id == i, 'mean_clusterSizeWeighted'] /
        res[res$parentCriterion_id==i, 'clusterTotalWeight'];

    }
  }

  ### Set weight for root
  res[res$parentCriterion_id==rootParentCriterion_id,
      c('clusterSize',
        'mean_clusterSizeWeighted',
        'clusterTotalWeight')] <-
    c(1, 1, 1);

  return(res);

}

