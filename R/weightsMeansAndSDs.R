#' @export
weightsMeansAndSDs <- function(weights,
                               rootWeight = 1,
                               rootParentCriterion_id = "-") {

  ### Compute means and standard deviations for the weights
  weightMeans <-
    unclass(by(weights$allWeights$weight,
               weights$allWeights$criterion_id,
               mean,
               na.rm=TRUE));
  weightSDs <-
    unclass(by(weights$allWeights$weight,
               weights$allWeights$criterion_id,
               sd,
               na.rm=TRUE));

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
               weight_sd = weightSDs,
               stringsAsFactors = FALSE);

  for (i in unique(res$parentCriterion_id)) {
    if (i != rootParentCriterion_id) {

      res[res$parentCriterion_id==i, 'clusterSize'] <-
        sum(res$parentCriterion_id == i);

      ### Rescale means so that the highest weight is always 100
      res[res$parentCriterion_id==i, 'weight_mean_rescaled'] <-
        res[res$parentCriterion_id==i, 'weight_mean'] /
        (max(res[res$parentCriterion_id==i, 'weight_mean']) / 100);

      ### Rescale mean weights to proportions
      #res[res$parentCriterion_id==i, 'mean_clusterSizeWeighted'] <-
      res[res$parentCriterion_id==i, 'weight_mean_proportion'] <-
        res[res$parentCriterion_id==i, 'weight_mean'] / 100;
      res[res$parentCriterion_id==i, 'weight_mean_rescaled_proportion'] <-
        res[res$parentCriterion_id==i, 'weight_mean_rescaled'] / 100;

      res[res$parentCriterion_id==i, 'clusterTotalWeight'] <-
        sum(res[res$parentCriterion_id == i, 'weight_mean_proportion']);

    }
  }

  ### Set weight for root
  res[res$parentCriterion_id==rootParentCriterion_id,
      c('clusterSize',
        'mean_clusterSizeWeighted',
        'clusterTotalWeight')] <-
    rep(rootWeight, 3);

  return(res);

}

