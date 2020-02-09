#' @export
rescale_and_round_weights <- function(weightsMeansAndSDs) {
  res <- weightsMeansAndSDs;
  for (i in unique(res$parentCriterion_id)) {
    if (nchar(i) > 1) {
      ### Rescale means
      res[res$parentCriterion_id==i, 'mean_rescaled'] <-
        res[res$parentCriterion_id==i, 'weight_mean'] /
        (max(res[res$parentCriterion_id==i, 'weight_mean']) / 100);

      ### Round to a multiple of 5
      res[res$parentCriterion_id==i, 'mean_rounded'] <-
        5 * round(res[res$parentCriterion_id==i, 'mean_rescaled'] / 5);

      ### Rescale again to 'percentage scale'
      res[weightsMeansAndSDs$parentCriterion_id==i, 'mean_percentage'] <-
        res[res$parentCriterion_id==i, 'mean_rescaled'] /
        sum(res[res$parentCriterion_id==i, 'mean_rescaled']);

    }
  }
  return(res);
}

