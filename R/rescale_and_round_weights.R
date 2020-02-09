#' @export
rescale_and_round_weights <- function(weightsMeansAndSDs) {
  for (i in unique(weightsMeansAndSDs$parentCriterion_id)) {
    if (nchar(i) > 1) {
      ### Rescale means
      weightsMeansAndSDs[weightsMeansAndSDs$parentCriterion_id==i,
                         'mean_rescaled'] <-
        weightsMeansAndSDs[weightsMeansAndSDs$parentCriterion_id==i,
                           'weight_mean'] /
        (max(weightsMeansAndSDs[weightsMeansAndSDs$parentCriterion_id==i,
                                'weight_mean']) / 100);
      ### Round to a multiple of 5
      weightsMeansAndSDs[weightsMeansAndSDs$parentCriterion_id==i,
                         'mean_rounded'] <-
        5 *
        round(weightsMeansAndSDs[weightsMeansAndSDs$parentCriterion_id==i,
                                 'mean_rescaled'] / 5);
    }
  }
  return(weightsMeansAndSDs);
}

