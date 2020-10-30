#' @export
combine_weights_and_criteria <- function(weightsMeansAndSDs,
                                         criteria,
                                         weightCols = c(raw = 'weight_mean_proportion',
                                                        rescaled = 'weight_mean_rescaled_proportion'),
                                         rootWeight = 1,
                                         rootParentCriterion_id = "-") {

  ### This function changes the `criteria` object; it returns that
  ### object, too, but we don't need that.
  add_weights_to_criteriaTree(
    weightsMeansAndSDs = weightsMeansAndSDs,
    criteria = criteria,
    weightCols = weightCols,
    rootWeight = rootWeight,
    rootParentCriterion_id = rootParentCriterion_id
  );

  ### Add results from tree accumulation to data frame
  for (i in weightsMeansAndSDs$criterion_id) {
    for (j in names(weightCols)) {
      weightsMeansAndSDs[weightsMeansAndSDs$criterion_id==i,
                         paste0(j, "_product")] <-
        criteria$criteriaTree$Get(paste0(j, "_product"),
                                  filterFun=function(node)
                                    node$name==i);
      weightsMeansAndSDs[weightsMeansAndSDs$criterion_id==i,
                         paste0(j, "_total")] <-
        criteria$criteriaTree$Get(paste0(j, "_total"),
                                  filterFun=function(node)
                                    node$name==i);
    }
  }

  for (j in names(weightCols)) {
    ### Add rescaled cluster size weighted total weights
    weightsMeansAndSDs[[paste0(j, "_total_proportion")]] <-
      weightsMeansAndSDs[[paste0(j, "_total")]] /
      sum(weightsMeansAndSDs[[paste0(j, "_total")]], na.rm=TRUE);
    weightsMeansAndSDs[[paste0(j, "_total_percentage")]] <-
      100 * weightsMeansAndSDs[[paste0(j, "_total_proportion")]];
  }

  ### Get names of clusters
  criteriaClusters <-
    names(criteria$criteriaTree$children);

  return(weightsMeansAndSDs);

}
