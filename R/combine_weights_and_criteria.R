#' Add weights from a data frame to criteria tree and update data frame
#'
#' Add weights from a data frame to criteria tree and update data frame.
#'
#' @param weightsMeansAndSDs The data frame with the weights; should have
#' a column `criterion_id` and one or more columns with weights.
#' @param criteria The `criteria` object.
#' @param weightCols A named character vector, where each value is a column
#' with weights in `weightsMeansAndSDs`, and each element's name is the
#' name that weight should get in the `criteria$criteriaTree` and in the
#' `weightsMeansAndSDs` that is returned after acculumation over the tree.
#' @param rootWeight The weight for the tree root.
#' @param rootParentCriterion_id The identifier for the tree root.
#'
#' @return The updated `weightsMeansAndSDs` (and `criteria$criteriaTree` is
#' updated in memory).
#' @export
combine_weights_and_criteria <- function(weightsMeansAndSDs,
                                         criteria,
                                         weightCols,
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
