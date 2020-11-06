#' @export
scorerWeights_to_profile <- function(weights,
                                     scorer,
                                     criteria,
                                     rootWeight = 1,
                                     rootParentCriterion_id = "-",
                                     weightName = paste0(scorer, "_weight")) {

  ### Note: this returns a data frame
  res <-
    add_scorerWeights_to_criteriaTree(
      weights,
      scorer,
      criteria,
      rootWeight = 1,
      rootParentCriterion_id = "-",
      weightName = paste0(scorer, "_weight")
    );

  ### Add results from tree accumulation to data frame
  for (i in res$criterion_id) {
    res[res$criterion_id==i,
        paste0(weightName, "_product")] <-
      criteria$criteriaTree$Get(
        paste0(weightName, "_product"),
        filterFun=function(node) node$name==i);
    res[res$criterion_id==i,
        paste0(weightName, "_total")] <-
      criteria$criteriaTree$Get(
        paste0(weightName, "_total"),
        filterFun=function(node) node$name==i);
  }

  ### Add rescaled cluster size weighted total weights
  res[[paste0(weightName, "_total_proportion")]] <-
    res[[paste0(weightName, "_total")]] /
    sum(res[[paste0(weightName, "_total")]], na.rm=TRUE);

  return(res);

}
