#' Add weights to criteria tree
#'
#' This function takes weights and adds them to the criteria tree. NOTE: the
#' criteria tree is an R6 object. Therefore, when this function is called,
#' the object will be changed in memory; i.e. the `$criteriaTree` `data.tree`
#' object that is passed in `criteria` is passed by _reference_, not by value
#' (the default for normal R objects).
#'
#' @param weights The weights object.
#' @param scorer The scorer id (a character vector).
#' @param criteria The criteria object.
#' @param rootWeight The weight for the root (normally, 1).
#' @param rootParentCriterion_id The identifier of the root.
#' @param weightName The name to use for the variable with these weights.
#'
#' @return A dataframe with weights (invisibly). Note the updated criteria
#' tree: that, after
#' all, is changed in memory because it's passed by reference instead of by
#' value.
#' @export
add_weights_to_criteriaTree <- function(weights,
                                        scorer,
                                        criteria,
                                        rootWeight = 1,
                                        rootParentCriterion_id = "-",
                                        weightName = paste0(scorer, "_weight")) {

  res <-
    as.data.frame(
      weights$individualWeights[[scorer]][, c('scorer',
                                              'id',
                                              'weight')]);
  names(res) <-
    c("scorer",
      "criterion_id",
      weightName);

  res[, weightName] <-
    as.numeric(res[, weightName]);

  ### Set weight for root
  res[res$parentCriterion_id==rootParentCriterion_id,
      weightName] <-
    rootWeight;

  for (i in res$criterion_id) {
    tmpNode <-
      data.tree::FindNode(criteria$criteriaTree, i);
    tmpNode[[weightName]] <-
      res[res$criterion_id==i,
          weightName] / 100;
  }

  ### Set to rootWeight for root
  criteria$criteriaTree[[rootParentCriterion_id]][[weightName]] <- rootWeight;
  criteria$criteriaTree[[rootParentCriterion_id]][[weightName]] <- rootWeight;

  ### Accumulate towards root
  criteria$criteriaTree$Do(function(node) {
    node[[paste0(weightName, "_product")]] <-
      prod(node$Get(weightName, traversal="ancestor"),
           na.rm=TRUE);
    node[[paste0(weightName, "_total")]] <-
      ifelse(data.tree::isLeaf(node),
             node[[paste0(weightName, "_product")]],
             NA);
  });

  return(invisible(res));

}
