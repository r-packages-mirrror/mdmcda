#' Add weights to criteria tree
#'
#' This function takes weights from a data frame and adds
#' them to the criteria tree. NOTE: the
#' criteria tree is an R6 object. Therefore, when this function is called,
#' the object will be changed in memory; i.e. the `$criteriaTree` `data.tree`
#' object that is passed in `criteria` is passed by _reference_, not by value
#' (the default for normal R objects).
#'
#' @param weightsMeansAndSDs A data frame with weights.
#' @param criteria The criteria object.
#' @param weightCols The column names of the columns to add.
#' @param rootWeight The weight for the root (normally, 1).
#' @param rootParentCriterion_id The identifier of the root.
#' @param weightName The name to use for the variable with these weights.
#'
#' @return Invisibly, the `criteria` object.
#' @export
add_weights_to_criteriaTree <- function(weightsMeansAndSDs,
                                        criteria,
                                        weightCols,
                                        rootWeight = 1,
                                        rootParentCriterion_id = "-") {

  ### Add to the criteria tree
  for (i in weightsMeansAndSDs$criterion_id) {
    tmpNode <-
      data.tree::FindNode(criteria$criteriaTree, i);
    for (j in names(weightCols)) {
      tmpNode[[j]] <-
        weightsMeansAndSDs[weightsMeansAndSDs$criterion_id==i,
                           weightCols[j]];
    }
  }

  ### Set to rootWeight for root
  for (j in names(weightCols)) {
    ### Why did I do this twice? Is that a copy-paste error or does it
    ### work around a bug in data.tree?
    criteria$criteriaTree[[rootParentCriterion_id]][[j]] <- rootWeight;
    criteria$criteriaTree[[rootParentCriterion_id]][[j]] <- rootWeight;
  }

  ### Accumulate towards root
  criteria$criteriaTree$Do(function(node) {
    for (j in names(weightCols)) {
      node[[paste0(j, "_product")]] <-
        prod(node$Get(j, traversal="ancestor"),
             na.rm=TRUE);
      node[[paste0(j, "_total")]] <-
        ifelse(data.tree::isLeaf(node),
               node[[paste0(j, "_product")]],
               NA);
    }
  });

  return(invisible(criteria));

}
