#' @export
scorerWeights_to_profile <- function(weights,
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

  ### Clone criteria tree
  critTree <- data.tree::Clone(criteria$criteriaTree);

  for (i in res$criterion_id) {
    tmpNode <-
      data.tree::FindNode(critTree, i);
    tmpNode[[weightName]] <-
        res[res$criterion_id==i,
            weightName] / 100;
  }

  ### Set to rootWeight for root
  critTree[[rootParentCriterion_id]][[weightName]] <- rootWeight;
  critTree[[rootParentCriterion_id]][[weightName]] <- rootWeight;

  ### Accumulate towards root
  critTree$Do(function(node) {
    node[[paste0(weightName, "_product")]] <-
      prod(node$Get(weightName, traversal="ancestor"),
           na.rm=TRUE);
    node[[paste0(weightName, "_total")]] <-
      ifelse(data.tree::isLeaf(node),
             node[[paste0(weightName, "_product")]],
             NA);
  });

  ### Add results from tree accumulation to data frame
  for (i in res$criterion_id) {
    res[res$criterion_id==i,
        paste0(weightName, "_product")] <-
      critTree$Get(paste0(weightName, "_product"),
                          filterFun=function(node)
                            node$name==i);
    res[res$criterion_id==i,
        paste0(weightName, "_total")] <-
      critTree$Get(paste0(weightName, "_total"),
                          filterFun=function(node)
                            node$name==i);
  }

  ### Add rescaled cluster size weighted total weights
  res[[paste0(weightName, "_total_proportion")]] <-
    res[[paste0(weightName, "_total")]] /
    sum(res[[paste0(weightName, "_total")]], na.rm=TRUE);

  return(res);

}
