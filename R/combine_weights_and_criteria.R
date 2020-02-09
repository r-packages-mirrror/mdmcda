#' @export
combine_weights_and_criteria <- function(weightsMeansAndSDs,
                                         criteria,
                                         weightCols = c(mean = 'weight_mean_percentage',
                                                        clusterSizeWeighted = 'mean_clusterSizeWeighted'),
                                         rootWeight = 1,
                                         rootParentCriterion_id = "-") {

  ### Add to the criteria tree
  for (i in weightsMeansAndSDs$criterion_id) {
    tmpNode <-
      data.tree::FindNode(criteria$criteriaTree, i)
    for (j in names(weightCols)) {
      tmpNode[[j]] <-
        weightsMeansAndSDs[weightsMeansAndSDs$criterion_id==i,
                           weightCols[j]];
    }
  }

  ### Set to rootWeight for root
  for (j in names(weightCols)) {
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
  }

  return(list(weightsMeansAndSDs=weightsMeansAndSDs,
              criteria = criteria));

}
