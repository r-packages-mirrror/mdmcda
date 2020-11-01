#' Read the criteria from a spreadsheet
#'
#' This spreadsheet has to have column names '`id`', '`parentCriterion`',
#' '`label`', '`description`', and '`isLeaf`'.
#'
#' @param input The file to read from.
#' @param showGraphs Whether to show the anchoring graphs (passed on to
#' [mdmcda::anchoringDf_to_anchoringGraphs()]).
#' @param rootCriterionId The identifier of the root criterion.
#' @param ... Passed on to [mdmcda::anchoringDf_to_anchoringGraphs()].
#'
#' @return A `criteria` object.
#' @export
read_criteria_from_xl <- function(input,
                                  showGraphs = TRUE,
                                  rootCriterionId = mdmcda::opts$get("rootCriterionId"),
                                  ...) {

  if (!file.exists(input)) {
    stop("Specified file ('", input, "') does not exist!");
  }

  fullCriteriaDf <-
    as.data.frame(openxlsx::read.xlsx(input),
                  stringsAsFactors = FALSE);

  criteriaDf <-
    fullCriteriaDf[, c('id',
                       'parentCriterion',
                       'label',
                       'description',
                       'isLeaf')];

  anchoringDf <-
    fullCriteriaDf[fullCriteriaDf$isLeaf,
                   c('id',
                     'lo_label',
                     'zero_label',
                     'hi_label',
                     'lo_score',
                     'hi_score')];

  anchoringDf$lo_score <-
    as.numeric(anchoringDf$lo_score);
  anchoringDf$hi_score <-
    as.numeric(anchoringDf$hi_score);

  anchoringGraphs <-
    anchoringDf_to_anchoringGraphs(anchoringDf=anchoringDf,
                                   criteriaDf=criteriaDf,
                                   showGraphs=showGraphs,
                                   ...);

  ### In data.tree, isLeaf is an active property, so we have to rename it
  names(criteriaDf)[names(criteriaDf) == "isLeaf"] <- "leafCriterion";

  criteriaTree <- data.tree::as.Node(criteriaDf, mode="network");

  ### Take first child, since the root element (e.g. 'outcomes' or
  ### 'criteria') will always have a parent in the spreadsheet)
  criteriaTree <- criteriaTree$children[[1]];

  res <- list(criteria = NA,
              criteriaTree = criteriaTree,
              criteriaDf = criteriaDf,
              anchoringDf = anchoringDf,
              fullCriteriaDf = fullCriteriaDf,
              anchoringGraphs = anchoringGraphs);

  ###-----------------------------------------------------------------------------
  ### Create convenience vector to find parents for criteria
  ###-----------------------------------------------------------------------------

  criterionIds <-
    sort(unique(res$criteriaDf$id));
  parentCriterionIds_by_childId <-
    as.data.frame(unique(res$criteriaDf[, c('id',
                                            'parentCriterion')]));
  parentCriterionIds_by_childId <-
    stats::setNames(parentCriterionIds_by_childId$parentCriterion,
                    parentCriterionIds_by_childId$id);
  parentCriterionIds <-
    unique(res$criteriaDf[res$criteriaDf$parentCriterion=="outcomes",
                          'id']);
  parentCriterionIds <-
    unique(parentCriterionIds_by_childId)[nchar(unique(parentCriterionIds_by_childId))>1];

  childCriterionIds_by_parentId <-
    stats::setNames(
      lapply(
        unique(parentCriterionIds_by_childId),
        function(i)
          names(parentCriterionIds_by_childId[parentCriterionIds_by_childId==i])),
      unique(parentCriterionIds_by_childId)
    );

  leafCriterionIds <-
    res$criteriaDf[res$criteriaDf$leafCriterion, 'id'];

  ### Store in 'criteria' object for convenience
  res$convenience <-
    list(
      rootCriterionId = rootCriterionId,
      criterionIds = criterionIds,
      leafCriterionIds = leafCriterionIds,
      parentCriterionIds = parentCriterionIds,
      parentCriterionIds_by_childId = parentCriterionIds_by_childId,
      childCriterionIds_by_parentId = childCriterionIds_by_parentId,
      topCriterionClusters = childCriterionIds_by_parentId[[rootCriterionId]]);

  class(res) <-
    c("dmcda", "criteria");

  return(res);

}
