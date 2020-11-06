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

  criterionId_col          <- mdmcda::opts$get("criterionId_col");
  criterionLabel_col       <- mdmcda::opts$get("criterionLabel_col");
  criterionDescription_col <- mdmcda::opts$get("criterionDescription_col");
  parentCriterionId_col    <- mdmcda::opts$get("parentCriterionId_col");
  decisionId_col           <- mdmcda::opts$get("decisionId_col");
  decisionLabel_col        <- mdmcda::opts$get("decisionLabel_col");
  alternativeValue_col     <- mdmcda::opts$get("alternativeValue_col");
  alternativeLabel_col     <- mdmcda::opts$get("alternativeLabel_col");
  scenarioId_col           <- mdmcda::opts$get("scenarioId_col");
  weightProfileId_col      <- mdmcda::opts$get("weightProfileId_col");
  score_col                <- mdmcda::opts$get("score_col");
  leafCriterion_col        <- mdmcda::opts$get("leafCriterion_col");
  rootCriterionId          <- mdmcda::opts$get("rootCriterionId");

  if (!file.exists(input)) {
    stop("Specified file ('", input, "') does not exist!");
  }

  fullCriteriaDf <-
    as.data.frame(openxlsx::read.xlsx(input),
                  stringsAsFactors = FALSE);

  # if ('isLeaf' %in% names(fullCriteriaDf)) {
  #   warning(
  #     paste0(
  #       "Column 'isLeaf' encountered; this is obsolete, renaming to ",
  #       "'", leafCriterion_col, "'!"
  #     )
  #   );
  #   names(fullCriteriaDf)[names(fullCriteriaDf) == 'isLeaf'] <-
  #     leafCriterion_col;
  # }

  criteriaDf <-
    fullCriteriaDf[, c(criterionId_col,
                       parentCriterionId_col,
                       criterionLabel_col,
                       criterionDescription_col,
                       leafCriterion_col)];

  anchoringDf <-
    fullCriteriaDf[fullCriteriaDf[, leafCriterion_col],
                   c(criterionId_col,
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
    sort(unique(res$criteriaDf[, criterionId_col]));
  parentCriterionIds_by_childId <-
    as.data.frame(unique(res$criteriaDf[, c(criterionId_col,
                                            parentCriterionId_col)]));
  parentCriterionIds_by_childId <-
    stats::setNames(parentCriterionIds_by_childId[, parentCriterionId_col],
                    parentCriterionIds_by_childId[, criterionId_col]);
  parentCriterionIds <-
    unique(res$criteriaDf[res$criteriaDf[, parentCriterionId_col]==rootCriterionId,
                          criterionId_col]);
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
    res$criteriaDf[res$criteriaDf[, leafCriterion_col], criterionId_col];

  ### Store in 'criteria' object for convenience
  res$convenience <-
    list(
      rootCriterionId = rootCriterionId,
      criterionIds = criterionIds,
      leafCriterionIds = leafCriterionIds,
      parentCriterionIds = parentCriterionIds,
      parentCriterionIds_by_childId = parentCriterionIds_by_childId,
      childCriterionIds_by_parentId = childCriterionIds_by_parentId,
      topCriterionClusters = childCriterionIds_by_parentId[[rootCriterionId]]
    );

  class(res) <-
    c("mdmcda", "criteria");

  return(res);

}
