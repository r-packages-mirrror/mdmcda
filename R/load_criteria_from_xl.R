#' Read the criteria from a spreadsheet
#'
#' This spreadsheet has to have column names '`id`', '`parentCriterion`',
#' '`label`', '`description`', and '`isLeaf`'.
#'
#' @param input The file to read from.
#' @param showGraphs Whether to show the anchoring graphs (passed on to
#' [mdmcda::anchoringDf_to_anchoringGraphs()]).
#' @param ... Passed on to [mdmcda::anchoringDf_to_anchoringGraphs()].
#'
#' @return A `criteria` object.
#' @export
load_criteria_from_xl <- function(input,
                                  showGraphs = TRUE,
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
    sort(unique(criteria$criteriaDf$id));
  parentCriterionIds <-
    as.data.frame(unique(criteria$criteriaDf[, c('id',
                                                 'parentCriterion')]));
  parentCriterionIds <-
    stats::setNames(parentCriterionIds$parentCriterion,
                    parentCriterionIds$id);
  parentCriteriaIds <-
    unique(criteria$criteriaDf[criteria$criteriaDf$parentCriterion=="outcomes",
                               'id']);
  parentCriteriaIds <-
    unique(parentCriterionIds)[nchar(unique(parentCriterionIds))>1];

  childCriteriaIds <-
    stats::setNames(lapply(unique(parentCriterionIds),
                           function(i)
                             names(parentCriterionIds[parentCriterionIds==i])),
                    unique(parentCriterionIds));

  childCriteriaByCluster <-
    criteria$criteriaDf[criteria$criteriaDf$isLeaf, ]
  childCriteriaByCluster <-
    childCriteriaByCluster$id[order(childCriteriaByCluster$parentCriterion)];

  ### Store in 'criteria' object for convenience
  res$convenience <-
    list(criterionIds = criterionIds,
         parentCriterionIds = parentCriterionIds,
         parentCriteriaIds = parentCriteriaIds,
         childCriteriaIds = childCriteriaIds,
         childCriteriaByCluster = childCriteriaByCluster);

  class(res) <-
    c("dmcda", "criteria");

  return(res);

}
