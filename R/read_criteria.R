#' @export
read_criteria <- function(input,
                          extension = "jmd",
                          regex = NULL,
                          recursive = TRUE,
                          encoding = "UTF-8",
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

  if (is.null(regex)) {
    regex <- paste0("^(.*)\\.", extension, "$");
  }

  criteria <-
    yum::read_and_simplify_dir(path=input,
                               fileRegexes = regex,
                               recursive = recursive,
                               encoding=encoding,
                               select="criteria");

  criteriaTree <-
    yum::build_tree(criteria);

  ### Set id as id and fill fields that are sometimes empty
  criteriaTree$Do(function(node) {
    node$id <- node$name;
    node$parentCriterion <- ifelse(is.null(node$parent),
                                   "-",
                                   node$parent$name);
    node$description <- ifelse(is.null(node$description),
                               "-",
                               node$description);
  });

  criteriaDf <-
    data.frame(criteriaTree$Get('name'),
               criteriaTree$Get('parentCriterion'),
               criteriaTree$Get('label'),
               criteriaTree$Get('description'),
               criteriaTree$Get(function(node) return(node$isLeaf)),
               stringsAsFactors=FALSE);
  names(criteriaDf) <-
    c('id', 'parentCriterion', 'label', 'description', 'leafCriterion');
  row.names(criteriaDf) <-
    NULL;

  fullCriteriaDf <-
    data.frame(criteriaTree$Get('label'),
               criteriaTree$Get('description'),
               criteriaTree$Get('lo_score'),
               criteriaTree$Get('lo_label'),
               0,
               criteriaTree$Get('zero_label'),
               criteriaTree$Get('hi_score'),
               criteriaTree$Get('hi_label'),
               criteriaTree$Get('name'),
               criteriaTree$Get('parentCriterion'),
               criteriaTree$Get(function(node) return(node$isLeaf)),
               stringsAsFactors=FALSE);
  names(fullCriteriaDf) <-
    c('label', 'description',
      'lo_score', 'lo_label',
      'zero_score', 'zero_label',
      'hi_score', 'hi_label',
      'id', 'parentCriterion', 'leafCriterion');
  row.names(fullCriteriaDf) <-
    NULL;

  anchoringDf <-
    data.frame(criteriaTree$Get('name'),
               criteriaTree$Get('lo_label'),
               criteriaTree$Get('zero_label'),
               criteriaTree$Get('hi_label'),
               criteriaTree$Get('lo_score'),
               criteriaTree$Get('hi_score'),
               stringsAsFactors=FALSE);
  names(anchoringDf) <-
    c('id', 'lo_label', 'zero_label', 'hi_label', 'lo_score', 'hi_score');
  row.names(anchoringDf) <-
    NULL;
  anchoringDf <-
    anchoringDf[criteriaDf$leafCriterion, ];

  anchoringDf$lo_score <-
    as.numeric(anchoringDf$lo_score);
  anchoringDf$hi_score <-
    as.numeric(anchoringDf$hi_score);

  anchoringGraphs <-
    anchoringDf_to_anchoringGraphs(anchoringDf=anchoringDf,
                                   criteriaDf=criteriaDf,
                                   ...);

  res <- list(criteria = criteria,
              criteriaTree = criteriaTree,
              criteriaDf = criteriaDf,
              anchoringDf = anchoringDf,
              fullCriteriaDf = fullCriteriaDf,
              anchoringGraphs = anchoringGraphs);

  class(res) <-
    c("mdmcda", "criteria");

  return(res);

}
