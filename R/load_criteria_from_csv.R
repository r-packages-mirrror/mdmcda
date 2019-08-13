#' @export
load_criteria_from_csv <- function(input,
                                   encoding = "UTF-8",
                                   ...) {

  if (!file.exists(input)) {
    stop("Specified file ('", input, "') does not exist!");
  }

  fullCriteriaDf <-
    read.csv(input);

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
    anchoringDf_to_anchoringGraphs(anchoringDf,
                                   ...);

  res <- list(criteria = NA,
              criteriaTree = NA,
              criteriaDf = criteriaDf,
              anchoringDf = anchoringDf,
              fullCriteriaDf = fullCriteriaDf,
              anchoringGraphs = anchoringGraphs);

  class(res) <-
    c("dmcda", "criteria");

  return(res);

}
