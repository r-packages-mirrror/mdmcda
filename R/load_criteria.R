#' @export
load_criteria <- function(path) {

  criteria <-
    yum::load_and_simplify_dir(path,
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
    c('id', 'parentCriterion', 'label', 'description', 'isLeaf');
  row.names(criteriaDf) <-
    NULL;

  res <- list(criteria = criteria,
              criteriaTree = criteriaTree,
              criteriaDf = criteriaDf);

  class(res) <-
    c("dmcda", "criteria");

  return(res);

}
