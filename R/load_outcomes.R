load_outcomes <- function(path) {

  outcomes <-
    yum::load_and_simplify_dir(path,
                               select="outcomes");

  outcomesTree <-
    yum::build_tree(outcomes);

  ### Set id as id and fill fields that are sometimes empty
  outcomesTree$Do(function(node) {
    node$id <- node$name;
    node$parentOutcome <- ifelse(is.null(node$parent),
                                 "-",
                                 node$parent$name);
    node$description <- ifelse(is.null(node$description),
                               "-",
                               node$description);
  });

  outcomesDf <-
    data.frame(outcomesTree$Get('name'),
               outcomesTree$Get('parentOutcome'),
               outcomesTree$Get('label'),
               outcomesTree$Get('description'),
               outcomesTree$Get(function(node) return(node$isLeaf)),
               stringsAsFactors=FALSE);
  names(outcomesDf) <-
    c('id', 'parentOutcome', 'label', 'description', 'isLeaf');
  row.names(outcomesDf) <-
    NULL;

  res <- list(outcomes = outcomes,
              outcomesTree = outcomesTree,
              outcomesDf = outcomesDf);

  return(res);

}
