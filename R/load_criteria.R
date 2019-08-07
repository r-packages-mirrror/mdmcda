#' @export
load_criteria <- function(input,
                          extension = "jmd",
                          regex = NULL,
                          recursive = TRUE,
                          encoding = "UTF-8") {

  if (is.null(regex)) {
    regex <- paste0("^(.*)\\.", extension, "$");
  }

  criteria <-
    yum::load_and_simplify_dir(path=input,
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
    c('id', 'parentCriterion', 'label', 'description', 'isLeaf');
  row.names(criteriaDf) <-
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
    anchoringDf[criteriaDf$isLeaf, ];

  anchoringDf$lo_score <-
    as.numeric(anchoringDf$lo_score);
  anchoringDf$hi_score <-
    as.numeric(anchoringDf$hi_score);

  print(anchoringDf$lo_score);

  anchoringGraphs <-
    apply(anchoringDf,
          1,
          function(x) {
            x['lo_score'] <- as.numeric(x['lo_score']);
            x['hi_score'] <- as.numeric(x['hi_score']);
            res <-
              ggplot2::ggplot() +
                ggplot2::geom_point(x = 0,
                                    y = 0,
                                    size = 2);
            yBreaks <- 0;
            yLabels <- x['zero_label'];
            print(x['lo_score']);
            if (x['lo_score'] != 0) {
              res <- res +
                ggplot2::geom_point(x = 0,
                                    y = x['lo_score'],
                                    size = 2);
              yBreaks <- c(x['lo_score'],
                           yBreaks);
              yLabels <- c(x['lo_label'],
                           yLabels);
            }
            print(x['hi_score']);
            if (x['hi_score'] != 0) {
              res <- res +
                ggplot2::geom_point(x = 0,
                                    y = x['hi_score'],
                                    size = 2);
                yBreaks <- c(yBreaks,
                             x['hi_score']);
                yLabels <- c(yLabels,
                             x['hi_label']);
            }

            print(length(yBreaks));
            print(length(yLabels));

            res <- res +
              ggplot2::theme_minimal() +
              ggplot2::scale_y_continuous(breaks = yBreaks,
                                          labels = yLabels);
          });

  names(anchoringGraphs) <-
    anchoringDf$id;

  res <- list(criteria = criteria,
              criteriaTree = criteriaTree,
              criteriaDf = criteriaDf,
              anchoringDf = anchoringDf,
              anchoringGraphs = anchoringGraphs);

  class(res) <-
    c("dmcda", "criteria");

  return(res);

}
