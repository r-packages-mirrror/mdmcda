#' @export
load_criteria <- function(input,
                          extension = "jmd",
                          regex = NULL,
                          recursive = TRUE,
                          encoding = "UTF-8",
                          graphLabelWrapping = 25) {

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

  anchoringGraphs <-
    apply(anchoringDf,
          1,
          function(x) {
            lo_score <- as.numeric(x['lo_score']);
            hi_score <- as.numeric(x['hi_score']);

            res <-
              ggplot2::ggplot(data=data.frame(x=0, y=0)) +
              ggplot2::geom_rect(xmin = -10,
                                 ymin = -100,
                                 xmax = 10,
                                 ymax = 100,
                                 fill="white") +
              ggplot2::geom_hline(yintercept = c(100, 50, 0, -50, -100),
                                  color="#BBBBBB",
                                  size=1) +
              ggplot2::geom_point(mapping=ggplot2::aes_string(x = 'x',
                                                              y = 'y'),
                                  size = 8) +
              ggplot2::annotate(geom="text",
                                x=-.25,
                                y=0,
                                hjust=1,
                                label=0,
                                size=10) +
              ggplot2::theme_minimal(base_size = 14) +
              ggplot2::theme(panel.grid = ggplot2::element_blank(),
                             axis.text.x = ggplot2::element_blank(),
                             plot.background = ggplot2::element_rect(color="black",
                                                                     fill="transparent",
                                                                     size=1),
                             plot.margin = grid::unit(rep(.025,4), units="npc")) +
              ggplot2::coord_cartesian(ylim=c(-125, 125),
                                       xlim=c(-1, 0.25)) +
              ggplot2::labs(x=NULL,
                            y=NULL);

            yBreaks <- 0;
            yLabels <- ifelse(!is.null(graphLabelWrapping),
                              paste0(strwrap(x['zero_label'],
                                             width=graphLabelWrapping),
                                     collapse="\n"),
                              x['zero_label']);


            if (lo_score != 0) {
              res <- res +
                ggplot2::geom_point(x = 0,
                                    y = lo_score,
                                    size = 8) +
                ggplot2::annotate(geom="text",
                                  y=lo_score,
                                  x=-.25,
                                  hjust=1,
                                  label=lo_score,
                                  size=10) +
                ggplot2::geom_segment(x=0,
                                      y=0,
                                      xend=0,
                                      yend=lo_score,
                                      size=2);
              yBreaks <- c(lo_score,
                           yBreaks);
              yLabels <- c(ifelse(!is.null(graphLabelWrapping),
                                  paste0(strwrap(x['lo_label'],
                                                 width=graphLabelWrapping),
                                         collapse="\n"),
                                  x['lo_label']),
                           yLabels);
            }

            if (hi_score != 0) {
              res <- res +
                ggplot2::geom_point(x = 0,
                                    y = hi_score,
                                    size = 8) +
                ggplot2::annotate(geom="text",
                                  x=-.25,
                                  y=hi_score,
                                  hjust=1,
                                  label=hi_score,
                                  size=10) +
                ggplot2::geom_segment(x=0,
                                      y=0,
                                      xend=0,
                                      yend=hi_score,
                                      size=2);

                yBreaks <- c(yBreaks,
                             hi_score);
                yLabels <- c(yLabels,
                             ifelse(!is.null(graphLabelWrapping),
                                    paste0(strwrap(x['hi_label'],
                                                   width=graphLabelWrapping),
                                           collapse="\n"),
                                    x['hi_label']));
            }

            res <- res +
              ggplot2::scale_y_continuous(breaks = yBreaks,
                                          labels = yLabels);

            return(res);

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
