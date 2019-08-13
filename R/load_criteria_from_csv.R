#' @export
load_criteria_from_csv <- function(input,
                                   encoding = "UTF-8",
                                   graphLabelWrapping = 25) {

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
    apply(anchoringDf,
          1,
          function(x) {

            plotTitle <-
              criteriaDf$label[criteriaDf$id == x['id']];
            plotSubtitle <-
              criteriaDf$description[criteriaDf$id == x['id']];

            lo_score <- as.numeric(x['lo_score']);
            hi_score <- as.numeric(x['hi_score']);

            if (is.na(lo_score) && is.na(hi_score)) {
              lo_score <- 0;
              hi_score <- 100;
            }

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
              ggplot2::theme_minimal(base_size = 10) +
              ggplot2::theme(panel.grid = ggplot2::element_blank(),
                             axis.text.x = ggplot2::element_blank(),
                             plot.background = ggplot2::element_rect(color="black",
                                                                     fill="transparent",
                                                                     size=1),
                             plot.margin = grid::unit(rep(.025,4), units="npc")) +
              ggplot2::coord_cartesian(ylim=c(-125, 125),
                                       xlim=c(-1, 0.25)) +
              ggplot2::labs(x=NULL,
                            y=NULL,
                            title = plotTitle,
                            subtitle = plotSubtitle);

            yBreaks <- 0;
            yLabels <- ifelse(!is.null(graphLabelWrapping),
                              paste0(strwrap(x['zero_label'],
                                             width=graphLabelWrapping),
                                     collapse="\n"),
                              x['zero_label']);

            if (!is.null(lo_score) && !is.na(lo_score) && !lo_score != 0) {
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

            if (!is.null(hi_score) && !is.na(hi_score) && !hi_score != 0) {
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
