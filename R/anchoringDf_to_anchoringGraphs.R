anchoringDf_to_anchoringGraphs <- function(anchoringDf,
                                           criteriaDf,
                                           graphTitleWrapping = 40,
                                           graphDescriptionWrapping = 60,
                                           graphLabelWrapping = 60,
                                           base_size = 12,
                                           score_size = 8,
                                           dot_size = 6,
                                           showGraphs = TRUE) {

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

  res <-
    apply(anchoringDf,
          1,
          function(x) {

            plotTitle <-
              paste0(
                strwrap(
                  criteriaDf[, criterionLabel_col][criteriaDf[, criterionId_col] == x[criterionId_col]],
                             width=graphTitleWrapping),
                collapse="\n"
              );
            plotSubtitle <-
              paste0(
                strwrap(
                  criteriaDf[, criterionDescription_col][criteriaDf[, criterionId_col] == x[criterionId_col]],
                  width=graphDescriptionWrapping),
                collapse="\n"
              );

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
                                  size = dot_size) +
              ggplot2::annotate(geom="text",
                                x=-.25,
                                y=0,
                                hjust=1,
                                label=0,
                                size=score_size) +
              ggplot2::theme_minimal(base_size = base_size) +
              ggplot2::theme(panel.grid = ggplot2::element_blank(),
                             axis.text.x = ggplot2::element_blank(),
                             plot.background = ggplot2::element_rect(color="black",
                                                                     fill="transparent",
                                                                     size=1),
                             plot.title = ggplot2::element_text(hjust=1),
                             plot.subtitle = ggplot2::element_text(hjust=1),
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

            if ((!is.null(lo_score)) && (!is.na(lo_score)) && (lo_score != 0)) {
              res <- res +
                ggplot2::geom_point(x = 0,
                                    y = lo_score,
                                    size = dot_size) +
                ggplot2::annotate(geom="text",
                                  y=lo_score,
                                  x=-.25,
                                  hjust=1,
                                  label=lo_score,
                                  size=score_size) +
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

            if ((!is.null(hi_score)) && (!is.na(hi_score)) && (hi_score != 0)) {
              res <- res +
                ggplot2::geom_point(x = 0,
                                    y = hi_score,
                                    size = dot_size) +
                ggplot2::annotate(geom="text",
                                  x=-.25,
                                  y=hi_score,
                                  hjust=1,
                                  label=hi_score,
                                  size=score_size) +
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

  names(res) <-
    anchoringDf[, criterionId_col];

  return(res);

}
