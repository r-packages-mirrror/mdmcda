#' @export
scorerWeightPlot <- function(weights,
                             weightsMeansAndSDs,
                             criteriaCluster_id,
                             meanColumns = NULL,
                             meanColors = NULL,
                             meanAlphas = NULL,
                             title = paste0("Scorer weight plot for criteria cluster",
                                            criteriaCluster_id),
                             theme = ggplot2::theme_minimal(base_size = dmcda::opts$get("ggBaseSize"))) {
  res <-
    ggplot2::ggplot(data=weights$allWeights[weights$allWeights$parentCriterion_id==criteriaCluster_id, ],
                    mapping=ggplot2::aes_string(y="criterion_id",
                                                x="weight")) +
    ggplot2::geom_jitter(mapping=ggplot2::aes_string(color="scorer"),
                         alpha=.5,
                         width=.5,
                         height=.1,
                         size=5,
                         na.rm=TRUE) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::labs(title=title) +
    theme +
    ggplot2::coord_cartesian(xlim=c(0, 100)) +
    ggrepel::geom_text_repel(mapping=ggplot2::aes_string(color="scorer",
                                                         label="scorerNr"),
                             size=2.5,
                             point.padding = .25,
                             segment.alpha=.5,
                             alpha=.5,
                             na.rm=TRUE);

  ### Add means
  if ((!is.null(meanColumns)) && (!is.null(meanColors)) && (!is.null(meanAlphas))) {
    if (length(unique(length(meanColumns),
                      length(meanColors),
                      length(meanAlphas))) != 1) {
      stop("Arguments `meanColumns`, `meanColors`, and `meanAlphas` must have the same length!");
    }

    tmpDf <- weightsMeansAndSDs[weightsMeansAndSDs$parentCriterion_id==criteriaCluster_id, ];
    tmpDf$ymin <- as.numeric(factor(tmpDf$criterion_id))-.4;
    tmpDf$ymax <- as.numeric(factor(tmpDf$criterion_id))+.4;

    for (i in seq_along(meanColumns)) {
      res <- res +
        ggplot2::geom_linerange(data=tmpDf,
                                mapping=ggplot2::aes_string(x=meanColumns[i],
                                                            ymin='ymin',
                                                            ymax='ymax'),
                                size=2,
                                alpha=meanAlphas[i],
                                color=meanColors[i]);
    }
  }

  return(res);
}
