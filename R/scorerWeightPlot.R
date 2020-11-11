#' @export
scorerWeightPlot <- function(weights,
                             weightsMeansAndSDs,
                             parentCriterion_id,
                             criterionOrder = NULL,
                             criterionLabels = NULL,
                             meanColumns = c("weight_mean",
                                             "weight_mean_rescaled"),
                             meanColors = c("black", "black"),
                             meanAlphas = c(0.2, 1),
                             title = "Scorer weight plot for criterion cluster %s",
                             subtitle = "Grey lines are means; black lines are rescaled means.",
                             theme = ggplot2::theme_minimal(base_size = mdmcda::opts$get("ggBaseSize"))) {

  criterionLabel_col <- mdmcda::opts$get("criterionLabel_col");
  criterionId_col <- mdmcda::opts$get("criterionId_col");
  parentCriterionId_col <- mdmcda::opts$get("parentCriterionId_col");
  decisionId_col <- mdmcda::opts$get("decisionId_col");
  decisionLabel_col <- mdmcda::opts$get("decisionLabel_col");
  scenarioId_col <- mdmcda::opts$get("scenarioId_col");
  scenarioLabel_col <- mdmcda::opts$get("scenarioLabel_col");

  if (!is.null(criterionLabels)) {
    parentCriterion_label <- criterionLabels[parentCriterion_id];
  } else {
    parentCriterion_label <- parentCriterion_id;
  }

  title <- sprintf(title, parentCriterion_label);

  tmpWeightDf <-
    weights$allWeights[
      weights$allWeights[, parentCriterionId_col]==parentCriterion_id,
    ];

  if (is.null(criterionOrder)) {
    criterionOrder <- sort(unique(tmpWeightDf[, criterionId_col]));
  } else {
    criterionOrder <-
      intersect(criterionOrder,
                unique(tmpWeightDf[, criterionId_col]));
  }
  if (is.null(criterionLabels)) {
    criterionLabels <-
      stats::setNames(criterionOrder,
                      nm = criterionOrder);
  }
  criterionLabels <-
    criterionLabels[criterionOrder];

  tmpWeightDf[, criterionLabel_col] <-
    factor(tmpWeightDf[, criterionId_col],
           levels = criterionOrder,
           labels = criterionLabels);

  res <-
    ggplot2::ggplot(
      data=tmpWeightDf,
      mapping=ggplot2::aes_string(y=criterionLabel_col,
                                  x="weight")
    ) +
    ggplot2::geom_jitter(mapping=ggplot2::aes_string(color="scorer"),
                         alpha=.5,
                         width=.5,
                         height=.1,
                         size=5,
                         na.rm=TRUE) +
    ggplot2::scale_color_viridis_d(end=.9) +
    ggplot2::coord_cartesian(xlim=c(0, 100)) +
    ggrepel::geom_text_repel(mapping=ggplot2::aes_string(color="scorer",
                                                         label="scorerNr"),
                             size=2.5,
                             point.padding = .25,
                             segment.alpha=.5,
                             alpha=.5,
                             na.rm=TRUE) +
    ggplot2::labs(title=title,
                  subtitle=subtitle,
                  x = parentCriterion_label,
                  y = "Criterion",
                  color = "Scorer",
                  fill = "Scorer",
                  label = "Scorer") +
    theme;

  ### Add means
  if ((!is.null(meanColumns)) && (!is.null(meanColors)) && (!is.null(meanAlphas))) {
    if (length(unique(length(meanColumns),
                      length(meanColors),
                      length(meanAlphas))) != 1) {
      stop("Arguments `meanColumns`, `meanColors`, and `meanAlphas` must have the same length!");
    }

    tmpDf <-
      weightsMeansAndSDs[
        weightsMeansAndSDs[, parentCriterionId_col]==parentCriterion_id,
      ];
    tmpDf[, criterionLabel_col] <-
      factor(tmpDf[, criterionId_col],
             levels = criterionOrder,
             labels = criterionLabels);

    tmpDf$ymin <- as.numeric(factor(tmpDf[, criterionId_col]))-.4;
    tmpDf$ymax <- as.numeric(factor(tmpDf[, criterionId_col]))+.4;

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
