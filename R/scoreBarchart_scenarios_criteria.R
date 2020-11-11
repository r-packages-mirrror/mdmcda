#' @export
scoreBarchart_scenarios_criteria <- function (weightedEstimates,
                                              estimateCol,
                                              criterionOrder = NULL,
                                              criterionLabels = NULL,
                                              scenarioOrder = NULL,
                                              scenarioLabels = NULL,
                                              strokeSize = 0,
                                              strokeColor = "black",
                                              wrapScenarioLabels = 10,
                                              wrapCriterionLabels = 50,
                                              title = "MDMCDA scores by scenario by criterion",
                                              xLab = "Scenarios",
                                              yLab = "Weighted estimated effect",
                                              theme = ggplot2::theme_minimal(base_size = mdmcda::opts$get("ggBaseSize")),
                                              guides = ggplot2::guide_legend(ncol = 2),
                                              axis.text.x.bottom = ggplot2::element_text(angle = 0,
                                                                                         hjust = .5,
                                                                                         vjust = 1),
                                              legend.position = "bottom",
                                              legend.box.margin = ggplot2::margin(.5, .5, .5, .5, "cm")) {

  criterionLabel_col <- mdmcda::opts$get("criterionLabel_col");
  criterionId_col <- mdmcda::opts$get("criterionId_col");
  decisionId_col <- mdmcda::opts$get("decisionId_col");
  decisionLabel_col <- mdmcda::opts$get("decisionLabel_col");
  scenarioId_col <- mdmcda::opts$get("scenarioId_col");
  scenarioLabel_col <- mdmcda::opts$get("scenarioLabel_col");

  if (is.null(criterionOrder)) {
    criterionOrder <- unique(weightedEstimates[, criterionId_col]);
  }
  if (is.null(criterionLabels)) {
    criterionLabels <- stats::setNames(criterionOrder,
                                       nm = criterionOrder);
  }
  if (is.null(decisionOrder)) {
    decisionOrder <- unique(weightedEstimates[, decisionId_col]);
  }
  if (is.null(decisionLabels)) {
    decisionLabels <- stats::setNames(decisionOrder,
                                      nm = decisionOrder);
  }
  if (is.null(scenarioOrder)) {
    scenarioOrder <- unique(weightedEstimates[, scenarioId_col]);
  }
  if (is.null(scenarioLabels)) {
    scenarioLabels <- stats::setNames(scenarioOrder,
                                      nm = scenarioOrder);
  }

  criterionLabels <-
    unlist(lapply(criterionLabels, function(x)
      paste(strwrap(x, wrapCriterionLabels), collapse="\n")
    ));
  scenarioLabels <-
    unlist(lapply(scenarioLabels, function(x)
      paste(strwrap(x, wrapScenarioLabels), collapse="\n")
    ));

  tmpDf <- weightedEstimates[, c(scenarioId_col,
                                decisionId_col,
                                criterionId_col,
                                estimateCol)];

  # tmpDf[, decisionLabel_col] <-
  #   factor(tmpDf[, decisionId_col],
  #          levels = decisionOrder,
  #          labels = decisionLabels[decisionOrder],
  #          ordered = TRUE);
  tmpDf[, criterionLabel_col] <-
    factor(tmpDf[, criterionId_col],
           levels = criterionOrder,
           labels = criterionLabels[criterionOrder],
           ordered = TRUE);
  tmpDf[, scenarioLabel_col] <-
    factor(tmpDf[, scenarioId_col],
           levels = scenarioOrder,
           labels = scenarioLabels[scenarioOrder],
           ordered = TRUE);

  if (strokeSize == 0) {
    strokeType <- 0;
  } else {
    strokeType <- 1;
  }

  res <-
    ggplot2::ggplot(data = tmpDf,
                    mapping = ggplot2::aes_string(x=scenarioLabel_col,
                                                  y=estimateCol,
                                                  fill=criterionLabel_col)) +
    ggplot2::geom_col(color = strokeColor,
                      size = strokeSize,
                      linetype = strokeType) +
    ggplot2::scale_fill_viridis_d(name = "Criterion") +
    ggplot2::scale_x_discrete(position="bottom") +
    theme +
    ggplot2::guides(fill = guides) +
    ggplot2::theme(axis.text.x.bottom = axis.text.x.bottom,
                   plot.title.position = "plot",
                   legend.position = legend.position,
                   legend.box.margin = legend.box.margin) +
    ggplot2::labs(title=title,
                  x=xLab,
                  y=yLab) +
    NULL;
  return(res);
}
