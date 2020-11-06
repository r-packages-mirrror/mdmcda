#' @export
scoreBarchart_decisions_criteria <- function(weighedEstimates,
                                             scenario_id,
                                             scenario_label = scenario_id,
                                             estimateCol,
                                             decisionOrder = NULL,
                                             decisionLabels = NULL,
                                             criterionOrder = NULL,
                                             criterionLabels = NULL,
                                             strokeSize = 0,
                                             strokeColor = "black",
                                             wrapDecisionLabels = 20,
                                             wrapCriterionLabels = 50,
                                             title = paste0("MDMCDA bar chart to compare decisions for ", scenario_label),
                                             xLab = "Decisions",
                                             yLab = "Weighed estimated effect",
                                             theme = ggplot2::theme_minimal(base_size = mdmcda::opts$get("ggBaseSize")),
                                             guides = ggplot2::guide_legend(ncol = 2),
                                             axis.text.x.bottom = ggplot2::element_text(angle = 90,
                                                                                        hjust = 1,
                                                                                        vjust = 1),
                                             legend.position = "bottom",
                                             legend.box.margin = ggplot2::margin(.5, .5, .5, .5, "cm")) {

  scenarioId_col <- mdmcda::opts$get("scenarioId_col");
  decisionId_col <- mdmcda::opts$get("decisionId_col");
  criterionId_col <- mdmcda::opts$get("criterionId_col");
  decisionLabel_col <- mdmcda::opts$get("decisionLabel_col");
  criterionLabel_col <- mdmcda::opts$get("criterionLabel_col");
  scenarioLabel_col <- mdmcda::opts$get("scenarioLabel_col");

  if (is.null(decisionOrder)) {
    decisionOrder <- unique(weighedEstimates[, decisionId_col]);
  }
  if (is.null(criterionOrder)) {
    criterionOrder <- unique(weighedEstimates[, criterionId_col]);
  }
  if (is.null(decisionLabels)) {
    decisionLabels <- stats::setNames(decisionOrder,
                                      nm = decisionOrder);
  }
  if (is.null(criterionLabels)) {
    criterionLabels <- stats::setNames(criterionOrder,
                                       nm = criterionOrder);
  }

  criterionLabels <-
    unlist(lapply(criterionLabels, function(x)
      paste(strwrap(x, wrapCriterionLabels), collapse="\n")
    ));
  decisionLabels <-
    unlist(lapply(decisionLabels, function(x)
      paste(strwrap(x, wrapDecisionLabels), collapse="\n")
    ));

  tmpDf <-
    weighedEstimates[weighedEstimates[, scenarioId_col] == scenario_id,
                     c(decisionId_col,
                       criterionId_col,
                       estimateCol)];

  tmpDf[, decisionLabel_col] <-
    factor(tmpDf[, decisionId_col],
           levels = decisionOrder,
           labels = decisionLabels[decisionOrder],
           ordered = TRUE);
  tmpDf[, criterionLabel_col] <-
    factor(tmpDf[, criterionId_col],
           levels = criterionOrder,
           labels = criterionLabels[criterionOrder],
           ordered = TRUE);

  if (strokeSize == 0) {
    strokeType <- 0;
  } else {
    strokeType <- 1;
  }

  res <-
    ggplot2::ggplot(data = tmpDf,
                    mapping = ggplot2::aes_string(x=decisionLabel_col,
                                                  y=estimateCol,
                                                  fill=criterionLabel_col)) +
    ggplot2::geom_col(color =strokeColor,
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
