#' Create a bar chart with scores per criteria cluster per scenario
#'
#' @param weighedEstimates A `weighedEstimates` object.
#' @param estimateCol The column name with the estimates to use.
#' @param parentCriterion_ids The parent criteria to include.
#' @param parentCriterion_labels The labels for the parent criteria.
#' @param scenarioOrder The scenarios to include.
#' @param scenarioLabels The labels for the scenarios.
#' @param sortByScore,decreasing Whether to sort the scenarios by their total
#' scores, and if so, whether to sort them in decreasing order (from the left
#' side) or in increasing order.
#' @param strokeColor,strokeSize The color and pen width of the stroke.
#' @param title,xLab,yLab The title and x and y axis labels.
#' @param theme The `ggplot2` theme to use.
#' @param guides A guides argument to tweak the legend.
#' @param legend.position,legend.box.margin The position and spacing for the
#' legend.
#'
#' @return A `ggplot2` plot.
#' @export
scoreBarchart_scenariosByCluster <- function(weighedEstimates,
                                             estimateCol,
                                             parentCriterionOrder = unique(weighedEstimates$parentCriterion_id),
                                             parentCriterionLabels = NULL,
                                             scenarioOrder = unique(weighedEstimates$scenario_id),
                                             scenarioLabels = NULL,
                                             sortByScore = FALSE,
                                             decreasing = TRUE,
                                             strokeSize = 0,
                                             strokeColor = "black",
                                             title = "MDMCDA scores by scenarios by criteria cluster",
                                             xLab = "Scenario",
                                             yLab = estimateCol,
                                             theme = ggplot2::theme_minimal(base_size = mdmcda::opts$get("ggBaseSize")),
                                             guides = ggplot2::guide_legend(nrow = 2),
                                             legend.position = "top") {

  parentCriterionLabel_col <- mdmcda::opts$get("parentCriterionLabel_col");
  scenarioLabel_col <- mdmcda::opts$get("scenarioLabel_col");

  tmpDf <-
    criteriaCluster_df(weighedEstimates = weighedEstimates,
                       estimateCol = estimateCol,
                       parentCriterionOrder = parentCriterionOrder,
                       parentCriterionLabels = parentCriterionLabels,
                       scenarioOrder = scenarioOrder,
                       scenarioLabels = scenarioLabels,
                       sortByScore = sortByScore,
                       decreasing = decreasing);

  if (strokeSize == 0) {
    strokeType <- 0;
  } else {
    strokeType <- 1;
  }

  res <-
    ggplot2::ggplot(data = tmpDf,
                    mapping = ggplot2::aes_string(x = scenarioLabel_col,
                                                  y = estimateCol,
                                                  group = parentCriterionLabel_col,
                                                  fill = parentCriterionLabel_col)) +
    ggplot2::geom_col(position=ggplot2::position_dodge(),
                      color = strokeColor,
                      size = strokeSize,
                      linetype = strokeType) +
    ggplot2::scale_fill_viridis_d(name=NULL) +
    theme +
    ggplot2::guides(fill = guides,
                    color = guides) +
    ggplot2::theme(legend.position=legend.position,
                   legend.box = "horizontal",
                   plot.title.position = "plot") +
    ggplot2::labs(title = title,
                  x = xLab,
                  y = yLab) +
    NULL;

  return(res);

}
