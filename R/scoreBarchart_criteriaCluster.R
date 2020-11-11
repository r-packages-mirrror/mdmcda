#' Create a bar chart with scores per criteria cluster
#'
#' @param weightedEstimates A `weightedEstimates` object.
#' @param estimateCol The column name with the estimates to use.
#' @param parentCriterion_ids The parent criteria to include.
#' @param parentCriterion_labels The labels for the parent criteria.
#' @param scenario_ids The scenarios to include.
#' @param scenario_labels The labels for the scenarios.
#' @param strokeColor,strokeSize The color and pen width of the stroke.
#' @param title,xLab,yLab The title and x and y axis labels.
#' @param theme The `ggplot2` theme to use.
#' @param guides A guides argument to tweak the legend.
#' @param legend.position,legend.box.margin The position and spacing for the
#' legend.
#'
#' @return A `ggplot2` plot.
#' @export
scoreBarchart_criteriaCluster <- function(weightedEstimates,
                                          estimateCol,
                                          parentCriterionOrder = unique(weightedEstimates$parentCriterion_id),
                                          parentCriterionLabels = parentCriterion_ids,
                                          scenarioOrder = unique(weightedEstimates$scenario_id),
                                          scenarioLabels = scenario_ids,
                                          strokeColor = "black",
                                          strokeSize = .1,
                                          title = "MDMCDA criteria cluster bar chart",
                                          xLab = "Criteria Cluster",
                                          yLab = estimateCol,
                                          theme = ggplot2::theme_minimal(base_size = mdmcda::opts$get("ggBaseSize")),
                                          guides = ggplot2::guide_legend(nrow = 1),
                                          legend.position = "top") {

  parentCriterionLabel_col <- mdmcda::opts$get("parentCriterionLabel_col");
  scenarioLabel_col <- mdmcda::opts$get("scenarioLabel_col");


  tmpDf <-
    criteriaCluster_df(weightedEstimates = weightedEstimates,
                       estimateCol = estimateCol,
                       parentCriterionOrder = parentCriterionOrder,
                       parentCriterionLabels = parentCriterionLabels,
                       scenarioOrder = scenarioOrder,
                       scenarioLabels = scenarioLabels);

  res <-
    ggplot2::ggplot(data = tmpDf,
                    mapping = ggplot2::aes_string(x = parentCriterionLabel_col,
                                                  y = estimateCol,
                                                  group = scenarioLabel_col,
                                                  fill = scenarioLabel_col)) +
    ggplot2::geom_col(position=ggplot2::position_dodge(),
                      color = strokeColor,
                      size = strokeSize) +
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
