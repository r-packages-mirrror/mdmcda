#' Create a bar chart with scores per criteria cluster per scenario
#'
#' @param weighedEstimates A `weighedEstimates` object.
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
scoreBarchart_scenariosByCluster <- function(weighedEstimates,
                                             estimateCol,
                                             parentCriterion_ids = unique(weighedEstimates$parentCriterion_id),
                                             parentCriterion_labels = NULL,
                                             scenario_ids = unique(weighedEstimates$scenario_id),
                                             scenario_labels = NULL,
                                             strokeColor = "black",
                                             strokeSize = .1,
                                             title = "MDMCDA scenarios by criteria cluster bar chart",
                                             xLab = "Scenario",
                                             yLab = estimateCol,
                                             theme = ggplot2::theme_minimal(base_size = mdmcda::opts$get("ggBaseSize")),
                                             guides = ggplot2::guide_legend(nrow = 2),
                                             legend.position = "top") {

  if (is.null(parentCriterion_labels)) {
    parentCriterion_labels <-
      stats::setNames(parentCriterion_ids,
                      nm = parentCriterion_ids);
  }

  if (is.null(scenario_labels)) {
    scenario_labels <-
      stats::setNames(scenario_ids,
                      nm = scenario_ids);
  }

  tmpDf <-
    criteriaCluster_df(weighedEstimates = weighedEstimates,
                       estimateCol = estimateCol,
                       parentCriterion_ids = parentCriterion_ids,
                       parentCriterion_labels = parentCriterion_labels,
                       scenario_ids = scenario_ids,
                       scenario_labels = scenario_labels);

  res <-
    ggplot2::ggplot(data = tmpDf,
                    mapping = ggplot2::aes_string(x = "scenario_label",
                                                  y = estimateCol,
                                                  group = "parentCriterion_label",
                                                  fill="parentCriterion_label")) +
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
