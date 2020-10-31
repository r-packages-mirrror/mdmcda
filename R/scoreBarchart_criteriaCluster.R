#' Create a bar chart with scores per criteria cluster
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
scoreBarchart_criteriaCluster <- function(weighedEstimates,
                                          estimateCol,
                                          parentCriterion_ids = unique(weighedEstimates$parentCriterion_id),
                                          parentCriterion_labels = parentCriterion_ids,
                                          scenario_ids = unique(weighedEstimates$scenario_id),
                                          scenario_labels = scenario_ids,
                                          strokeColor = "black",
                                          strokeSize = .1,
                                          title = "MDMCDA criteria cluster bar chart",
                                          xLab = "Criteria Cluster",
                                          yLab = estimateCol,
                                          theme = ggplot2::theme_minimal(base_size = mdmcda::opts$get("ggBaseSize")),
                                          guides = ggplot2::guide_legend(nrow = 1),
                                          legend.position = "top") {

  tmpDf <-
    do.call(rbind,
            lapply(unique(weighedEstimates$scenario_id),
                   function(scenario_id) {
                     res <-
                       aggregate_estimates_by_criterionCluster(weighedEstimates[weighedEstimates$scenario_id==scenario_id, ],
                                                               estimateCol);
                     res$scenario_id <- scenario_id;
                     return(res);
                   }));

  tmpDf$parentCriterion_id <- factor(as.character(tmpDf$parentCriterion_id),
                                     levels=parentCriterion_ids,
                                     labels=parentCriterion_labels,
                                     ordered=TRUE);

  tmpDf$scenario_id <- factor(as.character(tmpDf$scenario_id),
                              levels=scenario_ids,
                              labels=scenario_labels,
                              ordered=TRUE);

  res <-
    ggplot2::ggplot(data = tmpDf,
                    mapping = ggplot2::aes_string(x = "parentCriterion_id",
                                                  y = estimateCol,
                                                  group = "scenario_id",
                                                  fill="scenario_id")) +
    ggplot2::geom_col(position=ggplot2::position_dodge()) +
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
