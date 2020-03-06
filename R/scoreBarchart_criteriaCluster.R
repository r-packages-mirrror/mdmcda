#' @export
scoreBarchart_criteriaCluster <- function(weighedEstimates,
                                          estimateCol,
                                          parentCriterion_ids = unique(weighedEstimates$parentCriterion_id),
                                          parentCriterion_labels = parentCriterion_ids,
                                          scenario_ids = unique(weighedEstimates$scenario_id),
                                          scenario_labels = scenario_ids,
                                          xLab = "Criteria Cluster",
                                          yLab = estimateCol,
                                          title = "DMCDA criteria bar chart",
                                          theme = ggplot2::theme_minimal(base_size = dmcda::opts$get("ggBaseSize"))) {

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
    theme +
    ggplot2::scale_fill_viridis_d(name=NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1)) +
    ggplot2::theme(legend.position="top", legend.box = "horizontal") +
    ggplot2::labs(title = title,
                  x = xLab,
                  y = yLab) +
    NULL;

  return(res);

}
