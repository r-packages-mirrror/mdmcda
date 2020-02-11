#' @export
scoreBarchart_criteriaCluster <- function(weighedEstimates,
                                          estimateCol,
                                          title = "DMCDA criteria bar chart") {

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

  tmpDf$parentCriterion_id <- factor(tmpDf$parentCriterion_id,
                                     levels=c("use",
                                              "user_health",
                                              "crime",
                                              "economy",
                                              "political",
                                              "planet"),
                                     labels=c("Use",
                                              "User health",
                                              "Crime prevention",
                                              "Financial costs\nand benefits",
                                              "International\npolitics",
                                              "Environmental\nprotection"),
                                     ordered=TRUE);

  res <-
    ggplot2::ggplot(data = tmpDf,
                    mapping = ggplot2::aes_string(x = "parentCriterion_id",
                                                  y = estimateCol,
                                                  group = "scenario_id",
                                                  fill="scenario_id")) +
    ggplot2::geom_col(position=ggplot2::position_dodge()) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_viridis_d(name=NULL) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1)) +
    ggplot2::theme(legend.position="top", legend.box = "horizontal") +
    NULL;

  return(res);

}
