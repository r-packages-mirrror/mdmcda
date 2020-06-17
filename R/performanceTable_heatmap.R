#' @export
performanceTable_heatmap <- function(weighedEstimates,
                                     estimateCol,
                                     scenario_id,
                                     criterionLabels = NULL,
                                     decisionLabels = NULL,
                                     theme = ggplot2::theme_minimal(base_size = mdmcda::opts$get("ggBaseSize"))) {
  fullEstimateRange <-
    range(weighedEstimates[, estimateCol],
          na.rm=TRUE);
  tmpDf <-
    weighedEstimates[weighedEstimates$scenario_id==scenario_id,
                     c("decision_id",
                       "criterion_id",
                       estimateCol)];

  criteria_by_parent <-
    unique(weighedEstimates[, c('criterion_id', 'parentCriterion_id')]);
  criteria_by_parent <-
    criteria_by_parent[order(criteria_by_parent$parentCriterion_id),
                       'criterion_id'];

  criterionLevels <-
    criteria_by_parent;
  if (is.null(criterionLabels)) {
    criterionLabels <-
      stats::setNames(criterionLevels,
                      nm = criterionLevels);
  }

  decisionLevels <-
    rev(sort(unique(as.character(tmpDf$decision_id))));
  if (is.null(decisionLabels)) {
    decisionLabels <-
      stats::setNames(decisionLevels,
                      nm = decisionLevels);
  }

  tmpDf$decision_id <-
    factor(as.character(tmpDf$decision_id),
           levels = decisionLevels,
           labels = decisionLabels[decisionLevels],
           order = TRUE);
  tmpDf$criterion_id <-
    factor(as.character(tmpDf$criterion_id),
           levels = criterionLevels,
           labels = criterionLabels[criterionLevels],
           ordered = TRUE);

  res <-
    ggplot2::ggplot(data = tmpDf,
                    mapping = ggplot2::aes_string(x='criterion_id',
                                                  y='decision_id',
                                                  fill=estimateCol)) +
    ggplot2::geom_tile(color="black") +
    ggplot2::scale_fill_viridis_c(name = "Weighed\nestimated\neffect",
                                  limits=fullEstimateRange) +
    theme +
    ggplot2::scale_x_discrete(position="top") +
    ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle = 90,
                                                           hjust = 0,
                                                           vjust = 0.5)) +
    ggplot2::labs(title=paste0("Heatmap for ", scenario_id),
                  x="Criterion",
                  y="Decision") +
    NULL;
  return(res);
}
