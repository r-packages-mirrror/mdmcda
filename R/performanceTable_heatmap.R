#' Create a performance table heatmap
#'
#' This is a convenient visualisation that shows the (weighed) estimates
#' for each decision/criterion combination. Decisions are sorted
#' alphabetically and criteria are sorted alphabetically within their
#' clusters, which are also sorted alphabetically.
#'
#' @param weighedEstimates A `weighedEstimates` data frame as produced by
#' [mdmcda::weigh_estimates()].
#' @param estimateCol The name of the column with the estimates.
#' @param scenario_id The identifier of the scenario to create the heatmap
#' for.
#' @param criterionOrder,decisionOrder Optionally, vectors with criterion
#' and decision identifiers to indicate the order that should be used.
#' @param criterionLabels,decisionLabels Optionally, named vectors with labels
#' for the criteria and decisions. These have to be named vectors, with the
#' elements being the labels, and the names the identifiers.
#' @param criterionId_col,parentCriterionId_col,criterionLabel_col,decisionId_col,decisionLabel_col,scenarioId_col The
#' column names in the `weighedEstimates` data frame.
#' @param theme The `ggplot2` theme to use.
#'
#' @return A `ggplot2` plot.
#' @export
performanceTable_heatmap <- function(weighedEstimates,
                                     estimateCol,
                                     scenario_id,
                                     criterionOrder = NULL,
                                     decisionOrder = NULL,
                                     criterionLabels = NULL,
                                     decisionLabels = NULL,
                                     criterionId_col = mdmcda::opts$get("criterionId_col"),
                                     parentCriterionId_col = mdmcda::opts$get("parentCriterionId_col"),
                                     criterionLabel_col = mdmcda::opts$get("criterionLabel_col"),
                                     decisionId_col = mdmcda::opts$get("decisionId_col"),
                                     decisionLabel_col = mdmcda::opts$get("decisionLabel_col"),
                                     scenarioId_col = mdmcda::opts$get("scenarioId_col"),
                                     theme = ggplot2::theme_minimal(base_size = mdmcda::opts$get("ggBaseSize"))) {

  fullEstimateRange <-
    range(weighedEstimates[, estimateCol],
          na.rm=TRUE);
  tmpDf <-
    weighedEstimates[weighedEstimates[, scenarioId_col]==scenario_id,
                     c(decisionId_col,
                       criterionId_col,
                       estimateCol)];

  criteria_by_parent <-
    unique(weighedEstimates[, c(criterionId_col, parentCriterionId_col)]);
  criteria_by_parent <-
    criteria_by_parent[order(criteria_by_parent[, parentCriterionId_col]),
                       criterionId_col];

  if (is.null(criterionOrder)) {
    criterionOrder <-
      criteria_by_parent;
  }

  if (is.null(criterionLabels)) {
    criterionLabels <-
      stats::setNames(criterionOrder,
                      nm = criterionOrder);
  }

  if (is.null(decisionOrder)) {
    decisionOrder <-
      sort(unique(as.character(tmpDf[, decisionId_col])));
  }

  decisionOrder <-
    rev(decisionOrder);

  if (is.null(decisionLabels)) {
    decisionLabels <-
      stats::setNames(decisionOrder,
                      nm = decisionOrder);
  }

  tmpDf$decision_label <-
    factor(as.character(tmpDf[, decisionId_col]),
           levels = decisionOrder,
           labels = decisionLabels[decisionOrder],
           order = TRUE);
  tmpDf$criterion_label <-
    factor(as.character(tmpDf$criterion_id),
           levels = criterionOrder,
           labels = criterionLabels[criterionOrder],
           ordered = TRUE);

  res <-
    ggplot2::ggplot(data = tmpDf,
                    mapping = ggplot2::aes_string(x='criterion_label',
                                                  y='decision_label',
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
