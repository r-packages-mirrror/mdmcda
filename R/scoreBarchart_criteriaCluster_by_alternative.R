#' Create a bar chart with scores per criteria cluster for every alternative in a decision
#'
#' @param multiEstimateDf A `multiEstimateDf` data frame.
#' @param estimateCol The column name with the estimates to use.
#' @param criteria A `criteria` object.
#' @param decision_id The identifier of the decision to show.
#' @param alternativeValueRegex Regex to select alternative values to include.
#' @param parentCriterionOrder The parent criteria to use, and their order.
#' @param parentCriterion_labels The labels for the parent criteria (a named vector).
#' @param decisionLabels The labels for the decisions (a named vector).
#' @param alternativeLabels The labels for the alternatives (a list of named vectors).
#' @param wrapLabels Where to wrap labels (`NULL` to not wrap).
#' @param strokeColor,strokeSize The color and pen width of the stroke.
#' @param title,xLab,yLab The title and x and y axis labels.
#' @param theme The `ggplot2` theme to use.
#' @param guides A guides argument to tweak the legend.
#' @param legend.position,legend.box.margin The position and spacing for the
#' legend.
#'
#' @return A `ggplot2` plot.
#' @export
scoreBarchart_criteriaCluster_by_alternative <- function(multiEstimateDf,
                                                         estimateCol,
                                                         criteria,
                                                         decision_id,
                                                         alternativeValueRegex = ".*",
                                                         parentCriterionOrder = unique(weightedEstimates$parentCriterion_id),
                                                         parentCriterionLabels = parentCriterion_ids,
                                                         decisionLabels = NULL,
                                                         alternativeLabels = NULL,
                                                         wrapLabels = 30,
                                                         scoreRange = NULL,
                                                         strokeColor = "black",
                                                         strokeSize = .1,
                                                         title = "MDMCDA criteria cluster bar chart by alternative",
                                                         xLab = "Alternatives",
                                                         yLab = estimateCol,
                                                         theme = ggplot2::theme_minimal(base_size = mdmcda::opts$get("ggBaseSize")),
                                                         guides = ggplot2::guide_legend(nrow = 1),
                                                         legend.position = "top") {

  scenarioId_col <-           mdmcda::opts$get("scenarioId_col");
  parentCriterionId_col <-    mdmcda::opts$get("parentCriterionId_col");
  parentCriterionLabel_col <- mdmcda::opts$get("parentCriterionLabel_col");
  decisionId_col <-           mdmcda::opts$get("decisionId_col");
  criterionId_col <-          mdmcda::opts$get("criterionId_col");
  alternativeValue_col <-     mdmcda::opts$get("alternativeValue_col");
  alternativeLabel_col <-     mdmcda::opts$get("alternativeLabel_col");
  decisionLabel_col <-        mdmcda::opts$get("decisionLabel_col");
  criterionLabel_col <-       mdmcda::opts$get("criterionLabel_col");

  tmpDf <-
    mdmcda::criteriaCluster_df_perAlternative(
      multiEstimateDf = multiEstimateDf,
      estimateCol = estimateCol,
      criteria = criteria,
      parentCriterionOrder = parentCriterionOrder,
      parentCriterionLabels = parentCriterionLabels,
      decisionOrder = decisionOrder,
      decisionLabels = decisionLabels,
      alternativeLabels = alternativeLabels
    );

  if (is.null(decisionLabels)) {
    decisionLabel <-
      decision_id;
  } else {
    decisionLabel <-
      decisionLabels[decision_id];
  }

  tmpDf <- tmpDf[tmpDf[, decisionId_col] == decision_id &
                   grepl(alternativeValueRegex, tmpDf[, alternativeValue_col]), ];

  tmpDf$criterionCluster <-
    factor(tmpDf[, parentCriterionId_col],
           levels = parentCriterionOrder,
           labels = parentCriterionLabels[parentCriterionOrder],
           ordered = TRUE);

  if (is.null(wrapLabels)) {
    wrappedLabels <- alternativeLabels[[decision_id]];
  } else {
    wrappedLabels <-
      wrapVector(alternativeLabels[[decision_id]],
                 wrapLabels);
  }

  tmpDf$alternative <-
    factor(tmpDf[, alternativeValue_col],
           levels = names(alternativeLabels[[decision_id]]),
           labels = wrappedLabels,
           ordered = TRUE);

  res <-
    ggplot2::ggplot(data = tmpDf,
                    mapping = ggplot2::aes_string(x = "alternative",
                                                  y = estimateCol,
                                                  group = "criterionCluster",
                                                  fill = "criterionCluster")) +
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
                  subtitle = decisionLabel,
                  x = xLab,
                  y = yLab) +
    NULL;

  if (is.null(scoreRange)) {
    return(res);
  } else {
    return(
      res + ggplot2::coord_cartesian(ylim = clusterScoreRange)
    );
  }

}
