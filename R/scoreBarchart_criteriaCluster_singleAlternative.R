#' Create a bar chart with scores per criteria cluster for every alternative in a decision
#'
#' @param multiEstimateDf A `multiEstimateDf` data frame.
#' @param estimateCol The column name with the estimates to use.
#' @param criteria A `criteria` object.
#' @param decision_id The identifier of the decision to show.
#' @param alternative_value The alternative value to show.
#' @param parentCriterionOrder The parent criteria to use, and their order.
#' @param parentCriterion_labels The labels for the parent criteria (a named vector).
#' @param decisionLabels The labels for the decisions (a named vector).
#' @param alternativeLabels The labels for the alternatives (a list of named vectors).
#' @param wrapLabels Where to wrap labels (`NULL` to not wrap).
#' @param fillColor,strokeColor,strokeSize The color and pen width of the fill
#' and stroke.
#' @param flipCoordinates Whether to flip the coordinates.
#' @param title,xLab,yLab The title and x and y axis labels.
#' @param theme The `ggplot2` theme to use.
#' @param guides A guides argument to tweak the legend.
#' @param legend.position,legend.box.margin The position and spacing for the
#' legend.
#'
#' @return A `ggplot2` plot.
#' @export
scoreBarchart_criteriaCluster_singleAlternative <- function(multiEstimateDf,
                                                            estimateCol,
                                                            criteria,
                                                            decision_id,
                                                            alternative_value,
                                                            parentCriterionOrder = unique(weightedEstimates$parentCriterion_id),
                                                            parentCriterionLabels = parentCriterion_ids,
                                                            decisionLabels = NULL,
                                                            alternativeLabels = NULL,
                                                            wrapLabels = 100,
                                                            scoreRange = NULL,
                                                            fillColor = "black",
                                                            strokeColor = "black",
                                                            strokeSize = .1,
                                                            flipCoordinates = TRUE,
                                                            title = "MDMCDA criteria cluster bar chart by alternative",
                                                            xLab = "Criteria clusters",
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
                   tmpDf[, alternativeValue_col] == alternative_value,
                 ];

  if (flipCoordinates) {
    tmpDf$criterionCluster <-
      factor(tmpDf[, parentCriterionId_col],
             levels = rev(parentCriterionOrder),
             labels = rev(parentCriterionLabels[parentCriterionOrder]),
             ordered = TRUE);
  } else {
    tmpDf$criterionCluster <-
      factor(tmpDf[, parentCriterionId_col],
             levels = parentCriterionOrder,
             labels = parentCriterionLabels[parentCriterionOrder],
             ordered = TRUE);
  }

  if (is.null(wrapLabels)) {
    wrappedLabel <- alternativeLabels[[decision_id]][alternative_value];
  } else {
    wrappedLabel <-
      wrapVector(alternativeLabels[[decision_id]],
                 wrapLabels)[alternative_value];
  }

  res <-
    ggplot2::ggplot(data = tmpDf,
                    mapping = ggplot2::aes_string(x = "criterionCluster",
                                                  y = estimateCol)) +
    ggplot2::geom_col(position=ggplot2::position_dodge(),
                      fill = fillColor,
                      color = strokeColor,
                      size = strokeSize) +
    theme +
    ggplot2::guides(fill = guides,
                    color = guides) +
    ggplot2::theme(legend.position=legend.position,
                   legend.box = "horizontal",
                   plot.title.position = "plot") +
    ggplot2::labs(title = title,
                  subtitle = paste0(decisionLabel, ": ",
                                    wrappedLabel),
                  x = xLab,
                  y = yLab) +
    NULL;

  if (flipCoordinates) {
    coordFun <- ggplot2::coord_flip;
  } else {
    coordFun <- ggplot2::coord_cartesian;
  }

  if (is.null(scoreRange)) {
    return(
      res + coordFun()
    );
  } else {
    return(
      res + coordFun(ylim = clusterScoreRange)
    );
  }

}
