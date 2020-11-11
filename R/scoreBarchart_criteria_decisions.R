#' @param alternativeLabels The `alternativeLabels` object; optionally, to
#' override the alternative labels in the `weightedEstimates` object.
#' @param useDecisionAlternativeLabels Whether to label the decision plot with
#' the decisions or combined labels that also include the alternative for
#' the scenario.
#' @param decision_alternative_pre,decision_alternative_sep,decision_alternative_suf If
#' `useDecisionAlternativeLabels` is `TRUE`, these prefix, separator, and suffix
#' are used to compose the decision plot labels.
#' @export
scoreBarchart_criteria_decisions <- function(weightedEstimates,
                                             scenario_id,
                                             scenario_label = scenario_id,
                                             estimateCol,
                                             decisionOrder = NULL,
                                             decisionLabels = NULL,
                                             criterionOrder = NULL,
                                             criterionLabels = NULL,
                                             alternativeLabels = NULL,
                                             useDecisionAlternativeLabels = TRUE,
                                             decision_alternative_pre = "**",
                                             decision_alternative_sep = "**: ",
                                             decision_alternative_suf = "",
                                             strokeSize = 0,
                                             strokeColor = "black",
                                             wrapDecisionLabels = 35,
                                             wrapCriterionLabels = 50,
                                             title = paste0("MDMCDA bar chart to compare criteria for ", scenario_label),
                                             xLab = "Criteria",
                                             yLab = "Weighted estimated effect",
                                             theme = ggplot2::theme_minimal(base_size = mdmcda::opts$get("ggBaseSize")),
                                             guides = ggplot2::guide_legend(ncol = 2),
                                             axis.text.x.bottom = ggtext::element_markdown(
                                               angle = 90,
                                               hjust = 1,
                                               vjust = .5
                                             ),
                                             legend.position = "bottom",
                                             legend.box.margin = ggplot2::margin(.5, .5, .5, .5, "cm")) {

  scenarioId_col <- mdmcda::opts$get("scenarioId_col");
  decisionId_col <- mdmcda::opts$get("decisionId_col");
  criterionId_col <- mdmcda::opts$get("criterionId_col");
  decisionLabel_col <- mdmcda::opts$get("decisionLabel_col");
  criterionLabel_col <- mdmcda::opts$get("criterionLabel_col");
  scenarioLabel_col <- mdmcda::opts$get("scenarioLabel_col");
  alternativeValue_col <- mdmcda::opts$get("alternativeValue_col");
  alternativeLabel_col <- mdmcda::opts$get("alternativeLabel_col");

  if (is.null(decisionOrder)) {
    decisionOrder <- unique(weightedEstimates[, decisionId_col]);
  }
  if (is.null(criterionOrder)) {
    criterionOrder <- unique(weightedEstimates[, criterionId_col]);
  }
  if (is.null(decisionLabels)) {
    decisionLabels <- stats::setNames(decisionOrder,
                                      nm = decisionOrder);
  }
  if (is.null(criterionLabels)) {
    criterionLabels <- stats::setNames(criterionOrder,
                                       nm = criterionOrder);
  }

  tmpDf <-
    weightedEstimates[weightedEstimates[, scenarioId_col] == scenario_id,
                     c(decisionId_col,
                       criterionId_col,
                       alternativeValue_col,
                       estimateCol)];

  if (is.null(alternativeLabels)) {

    tmpDf[, alternativeLabel_col] <-
      paste0("value ", tmpDf[, alternativeValue_col]);

  } else {

    tmpDf[, alternativeLabel_col] <-
      get_alternativeLabel(tmpDf,
                           alternativeLabels);

  }

  tmpDf[, decisionLabel_col] <-
    decisionLabels[tmpDf[, decisionId_col]];

  tmpDf$decision_and_alternative <-
    paste0(decision_alternative_pre,
           tmpDf[, decisionLabel_col],
           decision_alternative_sep,
           tmpDf[, alternativeLabel_col],
           decision_alternative_suf);

  if (useDecisionAlternativeLabels) {
    decisionLabels <-
      stats::setNames(
        tmpDf$decision_and_alternative,
        nm = tmpDf[, decisionId_col]
      );
  }

  criterionLabels <-
    unlist(lapply(criterionLabels, function(x)
      paste(strwrap(x, wrapCriterionLabels), collapse="\n")
    ));
  decisionLabels <-
    unlist(lapply(decisionLabels, function(x)
      paste(strwrap(x, wrapDecisionLabels),
            collapse=ifelse(useDecisionAlternativeLabels,
                            "  \n",
                            "\n"))
    ));

  tmpDf[, decisionLabel_col] <-
    factor(tmpDf[, decisionId_col],
           levels = decisionOrder,
           labels = decisionLabels[decisionOrder],
           ordered = TRUE);
  tmpDf[, criterionLabel_col] <-
    factor(tmpDf[, criterionId_col],
           levels = criterionOrder,
           labels = criterionLabels[criterionOrder],
           ordered = TRUE);

  if (strokeSize == 0) {
    strokeType <- 0;
  } else {
    strokeType <- 1;
  }

  res <-
    ggplot2::ggplot(data = tmpDf,
                    mapping = ggplot2::aes_string(x=criterionLabel_col,
                                                  y=estimateCol,
                                                  fill=decisionLabel_col)) +
    ggplot2::geom_col(color =strokeColor,
                      size = strokeSize,
                      linetype = strokeType) +
    ggplot2::scale_fill_viridis_d(name = "Decision") +
    ggplot2::scale_x_discrete(position="bottom") +
    theme +
    ggplot2::guides(fill = guides) +
    ggplot2::theme(axis.text.x.bottom = axis.text.x.bottom,
                   plot.title.position = "plot",
                   legend.position = legend.position,
                   legend.box.margin = legend.box.margin) +
    ggplot2::labs(title=title,
                  x=xLab,
                  y=yLab) +
    NULL;
  return(res);
}
