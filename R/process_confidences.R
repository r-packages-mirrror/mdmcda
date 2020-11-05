#' Process the confidence scores and create plots
#'
#' @param estimates The `estimates` object.
#' @param parentCriterionOrder,parentCriterionLabels The order and labels of
#' the parent criteria (i.e. the criterion clusters).
#' @param decisionOrder,decisionLabels The order and labels of the decisions.
#' @param theme The `ggplot2` theme to use for the plots.
#'
#' @return The `estimates` object, with plots and a data frame added.
#' @export
process_confidences <- function(estimates,
                                parentCriterionOrder = NULL,
                                parentCriterionLabels = NULL,
                                decisionOrder = NULL,
                                decisionLabels = NULL,
                                theme = ggplot2::theme_minimal(base_size = mdmcda::opts$get("ggBaseSize"))) {

  criterionId_col          <- mdmcda::opts$get("criterionId_col");
  criterionLabel_col       <- mdmcda::opts$get("criterionLabel_col");
  criterionDescription_col <- mdmcda::opts$get("criterionDescription_col");
  parentCriterionId_col    <- mdmcda::opts$get("parentCriterionId_col");
  parentCriterionLabel_col <- mdmcda::opts$get("parentCriterionLabel_col");
  decisionId_col           <- mdmcda::opts$get("decisionId_col");
  decisionLabel_col        <- mdmcda::opts$get("decisionLabel_col");
  alternativeValue_col     <- mdmcda::opts$get("alternativeValue_col");
  alternativeLabel_col     <- mdmcda::opts$get("alternativeLabel_col");
  scenarioId_col           <- mdmcda::opts$get("scenarioId_col");
  weightProfileId_col      <- mdmcda::opts$get("weightProfileId_col");
  score_col                <- mdmcda::opts$get("score_col");
  leafCriterion_col        <- mdmcda::opts$get("leafCriterion_col");
  rootCriterionId          <- mdmcda::opts$get("rootCriterionId");

  ### Get number only
  estimates$mergedConfidences$ScorerNr <-
    gsub("[a-zA-Z]+([0-9]+)",
         "\\1",
         estimates$mergedConfidences$Scorer);

  ### Get unique texts and numbers for scorers
  scorerTxt <- unique(estimates$mergedConfidences$Scorer);
  scorerNrs <- unique(estimates$mergedConfidences$ScorerNr);

  ### Store scorers as ordered factor
  estimates$mergedConfidences$Scorer <-
    factor(estimates$mergedConfidences$Scorer,
           levels=scorerTxt[order(as.numeric(scorerNrs))],
           ordered=TRUE);

  ### Set order and labels if those were not provided
  if (is.null(parentCriterionOrder)) {
    parentCriterionOrder <-
      rev(sort(unique(estimates$mergedConfidences[, parentCriterionId_col])));
  }
  if (is.null(parentCriterionLabels)) {
    parentCriterionLabels <- stats::setNames(parentCriterionOrder,
                                             parentCriterionOrder);
  }
  if (is.null(decisionOrder)) {
    decisionOrder <-
      rev(sort(unique(estimates$mergedConfidences[, decisionId_col])));
  }
  if (is.null(decisionLabels)) {
    decisionLabels <- stats::setNames(decisionOrder,
                                      decisionOrder);
  }

  ### Store decisions and criteria labels as ordered factors; flip
  ### the order so that the first ones are displayed at the top.
  estimates$mergedConfidences[, parentCriterionLabel_col] <-
    factor(estimates$mergedConfidences[, parentCriterionId_col],
           levels=rev(parentCriterionOrder),
           labels=rev(parentCriterionLabels[parentCriterionOrder]),
           ordered=TRUE);
  estimates$mergedConfidences[, decisionLabel_col] <-
    factor(estimates$mergedConfidences[, decisionId_col],
           levels=rev(decisionOrder),
           labels=rev(decisionLabels[decisionOrder]),
           ordered=TRUE);

  ### Add collapsed version (over performance table)
  estimates$collapsedConfidences <-
    do.call(rbind,
            by(estimates$mergedConfidences,
               estimates$mergedConfidences$performance_table,
               function(x) {
                 return(data.frame(decision_id = unique(x[, decisionId_col]),
                                   criterion_id = unique(x[, parentCriterionId_col]),
                                   confidenceMean = mean(x$Confidence, na.rm=TRUE),
                                   confidenceSD = sd(x$Confidence, na.rm=TRUE)));
               }));
  names(estimates$collapsedConfidences) <-
    c(decisionId_col, parentCriterionId_col, "confidenceMean", "confidenceSD");
  row.names(estimates$collapsedConfidences) <- NULL;

  ### Confidences per decision (instrument)
  estimates$confidencesByDecisionPlot <-
    ggplot2::ggplot(data=estimates$mergedConfidences,
                    mapping=ggplot2::aes_string(y=decisionLabel_col,
                                                x='Confidence',
                                                color='Scorer')) +
    ggplot2::geom_jitter(alpha=.25,
                         width=2,
                         height=.25) +
    ggplot2::scale_color_viridis_d(end=.9) +
    ggplot2::coord_cartesian(xlim=c(0, 100)) +
    ggplot2::labs(title = "Confidence scores per decision",
                  y = "Decisions") +
    theme;

  ### Confidences per criterion (outcome)
  estimates$confidencesByCriterionPlot <-
    ggplot2::ggplot(data=estimates$mergedConfidences,
                    mapping=ggplot2::aes_string(y=parentCriterionLabel_col,
                                                x='Confidence',
                                                color='Scorer')) +
    ggplot2::geom_jitter(alpha=.25,
                         width=2,
                         height=.25) +
    ggplot2::scale_color_viridis_d(end=.9) +
    ggplot2::coord_cartesian(xlim=c(0, 100)) +
    ggplot2::labs(title = "Confidence scores per parent criterion (cluster)",
                  y = "Criteria") +
    theme;

  estimates$confidencesInDetail <- list();
  for (i in decisionOrder) {
    tmpDf <-
      estimates$mergedConfidences[
        (estimates$mergedConfidences[, decisionId_col] == i),
      ];
    estimates$confidencesInDetail[[i]] <-
      ggplot2::ggplot(data=tmpDf,
                      mapping=ggplot2::aes_string(y=parentCriterionLabel_col,
                                                  x='Confidence',
                                                  color='Scorer')) +

      ggplot2::geom_jitter(mapping=ggplot2::aes_string(color="Scorer"),
                           alpha=.5,
                           width=.5,
                           height=.1,
                           size=5) +
      ggplot2::scale_color_viridis_d(end=.9) +
      ggplot2::labs(title=paste0("Confidence scores for ", i),
                    y = "Parent criterion (cluster)") +
      theme +
      ggplot2::coord_cartesian(xlim=c(0, 100)) +
      ggrepel::geom_text_repel(mapping=ggplot2::aes_string(color="Scorer",
                                                           label="ScorerNr"),
                               size=4,
                               point.padding = .25,
                               segment.alpha=.5,
                               alpha=.5);
  }
  names(estimates$confidencesInDetail) <-
    decisionOrder;

  return(estimates);

}
