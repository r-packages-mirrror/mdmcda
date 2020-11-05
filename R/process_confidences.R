#' Process the confidence scores and create plots
#'
#' @param estimates The `estimates` object.
#' @param criterionOrder,criterionLabels The order and labels of the criteria.
#' @param decisionOrder,decisionLabels The order and labels of the decisions.
#' @param theme The `ggplot2` theme to use for the plots.
#'
#' @return The `estimates` object, with plots and a data frame added.
#' @export
process_confidences <- function(estimates,
                                criterionOrder = NULL,
                                criterionLabels = NULL,
                                decisionOrder = NULL,
                                decisionLabels = NULL,
                                theme = ggplot2::theme_minimal(base_size = mdmcda::opts$get("ggBaseSize"))) {

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

  ### Remove "_by_all" from criteria
  estimates$mergedConfidences$criterion <-
    gsub("_by_all",
         "",
         estimates$mergedConfidences$criterion);

  ### Set order and labels if those were not provided
  if (is.null(decisionOrder)) {
    decisionOrder <- rev(sort(unique(estimates$mergedConfidences$decision)));
  }
  if (is.null(criterionLabels)) {
    decisionLabels <- stats::setNames(decisionOrder,
                                      decisionOrder);
  }
  if (is.null(criterionOrder)) {
    criterionOrder <- rev(sort(unique(estimates$mergedConfidences$criterion)));
  }
  if (is.null(criterionLabels)) {
    criterionLabels <- stats::setNames(criterionOrder,
                                       criterionOrder);
  }

  #### Also store decisions and criteria as ordered factors
  estimates$mergedConfidences$criterion <-
    factor(estimates$mergedConfidences$criterion,
           levels=criterionOrder,
           labels=criterionLabels,
           ordered=TRUE);
  estimates$mergedConfidences$decision <-
    factor(estimates$mergedConfidences$decision,
           levels=decisionOrder,
           labels=decisionLabels,
           ordered=TRUE);

  ### Add collapsed version (over performance table)
  estimates$collapsedConfidences <-
    do.call(rbind,
            by(estimates$mergedConfidences,
               estimates$mergedConfidences$performance_table,
               function(x) {
                 return(data.frame(decision = unique(x$decision),
                                   criterion = unique(x$criterion),
                                   confidenceMean = mean(x$Confidence, na.rm=TRUE),
                                   confidenceSD = sd(x$Confidence, na.rm=TRUE)));
               }));
  row.names(estimates$collapsedConfidences) <- NULL;

  ### Confidences per decision (instrument)
  estimates$confidencesByDecisionPlot <-
    ggplot2::ggplot(data=estimates$mergedConfidences,
                    mapping=ggplot2::aes_string(y='decision',
                                                x='Confidence',
                                                color='Scorer')) +
    ggplot2::geom_jitter(alpha=.25,
                         width=2,
                         height=.25) +
    ggplot2::scale_color_viridis_d(end=.9) +
    ggplot2::coord_cartesian(xlim=c(0, 100)) +
    theme;

  ### Confidences per criterion (outcome)
  estimates$confidencesByCriterionPlot <-
    ggplot2::ggplot(data=estimates$mergedConfidences,
                    mapping=ggplot2::aes_string(y='criterion',
                                                x='Confidence',
                                                color='Scorer')) +
    ggplot2::geom_jitter(alpha=.25,
                         width=2,
                         height=.25) +
    ggplot2::scale_color_viridis_d(end=.9) +
    ggplot2::coord_cartesian(xlim=c(0, 100)) +
    theme;

  estimates$confidencesInDetail <- list();
  for (i in rev(levels(estimates$mergedConfidences$decision))) {
    tmpDf <-
      estimates$mergedConfidences[(estimates$mergedConfidences$decision == i), ];
    estimates$confidencesInDetail[[i]] <-
      ggplot2::ggplot(data=tmpDf,
                      mapping=ggplot2::aes_string(y='criterion',
                                                  x='Confidence',
                                                  color='Scorer')) +

      ggplot2::geom_jitter(mapping=ggplot2::aes(color=Scorer),
                           alpha=.5,
                           width=.5,
                           height=.1,
                           size=5) +
      ggplot2::scale_color_viridis_d(end=.9) +
      ggplot2::labs(title=paste0("Confidence scores for ", i)) +
      theme +
      ggplot2::coord_cartesian(xlim=c(0, 100)) +
      ggrepel::geom_text_repel(mapping=ggplot2::aes(color=Scorer,
                                                    label=ScorerNr),
                               size=2.5,
                               point.padding = .25,
                               segment.alpha=.5,
                               alpha=.5);
  }
  names(estimates$confidencesInDetail) <-
    rev(levels(estimates$mergedConfidences$decision));

  return(estimates);

}
