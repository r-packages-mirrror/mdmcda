#' @export
process_confidences <- function(estimates,
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

  #### Also store decisions and criteria as ordered factors
  estimates$mergedConfidences$decision <-
    factor(estimates$mergedConfidences$decision,
           levels=rev(sort(unique(estimates$mergedConfidences$decision))),
           ordered=TRUE);
  estimates$mergedConfidences$criterion <-
    factor(estimates$mergedConfidences$criterion,
           levels=rev(sort(unique(estimates$mergedConfidences$criterion))),
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
      ggplot2::labs(title=sprintf(confidencesInDetail, i)) +
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
