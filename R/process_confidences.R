#' @export
process_confidences <- function(estimates,
                                path = NULL,
                                confidencesByDecisionPlot = "Estimate confidence scores by decision",
                                confidencesByCriterionPlot = "Estimate confidence scores by criterion",
                                confidencesInDetail = "Estimate confidence scores for effects of %s",
                                figWidth = mdmcda::opts$get("ggSaveFigWidth"),
                                figHeight = mdmcda::opts$get("ggSaveFigHeight"),
                                theme = ggplot2::theme_minimal(base_size = dmcda::opts$get("ggBaseSize"))) {
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
  if (!is.null(confidencesByDecisionPlot)) {
    estimates$confidencesByDecisionPlot <-
      ggplot2::ggplot(data=estimates$mergedConfidences,
                      mapping=ggplot2::aes(y=decision,
                                           x=Confidence,
                                           color=Scorer)) +
      ggplot2::geom_jitter(alpha=.25,
                           width=2,
                           height=.25) +
      ggplot2::scale_color_viridis_d(end=.9) +
      ggplot2::coord_cartesian(xlim=c(0, 100)) +
      theme;
    if (!is.null(path) && is.character(confidencesByDecisionPlot)) {
      ufs::cat0("\n\n");
      ufs::knitAndSave(estimates$confidencesByDecisionPlot,
                       figCaption=confidencesByDecisionPlot,
                       path = path,
                       figWidth = figWidth,
                       figHeight = figHeight);
      ufs::cat0("\n\n");
    }
  }

  ### Confidences per criterion (outcome)
  if (!is.null(confidencesByCriterionPlot)) {
    estimates$confidencesByCriterionPlot <-
      ggplot2::ggplot(data=estimates$mergedConfidences,
                      mapping=ggplot2::aes(y=criterion,
                                           x=Confidence,
                                           color=Scorer)) +
      ggplot2::geom_jitter(alpha=.25,
                           width=2,
                           height=.25) +
      ggplot2::scale_color_viridis_d(end=.9) +
      ggplot2::coord_cartesian(xlim=c(0, 100)) +
      theme;
    if (!is.null(path) && is.character(confidencesByCriterionPlot)) {
      ufs::cat0("\n\n");
      ufs::knitAndSave(estimates$confidencesByCriterionPlot,
                       figCaption=confidencesByCriterionPlot,
                       path = path,
                       figWidth = figWidth,
                       figHeight = figHeight);
      ufs::cat0("\n\n");
    }
  }

  if (!is.null(confidencesInDetail)) {
    estimates$confidencesInDetail <- list();
    for (i in rev(levels(estimates$mergedConfidences$decision))) {
      tmpDf <-
        estimates$mergedConfidences[(estimates$mergedConfidences$decision == i), ];
      estimates$confidencesInDetail[[i]] <-
        ggplot2::ggplot(data=tmpDf,
                        mapping=ggplot2::aes(y=criterion,
                                             x=Confidence,
                                             color=Scorer)) +

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
      if (!is.null(path) && is.character(confidencesInDetail)) {
        ufs::cat0("\n\n");
        ufs::knitAndSave(estimates$confidencesInDetail[[i]],
                         figCaption=sprintf(confidencesInDetail, i),
                         path = path,
                         figWidth = figWidth,
                         figHeight = figHeight);
        ufs::cat0("\n\n");
      }
    }
  }

  return(estimates);

}
