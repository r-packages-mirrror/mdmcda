#' @rdname compute_bestOrWorst_alternative
#' @export
compute_worst_alternatives <- function(scores_per_alternative,
                                       ignoreRegex = NULL) {
  ### Create dataframe to store result
  worstAlternatives <- data.frame(weightProfile = character(),
                                 decision_id = character(),
                                 alternative_id = character(),
                                 score = numeric(),
                                 stringsAsFactors = FALSE);
  if (!is.null(ignoreRegex)) {
    scores_per_alternative <-
      scores_per_alternative[!grepl(ignoreRegex,
                                    scores_per_alternative$alternative_id), ];
  }
  for (currentWeightProfile in unique(scores_per_alternative[, 'weightProfile'])) {
    for (currentDecision in unique(scores_per_alternative[, 'decision_id'])) {
      ### Get temporary dataframe for convenience
      tmpDf <-
        scores_per_alternative[scores_per_alternative[, 'weightProfile'] == currentWeightProfile &
                                 scores_per_alternative[, 'decision_id'] == currentDecision,
                               ];

      ### Get max score (or scores)
      minScore <-
        min(tmpDf[, 'score'], na.rm=TRUE);

      ### Get indices of all alternatives with the min score
      alternativesWithMinScore <-
        which(tmpDf$score==minScore);

      if (length(alternativesWithMinScore) == 1) {
        newRow <- tmpDf[alternativesWithMinScore, , drop=FALSE];
      } else {
        newRow <- tmpDf[alternativesWithMinScore[1], , drop=FALSE];
        newRow$alternative_id <-
          paste(alternativesWithMinScore,
                collapse = " or ");
      }
      worstAlternatives <-
        rbind(worstAlternatives,
              newRow,
              stringsAsFactors = FALSE);
    }
  }
  return(worstAlternatives);
}
