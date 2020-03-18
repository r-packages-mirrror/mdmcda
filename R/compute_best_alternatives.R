#' @export
compute_best_alternatives <- function(scores_per_alternative,
                                      ignoreRegex = NULL) {
  ### Create dataframe to store result
  bestAlternatives <- data.frame(weightProfile = character(),
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
      maxScore <-
        max(tmpDf[, 'score'], na.rm=TRUE);

      ### Get indices of all alternatives with the max score
      indicesOfAlternativesWithMaxScore <-
        which(tmpDf$score==maxScore);
      alternativesWithMaxScore <-
        tmpDf$alternative_id[indicesOfAlternativesWithMaxScore];

      if (length(indicesOfAlternativesWithMaxScore) == 1) {
        newRow <- tmpDf[indicesOfAlternativesWithMaxScore, , drop=FALSE];
      } else {
        newRow <- tmpDf[indicesOfAlternativesWithMaxScore[1], , drop=FALSE];
        newRow$alternative_id <-
          paste(alternativesWithMaxScore,
                collapse = " or ");
      }
      bestAlternatives <-
        rbind(bestAlternatives,
              newRow,
              stringsAsFactors = FALSE);
    }
  }
  return(bestAlternatives);
}
