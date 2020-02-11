#' @export
compute_best_alternatives <- function(scores_per_alternative) {
  ### Create dataframe to store result
  bestAlternatives <- data.frame(weightProfile = character(),
                                 decision_id = character(),
                                 alternative_id = character(),
                                 score = numeric(),
                                 stringsAsFactors = FALSE);
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
      alternativesWithMaxScore <-
        which(tmpDf$score==maxScore);

      if (length(alternativesWithMaxScore) == 1) {
        newRow <- tmpDf[alternativesWithMaxScore, , drop=FALSE];
      } else {
        newRow <- tmpDf[alternativesWithMaxScore[1], , drop=FALSE];
        newRow$alternative_id <-
          paste(alternativesWithMaxScore,
                collapse = " or ");
      }
      bestAlternatives <-
        rbind(bestAlternatives,
              newRow,
              stringsAsFactors = FALSE);

      if (length(alternativesWithMaxScore) > 1) {
        alternativesWithMaxScore <-
          alternativesWithMaxScore[1];
      }
    }
  }
  return(bestAlternatives);
}
