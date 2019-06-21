#' @export
details_scores <- function(scenario_scores,
                           header = "Details of scores",
                           headerLevel = 2) {

  if (is.null(header)) {
    res <- "\n\n";
  } else {
    res <- paste0("\n\n",
                  repStr("#", headerLevel),
                  " ",
                  header,
                  "\n\n");
  }

  for (i in names(scenario_scores$scenarioScores)) {
    for (j in names(scenario_scores$scenarioScores[[i]])) {
      res <- paste0(res,
                    "\n\n## Scenario: ",
                    i,
                    " {.tabset}\n\n",
                    "\n\n### Weighing profile: ",
                    j," {.tabset}\n\n");
      res <-
        paste0(res,
               paste0(knitr::kable(scenario_scores$scenarioScores[[i]][[j]][,
                                                                            c('decision_id',
                                                                              'decision_alternative_value',
                                                                              'criterion_id',
                                                                              'score')],
                                   row.names = FALSE),
                      collapse="\n"));
    }
  }

  res <- knitr::asis_output(res);

  return(res);

}
