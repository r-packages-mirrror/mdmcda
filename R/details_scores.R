#' @export
details_scores <- function(scenario_scores,
                           heading = "Details of scores",
                           headingLevel = 2) {

  if (is.null(heading)) {
    res <- "\n\n";
  } else {
    res <- paste0("\n\n",
                  repStr("#", headingLevel),
                  " ",
                  heading,
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

      if (knitr::is_latex_output()) {
        res <-
          paste0(res,
                 paste0(knitr::kable(scenario_scores$scenarioScores[[i]][[j]][,
                                                                              c('decision_id',
                                                                                'decision_alternative_value',
                                                                                'criterion_id',
                                                                                'score')],
                                     format="latex", booktabs = TRUE, longtable = TRUE,
                                     row.names = FALSE),
                        collapse="\n"));
      } else {
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
  }

  res <- knitr::asis_output(res);

  return(res);

}
