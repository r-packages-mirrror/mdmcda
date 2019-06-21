details_scenario_scores <- function(scenario_scores) {
  for (i in names(scenario_scores)) {
    for (j in names(scenario_scores[[i]])) {
      cat0("\n\n## Scenario: ",
                i,
                "\n\n");
      cat0("\n\n### Wegingsprofiel: ",
                j,
                "{.tabset}\n\n");
      cat0("\n\n#### Samenvatting\n\n");
      cat0("**Score:** ", sum(scenario_scores[[i]][[j]]$score), "\n\n");
      cat0("\n\n#### Details\n\n");
      print(knitr::kable(scenario_scores[[i]][[j]][, c('instrument_id',
                                                      'instrument_alternative_value',
                                                      'outcome_id',
                                                      'score')]));
    }
  }
}
