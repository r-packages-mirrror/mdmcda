#' @export
select_scenario_estimates <- function(multiEstimateDf,
                                      scenario) {

  decisionId_col <- mdmcda::opts$get("decisionId_col");
  alternativeValue_col <- mdmcda::opts$get("alternativeValue_col");

  return(
    do.call(
      rbind,
      lapply(
        names(scenario),
        function(decision_id) {
          return(multiEstimateDf[(multiEstimateDf[, decisionId_col] == decision_id) &
                                   (multiEstimateDf[, alternativeValue_col] ==
                                      scenario[decision_id]), ])}
      )
    )
  );
}

