#' @export
select_scenario_estimates <- function(multiEstimateDf,
                                      scenario) {
  return(do.call(rbind,
                 lapply(names(scenario),
                        function(decision) {
                          return(multiEstimateDf[(multiEstimateDf$decision_id == decision) &
                                                   (multiEstimateDf$decision_alternative_value ==
                                                      scenario[decision]), ])})));
}

