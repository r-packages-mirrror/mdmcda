### Add classes to estimates$multiEstimateDf and to weightedEstimates
### Then make add_weights smart

#' Add weights to a data frame for weighted estimates
#'
#' @param weightedEstimates A `weightedEstimates` data frame as returned
#' by [mdmcda::build_weighted_estimate_df()].
#' @param weightProfiles The weight profiles object
#' @param weightProfileNames The name(s) of the weight profile(s) to add
#' weights for.
#'
#' @return The `weightedEstimates` object with added weights.
#' @export
add_weights <- function(weightedEstimates,
                        weightProfiles,
                        weightProfileNames) {

  if (!("mdmcda_weightedEstimates" %in% class(weightedEstimates))) {
    stop("The object you pass as `weightedEstimates` must have class ",
         "`mdmcda_weightedEstimates`, but it had class ",
         vecTxtQ(class(weightedEstimates)), ".");
  }

  for (i in weightProfileNames) {
    weightedEstimates[, paste0(i, "_weight")] <-
      weightProfiles[[i]][weightedEstimates$criterion_id];
    weightedEstimates[, paste0(i, "_weighted_estimate")] <-
      weightedEstimates$estimate *
      weightedEstimates[, paste0(i, "_weight")];
  }

  return(weightedEstimates);

}
