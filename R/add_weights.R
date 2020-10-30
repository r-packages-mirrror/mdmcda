### Add classes to estimates$multiEstimateDf and to weighedEstimates
### Then make add_weights smart

#' Add weights to a data frame for weighed estimates
#'
#' @param weighedEstimates A `weighedEstimates` data frame as returned
#' by [mdmcda::build_weighed_estimate_df()].
#' @param weightProfiles The weight profiles object
#' @param weightProfileNames The name(s) of the weight profile(s) to add
#' weights for.
#'
#' @return The `weighedEstimates` object with added weights.
#' @export
add_weights <- function(weighedEstimates,
                        weightProfiles,
                        weightProfileNames) {

  if (!("mdmcda_weighedEstimates" %in% class(weighedEstimates))) {
    stop("The object you pass as `weighedEstimates` must have class ",
         "`mdmcda_weighedEstimates`, but it had class ",
         vecTxtQ(class(weighedEstimates)), ".");
  }

  for (i in weightProfileNames) {
    weighedEstimates[, paste0(i, "_weight")] <-
      weightProfiles[[i]][weighedEstimates$criterion_id];
    weighedEstimates[, paste0(i, "_weighed_estimate")] <-
      weighedEstimates$estimate *
      weighedEstimates[, paste0(i, "_weight")];
  }

  return(weighedEstimates);

}
