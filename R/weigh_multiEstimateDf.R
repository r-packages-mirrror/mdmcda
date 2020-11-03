#' Weigh estimates in a multiEstimateDf
#'
#' @param multiEstimateDf A multi estimate data frame that should contain
#' columns `decision_id`, `decision_label`, `alternative_value`,
#' `alternative_label`, `criterion_id`, `criterion_label`, and
#' one or more estimates in columns named with the scorer identifiers. Columns
#' with the `_id` suffix contain identifiers, and columns with the `_label`
#' suffix contain human-readable labels. This dataframe is stored in the
#' object called `multiEstimateDf` returned by a call to
#' [read_performance_tables()] to read a set of scored performance tables.
#' @param weight_profiles The weight profiles: a named list of vectors, where
#' every vector element's value is the weight of a criterion, that
#' element's name is the criterion's identifier, and the vector's name is the
#' identifier of the weight profile.
#' @param scorer The name of the scorer whose estimates to process.
#' @param weightProfileNames The weight profile name(s) to process.
#'
#' @return The `multiEstimateDf` with columns appended with the weights
#' and the weighed estimates.
#'
#' @export
weigh_multiEstimateDf <- function(multiEstimateDf,
                                  weightProfiles,
                                  scorer,
                                  weightProfileNames = names(weightProfiles)) {

  for (currentWeightProfile in weightProfileNames) {

    multiEstimateDf[[paste0(currentWeightProfile, "_weight")]] <-
      weightProfiles[[currentWeightProfile]][multiEstimateDf$criterion_id];

    multiEstimateDf[[paste0(scorer,
                            "_",
                            currentWeightProfile,
                            "_weighed_estimate")]] <-
      multiEstimateDf[, scorer] *
      multiEstimateDf[[paste0(currentWeightProfile,
                              "_weight")]];

  }

  return(multiEstimateDf);

}
