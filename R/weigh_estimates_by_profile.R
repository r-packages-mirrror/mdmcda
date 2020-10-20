#' Weigh estimates by one or more weight profiles
#'
#' @param weighed_estimate_df A data frame with columns `scenario_id`,
#' `decision_id`, `alternative_value`, `criterion_id`, and `estimate`. This
#' data frame can be produced by a call to [weigh_estimates_by_profile()].
#' @param weight_profiles The weight profiles: a named list of vectors, where
#' every vector element's value is the weight of a criterion, that
#' element's name is the criterion's identifier, and the vector's name is the
#' identifier of the weight profile.
#' @param weightProfileNames The weight profile name(s) to process.
#'
#' @return A data frame with columns `scenario_id`, `decision_id`,
#' `alternative_value`, `criterion_id`, `estimate`, and two columns for
#' every weight profile. These columns' names are the weight profile name
#' appended with `_weight` and `_weighed_estimate`.
#'
#' @export
weigh_estimates_by_profile <-
  function(weighed_estimate_df,
           weight_profiles,
           weightProfileNames = names(weight_profiles)) {
    for (currentWeightProfile in weightProfileNames) {
      weighed_estimate_df[[paste0(currentWeightProfile, "_weight")]] <-
        weight_profiles[[currentWeightProfile]][weighed_estimate_df$criterion_id]
      weighed_estimate_df[[paste0(currentWeightProfile, "_weighed_estimate")]] <-
        weighed_estimate_df$estimate * weighed_estimate_df[[paste0(currentWeightProfile, "_weight")]];
      return(weighed_estimate_df);
    }
  }
