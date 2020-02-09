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
