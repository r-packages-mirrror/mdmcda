#' @export
autofill_estimates <- function(estimates,
                               criteria,
                               decisions_and_options,
                               autofill = NA) {

  estimatesDf <- estimates$estimatesDf;
  criteriaDf <- criteria$criteriaDf;
  optionsDf <- decisions_and_options$optionsDf;
  decisionsDf <- decisions_and_options$decisionsDf;

  autofillLogs <- "\n";
  autofilledEstimatesDf <-
    estimatesDf;
  for (i in 1:nrow(optionsDf)) {
    for (j in which(criteriaDf$isLeaf)) {
      if (any((estimatesDf$decision_id == optionsDf[i, 'instrument_id']) &
              (estimatesDf$decision_option_value == optionsDf[i, 'value']) &
              (estimatesDf$criterion_id == criteriaDf[j, 'id']))) {
        autofillLogs <-
          c(autofillLogs,
            paste0("- Estimate found for the effect of option '",
                   optionsDf[i, 'label'],
                   "' for decision '",
                   decisionsDf[decisionsDf$id==optionsDf[i, 'instrument_id'], 'label'],
                   "' for criterion '",
                   criteriaDf[j, 'id'],
                   "': ",
                   estimatesDf[(estimatesDf$decision_id==optionsDf[i, 'decision_id']) &
                                 (estimatesDf$decision_option_value==optionsDf[i, 'value']) &
                                 (estimatesDf$criterion_id==criteriaDf[j, 'id']),
                               "value"]));
      } else {
        autofillLogs <-
          c(autofillLogs,
            paste0("- **No** estimate found for the effect of option '",
                   optionsDf[i, 'label'],
                   "' for decision '",
                   decisionsDf[decisionsDf$id==optionsDf[i, 'decision_id'], 'label'],
                   "' for criterion '",
                   criteriaDf[j, 'id'],
                   "'. Setting this estimate to 0."));
        autofilledEstimatesDf <-
          rbind(autofilledEstimatesDf,
                data.frame(decision_id = optionsDf[i, 'decision_id'],
                           decision_label = optionsDf[i, 'decision_label'],
                           decision_option_value = optionsDf[i, 'value'],
                           option_label = optionsDf[i, 'label'],
                           criterion_id = criteriaDf[j, 'id'],
                           criterion_label = criteriaDf[j, 'label'],
                           value = autofill,
                           label = "Autofilled",
                           description = "This estimate was not 'manually' specified, and has therefore been autofilled with 0.",
                           id = paste0("id_", max(as.numeric(gsub("id_", "", estimatesDf$id)))+1),
                           stringsAsFactors = FALSE));
      }
    }
  }

  estimates$autofilledEstimatesDf <-
    autofilledEstimatesDf;
  estimates$autofillLogs <-
    autofillLogs;

  return(invisible(estimates));

}
