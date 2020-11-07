autofill_estimates <- function(estimates,
                               criteria,
                               decisions_and_alternatives,
                               autofill = NA) {

  criterionId_col <- mdmcda::opts$get("criterionId_col");
  decisionId_col <- mdmcda::opts$get("decisionId_col");
  decisionLabel_col <- mdmcda::opts$get("decisionLabel_col");
  alternativeValue_col <- mdmcda::opts$get("alternativeValue_col");
  alternativeLabel_col <- mdmcda::opts$get("alternativeLabel_col");

  estimatesDf <- estimates$estimatesDf;
  criteriaDf <- criteria$criteriaDf;
  alternativesDf <- decisions_and_alternatives$alternativesDf;
  decisionsDf <- decisions_and_alternatives$decisionsDf;

  autofillLogs <- "\n";
  autofilledEstimatesDf <-
    estimatesDf;

  for (i in 1:nrow(alternativesDf)) {
    for (j in which(criteriaDf$leafCriterion)) {
      if (any((estimatesDf[, decisionId_col] == alternativesDf[i, decisionId_col]) &
              (estimatesDf[, alternativeValue_col] == alternativesDf[i, 'value']) &
              (estimatesDf[, criterionId_col] == criteriaDf[j, 'id']))) {
        autofillLogs <-
          c(autofillLogs,
            paste0("- Estimate found for the effect of alternative '",
                   alternativesDf[i, 'label'],
                   "' for decision '",
                   decisionsDf[decisionsDf$id==alternativesDf[i, 'instrument_id'], 'label'],
                   "' for criterion '",
                   criteriaDf[j, 'id'],
                   "': ",
                   estimatesDf[(estimatesDf$decision_id==alternativesDf[i, decisionId_col]) &
                                 (estimatesDf[, alternativeValue_col]==alternativesDf[i, 'value']) &
                                 (estimatesDf$criterion_id==criteriaDf[j, 'id']),
                               "value"]));
      } else {
        autofillLogs <-
          c(autofillLogs,
            paste0("- **No** estimate found for the effect of alternative '",
                   alternativesDf[i, 'label'],
                   "' for decision '",
                   decisionsDf[decisionsDf$id==alternativesDf[i, decisionId_col], 'label'],
                   "' for criterion '",
                   criteriaDf[j, 'id'],
                   "'. Setting this estimate to '",
                   autofill,
                   "'."));
        autofilledEstimatesDf <-
          rbind(autofilledEstimatesDf,
                data.frame(decision_id = alternativesDf[i, decisionId_col],
                           decision_label = alternativesDf[i, decisionLabel_col],
                           alternative_value = alternativesDf[i, 'value'],
                           alternative_label = alternativesDf[i, 'label'],
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
