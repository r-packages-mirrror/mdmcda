#' @export
performance_table <- function(criteria,
                              decisions_and_alternatives) {
  criteria_ids <-
    criteria$criteriaDf[criteria$criteriaDf$isLeaf, 'id'];
  criteria_labels <-
    criteria$criteriaDf[criteria$criteriaDf$isLeaf, 'label'];
  decision_ids <- decisions_and_alternatives$alternativesDf$decision_id;
  decision_labels <- decisions_and_alternatives$alternativesDf$decision_label;
  alternative_values <- decisions_and_alternatives$alternativesDf$value;
  alternative_labels <- decisions_and_alternatives$alternativesDf$label;

  resTop <-
    matrix(c(NA, NA, NA, NA, criteria_ids),
           c(NA, NA, NA, NA, criteria_labels),
           byrow=TRUE);

  resTop <-
    matrix(c(decision_ids,
             alternative_values,
             decision_labels,
             alternative_labels,
             rep(NA, length(criteria_ids) * length(decision_ids))));

  res <- rbind(resTop,
               resBottom);

  return(res);
}
