#' @export
create_performance_table <- function(criteria,
                                     decisions_and_alternatives) {
  criteria_ids <-
    criteria$criteriaDf[criteria$criteriaDf$isLeaf, 'id'];
  criteria_labels <-
    criteria$criteriaDf[criteria$criteriaDf$isLeaf, 'label'];
  decision_ids <- decisions_and_alternatives$alternativesDf$decision_id;
  decision_labels <- decisions_and_alternatives$alternativesDf$decision_label;
  alternative_values <- decisions_and_alternatives$alternativesDf$value;
  alternative_labels <- decisions_and_alternatives$alternativesDf$label;

  if (length(criteria_ids) != length(criteria_labels)) {
    stop("Wow, this is weird. This definitely shouldn't happen.");
  }

  nCols <- 4 + length(criteria_ids);

  resTop <-
    matrix(c(NA_character_, NA_character_, NA_character_, NA_character_, criteria_ids,
             NA_character_, NA_character_, NA_character_, NA_character_, criteria_labels),
           byrow=TRUE,
           ncol=nCols);

  resBottom <-
    matrix(c(decision_ids,
             alternative_values,
             decision_labels,
             alternative_labels,
             rep(NA_character_, length(criteria_ids) * length(decision_ids))),
           ncol=nCols);

  res <- rbind(resTop,
               resBottom);

  class(res) <- c('dmcda', 'performance_table', class(res));

  return(res);
}
