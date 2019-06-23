#' @export
estimateDf_to_performance_table <- function(estimateDf,
                                            valueCol = c('value', 'value_mean')) {

  if (!is.data.frame(estimateDf)) {
    stop("As 'estimateDf', you have to pass a dataframe with estimates!");
  }

  existingValueCols <-
    valueCol %in% names(estimateDf);

  if (!any(existingValueCols)) {
    stop("At least one of the columns specified as 'valueCol' must ",
         "exist in the dataframe provided as 'estimateDf'!");
  }

  valueCol <- valueCol[min(which(existingValueCols))];

  requiredCols <- c('decision_id',
                    'decision_label',
                    'decision_alternative_value',
                    'decision_alternative_label',
                    'criterion_id',
                    'criterion_label');

  if (!all(requiredCols %in% names(estimateDf))) {
    stop("The dataframe suppied as 'estimateDf' does not seem to be ",
         "an estimateDf - it does not include columns ",
         vecTxtQ(requiredCols),
         ".");
  }

  ### Get all criteria and labels
  criteria_and_labels <-
    unique(estimateDf[, c('criterion_id', 'criterion_label')]);

  ### Get all decisions and alternatives; use 'duplicated' instead of
  ### 'unique' because the labels can be ignored
  unique_decisions_and_alternatives <-
    !duplicated(estimateDf[, c('decision_id',
                               'decision_alternative_value')]);
  decisions_and_alternatives <-
    estimateDf[unique_decisions_and_alternatives,
               c('decision_id', 'decision_label',
                 'decision_alternative_value',
                 'decision_alternative_label')];

  ### Since the performance table has the criteria (and labels)
  ### along the columns and the decisons and alternatives (and
  ### their labels) along the rows, we can now generate the table

  ### Build dataframe filled with NAs
  performance_table_leftside <-
    data.frame(decision_id = c(rep(NA, 2), decisions_and_alternatives$decision_id),
               decision_alternative_value = c(rep(NA, 2), decisions_and_alternatives$decision_alternative_value),
               decision_label = c(rep(NA, 2), decisions_and_alternatives$decision_label),
               decision_alternative_label = c(rep(NA, 2), decisions_and_alternatives$decision_alternative_label),
               stringsAsFactors = FALSE);

  performance_table_topside <-
    t(as.matrix(criteria_and_labels));

  performance_table_cells <-
    matrix(rep(NA, nrow(criteria_and_labels) * length(decisions_and_alternatives$decision_id)),
           ncol=nrow(criteria_and_labels));

  performance_table <-
    cbind(performance_table_leftside,
          rbind(performance_table_topside,
                performance_table_cells),
          stringsAsFactors = FALSE);

  row.names(performance_table) <-
    NULL;

  names(performance_table) <-
    c(names(performance_table)[1:4],
      criteria_and_labels[, 'criterion_id']);

  for (i in 1:nrow(decisions_and_alternatives)) {
    targetRow <-
      which(performance_table$decision_id == decisions_and_alternatives$decision_id[i] &
              performance_table$decision_alternative_value == decisions_and_alternatives$decision_alternative_value[i]);
    if (length(targetRow) != 1) {
      stop("Either zero or more than one target row identified - this should be impossible, ",
           "given how the performance table is constructed - unless labels have been changed.");
    }
    for (targetCol in criteria_and_labels$criterion_id) {
      performance_table[targetRow, targetCol] <-
        estimateDf[estimateDf$decision_id == decisions_and_alternatives$decision_id[i] &
                     estimateDf$decision_alternative_value == decisions_and_alternatives$decision_alternative_value[i] &
                     estimateDf$criterion_id == targetCol, valueCol];
    }
  }

  class(performance_table) <- c('performance_table', class(performance_table));

  return(performance_table);

}
