#' @export
estimateDf_to_performance_table <- function(estimateDf,
                                            valueCol = c('value', 'value_mean')) {

  criterionId_col <- mdmcda::opts$get("criterionId_col");
  criterionLabel_col <- mdmcda::opts$get("criterionLabel_col");
  decisionId_col <- mdmcda::opts$get("decisionId_col");
  decisionLabel_col <- mdmcda::opts$get("decisionLabel_col");
  alternativeValue_col <- mdmcda::opts$get("alternativeValue_col");
  alternativeLabel_col <- mdmcda::opts$get("alternativeLabel_col");

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

  requiredCols <- c(decisionId_col,
                    decisionLabel_col,
                    alternativeValue_col,
                    alternativeLabel_col,
                    criterionId_col,
                    criterionLabel_col);

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
                               alternativeValue_col)]);
  decisions_and_alternatives <-
    estimateDf[unique_decisions_and_alternatives,
               c('decision_id', 'decision_label',
                 alternativeValue_col,
                 alternativeLabel_col)];

  ### Since the performance table has the criteria (and labels)
  ### along the columns and the decisons and alternatives (and
  ### their labels) along the rows, we can now generate the table

  ### Build dataframe filled with NAs
  performance_table_leftside <-
    data.frame(decision_id = c(rep(NA, 2), decisions_and_alternatives[, decisionId_col]),
               alternative_value = c(rep(NA, 2), decisions_and_alternatives[, alternativeValue_col]),
               decision_label = c(rep(NA, 2), decisions_and_alternatives[, decisionLabel_col]),
               alternative_label = c(rep(NA, 2), decisions_and_alternatives[, alternativeLabel_col]),
               stringsAsFactors = FALSE);

  names(performance_table_leftside) <-
    c(decisionId_col,
      alternativeValue_col,
      decisionLabel_col,
      alternativeLabel_col);

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
              performance_table[, alternativeValue_col] == decisions_and_alternatives[, alternativeValue_col][i]);
    if (length(targetRow) != 1) {
      stop("Either zero or more than one target row identified - this should be impossible, ",
           "given how the performance table is constructed - unless labels have been changed.");
    }
    for (targetCol in criteria_and_labels$criterion_id) {
      performance_table[targetRow, targetCol] <-
        estimateDf[estimateDf$decision_id == decisions_and_alternatives$decision_id[i] &
                     estimateDf[, alternativeValue_col] == decisions_and_alternatives[, alternativeValue_col][i] &
                     estimateDf$criterion_id == targetCol, valueCol];
    }
  }

  class(performance_table) <- c('performance_table', class(performance_table));

  return(performance_table);

}
