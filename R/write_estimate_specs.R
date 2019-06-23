write_estimate_specs <- function(estimateDf,
                                 path,
                                 overwrite = c("never",
                                               "sometimes",
                                               "always"),
                                 valueCol = c('value', 'value_mean'),
                                 encoding="UTF-8",
                                 filenameCol = NULL) {

  if (!is.data.frame(estimateDf)) {
    stop("As 'estimateDf', you have to pass a dataframe with estimates!");
  }

  existingValueCols <-
    valueCol %in% names(estimateDf);
  overwrite <-
    overwrite[1];

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

  if (!dir.exists(path)) {
    stop("The director specified in `path`, '",
         path,
         "', does not exist!");
  }

  if (is.null(filenameCol)) {
    filenameCol <- 'filenameCol';
    estimateDf$filenameCol <-
      paste0("estimate--",
             estimateDf$decision_id,
             "--",
             estimateDf$decision_alternative_value,
             "--",
             estimateDf$criterion_id,
             ".jmd");
  }

  for (i in 1:nrow(estimateDf)) {
    filename <-
      file.path(path, estimateDf[i, filenameCol]);
    if (file.exists(filename) && (overwrite == "never")) {
      ### We do nothing
    } else if (file.exists(filename) && (overwrite == "sometimes")) {
      ### For now, we also do nothing
    } else {
      contents <-
        paste0("This is the specification of a estimate for a dynamic multi criteria decision aid procedure.

All text above and below the triple dashes will be ignored, and can be used to add comments.

---
justification:
  -
    ### Specify the identifier of the decision and the value of the alternative
    ### within that decision that this estimate pertains to.
    decision_id: ", estimateDf[i, 'decision_id'], "
    decision_alternative_value: ", estimateDf[i, 'decision_alternative_value'], "

    ### Specify the identifier of the criterion to which this estimate applies.
    criterion_id: ", estimateDf[i, 'criterion_id'], "

    ### Specify the value of the estimate on the agreed-upon scale.
    value: ", estimateDf[i, valueCol], "

    ### Specify a human-readable short label.
    label: \"", estimateDf[i, 'label'], "\"

    ### Specify a description that can be as long as you want.
    description: \"", estimateDf[i, 'description'], "\"

    ### Specify the identifier of one or more assertions that this
    ### justifications is based on. Separate identifiers using commas.
    assertion: [placeholder_assertion]

    ### Don't change this (has to be 'estimate' for all estimates)
    type: estimate
---
");
      writeLines(text = contents,
                 con <- file(filename,
                             encoding=encoding));
      close(con);
    }
  }

  return(invisible(filenameCol));

}
