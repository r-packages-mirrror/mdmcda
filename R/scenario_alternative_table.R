#' Create a scenario overview with the selected alternatives
#'
#' @param scenarioDefinition The `scenarioDefinition` object.
#' @param alternativeLabels The `alternativeLabels` object.
#' @param decisionOrder The vector with the decision identifiers to include
#' (in the right order).
#' @param decisionLabels The named vector with the decision labels.
#' @param colNames The (two) column names to set in the resulting pretty table.
#' @param caption The table caption to pass to [knitr::kable()].
#' @param returnTableOnly Whether to return the table only, or the full
#' object that includes intermediate steps.
#'
#' @return
#' @export
#'
#' @examples
scenario_alternative_table <-
  function(scenarioDefinition,
           alternativeLabels,
           decisionOrder = NULL,
           decisionLabels = NULL,
           colNames = c("Decision",
                        "Alternative"),
           caption = NULL,
           returnTableOnly = TRUE) {

    decisionId_col           <- mdmcda::opts$get("decisionId_col");
    decisionLabel_col        <- mdmcda::opts$get("decisionLabel_col");
    alternativeValue_col     <- mdmcda::opts$get("alternativeValue_col");
    alternativeLabel_col     <- mdmcda::opts$get("alternativeLabel_col");

    res <- list();
    res$dat.clean <-
      do.call(
        rbind,
        lapply(
          decisionOrder,
          function(decision_id) {
            selectedAlternative <-
              alternativeLabels[[decision_id]][as.character(scenarioDefinition[decision_id])];
            res <-
              data.frame(
                decision_id = decision_id,
                decision_label = decisionLabels[decision_id],
                alternative_value = names(selectedAlternative),
                alternative_label = selectedAlternative,
                stringsAsFactors = FALSE
              );
            names(res) <-
              c(decisionId_col, decisionLabel_col,
                alternativeValue_col, alternativeLabel_col);
            return(res);
          }
        )
      );

  res$table <-
    kableExtra::kable_styling(
      knitr::kable(
        res$dat.clean[, c('decision_label', 'alternative_label')],
        col.names = colNames,
        caption = caption,
        row.names = FALSE
      )
    );

  if (returnTableOnly) {
    return(res$table);
  } else {
    return(res);
  }

}
