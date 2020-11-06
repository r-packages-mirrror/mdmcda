#' Create a scenario overview with highlighted alternatives
#'
#' @param scores_per_alternative The `scorer_per_alternative` object as created
#' by [compute_scores_per_alternative()].
#' @param alternativeLabels The `alternativeLabels` object.
#' @param decreasing Whether to sort the alternatives in decreasing order
#' based on their final score.
#' @param scenarioDefinition The `scenarioDefinition` object to highlight;
#' if omitted, no highlighting is done.
#' @param decisionOrder The vector with the decision identifiers to include
#' (in the right order).
#' @param decisionLabels The named vector with the decision labels.
#' @param colNames The (two) column names to set in the resulting table.
#' @param caption The table caption to pass to [knitr::kable()].
#' @param estimateParseFunction Optionally, a function that is called to
#' parse the estimates before adding them to the table.
#' @param omitAlternativeLabelRegex A regular expression that can be used to
#' omit one or more alternatives based on their labels.
#' @param returnTableOnly Whether to return the table only, or the full
#' object that includes intermediate steps.
#' @param ... Any additional arguments are passed on to `estimateParseFunction`.
#'
#' @return
#' @export
highlighted_alternative_table <-
  function(scores_per_alternative,
           alternativeLabels,
           decreasing = FALSE,
           scenarioDefinition = NULL,
           decisionOrder = NULL,
           decisionLabels = NULL,
           colNames = c("Decisions and alternatives",
                        "Scores"),
           caption = NULL,
           estimateParseFunction = NULL,
           omitAlternativeLabelRegex = NULL,
           returnTableOnly = TRUE,
           ...) {

    decisionId_col <- mdmcda::opts$get("decisionId_col");

    if (is.null(decisionOrder)) {
      decisionOrder <-
        unique(scores_per_alternative[, decisionId_col]);
    }

    if (is.null(decisionLabels)) {
      decisionLabels <-
        stats::setNames(decisionOrder,
                        decisionOrder);
    }

    res <- list();
    res$raw <-
      lapply(
        decisionOrder,
        function(decision_id) {
          ### Select scores for this decision
          res <-
            scores_per_alternative[
              scores_per_alternative[, decisionId_col] == decision_id,
            ];
          ### Set the alternative labels
          res$alternative_label <-
            unlist(alternativeLabels[[decision_id]])[res$alternative_id];
          ### Determine order of alternatives
          if (is.null(decreasing)) {
            alternativesOrder <-
              seq_along(res$score);
          } else {
            alternativesOrder <-
              order(res$score,
                    decreasing = decreasing);
          }
          ### Create dataframe to return
          res <- res[alternativesOrder,
                     c('decision_id',
                       'alternative_id',
                       'alternative_label',
                       'score')];
          if (is.null(scenarioDefinition)) {
            res$highlight <-
              FALSE;
          } else {
            res$highlight <-
              res$alternative_id == scenarioDefinition[decision_id];
          }
          if (is.null(omitAlternativeLabelRegex)) {
            res$omit <-
              FALSE;
          } else {
            res$omit <-
              grepl(omitAlternativeLabelRegex,
                    res$alternative_label);
          }
          return(as.data.frame(res));
        }
      );
    names(res$raw) <-
      unique(scores_per_alternative$decision_id);

    res$dat.full <-
      do.call(rbind,
              res$raw);
    row.names(res$dat.full) <- NULL;

    res$dat <-
      res$dat.full[
        (!res$dat.full$omit) | res$dat.full$highlight,
      ];
    row.names(res$dat) <- NULL;

    alternativesPerDecision <-
      table(res$dat$decision_id)[unique(res$dat$decision_id)];
    packStartRows <-
      newSequenceAt(res$dat$decision_id);
    packEndRows <-
      newSequenceEnds(res$dat$decision_id);
    # decisionPacks <-
    #   mapply(c,
    #          packStartRows,
    #          packEndRows,
    #          SIMPLIFY = FALSE);

    res$decisionPacking <-
      list(alternativesPerDecision = alternativesPerDecision,
           packStartRows = packStartRows,
           packEndRows = packEndRows);

    res$dat.clean <-
      res$dat[, c("alternative_label",
                  "score")];

    if (!is.null(estimateParseFunction)) {
      res$dat.clean$score <-
        estimateParseFunction(res$dat.clean$score,
                              ...);
    }

    res$table <-
      kableExtra::kable_styling(
        knitr::kable(
          res$dat.clean,
          col.names = colNames,
          caption = caption
        )
      );

    res$table <-
      kableExtra::row_spec(
        res$table,
        row = which(res$dat$highlight),
        bold = TRUE
      );

    ### Pack the rows into groups
    for (i in seq_along(packStartRows)) {
      res$table <-
        kableExtra::pack_rows(
          res$table,
          decisionLabels[unique(res$dat$decision_id)[i]],
          packStartRows[i],
          packEndRows[i]
        );
    }

    if (returnTableOnly) {
      return(res$table);
    } else {
      return(res);
    }
  }
