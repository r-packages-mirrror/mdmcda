#' @export
highlighted_alternative_table <-
  function(scores_per_alternative,
           alternativeLabels,
           decreasing = FALSE,
           scenario = NULL,
           decisionLabels = NULL,
           decisionOrder = NULL,
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
          if (is.null(scenario)) {
            res$highlight <-
              FALSE;
          } else {
            res$highlight <-
              res$alternative_id == scenario[decision_id];
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
