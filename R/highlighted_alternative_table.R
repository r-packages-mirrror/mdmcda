#' @export
highlighted_alternative_table <-
  function(scores_per_alternative,
           decreasing = FALSE,
           scenario = NULL,
           decisionLabels = NULL,
           alternativeValues = NULL,
           caption = NULL,
           omitAlternativeLabelRegex = NULL,
           returnTableOnly = TRUE) {

    if (is.null(decisionLabels)) {
      decisionLabels <-
        stats::setNames(unique(scores_per_alternative$decision_id),
                        unique(scores_per_alternative$decision_id));
    }

    res <- list();
    res$raw <-
      lapply(
        unique(scores_per_alternative$decision_id),
        function(decision_id) {
          res <-
            scores_per_alternative[scores_per_alternative$decision_id == decision_id, ];
          res$alternative_label <-
            unlist(estimates$alternativeValues[[decision_id]])[res$alternative_id];
          if (is.null(decreasing)) {
            altOrder <-
              seq_along(res$score);
          } else {
            altOrder <-
              order(res$score,
                    decreasing = decreasing);
          }
          res <- res[altOrder,
                     c('decision_id', 'alternative_id', 'alternative_label', 'score')];
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
      newSequenceAt(res$dat$decision_id, shift=FALSE);
    packEndRows <-
      newSequenceEnds(res$dat$decision_id, shift=FALSE);
    # decisionPacks <-
    #   mapply(c,
    #          packStartRows,
    #          packEndRows,
    #          SIMPLIFY = FALSE);

    res$dat.clean <-
      res$dat[, c("alternative_label",
                  "score")];

    res$table <-
      kableExtra::kable_styling(
        knitr::kable(
          res$dat.clean,
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
