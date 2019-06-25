#' @export
details_scores <- function(scenario_scores,
                           heading = "Details of scores",
                           headingLevel = 2,
                           pdfCols = c(3, 4, 5, 6),
                           pdfColLabels = c("Decision identifier",
                                            "Alternative value",
                                            "Criterion identifier",
                                            "Value"),
                           pdfColWidths = c("5cm", "3cm", "4cm", "2cm")) {

  if (is.null(heading)) {
    res <- "\n\n";
  } else {
    res <- paste0("\n\n",
                  repStr("#", headingLevel),
                  " ",
                  heading,
                  "\n\n");
  }

  for (i in names(scenario_scores$scenarioScores)) {
    for (j in names(scenario_scores$scenarioScores[[i]])) {
      res <- paste0(res,
                    "\n\n## Scenario: ",
                    i,
                    " {.tabset}\n\n",
                    "\n\n### Weighing profile: ",
                    j," {.tabset}\n\n");

      if (knitr::is_latex_output()) {
        table <-
          knitr::kable(scenario_scores$scenarioScores[[i]][[j]][, pdfCols],
                       format="latex",
                       row.names = FALSE,
                       col.names=pdfColLabels,
                       booktabs = TRUE, longtable = TRUE);
        for (i in seq_along(pdfCols)) {
          table <-
            kableExtra::column_spec(table,
                                    column = i,
                                    width = pdfColWidths[i]);
        }
      } else {
        table <-
          knitr::kable(scenario_scores$scenarioScores[[i]][[j]][, pdfCols],
                       row.names = FALSE);
      }

      res <-
        paste0(res,
               paste0(table,
                      collapse="\n"));
    }
  }

  res <- knitr::asis_output(res);

  return(res);

}
