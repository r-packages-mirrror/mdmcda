#' @export
summary_scores <- function(scores,
                           heading = "Summary of scores",
                           headingLevel = 2,
                           pdfCols = c(1, 2, 3),
                           pdfColLabels = c("Scenario",
                                            "Weight Profile",
                                            "Score"),
                           pdfColWidths = c("3cm", "3cm", "2cm")) {

  ### If we're not knitting, immediately return the decision
  ### dataframe
  if (is.null(knitr::opts_knit$get("rmarkdown.pandoc.to"))) {
    return(scores$scoresDf);
  }

  if (is.null(heading)) {
    res <- "\n\n";
  } else {
    res <- paste0("\n\n",
                  repStr("#", headingLevel),
                  " ",
                  heading,
                  "\n\n");
  }

  if (!(length(pdfCols) == length(pdfColLabels) &&
        length(pdfColLabels) == length(pdfColWidths))) {
    stop("Exactly equal lengths have to be provided for the ",
         "arguments 'pdfCols', 'pdfColLabels', and 'pdfColWidths'.");
  }

  if (knitr::is_latex_output()()) {
    table <-
      knitr::kable(scores$scoresDf[, pdfCols],
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
      knitr::kable(scores$scoresDf,
                   row.names = FALSE);
  }

  res <-
    paste0(res,
           paste0(table,
                  collapse="\n"));

  res <- knitr::asis_output(res);

  return(res);

}


#' #' @export
#' #' @method print scenario_scores
#' print.scenario_scores
