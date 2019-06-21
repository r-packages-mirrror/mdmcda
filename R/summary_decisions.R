#' @export
summary_decisions <- function(decisions_and_alternatives,
                              heading = "Summary of decisions",
                              headingLevel = 2,
                              pdfCols = c(2, 3, 4),
                              pdfColLabels = c("Decision",
                                               "Description",
                                               "Alternatives"),
                              pdfColWidths = c("5cm", "5cm", "5cm")) {

  ### IF we're not knitting, immediately return the decision
  ### dataframe
  if (is.null(knitr::opts_knit$get("rmarkdown.pandoc.to"))) {
    return(decisions_and_alternatives$decisionsDf);
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

  if (knitr::is_latex_output()) {
    table <-
      knitr::kable(decisions_and_alternatives$decisionsDf[, pdfCols],
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
      knitr::kable(decisions_and_alternatives$decisionsDf,
                   row.names = FALSE);
  }

  res <-
    paste0(res,
           paste0(table,
                  collapse="\n"));

  res <- knitr::asis_output(res);

  return(res);
}
