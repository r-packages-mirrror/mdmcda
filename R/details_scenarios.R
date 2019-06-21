#' @export
details_scenarios <- function(scenarios_and_alternatives,
                              header = "Details of scenarios",
                              headerLevel = 2,
                              pdfCols = c(1, 2, 3),
                              pdfColLabels = c("Scenario",
                                               "Decision",
                                               "alternative"),
                              pdfColWidths = c("3cm", "6cm", "6cm")) {

  ### IF we're not knitting, immediately return the decision
  ### dataframe
  if (is.null(knitr::opts_knit$get("rmarkdown.pandoc.to"))) {
    return(scenarios_and_alternatives$scenarioalternativesDf);
  }

  if (is.null(header)) {
    res <- "\n\n";
  } else {
    res <- paste0("\n\n",
                  ufs::repStr("#", headerLevel),
                  " ",
                  header,
                  "\n\n");
  }

  if (!(length(pdfCols) == length(pdfColLabels) &&
        length(pdfColLabels) == length(pdfColWidths))) {
    stop("Exactly equal lengths have to be provided for the ",
         "arguments 'pdfCols', 'pdfColLabels', and 'pdfColWidths'.");
  }

  if ("pdf_document" %in% knitr::opts_knit$get("rmarkdown.pandoc.to")) {
    table <-
      knitr::kable(scenarios_and_alternatives$scenarioalternativesDf[, pdfCols],
                   row.names = FALSE,
                   col.names=pdfColLabels,
                   booktabs = TRUE, longtable = TRUE);
    for (i in seq_along(pdfCols)) {
      table <-
        kableExtra::column_spec(tale,
                                column = i,
                                width = pdfColWidths[i]);
    }
  } else {
    table <-
      knitr::kable(scenarios_and_alternatives$scenarioalternativesDf,
                   row.names = FALSE);
  }

  res <-
    paste0(res,
           paste0(table,
                  collapse="\n"));

  res <- knitr::asis_output(res);

  return(res);
}
