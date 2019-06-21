#' @export
details_autofilled_estimates <- function(estimates,
                                         header = "Details of estimates after autofilling",
                                         headerLevel = 2,
                                         pdfCols = c(2, 4, 6, 7, 8, 9),
                                         pdfColLabels = c("Decision",
                                                          "alternative",
                                                          "Criterion",
                                                          "Value",
                                                          "Label",
                                                          "Description"),
                                         pdfColWidths = c("2cm", "1.5cm", "2cm",
                                                          "1cm", "4cm", "4cm")) {

  if (is.null(estimates$autofilledEstimatesDf)) {
    stop("Estimates have not been autofilled yet! First run 'autofill_estimates'!");
  }

  ### IF we're not knitting, immediately return the decision
  ### dataframe
  if (is.null(knitr::opts_knit$get("rmarkdown.pandoc.to"))) {
    return(estimates$autofilledEstimatesDf);
  }

  if (is.null(header)) {
    res <- "\n\n";
  } else {
    res <- paste0("\n\n",
                  repStr("#", headerLevel),
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
      knitr::kable(estimates$autofilledEstimatesDf[, pdfCols],
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
      knitr::kable(estimates$autofilledEstimatesDf,
                   row.names = FALSE);
  }

  res <-
    paste0(res,
           paste0(table,
                  collapse="\n"));

  res <- knitr::asis_output(res);

  return(res);
}
