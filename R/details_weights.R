#' @export
details_weights <- function(weights,
                            heading = "Details of weights",
                            headingLevel = 2,
                            pdfCols = c(1, 2, 3, 4),
                            pdfColLabels = c("Weight profile",
                                             "Criterion id",
                                             "Criterion label",
                                             "Weight"),
                            pdfColWidths = c("2cm", "4cm", "4cm", "1cm")) {

  ### IF we're not knitting, immediately return the decision
  ### dataframe
  if (is.null(knitr::opts_knit$get("rmarkdown.pandoc.to"))) {
    return(weights$weightsDf);
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

  if (any(c("pdf_document", "latex") %in% knitr::opts_knit$get("rmarkdown.pandoc.to"))) {
    table <-
      knitr::kable(weights$weightsDf[, pdfCols],
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
      knitr::kable(weights$weightsDf,
                   row.names = FALSE);
  }

  res <-
    paste0(res,
           paste0(table,
                  collapse="\n"));

  res <- knitr::asis_output(res);

  return(res);
}
