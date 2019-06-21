#' @export
summary_scores <- function(scores,
                           header = "Summary of scores",
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

  ### If we're not knitting, immediately return the decision
  ### dataframe
  if (is.null(knitr::opts_knit$get("rmarkdown.pandoc.to"))) {
    return(scores$scoresDf);
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
      knitr::kable(scores$scoresDf[, pdfCols],
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
