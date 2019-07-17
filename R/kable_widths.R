#' @export
kable_widths <- function(x,
                         heading = NULL,
                         headingLevel = 2,
                         pdfCols = ncol(x),
                         pdfColLabels = names(x),
                         pdfColWidths = paste0((16 / length(pdfCols)), "cm"),
                         row.names=FALSE) {

  ### If we're not knitting, immediately return the provided dataframe
  if (is.null(knitr::opts_knit$get("rmarkdown.pandoc.to"))) {
    return(x);
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
      knitr::kable(x[, pdfCols],
                   format="latex",
                   row.names = row.names,
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
      knitr::kable(x,
                   row.names = row.names);
  }

  res <-
    paste0(res,
           paste0(table,
                  collapse="\n"));

  res <- knitr::asis_output(res);

  return(res);

}
