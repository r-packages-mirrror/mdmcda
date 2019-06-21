#' @export
details_sources <- function(estimates,
                            heading = "Details of sources",
                            headingLevel = 2,
                            pdfCols = c(1, 2, 3),
                            pdfColLabels = c("Identifier",
                                             "Label",
                                             "xdoi"),
                            pdfColWidths = c("4cm", "6cm", "4cm")) {

  sourcesDf <-
    do.call(rbind,
            lapply(estimates$sources,
                   function(x) {
                     id <- ifelse(is.null(x$id), "no id specified", x$id);
                     label <- ifelse(is.null(x$label), "no label specified", x$label);
                     xdoi <- ifelse(is.null(x$xdoi), "no xdoi specified", x$xdoi);
                     return(data.frame(id=id,
                                       label=label,
                                       xdoi=xdoi));
                   }));

  row.names(sourcesDf) <- NULL;

  ### If we're not knitting, immediately return the decision
  ### dataframe
  if (is.null(knitr::opts_knit$get("rmarkdown.pandoc.to"))) {
    return(sourcesDf);
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
      knitr::kable(sourcesDf[, pdfCols],
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
      knitr::kable(sourcesDf,
                   row.names = FALSE);
  }

  res <-
    paste0(res,
           paste0(table,
                  collapse="\n"));

  res <- knitr::asis_output(res);

  return(res);
}
