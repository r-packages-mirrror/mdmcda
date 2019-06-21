#' @export
details_assertions <- function(estimates,
                               header = "Details of assertions",
                               headerLevel = 2,
                               pdfCols = c(1, 2, 3),
                               pdfColLabels = c("Identifier",
                                                "Label",
                                                "Source"),
                               pdfColWidths = c("4cm", "6cm", "4cm")) {

  assertions <-
    unlist(lapply(estimates$assertions,
                  function(x) {
                    return(is.null(x$type) || x$type == "assertion");
                  }));

  assertions <- estimates$assertions[assertions];

  assertionsDf <-
    do.call(rbind,
            lapply(assertions,
                   function(x) {
                     id <- ifelse(is.null(x$id), "no id specified", x$id);
                     label <- ifelse(is.null(x$label), "no label specified", x$label);
                     source <- ifelse(is.null(x$label), "no source specified", "one or multiple sources specified");
                     return(data.frame(id=id,
                                       label=label,
                                       source=source));
                   }));

  row.names(assertionsDf) <- NULL;

  ### If we're not knitting, immediately return the decision
  ### dataframe
  if (is.null(knitr::opts_knit$get("rmarkdown.pandoc.to"))) {
    return(assertionsDf);
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
      knitr::kable(assertionsDf[, pdfCols],
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
      knitr::kable(assertionsDf,
                   row.names = FALSE);
  }

  res <-
    paste0(res,
           paste0(table,
                  collapse="\n"));

  res <- knitr::asis_output(res);

  return(res);
}
