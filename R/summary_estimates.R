#' @export
summary_estimates <- function(estimates,
                              heading = "Summary of estimates",
                              colNames = c("Decision", "Number of estimates"),
                              headingLevel = 2) {

  table <-
    table(estimates$estimatesDf$decision_label, estimates$estimatesDf$criterion_label);

  ### IF we're not knitting, immediately return the decision
  ### dataframe
  if (is.null(knitr::opts_knit$get("rmarkdown.pandoc.to"))) {
    return(table);
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

  if ("pdf_document" %in% knitr::opts_knit$get("rmarkdown.pandoc.to")) {
    table <-
      knitr::kable(table,
                   booktabs = TRUE,
                   longtable = TRUE);
  } else {
    table <-
      knitr::kable(table);
  }

  res <-
    paste0(res,
           paste0(table,
                  collapse="\n"));

  res <- knitr::asis_output(res);

  return(res);
}
