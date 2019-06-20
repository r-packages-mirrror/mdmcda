#' @export
summary_decisions <- function(decisions_and_options,
                                header = "Summary of decisions",
                                headerLevel = 2) {

  if (is.null(header)) {
    res <- "\n\n";
  } else {
    res <- paste0("\n\n",
                  ufs::repStr("#", headerLevel),
                  " ",
                  header,
                  "\n\n");
  }

  res <-
    paste0(res,
           paste0(knitr::kable(decisions_and_options$decisionsDf,
                             row.names = FALSE),
                  collapse="\n"));

  res <- knitr::asis_output(res);

  return(res);
}
