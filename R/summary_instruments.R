#' @export
summary_instruments <- function(instruments_and_options,
                                header = "Summary",
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
           paste0(knitr::kable(instruments_and_options$instrumentsDf,
                             row.names = FALSE),
                  collapse="\n"));

  res <- knitr::asis_output(res);

  return(res);
}
