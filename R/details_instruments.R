#' @export
details_instruments <- function(instruments_and_options,
                                header = "Details",
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
  for (x in instruments_and_options$instruments) {
    res <- paste0(res, "\n\n",
                  ufs::repStr("#", headerLevel+1),
                  " ", x$label, "\n\n");
    res <- paste0(res, "\n\n", x$description, "\n\n");
    for (i in x$allowedValues) {
      res <-
        paste0(res, "- **", i$label, "**: ", i$description, "\n\n");
    }
  }
  res <- knitr::asis_output(res);
  return(res);
}
