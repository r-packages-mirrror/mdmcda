#' @export
details_decisions <- function(decisions_and_alternatives,
                              header = "Details of decisions",
                              headerLevel = 2) {
  if (is.null(header)) {
    res <- "\n\n";
  } else {
    res <- paste0("\n\n",
                  repStr("#", headerLevel),
                  " ",
                  header,
                  "\n\n");
  }
  for (x in decisions_and_alternatives$decisions) {
    res <- paste0(res, "\n\n",
                  repStr("#", headerLevel+1),
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
