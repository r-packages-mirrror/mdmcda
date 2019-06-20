#' @export
details_criteria <- function(criteria,
                                header = "Details of criteria",
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
    knitr::kable(criteria$criteriaDf);

  res <- knitr::asis_output(res);

  return(res);
}
