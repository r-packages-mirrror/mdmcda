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

  table <-
    knitr::kable(criteria$criteriaDf);

  res <-
    paste0(res,
           paste0(table,
                  collapse="\n"));

  res <- knitr::asis_output(res);

  return(res);
}
