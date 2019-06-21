#' @export
details_criteria <- function(criteria,
                             heading = "Details of criteria",
                             headingLevel = 2) {
  if (is.null(heading)) {
    res <- "\n\n";
  } else {
    res <- paste0("\n\n",
                  repStr("#", headingLevel),
                  " ",
                  heading,
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
