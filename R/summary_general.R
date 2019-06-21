#' @export
summary_general <- function(criteria,
                            decisions_and_options,
                            estimates) {

  res <- paste0("In this dynamic multi criteria decision aid, ",
                sum(criteria$criteriaDf$isLeaf),
                " criteria have been specified, as well as ",
                nrow(decisions_and_options$decisionsDf),
                " decisions that together encompass a total of ",
                nrow(decisions_and_options$optionsDf),
                " options. This means that ",
                sum(criteria$criteriaDf$isLeaf) * nrow(decisions_and_options$optionsDf),
                " estimates are required. Of these, ",
                nrow(estimates$estimatesDf),
                " have been specified.");

  if (is.null(knitr::opts_knit$get("rmarkdown.pandoc.to"))) {
    cat(res);
    return(invisible(res));
  } else {
    res <- knitr::asis_output(paste0("\n\n",
                                     res,
                                     "\n\n"));
    return(res);
  }

}
