#' @export
summary_criteria <- function(criteria,
                             heading = "Summary of criteria",
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

  knit_print(criteria$criteriaTree);

  cat0("\n\n");

  return(suppressWarnings(
    plot(rev(as.dendrogram(criteria$criteriaTree)),
         horiz=TRUE,
         center=FALSE,
         asp=25,
         nodePar = list(pch = c(18, 20),
                        cex=1.5,
                        lab.cex = 2))
  ));

}
