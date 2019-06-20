#' @export
summary_criteria <- function(criteria,
                             header = "Summary of criteria",
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

  return(suppressWarnings(
    plot(rev(as.dendrogram(criteria$criteriaTree)),
         horiz=TRUE,
         center=FALSE,
         asp=25,
         nodePar = list(pch = c(18, 20),
                        cex=1,
                        lab.cex = 3))
  ));

}
