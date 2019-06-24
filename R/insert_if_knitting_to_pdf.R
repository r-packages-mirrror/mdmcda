#' Insert text only if knitting to pdf
#'
#' @param ... The text to insert; will be paste together the separator.
#' @param sep The separator.
#'
#' @return Nothing; the text is printed using [cat()].
#' @export
#'
#' @examples ### This will show nothing unless you happen to knit this
#' ### document, like in the Pkgdown version of the manual.
#' insert_if_knitting_to_pdf("This only shows up in the PDF version.");
insert_if_knitting_to_pdf <- function(...,
                                      sep="") {
  if ("latex" %in% knitr::opts_knit$get("rmarkdown.pandoc.to")) {
    cat(..., sep=sep);
  } else {
    cat("");
  }
}
