#' Insert text only if knitting to HTML
#'
#' @param ... The text to insert; will be paste together the separator.
#' @param sep The separator.
#'
#' @return Nothing; the text is printed using [cat()].
#' @export
#'
#' @examples ### This will show nothing unless you happen to knit this
#' ### document, like in the Pkgdown version of the manual.
#' insert_if_knitting_to_html("This only shows up in the HTML version.");
insert_if_knitting_to_html <- function(...,
                                       sep="") {
  if ("html_document" %in% knitr::opts_knit$get("rmarkdown.pandoc.to")) {
    cat(..., sep=sep);
  } else {
    cat("");
  }
}
