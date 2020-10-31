#' Create data frame with identifiers and labels
#'
#' Convenience function to convert a label vector to a label data frame.
#'
#' @param namedVector The named vector, names should be identifiers,
#' values should be labels.
#' @param idCol,labelCol The names to use for the identifier column and the
#' label column.
#'
#' @return The data frame
#' @export
#'
#' @examples mdmcda::create_labelDf(
#'   c(
#'     id1 = "first element",
#'     id2 = "second element"
#'   ),
#'   idCol = "example_id",
#'   labelCol = "example_label"
#' );
create_labelDf <- function(namedVector,
                           idCol,
                           labelCol) {
  res <-
    data.frame(
      id = names(namedVector),
      label = unname(namedVector)
    );
  names(res) <-
    c(idCol, labelCol);
  return(res);
}
