#' Create data frame with identifiers and labels
#'
#' Convenience function to convert a label vector to a label data frame.
#'
#' @param namedVector The named vector, names should be identifiers,
#' values should be labels.
#' @param type The type is prefixed to the column names (the suffixes
#' `_id` and `_label` are added). Common types are "`decision`", "`criterion`",
#' and "`scenario`".
#'
#' @return The data frame
#' @export
#'
#' @examples create_labelDf(
#'   c(
#'     id1 = "first element",
#'     id2 = "second element"
#'   ),
#'   type = "example"
#' );
create_labelDf <- function(namedVector,
                           type) {
  res <-
    data.frame(
      id = names(namedVector),
      label = unname(namedVector)
    );
  names(res) <-
    paste0(type, c("_id", "_label"));
  return(res);
}
