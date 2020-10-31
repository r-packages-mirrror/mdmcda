#' Write labels to an Excel file
#'
#' These function writes labels to an Excel file (by first converting them
#' to a data frame).
#'
#' The `write_labels_to_xl()` function is the underlying function; users
#' typically don't interact with this function, unless they specified custom
#' column names.
#'
#' @param x The object with the labels: a named vector, except for
#' alternative labels, which is a nested named list (see
#' [read_labels_from_xl()]).
#' @param file The filename to write to.
#' @param idCol,labelCol For `write_labels_to_xl()`, the name of the
#' columns with the identifiers and the labels.
#'
#' @return The written data frame.
#'
#' @rdname write_labels_to_xl
#' @export
write_labels_to_xl <- function(x,
                               file,
                               idCol,
                               labelCol,
                               preventOverwriting = mdmcda::opts$get("preventOverwriting"),
                               quiet = mdmcda::opts$get("quiet")) {

  res <-
    create_labelDf(
      x,
      idCol,
      labelCol
    );

  if (file.exists(file)) {
    if (preventOverwriting) {
      if (!quiet) {
        warning("You specified file '", file,
                "' to write to, but it already exists!");
      }
      return(invisible(res));
    }
  }

  openxlsx::write.xlsx(
    res,
    file
  );

  return(invisible(res));

}

#' @rdname write_labels_to_xl
#' @export
read_criterionLabels_from_xl <- function(file) {
  return(
    write_labels_to_xl(
      file = file,
      idCol = mdmcda::opts$get("criterionId_col"),
      labelCol = mdmcda::opts$get("criterionLabel_col")
    )
  );
}

#' @rdname write_labels_to_xl
#' @export
write_decisionLabels_to_xl <- function(file) {
  return(
    write_labels_to_xl(
      file = file,
      idCol = mdmcda::opts$get("decisionId_col"),
      labelCol = mdmcda::opts$get("decisionLabel_col")
    )
  );
}

#' @rdname write_labels_to_xl
#' @export
write_scenarioLabels_to_xl <- function(file) {
  return(
    write_labels_to_xl(
      file = file,
      idCol = mdmcda::opts$get("scenarioId_col"),
      labelCol = mdmcda::opts$get("scenarioLabel_col")
    )
  );
}

#' @rdname write_labels_to_xl
#' @export
write_alternativeLabels_to_xl <- function(x,
                                          file,
                                          preventOverwriting = mdmcda::opts$get("preventOverwriting"),
                                          quiet = mdmcda::opts$get("quiet")) {

  decisionId_col <- mdmcda::opts$get("decisionId_col");
  alternativeValue_col <- mdmcda::opts$get("alternativeValue_col");
  alternativeLabel_col <- mdmcda::opts$get("alternativeLabel_col");

  res <-
    do.call(
      rbind,
      lapply(
        names(x),
        function(decision_id) {
          res <-
            data.frame(
              decision_id,
              names(unlist(x[[decision_id]])),
              unlist(x[[decision_id]])
            );
          names(res) <- c(
            decisionId_col,
            alternativeValue_col,
            alternativeLabel_col
          );
          return(res);
        }
      )
    );

  if (file.exists(file)) {
    if (preventOverwriting) {
      if (!quiet) {
        warning("You specified file '", file,
                "' to write to, but it already exists!");
      }
      return(invisible(res));
    }
  }

  openxlsx::write.xlsx(
    res,
    file
  );

  return(invisible(res));

}
