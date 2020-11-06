#' Read labels or descriptions from an Excel file
#'
#' These function reads labels or descriptions from an Excel file and returns
#' them as a named vector (except when reading alternative labels; see Details).
#'
#' The `read_labels_from_xl()` function is the underlying function; users
#' typically don't interact with this function, unless they specified custom
#' column names.
#'
#' @param file The filename.
#' @param idCol,labelCol For `read_labels_from_xl()`, the name of the
#' columns with the identifiers and the labels.
#'
#' @return A named vector, unless alternative labels are read, in which case
#' a named nested list is returned. Each list element is named with a
#' decision identifiers, and consist of a named list, where each element is an
#' alternative label, and each name the corresponding alternative value.
#'
#' @rdname read_labels_from_xl
#' @export
read_labels_from_xl <- function(file,
                                idCol,
                                labelCol) {

  if (!file.exists(file)) {
    stop("You specified file '", file,
         "' to read from, but it does not exist!");
  }

  dat <- openxlsx::read.xlsx(file);

  if (!all(c(idCol, labelCol) %in% names(dat))) {
    stop("You specified that you wanted to load labels from a ",
         "spreadsheet containing the identifiers in column '",
         idCol, "' and the labels in column '", labelCol, "', but ",
         "at least one of those two does not exist in the file you ",
         "speified ('", file, "').");
  }

  return(stats::setNames(dat[, labelCol],
                         dat[, idCol]));

}

#' @rdname read_labels_from_xl
#' @export
read_criterionLabels_from_xl <- function(file) {
  return(
    read_labels_from_xl(
      file = file,
      idCol = mdmcda::opts$get("criterionId_col"),
      labelCol = mdmcda::opts$get("criterionLabel_col")
    )
  );
}

#' @rdname read_labels_from_xl
#' @export
read_decisionLabels_from_xl <- function(file) {
  return(
    read_labels_from_xl(
      file = file,
      idCol = mdmcda::opts$get("decisionId_col"),
      labelCol = mdmcda::opts$get("decisionLabel_col")
    )
  );
}

#' @rdname read_labels_from_xl
#' @export
read_decisionDescriptions_from_xl <- function(file) {
  return(
    read_labels_from_xl(
      file = file,
      idCol = mdmcda::opts$get("decisionId_col"),
      labelCol = mdmcda::opts$get("decisionDescription_col")
    )
  );
}

#' @rdname read_labels_from_xl
#' @export
read_scenarioLabels_from_xl <- function(file) {
  return(
    read_labels_from_xl(
      file = file,
      idCol = mdmcda::opts$get("scenarioId_col"),
      labelCol = mdmcda::opts$get("scenarioLabel_col")
    )
  );
}

#' @rdname read_labels_from_xl
#' @export
read_alternativeLabels_from_xl <- function(file) {

  decisionId_col <- mdmcda::opts$get("decisionId_col");
  alternativeValue_col <- mdmcda::opts$get("alternativeValue_col");
  alternativeLabel_col <- mdmcda::opts$get("alternativeLabel_col");

  if (!file.exists(file)) {
    stop("You specified file '", file,
         "' to read from, but it does not exist!");
  }

  dat <- openxlsx::read.xlsx(file);

  if (!all(c(decisionId_col,
             alternativeValue_col,
             alternativeLabel_col)
           %in% names(dat))) {
    stop("You specified that you wanted to load labels from the ",
         "spreadsheet in '", file, "', but it does not have the ",
         "columns '", decisionId_col, "', '", alternativeValue_col,
         "', and '", alternativeLabel_col, "'.");
  }

  decisionIds <- unique(dat[, decisionId_col]);

  res <- lapply(
    decisionIds,
    function(decisionId) {
      res <-
        as.list(
          dat[dat[, decisionId_col] == decisionId,
              alternativeLabel_col]
        );
      names(res) <-
        dat[dat[, decisionId_col] == decisionId,
            alternativeValue_col];
      return(res);
    }
  );
  names(res) <- decisionIds;

  return(res);

}
