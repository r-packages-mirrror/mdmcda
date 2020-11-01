#' Import scenario definitions
#'
#' @param file The filename holding the scenario definitions as exported by
#' [export_scenarioDefinitions_to_xl()], where each worksheet has a
#' column `decision_id` and a column `alternative_value`.
#'
#' @return A list of vectors (i.e. a scenarioDefinitions object).
#' @export
read_scenarioDefinitions_from_xl <- function(file) {

  if (!file.exists(file)) {
    stop("The file you specified does not exist!");
  }

  sheetNames <-
    openxlsx::getSheetNames(
      file
    );

  res <-
    lapply(
      sheetNames,
      function(sheetName) {
        ### Read data from this worksheet
        res <-
          openxlsx::read.xlsx(
            file,
            sheet = sheetName
          );
        ### Convert to vector and return
        res <-
          stats::setNames(
            res$alternative_value,
            nm = res$decision_id
          );
        return(res);
      }

    );

  names(res) <- sheetNames;

  return(res);

}
