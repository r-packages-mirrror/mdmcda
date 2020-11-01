#' Import scenario definitions defined in columns
#'
#' @param file The filename holding the scenario definitions, with the first
#' three columns specifying the decision identifiers, labels, and descriptions,
#' the fourth column specifying the concatenated alternative labels and values,
#' and the other columns specifying scenarios, with each column name being
#' a scenario identifier, and the values in each column indicating the selected
#' alternative value for each decision.
#' @param decisionId_col,decisionLabel_col,decisionDescription_col,decisionAlternatives_col The
#' column names.
#'
#' @return A list of vectors (i.e. a scenarioDefinitions object).
#' @export
read_scenarioDefinitions_in_columns_from_xl <-
  function(
    file,
    decisionId_col = mdmcda::opts$get("decisionId_col"),
    decisionLabel_col = mdmcda::opts$get("decisionLabel_col"),
    decisionDescription_col = mdmcda::opts$get("decisionDescription_col"),
    decisionAlternatives_col = mdmcda::opts$get("decisionAlternatives_col")
  ) {

    allCols <- c(decisionId_col,
                 decisionLabel_col,
                 decisionDescription_col,
                 decisionAlternatives_col);

  if (!file.exists(file)) {
    stop("The file you specified does not exist!");
  }

  dat <- openxlsx::read.xlsx(file);

  if (!all(allCols %in% names(dat))) {
    stop("You specified that you wanted to load scenario definitions from a ",
         "spreadsheet containing the identifiers in columns ",
         vecTxtQ(allCols), ", but ",
         "at least one of those does not exist in the file you ",
         "specified ('", file, "').");
  }

  scenarioIds <-
    setdiff(names(dat), allCols);

  res <-
    lapply(scenarioIds,
           function(scenarioId) {
             return(
               stats::setNames(
                 dat[, scenarioId],
                 dat[, decisionId_col]
               )
             );
           });

  names(res) <- scenarioIds;

  return(res);

}
