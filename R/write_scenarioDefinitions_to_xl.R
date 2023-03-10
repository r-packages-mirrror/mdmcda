#' Write scenario definitions to an Excel spreadsheet
#'
#' Scenario defintions are lists of vectors with selected alternatives.
#' Every vector is named, with the names being the decision identifiers of
#' each decision, and the values being the chosen alternative value for that
#' decision in that scenario.
#'
#' @param scenarioDefinitions The scenario definitions object.
#' @param file The file to write to.
#' @param scenarios Optionally, a selection of scenarios to export.
#'
#' @return Invisibly, the list of dataframes that's written to the file.
#' @export
write_scenarioDefinitions_to_xl <- function(scenarioDefinitions,
                                             file,
                                             scenarios = names(scenarioDefinitions)) {

  if (!dir.exists(dirname(file))) {
    stop("The file you specified is located in a directory that ",
         "does not exist (", dirname(file), ")!");
  }

  scenariosToWrite <-
    scenarioDefinitions[
      scenarios
    ];

  openxlsx::write.xlsx(
    stats::setNames(
      lapply(
        scenariosToWrite,
        function(namedVector) {
          return(
            data.frame(
              decision_id = names(namedVector),
              alternative_value = unname(namedVector)
            )
          );
        }
      ),
      nm = names(scenariosToWrite)
    ),
    file,
    overwrite = TRUE
  );

  return(invisible(scenariosToWrite));

}


