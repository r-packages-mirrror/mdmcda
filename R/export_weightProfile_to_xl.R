#' Export weight profiles to an Excel spreadsheet
#'
#' Weight profiles are lists of vectors with weights. Every vector is named,
#' with the names being the criterion identifiers of the criterion each weight
#' corresponds to. Every named vector (i.e. very weight profile) also has
#' an attribute that is another named vector: the weights of the criteria
#' clusters. Those are named vectors, too (again, with the names being the
#' criteria identifiers, but this time of the criteria clusters).
#'
#' @param weightProfiles The weightprofiles object, for example as created
#' by a call to [create_weight_profile()].
#' @param file The file to write to.
#'
#' @return Invisibly, the list of dataframes that's written to the file.
#' @export
export_weightProfile_to_xl <- function(weightProfiles,
                                       file) {
  if (!dir.exists(dirname(file))) {
    stop("The file you specified is located in a directory that ",
         "does not exist (", dirname(file), ")!");
  }

  dat <-
    lapply(
      weightProfiles,
      function(x) {
        return(
          rbind(
            data.frame(
              type = "criterion",
              weight = x,
              criterion_id = names(x)
            ),
            data.frame(
              type = "cluster",
              weight = attr(x, "criteriaClusterWeights"),
              criterion_id = names(attr(x, "criteriaClusterWeights"))
            )
          )
        );
      }
    );

  row.names(dat) <- NULL;

  openxlsx::write.xlsx(
    dat,
    file
  );

  return(invisible(dat));

}
