#' @export

### Add classes to estimates$multiEstimateDf and to weighedEstimates
### Then make add_weights smart

add_weights <- function(weighedEstimates,
                        weightProfiles,
                        weightProfileNames) {

  for (i in weightProfileNames) {
    weighedEstimates[, paste0(i, "_weight")] <-
      weightProfiles[[i]][weighedEstimates$criterion_id];
    weighedEstimates[, paste0(i, "_weighed_estimate")] <-
      weighedEstimates$estimate *
      weighedEstimates[, paste0(i, "_weight")];
  }

  return(weighedEstimates);

}
