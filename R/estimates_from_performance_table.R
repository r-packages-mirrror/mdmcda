#' @export
estimates_from_performance_table <- function(performance_table,
                                             idprefix="ptimport_id_") {

  if (!('performance_table' %in% class(performance_table))) {
    stop("As argument 'performance_table', you have to provide a ",
         "performance table (sorry if that was not clear :-)). You ",
         "provided an object with class ", vecTxtQ(performance_table),
         ".");
  }

  estimatesDf <-
    data.frame();

  idcounter <- 0;
  for (i in 5:ncol(performance_table)) {
    ### Process column by column
    for (j in 2:nrow(performance_table)) {
      newrowNr <- nrow(estimatesDf) + 1;
      idcounter <- idcounter + 1;
      estimatesDf[newrowNr, 'decision_id'] <- performance_table[j, 1];
      estimatesDf[newrowNr, 'decision_label'] <- performance_table[j, 3];
      estimatesDf[newrowNr, 'decision_alternative_value'] <- performance_table[j, 2];
      estimatesDf[newrowNr, 'decision_alternative_label'] <- performance_table[j, 4];
      estimatesDf[newrowNr, 'criterion_id'] <- names(performance_table)[i];
      estimatesDf[newrowNr, 'criterion_label'] <- performance_table[1, i];
      estimatesDf[newrowNr, 'value'] <- performance_table[j, i];
      estimatesDf[newrowNr, 'label'] <- "Imported from performance table";
      estimatesDf[newrowNr, 'description'] <- "This estimation has no description because it was imported from a performance table.";
      estimatesDf[newrowNr, 'id'] <- paste0(idprefix, idcounter);
    }
  }

  res <- list(estimates_raw = NULL,
              estimates = NULL,
              assertions = NULL,
              sources = NULL,
              performance_table = performance_table,
              estimatesDf = estimatesDf);

  class(res) <-
    c("dmcda", "estimates");

  return(res);

}
