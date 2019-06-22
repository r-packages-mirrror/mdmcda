#' @export
decisions_from_performance_table <- function(performance_table) {

  if (!('performance_table' %in% class(performance_table))) {
    stop("As argument 'performance_table', you have to provide a ",
         "performance table (sorry if that was not clear :-)). You ",
         "provided an object with class ", vecTxtQ(performance_table),
         ".");
  }

  decision_ids <-
    performance_table[1, 3:nrow(performance_table)];
  decision_labels <-
    performance_table[3, 3:nrow(performance_table)];
  alternative_values <-
    performance_table[2, 3:nrow(performance_table)];
  alternative_labels <-
    performance_table[4, 3:nrow(performance_table)];

  res <-
    list(decision_ids=decision_ids,
         decision_labels=decision_labels,
         alternative_values=alternative_values,
         alternative_labels=alternative_labels);

  return(res);

}
