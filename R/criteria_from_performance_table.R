#' @export
criteria_from_performance_table <- function(performance_table) {

  if (!('performance_table' %in% class(performance_table))) {
    stop("As argument 'performance_table', you have to provide a ",
         "performance table (sorry if that was not clear :-)). You ",
         "provided an object with class ", vecTxtQ(performance_table),
         ".");
  }

  res <-
    performance_table[2, 5:ncol(performance_table)];

  names(res) <-
    performance_table[1, 5:ncol(performance_table)];

  return(res);

}
