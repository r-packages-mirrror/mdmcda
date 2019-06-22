#' @export
estimates_from_performance_table <- function(performance_table) {

  if (!('performance_table' %in% class(performance_table))) {
    stop("As argument 'performance_table', you have to provide a ",
         "performance table (sorry if that was not clear :-)). You ",
         "provided an object with class ", vecTxtQ(performance_table),
         ".");
  }



  estimatesDf <-
    data.frame();



    do.call(rbind,
            lapply(estimates,
                   function(x) {
                     decision <- x$decision;
                     res <-
                       data.frame(decision_id = x$decision_id,
                                  decision_label = decisionsDf[decisionsDf$id==x$decision_id, 'label'],
                                  decision_alternative_value = x$decision_alternative_value,
                                  alternative_label = alternativesDf[alternativesDf$decision_id==x$decision_id &
                                                             alternativesDf$value==x$decision_alternative_value, 'label'],
                                  criterion_id = x$criterion_id,
                                  criterion_label = criteriaDf[criteriaDf$id==x$criterion_id, 'label'],
                                  value = x$value,
                                  label = x$label,
                                  description = x$description,
                                  id = x$id,
                                  stringsAsFactors = FALSE);
                     return(res);
                   }));

  row.names(estimatesDf) <-
    NULL;

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
