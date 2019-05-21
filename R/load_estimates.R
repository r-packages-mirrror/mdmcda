load_estimates <- function(path,
                           instruments_and_options,
                           outcomes) {

  instrumentsDf <-
    instruments_and_options$instrumentsDf;
  optionsDf <-
    instruments_and_options$optionsDf;
  outcomesDf <-
    outcomes$outcomesDf;

  ### Use suppressWarnings because we do not need identifiers
  suppressWarnings(
    estimates_raw <-
      justifier::load_justifications_dir(path)
  );

  ### Get all estimates
  estimates <-
    lapply(estimates_raw$supplemented$justifications,
           function(x) {
             if (!is.null(x$type) && x$type == "estimate") {
               return(x);
             } else {
               return(NULL);
             }
           });

  ### Get all assertions and sources
  assertions <-
    estimates_raw$supplemented$assertions;
  sources <-
    estimates_raw$supplemented$sources;

  ### Remove NULL elements
  estimates <-
    estimates[!unlist(lapply(estimates, is.null))];

  estimatesDf <-
    do.call(rbind,
            lapply(estimates,
                   function(x) {
                     instrument <- x$instrument;
                     res <-
                       data.frame(instrument_id = x$instrument_id,
                                  instrument_label = instrumentsDf[instrumentsDf$id==x$instrument_id, 'label'],
                                  instrument_option_value = x$instrument_option_value,
                                  option_label = optionsDf[optionsDf$instrument_id==x$instrument_id &
                                                             optionsDf$value==x$instrument_option_value, 'label'],
                                  outcome_id = x$outcome_id,
                                  outcome_label = outcomesDf[outcomesDf$id==x$outcome_id, 'label'],
                                  value = x$value,
                                  label = x$label,
                                  description = x$description,
                                  id = x$id,
                                  stringsAsFactors = FALSE);
                     return(res);
                   }));

  row.names(estimatesDf) <-
    NULL;

  res <- list(estimates_raw = estimates_raw,
              estimates = estimates,
              assertions = assertions,
              sources = sources,
              estimatesDf = estimatesDf);

  return(res);

}
