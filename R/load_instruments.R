#' @export
load_instruments <- function(path) {

  ### Use suppressWarnings because we do not need identifiers
  suppressWarnings(
    instruments_raw <-
      justifier::load_justifications_dir(path)
  );

  ### Get all policy domains
  instruments <-
    lapply(instruments_raw$supplemented$assertions,
           function(x) {
             if (!is.null(x$type) && x$type == "instrument") {
               return(x);
             } else {
               return(NULL);
             }
           });

  ### Remove NULL elements
  instruments <-
    instruments[!unlist(lapply(instruments, is.null))];

  instrumentsDf <-
    do.call(rbind,
            lapply(instruments,
                   function(x) {
                     return(data.frame(id = x$id,
                                       label = x$label,
                                       description = x$description,
                                       choices = ufs::vecTxtQ(purrr::map_chr(x$allowedValues,
                                                                             "label")),
                                       stringsAsFactors = FALSE));
                   }));
  row.names(instrumentsDf) <-
    NULL;

  optionsDf <-
    do.call(rbind,
            lapply(instruments,
                   function(x) {
                     return(data.frame(instrument_id = rep(x$id, length(x$allowedValues)),
                                       instrument_label = rep(x$label, length(x$allowedValues)),
                                       value = purrr::map_chr(x$allowedValues,
                                                              "value"),
                                       label = purrr::map_chr(x$allowedValues,
                                                              "label"),
                                       stringsAsFactors = FALSE));
                   }));
  row.names(optionsDf) <-
    NULL;

  res <- list(instruments_raw = instruments_raw,
              instruments = instruments,
              instrumentsDf = instrumentsDf,
              optionsDf = optionsDf);

  class(res) <-
    c("dmcda", "instruments_and_options");

  return(res);

}
