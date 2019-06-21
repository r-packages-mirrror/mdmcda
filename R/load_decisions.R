#' @export
load_decisions <- function(path) {

  ### Use suppressWarnings because we do not need identifiers
  suppressWarnings(
    decisions_raw <-
      justifier::load_justifications_dir(path)
  );

  ### Get all policy domains
  decisions <-
    lapply(decisions_raw$supplemented$assertions,
           function(x) {
             if (!is.null(x$type) && x$type == "decision") {
               return(x);
             } else {
               return(NULL);
             }
           });

  ### Remove NULL elements
  decisions <-
    decisions[!unlist(lapply(decisions, is.null))];

  decisionsDf <-
    do.call(rbind,
            lapply(decisions,
                   function(x) {
                     return(data.frame(id = x$id,
                                       label = x$label,
                                       description = x$description,
                                       choices = vecTxtQ(purrr::map_chr(x$allowedValues,
                                                                             "label")),
                                       stringsAsFactors = FALSE));
                   }));
  row.names(decisionsDf) <-
    NULL;

  alternativesDf <-
    do.call(rbind,
            lapply(decisions,
                   function(x) {
                     return(data.frame(decision_id = rep(x$id, length(x$allowedValues)),
                                       decision_label = rep(x$label, length(x$allowedValues)),
                                       value = purrr::map_chr(x$allowedValues,
                                                              "value"),
                                       label = purrr::map_chr(x$allowedValues,
                                                              "label"),
                                       stringsAsFactors = FALSE));
                   }));
  row.names(alternativesDf) <-
    NULL;

  res <- list(decisions_raw = decisions_raw,
              decisions = decisions,
              decisionsDf = decisionsDf,
              alternativesDf = alternativesDf);

  class(res) <-
    c("dmcda", "decisions_and_alternatives");

  return(res);

}
