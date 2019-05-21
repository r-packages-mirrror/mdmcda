load_weights <- function(path,
                         outcomes) {

  outcomesTree <- outcomes$outcomesTree;
  outcomesDf <- outcomes$outcomesDf;

  ### Use suppressWarnings because we do not need identifiers
  suppressWarnings(
    weights_raw <-
      justifier::load_justifications_dir(path)
  );

  ### Get all policy instrument weights
  weights <-
    lapply(weights_raw$supplemented$justifications,
           function(x) {
             if (x$type == "outcome_weight") {
               return(x);
             } else {
               return(NULL);
             }
           });

  ### Remove NULL elements
  weights <-
    weights[!unlist(lapply(weights, is.null))];

  weightsDf <-
    do.call(rbind,
            lapply(weights,
                   function(x) {
                     res <-
                       data.frame(x$weight_profile_id,
                                  x$outcome_id,
                                  unname(outcomesDf[outcomesDf$id==x$outcome_id, 'label']),
                                  x$weight,
                                  stringsAsFactors = FALSE);
                     names(res) <-
                       c('weight_profile_id',
                         'outcome_id',
                         'outcome_label',
                         'weight');
                     return(res);
                   }));

  row.names(weightsDf) <-
    NULL;

  ### Multiply the weights

  multipliedWeights <-
    lapply(unique(weightsDf$weight_profile_id),
           function(profile) {
             res <-
               data.frame(outcome_id = weightsDf[weightsDf$weight_profile_id==profile, 'outcome_id'],
                          multipliedWeight = unlist(lapply(weightsDf[weightsDf$weight_profile_id==profile, 'outcome_id'],
                                                           function(id) {
                                                             idList <- data.tree::FindNode(outcomesTree,
                                                                                           id)$Get("id",
                                                                                                   traversal="ancestor");
                                                             return(prod(weightsDf[weightsDf$weight_profile_id==profile &
                                                                                     weightsDf$outcome_id %in% idList, 'weight']));
                                                           })));
             res$profile_id <-
               rep(profile,
                   nrow(res));
             return(res);
           });

  multipliedWeights <-
    do.call(rbind,
            multipliedWeights);

  res <- list(weights_raw = weights_raw,
              weights = weights,
              weightsDf = weightsDf,
              multipliedWeights = multipliedWeights);

  return(res);

}
