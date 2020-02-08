#' @export
load_weights_from_xl <- function(input) {

  weightsSheets <-
    readxl::excel_sheets(input);

  weightSheets <-
    grep('Scorer',
         weightsSheets,
         value=TRUE);

  individualWeights <- list();
  for (i in weightSheets) {
    suppressMessages(individualWeights[[i]] <-
      readxl::read_excel(input,
                         sheet = i));
  }

  ### Reject sheets with missing values and remove
  ### any additional columns
  individualWeights <-
    lapply(individualWeights,
           function(x) {
             ### To reject all empty sheets
             #if (length(unique(x$weight)) < 3) {
             if (any(is.na(x$weight))) {
               return(NULL);
             } else {
               return(x[, c("scorer",
                            "parentCriterion",
                            "id",
                            "weight",
                            "label")]);
             }
           });

  individualWeights <-
    individualWeights[!unlist(lapply(individualWeights, is.null))];

  ### Merge weights into one dataframe
  allWeights <-
    do.call(rbind,
            individualWeights);

  names(allWeights)[2:3] <-
    c('parentCriterion_id',
      'criterion_id');

  ### Get list of parent criteria (eliminate dash for the root)
  parentCriteria <-
    unique(allWeights$parentCriterion_id);
  parentCriteria <- parentCriteria[nchar(parentCriteria)>1];

  # allWeights$scorer <-
  #   gsub("^([a-zA-Z]+)([0-9])$",
  #        "\\10\\2",
  #        allWeights$scorer);

  ### Get number only
  allWeights$scorerNr <-
    gsub("[a-zA-Z]+([0-9]+)",
         "\\1",
         allWeights$scorer);

  ### Get unique texts and numbers for scorers
  scorerTxt <- unique(allWeights$scorer);
  scorerNrs <- unique(allWeights$scorerNr);

  ### Store scorers as ordered factor
  allWeights$scorer <-
    factor(allWeights$scorer,
           levels=scorerTxt[order(as.numeric(scorerNrs))],
           ordered=TRUE);

  ### Convert weight to numeric
  allWeights$weight <-
    ufs::convertToNumeric(allWeights$weight);

  res <- list(individualWeights = individualWeights,
              allWeights = allWeights,
              parentCriteria = parentCriteria,
              scorerTxt = scorerTxt,
              scorerNrs = scorerNrs);

  return(res);

}
