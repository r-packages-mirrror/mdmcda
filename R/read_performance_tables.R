#' @export
read_performance_tables <- function(input,
                                    extension="xlsx",
                                    regex=NULL,
                                    ignore="empty-performance-table",
                                    estimatesSheet="Estimates",
                                    confidencesSheet = "Confidences",
                                    sep=",",
                                    recursive=TRUE,
                                    silent=TRUE,
                                    ...) {

  criterionId_col          <- mdmcda::opts$get("criterionId_col");
  criterionLabel_col       <- mdmcda::opts$get("criterionLabel_col");
  criterionDescription_col <- mdmcda::opts$get("criterionDescription_col");
  parentCriterionId_col    <- mdmcda::opts$get("parentCriterionId_col");
  decisionId_col           <- mdmcda::opts$get("decisionId_col");
  decisionLabel_col        <- mdmcda::opts$get("decisionLabel_col");
  alternativeValue_col     <- mdmcda::opts$get("alternativeValue_col");
  alternativeLabel_col     <- mdmcda::opts$get("alternativeLabel_col");
  scenarioId_col           <- mdmcda::opts$get("scenarioId_col");
  weightProfileId_col      <- mdmcda::opts$get("weightProfileId_col");
  score_col                <- mdmcda::opts$get("score_col");
  leafCriterion_col        <- mdmcda::opts$get("leafCriterion_col");
  rootCriterionId          <- mdmcda::opts$get("rootCriterionId");

  if (!is.character(input) || !length(input)==1) {
    stop("Only specify a single string as 'input'!");
  }

  if (!dir.exists(input)) {
    stop("Directory provided to read from ('",
         input,
         "') does not exist!");
  }

  if (is.null(regex)) {
    regex <- paste0("^(.*)\\.", extension, "$");
  }

  if (grepl('\\.xls', regex)) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("To import from excel format, the \"readxl\" package is required.");
    }
  }

  ### Get list of files
  performanceTableFiles <-
    list.files(input,
               pattern=regex,
               full.names=TRUE,
               recursive = recursive);

  ### Remove files to ignore
  performanceTableFiles <-
    performanceTableFiles[!grepl(ignore,
                                 performanceTableFiles)];

  if (!silent) {
    cat(paste0("\nStarting to process files in directory '",
               input,
               "'. Specifically, Starting to process the following list of files:\n\n"));
    cat(paste0("- ", gsub(input, "", performanceTableFiles), "\n"));
    cat("\n\nStarting file processing.\n\n");
  }

  ### Load all performance tables and confidences
  res <- list(performance_subtables = list(),
              confidences = list(),
              estimateCoders = list());
  for (filename in performanceTableFiles) {
    if (!silent) {
      cat(paste0("\n  Starting to process file '",
                 basename(filename), "'.\n"));
    }
    currentTable <-
      read_performance_table(filename,
                             estimatesSheet=estimatesSheet,
                             confidencesSheet=confidencesSheet,
                             sep=sep,
                             ...);

    res$performance_subtables[[basename(filename)]] <-
      currentTable$estimates;
    res$confidences[[basename(filename)]] <-
      currentTable$confidences;
    res$estimateCoders[[basename(filename)]] <-
      attr(currentTable, "estimatorCode");
  }

  if (!silent) {
    cat("\nRead all files.");
  }

  ### Create estimatorCode vector
  # res$estimatorCodeVector <-
  #   unlist(lapply(res$performance_subtables,
  #                 function(x) {
  #                   return(attr(x, 'estimatorCode'));
  #                 }));
  ## This is superseded now that we do this in the loop above
  res$estimatorCodeVector <-
    unlist(res$estimateCoders);

  if (!silent) {
    cat("\nSetting the estimator code to 'all' where it was missing.");
  }

  res$estimatorCodeVector[is.na(res$estimatorCodeVector)] <-
    "all";

  if (!silent) {
    cat("\nRemoving duplicate estimator codes.");
  }

  res$estimatorCodes <- unique(na.omit(res$estimatorCodeVector));

  if (!silent) {
    cat0("\nFull final list of estimator codes: ", vecTxtQ(res$estimatorCodes), ".");
    cat("\nProcessing performance subtables to extract the estimates.");
  }

  ### Store estimate dataframes
  res$estimates <-
    lapply(res$performance_subtables,
           estimates_from_performance_table);

  ### Set identifier legend to combine estimates in one dataframe
  res$multiEstimateLegend <-
    res$estimatorCodeVector;
  res$multiEstimateInverseLegend <-
    names(res$multiEstimateLegend);
  names(res$multiEstimateInverseLegend) <-
    res$multiEstimateLegend;

  if (!silent) {
    cat("\nStarting to build dataframe with all estimates of all estimators.");
  }

  ### Combine estimates in one dataframe
  res$multiEstimateDf <- data.frame(decision_id = character(),
                                    decision_label = character(),
                                    alternative_value = character(),
                                    alternative_label = character(),
                                    criterion_id = character(),
                                    criterion_label = character(),
                                    stringsAsFactors = FALSE);

  names(res$multiEstimateDf) <-
    c(decisionId_col,
      decisionLabel_col,
      alternativeValue_col,
      alternativeLabel_col,
      criterionId_col,
      criterionLabel_col);

  for (i in names(res$multiEstimateLegend)) {
    if (!silent) {
      cat0("\n  Starting to process estimates for '",
                i, "'.");
    }
    for (j in 1:nrow(res$estimates[[i]]$estimatesDf)) {
      rowNr <-
        which(res$multiEstimateDf$decision_id %in% res$estimates[[i]]$estimatesDf[j, 'decision_id'] &
              res$multiEstimateDf[, alternativeValue_col] %in% res$estimates[[i]]$estimatesDf[j, alternativeValue_col] &
              res$multiEstimateDf$criterion_id %in% res$estimates[[i]]$estimatesDf[j, 'criterion_id']);
      if (is.null(rowNr) || !length(rowNr)) {
        rowNr <-
          nrow(res$multiEstimateDf) + 1;
      } else if (length(rowNr) > 1) {
        stop("Error: duplicate rows!");
      }
      res$multiEstimateDf[rowNr, 'decision_id'] <-
        res$estimates[[i]]$estimatesDf[j, 'decision_id'];
      res$multiEstimateDf[rowNr, 'decision_label'] <-
        res$estimates[[i]]$estimatesDf[j, 'decision_label'];
      res$multiEstimateDf[rowNr, alternativeValue_col] <-
        res$estimates[[i]]$estimatesDf[j, alternativeValue_col];
      res$multiEstimateDf[rowNr, alternativeLabel_col] <-
        res$estimates[[i]]$estimatesDf[j, alternativeLabel_col];
      res$multiEstimateDf[rowNr, 'criterion_id'] <-
        res$estimates[[i]]$estimatesDf[j, 'criterion_id'];
      res$multiEstimateDf[rowNr, 'criterion_label'] <-
        res$estimates[[i]]$estimatesDf[j, 'criterion_label'];
      res$multiEstimateDf[rowNr, res$multiEstimateLegend[i]] <-
        res$estimates[[i]]$estimatesDf[j, 'value'];
    }
  }

  if (!silent) {
    cat0("\nFinished building the 'multi estimate dataframe'. Starting to convert the ",
              "estimates to numeric.");
  }

  ### Converting estimates to numeric
  res$multiEstimateDf[, res$estimatorCodes] <-
    lapply(res$estimatorCodes,
           function(i) {
             x <-
               res$multiEstimateDf[, i];
             if (!silent) {
               cat0("\nStarting to process '",
                         res$multiEstimateInverseLegend[i],
                         "'.");
             }
             originalMissings <-
               is.na(x) | (toupper(x) == "NA");
             x <- ifelse(x=="NA",
                         NA,
                         x);
             suppressWarnings(y <- as.numeric(x));
             z <- as.character(y);
             ### z & z should now be equal. If not, z will have
             ### deviating values or missing values
             matchResults <-
               x == z;
             matchMissings <-
               which(is.na(matchResults));
             newMissings <-
               is.na(y);
             ### Set match results to TRUE if they are a missing value in the new
             ### vector and were also a missing value in the original
             matchResults[originalMissings == newMissings] <-
               TRUE;
             if (all(matchResults)) {
               return(y);
             } else {
               mismatches <- !matchResults;
               mismatchMsg <-
                 paste0(length(mismatches), " of the estimates loaded from '",
                        i, "' in file '",
                        res$multiEstimateInverseLegend[i],
                        "' cannot be converted to a numeric value. Specifically, this concerns ",
                        "alternative(s) ", res$multiEstimateDf[mismatches, alternativeValue_col],
                        " in decision(s) ", res$multiEstimateDf[mismatches, 'decision_id'],
                        " for criterion or criteria ", res$multiEstimateDf[mismatches, 'criterion_id'],
                        ", where ",
                        "the original version ('", x,"') is not identical to the version ",
                        "converted to numeric ('", y, "') and back to character ('", z, "').");
               if (!silent) {
                 cat("\n\n<<<WARNING>>>\n");
                 cat(mismatchMsg);
                 cat("\n\n");
               }
               warning(mismatchMsg);
               return(x);
             }
           });

  if (!silent) {
    cat0("\nDone converting the estimates to numeric.");
              #"Starting to generate complete performance tables. ");
  }

  ### Add merged confidences
  res$mergedConfidences <-
    dplyr::bind_rows(res$confidences, .id="performance_table");

  ### Clean up a bit
  res$mergedConfidences <-
    res$mergedConfidences[!is.na(res$mergedConfidences$Confidence), c("performance_table",
                                                                      "Scorer", "Confidence")];

  ### Derive decisions and criteria from performance subtable names
  res$mergedConfidences$decision <-
    gsub("performance_subtable_for_(.*)_on_.*$",
         "\\1",
         res$mergedConfidences$performance_table);
  res$mergedConfidences$criterion <-
    gsub("performance_subtable_for_.*_on_(.*)\\.xlsx$",
         "\\1",
         res$mergedConfidences$performance_table);

  ### Store aggregate indicators
  res$processedConfidences <-
    list(meanPerTable =
           unclass(by(res$mergedConfidences$Confidence,
                      res$mergedConfidences$performance_table,
                      mean, na.rm=TRUE)),
         meanPerScorer =
           unclass(by(res$mergedConfidences$Confidence,
                      res$mergedConfidences$Scorer,
                      mean, na.rm=TRUE)),
         sdPerTable =
           unclass(by(res$mergedConfidences$Confidence,
                      res$mergedConfidences$performance_table,
                      sd, na.rm=TRUE)),
         sdPerScorer =
           unclass(by(res$mergedConfidences$Confidence,
                      res$mergedConfidences$Scorer,
                      sd, na.rm=TRUE)));
  res$processedConfidences$meanPerTable <-
    data.frame(performance_table = names(res$processedConfidences$meanPerTable),
               confidence_mean = res$processedConfidences$meanPerTable);
  res$processedConfidences$meanPerScorer <-
    data.frame(scorer = names(res$processedConfidences$meanPerScorer),
               confidence_mean = res$processedConfidences$meanPerScorer);
  res$processedConfidences$sdPerTable <-
    data.frame(performance_table = names(res$processedConfidences$sdPerTable),
               confidence_sd = res$processedConfidences$sdPerTable);
  res$processedConfidences$sdPerScorer <-
    data.frame(scorer = names(res$processedConfidences$sdPerScorer),
               confidence_sd = res$processedConfidences$sdPerScorer);

  # ### Generate complete performance_tables
  # res$performance_tables <- list();
  # for (i in res$estimatorCodes) {
  #      res$performance_tables[[i]] <-
  #     estimateDf_to_performance_table(res$multiEstimateDf,
  #                                     valueCol = i);
  #   res$performance_tables[[i]][1,1] <- i;
  # }
  #
  # numericValues <-
  #   res$estimatorCodes[
  #     unlist(lapply(res$multiEstimateDf[, res$estimatorCodes], is.numeric))
  #   ];
  #
  # if (length(numericValues) > 1) {
  #
  #   res$multiEstimateDf$variance <-
  #     apply(res$multiEstimateDf[, numericValues],
  #           1,
  #           stats::var,
  #           na.rm=TRUE);
  #
  #   res$multiEstimateDf$value_mean <-
  #     apply(res$multiEstimateDf[, numericValues],
  #           1,
  #           mean,
  #           na.rm=TRUE);
  #
  #   res$performance_table_var <-
  #     estimateDf_to_performance_table(res$multiEstimateDf,
  #                                     valueCol = "variance");
  #
  #   res$performance_table_mean <-
  #     estimateDf_to_performance_table(res$multiEstimateDf,
  #                                     valueCol = "value_mean");
  #
  #
  #   ### Get consensus matrix
  #   res$consensusDf <-
  #     res$multiEstimateDf[, c('criterion_label', 'variance')];
  #   res$consensusDf$selectedAlternative <-
  #     paste(res$multiEstimateDf$decision_label,
  #           res$multiEstimateDf$decision_alternative_label,
  #           sep=":\n");
  #
  #   precision <- max(nchar(as.character(res$multiEstimateDf$value_mean)));
  #
  #   res$consensusDf$formattedMean <-
  #     round(res$multiEstimateDf$value_mean,
  #           min(precision, 2));
  #
  #   res$consensusDf$invertedVariance <-
  #     max(res$multiEstimateDf$variance) -
  #     res$multiEstimateDf$variance;
  #
  #   res$consensusMap <-
  #     ggplot2::ggplot(res$consensusDf,
  #                     mapping=ggplot2::aes_string(x='criterion_label',
  #                                                 y='selectedAlternative',
  #                                                 fill='variance',
  #                                                 label = 'formattedMean')) +
  #     ggplot2::geom_tile() +
  #     ggplot2::geom_text(mapping=ggplot2::aes_string(color='invertedVariance')) +
  #     ggplot2::scale_fill_viridis_c(name = "Variance\n(disagreement)") +
  #     ggplot2::scale_color_viridis_c(name = "Variance\n(disagreement)") +
  #     ggplot2::theme_minimal() +
  #     ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90, hjust=1)) +
  #     ggplot2::labs(title = "Consensus Map",
  #                   subtitle = paste0("Based on ", length(numericValues),
  #                                     " estimates from a DMCDA"),
  #                   x = "Criteria",
  #                   y = "Decisions");
  #
  # } else {
  #   consensusDf <- NA;
  #   consensusMap <- NA;
  # }

  res$alternativeValues <-
    lapply(by(res$multiEstimateDf,
              res$multiEstimateDf$decision_id,
              function(x) {
                uniqueAlts <- unique(x[, c(alternativeValue_col,
                                           alternativeLabel_col)]);
                return(stats::setNames(uniqueAlts[, alternativeLabel_col],
                                       nm=as.character(uniqueAlts[, alternativeValue_col])));
              }),
           as.list);

  return(invisible(res));
}
